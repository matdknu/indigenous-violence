# =============================================================================
# 04_robustez.R — Balance, propensity score y análisis placebo
#
# Propósito: fortalecer el diseño cuasi-experimental con diagnósticos de
#            balance pretratamiento, matching/IPW, placebo ola 1–2 y modelos
#            por ítem.
# Input:     data/subset_data.rds
#            data/panel_completo.rds (placebo olas 1–2)
#            data/modelos.rds (opcional; re-estima si no existe)
# Output:    output/tablas/tabla_balance_ola2.html
#            output/figuras/fig_loveplot_balance_pre.png
#            output/tablas/tabla_robustez_psm.html
#            output/tablas/tabla_robustez_ipw.html
#            output/tablas/tabla_robustez_ipw_trimming.html
#            output/figuras/fig_hist_pesos_ipw.png
#            output/figuras/fig_loveplot_ipw.png
#            output/figuras/fig_robustez_comparativa.png
#            output/tablas/tabla_placebo_pretratamiento.html
#            output/tablas/tabla_modelos_por_item.html
#            output/tablas/tabla_resumen_robustez.{csv,html}
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, broom.mixed, broom,
  modelsummary, ggplot2, stringr, gt, haven,
  MatchIt, cobalt, WeightIt, fixest
)
# DRDID opcional (Fase C)
has_drdid <- requireNamespace("DRDID", quietly = TRUE)
if (!has_drdid) {
  tryCatch(install.packages("DRDID", repos = "https://cloud.r-project.org"),
           error = function(e) NULL)
  has_drdid <- requireNamespace("DRDID", quietly = TRUE)
}

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

# ── Utilidades ────────────────────────────────────────────────────────────────

TERM_DID <- "periododecreto:indigeneousindi:cerca_conflictocerca"
TERM_DID_ESTALLIDO <- "periodoestallido:indigeneousindi:cerca_conflictocerca"
TERM_DID_DECRETO   <- "periododecreto:indigeneousindi:cerca_conflictocerca"
TERM_DID_A         <- "T1_estallido:indigeneousindi:cerca_conflictocerca"
TERM_DID_B         <- "T2_decreto:indigeneousindi:cerca_conflictocerca"
TERM_PLACEBO_REAL  <- "T_placebo:indigeneousindi:cerca_conflictocerca"
TERM_NUCLEO_DID    <- "post_decreto:indigeneousindi:nucleo_conflictonucleo"

signif_stars <- function(p) {
  case_when(
    is.na(p)       ~ "",
    p < 0.001      ~ "***",
    p < 0.01       ~ "**",
    p < 0.05       ~ "*",
    p < 0.1        ~ "+",
    TRUE           ~ ""
  )
}

extract_coef <- function(model, term_interest, modelo, variable_dependiente) {
  empty <- tibble(
    modelo = modelo,
    variable_dependiente = variable_dependiente,
    term = term_interest,
    estimate = NA_real_,
    std.error = NA_real_,
    p.value = NA_real_,
    signif = ""
  )
  if (is.null(model)) return(empty)
  td <- tryCatch(
    broom.mixed::tidy(model, effects = "fixed"),
    error = function(e) {
      tryCatch(broom::tidy(model), error = function(e2) tibble())
    }
  )
  if (!nrow(td) || !"term" %in% names(td)) return(empty)
  # feols puede usar i() names; permitir match parcial
  row <- td |> filter(.data$term == .env$term_interest)
  if (nrow(row) == 0) {
    row <- td |> filter(grepl(term_interest, .data$term, fixed = TRUE) |
                          grepl("periododecreto.*indi.*cerca|decreto.*indi.*cerca",
                                .data$term))
  }
  if (nrow(row) == 0) return(empty)
  pval <- if ("p.value" %in% names(row)) row$p.value[1] else NA_real_
  tibble(
    modelo = modelo,
    variable_dependiente = variable_dependiente,
    term = term_interest,
    estimate = row$estimate[1],
    std.error = row$std.error[1],
    p.value = pval,
    signif = signif_stars(pval)
  )
}

bal_tab_to_gt <- function(bal_obj, title) {
  bal_df <- as.data.frame(bal_obj$Balance) |>
    rownames_to_column("Covariate") |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  bal_df |>
    gt() |>
    tab_header(title = title) |>
    tab_options(
      table.border.top.style = "solid",
      table.border.bottom.style = "solid",
      table.font.size = px(11)
    )
}

formula_did <- function(y, controles) {
  as.formula(paste0(
    y, " ~ periodo * indigeneous * cerca_conflicto + ",
    controles, " + (1 | folio)"
  ))
}

formula_placebo_real <- function(y, controles) {
  as.formula(paste0(
    y, " ~ T_placebo * indigeneous * cerca_conflicto + ",
    controles, " + (1 | folio)"
  ))
}

formula_nucleo <- function(y, controles) {
  as.formula(paste0(
    y, " ~ post_decreto * indigeneous * nucleo_conflicto + ",
    controles, " + (1 | folio)"
  ))
}

# ── Carga de datos y controles ────────────────────────────────────────────────

subset_data <- readRDS("data/subset_data.rds")

cor_ur_cc <- cor(
  as.numeric(subset_data$urbano_rural),
  as.numeric(subset_data$cerca_conflicto),
  use = "complete.obs"
)
incluir_urbano_rural <- abs(cor_ur_cc) <= 0.5

if (file.exists("data/analysis_metadata.rds")) {
  metadata <- readRDS("data/analysis_metadata.rds")
  controles_base <- metadata$controles_base
} else {
  # malestar_diferen excluida por >5% NA (REGLA 3)
  controles_base <- if (incluir_urbano_rural) {
    "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + apoyo_movil"
  } else {
    "mujer + edad + id_chile + id_causa + perc_desigualdad + apoyo_movil"
  }
}

# PS sin outcomes baseline (evita condicionar por Y pre en matching/IPW)
ps_formula <- as.formula(paste(
  "tratado_zona ~ indigeneous + mujer + edad + urbano_rural +",
  "id_chile + id_causa + perc_desigualdad"
))

ps_covars <- c(
  "indigeneous", "mujer", "edad", "urbano_rural",
  "id_chile", "id_causa", "perc_desigualdad"
)

# Modelos principales (cargar o re-estimar)
if (file.exists("data/modelos.rds")) {
  modelos <- readRDS("data/modelos.rds")
  mC_ctrl <- modelos$mC_ctrl
  mC_resg <- modelos$mC_resg
  mA_ctrl <- modelos$mA_ctrl
  mA_resg <- modelos$mA_resg
  mB_ctrl <- modelos$mB_ctrl
  mB_resg <- modelos$mB_resg
  mFE_ctrl <- modelos$mFE_ctrl
  mFE_resg <- modelos$mFE_resg
  coef_rename_did <- modelos$coef_rename_did
  m2_ctrl <- mC_ctrl
  m2_resg <- mC_resg
  cat("✓ Modelos A/B/C + FE magro cargados desde data/modelos.rds\n")
} else {
  cat("⚠ data/modelos.rds no encontrado; re-estimando modelos principales...\n")
  m2_ctrl <- lmer(
    formula_did("idx_vio_control", controles_base),
    data = subset_data, REML = FALSE
  )
  m2_resg <- lmer(
    formula_did("idx_vio_resguardo", controles_base),
    data = subset_data, REML = FALSE
  )
}

cat("\n", strrep("=", 60), "\n")
cat("04_robustez.R — BALANCE, PSM, IPW Y PLACEBO\n")
cat(strrep("=", 60), "\n\n")

# ── 15. ROBUSTEZ: BALANCE, PROPENSITY SCORE Y PLACEBO ─────────────────────────

# ── 15.1 Balance en ola 2 ─────────────────────────────────────────────────────

cat("--- 15.1 Balance pretratamiento (ola 2) ---\n\n")

baseline_ola2 <- subset_data |>
  filter(ola == 2) |>
  mutate(tratado_zona = as.integer(cerca_conflicto == "cerca"))

baseline_cc <- baseline_ola2 |>
  filter(if_all(all_of(ps_covars), ~ !is.na(.x)))

cat("N ola 2 (total):", nrow(baseline_ola2), "\n")
cat("N ola 2 (casos completos PS):", nrow(baseline_cc), "\n")
cat("Tratados / control:", paste(table(baseline_cc$tratado_zona), collapse = " / "), "\n\n")

bal_pre <- bal.tab(
  ps_formula,
  data = baseline_cc,
  estimand = "ATE",
  stats = c("m", "v"),
  abs = TRUE,
  un = TRUE,
  thresholds = c(m = 0.1)
)

print(bal_pre)

bal_tab_to_gt(bal_pre, "Balance pretratamiento — ola 2 (SMD)") |>
  gtsave("output/tablas/tabla_balance_ola2.html")
cat("✓ Tabla balance guardada: output/tablas/tabla_balance_ola2.html\n")

png("output/figuras/fig_loveplot_balance_pre.png", width = 2400, height = 1600, res = 300)
love.plot(
  bal_pre,
  abs = TRUE,
  thresholds = c(m = 0.1),
  title = "Balance pretratamiento (ola 2) — Love plot",
  var.order = "unadjusted"
)
dev.off()
cat("✓ Love plot pre guardado: output/figuras/fig_loveplot_balance_pre.png\n\n")

# ── 15.2 Propensity score matching ────────────────────────────────────────────

cat("--- 15.2 Propensity score matching (nearest neighbor) ---\n\n")

m2_ctrl_matched <- NULL
m2_resg_matched <- NULL
m_psm <- NULL

if (min(table(baseline_cc$tratado_zona)) < 5) {
  cat("⚠ Muy pocos casos en algún grupo; PSM omitido.\n\n")
} else {
  set.seed(2024)
  m_psm <- tryCatch(
    matchit(
      ps_formula,
      data = baseline_cc,
      method = "nearest",
      distance = "logit",
      caliper = 0.2,
      ratio = 1,
      replace = FALSE
    ),
    error = function(e) {
      cat("⚠ Error en matchit:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (!is.null(m_psm)) {
    cat("Resumen MatchIt (caliper=0.2):\n")
    print(summary(m_psm))
    n_unmatched <- sum(m_psm$weights == 0 & baseline_cc$tratado_zona == 1)
    cat("Tratados sin pareja (caliper):", n_unmatched, "\n")

    bal_psm <- bal.tab(m_psm, un = TRUE, abs = TRUE, thresholds = c(m = 0.1))
    cat("\nBalance post-matching:\n")
    print(bal_psm)

    matched_ola2 <- match.data(m_psm)
    folios_matched <- unique(matched_ola2$folio)
    subset_matched <- subset_data |> filter(folio %in% folios_matched)

    cat("\nFolios emparejados:", length(folios_matched), "\n")
    cat("Observaciones panel emparejado:", nrow(subset_matched), "\n\n")

    m2_ctrl_matched <- lmer(
      formula_did("idx_vio_control", controles_base),
      data = subset_matched,
      REML = FALSE
    )
    m2_resg_matched <- lmer(
      formula_did("idx_vio_resguardo", controles_base),
      data = subset_matched,
      REML = FALSE
    )

    coef_psm_ctrl <- extract_coef(
      m2_ctrl_matched, TERM_DID, "PSM matched", "idx_vio_control"
    )
    coef_psm_resg <- extract_coef(
      m2_resg_matched, TERM_DID, "PSM matched", "idx_vio_resguardo"
    )

    cat("Coeficiente DiD PSM — control social:\n")
    print(coef_psm_ctrl)
    cat("\nCoeficiente DiD PSM — resguardo territorial:\n")
    print(coef_psm_resg)
    cat("\n")

    modelsummary(
      list(
        "Control (principal)" = m2_ctrl,
        "Control (PSM)"       = m2_ctrl_matched,
        "Resguardo (principal)" = m2_resg,
        "Resguardo (PSM)"       = m2_resg_matched
      ),
      statistic = "({std.error})",
      stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
      fmt = 3,
      gof_map = c("nobs", "icc", "rmse"),
      output = "output/tablas/tabla_robustez_psm.html"
    )
    cat("✓ Tabla PSM guardada: output/tablas/tabla_robustez_psm.html\n\n")
  }
}

# ── 15.3 Propensity score weighting (IPW) ─────────────────────────────────────

cat("--- 15.3 Inverse probability weighting (IPW) ---\n\n")

m2_ctrl_ipw <- NULL
m2_resg_ipw <- NULL
m2_ctrl_ipw_trim_1_99 <- NULL
m2_resg_ipw_trim_1_99 <- NULL
m2_ctrl_ipw_trim_5_95 <- NULL
m2_resg_ipw_trim_5_95 <- NULL
w_ipw <- NULL
resumen_ipw_trimming <- NULL

summarizar_pesos <- function(w, label = "IPW") {
  w <- w[!is.na(w)]
  tibble(
    especificacion = label,
    n = length(w),
    min = min(w),
    p01 = unname(quantile(w, 0.01)),
    p05 = unname(quantile(w, 0.05)),
    media = mean(w),
    mediana = median(w),
    p95 = unname(quantile(w, 0.95)),
    p99 = unname(quantile(w, 0.99)),
    max = max(w)
  )
}

# IPW con feols + cluster comunal (SE sandwich; NO weights= de lme4)
fit_ipw_did <- function(data, weights_col, controles) {
  dat <- data
  dat$.w_ipw <- as.numeric(dat[[weights_col]])
  dat <- dat |>
    dplyr::filter(!is.na(.data$.w_ipw), .data$.w_ipw > 0, !is.na(.data$comuna_cod))
  # Interacciones DiD como factores para feols
  f_ctrl <- as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +", controles
  ))
  f_resg <- as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +", controles
  ))
  list(
    ctrl = tryCatch(
      feols(f_ctrl, data = dat, weights = ~.w_ipw, cluster = ~comuna_cod),
      error = function(e) {
        cat("⚠ feols IPW ctrl:", conditionMessage(e), "\n")
        NULL
      }
    ),
    resg = tryCatch(
      feols(f_resg, data = dat, weights = ~.w_ipw, cluster = ~comuna_cod),
      error = function(e) {
        cat("⚠ feols IPW resg:", conditionMessage(e), "\n")
        NULL
      }
    )
  )
}

if (min(table(baseline_cc$tratado_zona)) < 5) {
  cat("⚠ Muy pocos casos en algún grupo; IPW omitido.\n\n")
} else {
  set.seed(2024)
  w_ipw <- tryCatch(
    weightit(
      ps_formula,
      data = baseline_cc,
      method = "ps",
      estimand = "ATE"
    ),
    error = function(e) {
      cat("⚠ Error en weightit:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (!is.null(w_ipw)) {
    bal_ipw <- bal.tab(w_ipw, un = TRUE, abs = TRUE, thresholds = c(m = 0.1))
    cat("Balance IPW (ponderado vs. no ponderado):\n")
    print(bal_ipw)

    png("output/figuras/fig_loveplot_ipw.png", width = 2400, height = 1600, res = 300)
    love.plot(
      bal_ipw,
      abs = TRUE,
      thresholds = c(m = 0.1),
      title = "Balance IPW — Love plot (ola 2)"
    )
    dev.off()
    cat("✓ Love plot IPW guardado: output/figuras/fig_loveplot_ipw.png\n")

    pesos_ola2 <- baseline_cc |>
      mutate(w_ipw = w_ipw$weights) |>
      select(folio, w_ipw)

    # ── 15.3.1 Diagnóstico de pesos IPW ───────────────────────────────────────

    cat("\n--- Diagnóstico pesos IPW (ola 2, casos completos) ---\n\n")
    cat("summary(w_ipw$weights):\n")
    print(summary(pesos_ola2$w_ipw))

    diag_pesos <- summarizar_pesos(pesos_ola2$w_ipw, "IPW original")
    cat("\nResumen ampliado:\n")
    print(diag_pesos)

  w_lo <- quantile(pesos_ola2$w_ipw, 0.01, na.rm = TRUE)
  w_hi <- quantile(pesos_ola2$w_ipw, 0.99, na.rm = TRUE)
  w_lo5 <- quantile(pesos_ola2$w_ipw, 0.05, na.rm = TRUE)
  w_hi5 <- quantile(pesos_ola2$w_ipw, 0.95, na.rm = TRUE)

  cat("\nUmbrales trimming:\n")
  cat("  1–99%: [", round(w_lo, 4), ", ", round(w_hi, 4), "]\n", sep = "")
  cat("  5–95%: [", round(w_lo5, 4), ", ", round(w_hi5, 4), "]\n\n", sep = "")

  pesos_ola2 <- pesos_ola2 |>
    mutate(
      w_ipw_trim_1_99 = pmin(pmax(w_ipw, w_lo), w_hi),
      w_ipw_trim_5_95 = pmin(pmax(w_ipw, w_lo5), w_hi5)
    )

  diag_pesos <- bind_rows(
    diag_pesos,
    summarizar_pesos(pesos_ola2$w_ipw_trim_1_99, "IPW trim 1–99%"),
    summarizar_pesos(pesos_ola2$w_ipw_trim_5_95, "IPW trim 5–95%")
  )
  cat("\nComparación pesos original vs. truncados:\n")
  print(diag_pesos)
  cat("\n")

  p_hist <- ggplot(pesos_ola2, aes(x = w_ipw)) +
    geom_histogram(bins = 40, fill = "#4575B4", color = "white", alpha = 0.85) +
    geom_vline(xintercept = c(w_lo, w_hi), linetype = "dashed", color = "#D73027") +
    labs(
      title = "Distribución de pesos IPW (ola 2)",
      subtitle = "Líneas rojas = percentiles 1 y 99",
      x = "Peso IPW", y = "Frecuencia"
    ) +
    theme_minimal(base_size = 12)

  ggsave("output/figuras/fig_hist_pesos_ipw.png", p_hist, width = 8, height = 5, dpi = 300)
  cat("✓ Histograma pesos guardado: output/figuras/fig_hist_pesos_ipw.png\n")

    subset_weighted <- subset_data |>
      left_join(pesos_ola2, by = "folio") |>
      filter(!is.na(w_ipw))

    cat("Observaciones panel ponderado:", nrow(subset_weighted), "\n\n")

    ipw_orig <- fit_ipw_did(subset_weighted, "w_ipw", controles_base)
    m2_ctrl_ipw <- ipw_orig$ctrl
    m2_resg_ipw <- ipw_orig$resg

    ipw_t199 <- fit_ipw_did(subset_weighted, "w_ipw_trim_1_99", controles_base)
    m2_ctrl_ipw_trim_1_99 <- ipw_t199$ctrl
    m2_resg_ipw_trim_1_99 <- ipw_t199$resg

    ipw_t595 <- fit_ipw_did(subset_weighted, "w_ipw_trim_5_95", controles_base)
    m2_ctrl_ipw_trim_5_95 <- ipw_t595$ctrl
    m2_resg_ipw_trim_5_95 <- ipw_t595$resg

    coef_ipw_ctrl <- extract_coef(
      m2_ctrl_ipw, TERM_DID, "IPW original", "idx_vio_control"
    )
    coef_ipw_resg <- extract_coef(
      m2_resg_ipw, TERM_DID, "IPW original", "idx_vio_resguardo"
    )
    coef_ipw_t199_ctrl <- extract_coef(
      m2_ctrl_ipw_trim_1_99, TERM_DID, "IPW trim 1–99%", "idx_vio_control"
    )
    coef_ipw_t199_resg <- extract_coef(
      m2_resg_ipw_trim_1_99, TERM_DID, "IPW trim 1–99%", "idx_vio_resguardo"
    )
    coef_ipw_t595_ctrl <- extract_coef(
      m2_ctrl_ipw_trim_5_95, TERM_DID, "IPW trim 5–95%", "idx_vio_control"
    )
    coef_ipw_t595_resg <- extract_coef(
      m2_resg_ipw_trim_5_95, TERM_DID, "IPW trim 5–95%", "idx_vio_resguardo"
    )

    resumen_ipw_trimming <- bind_rows(
      coef_ipw_ctrl, coef_ipw_resg,
      coef_ipw_t199_ctrl, coef_ipw_t199_resg,
      coef_ipw_t595_ctrl, coef_ipw_t595_resg
    ) |>
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        p.value = round(p.value, 4)
      )

    cat("Coeficiente DiD IPW — comparación trimming (", TERM_DID, "):\n\n", sep = "")
    print(resumen_ipw_trimming)
    cat("\n")

    cat("Coeficiente DiD IPW original — control social:\n")
    print(coef_ipw_ctrl)
    cat("\nCoeficiente DiD IPW original — resguardo territorial:\n")
    print(coef_ipw_resg)
    cat("\n")

    modelsummary(
      list(
        "Control (principal)"     = m2_ctrl,
        "Control (IPW original)"  = m2_ctrl_ipw,
        "Control (IPW trim 1–99)" = m2_ctrl_ipw_trim_1_99,
        "Control (IPW trim 5–95)" = m2_ctrl_ipw_trim_5_95,
        "Resguardo (principal)"   = m2_resg,
        "Resguardo (IPW original)"  = m2_resg_ipw,
        "Resguardo (IPW trim 1–99)" = m2_resg_ipw_trim_1_99,
        "Resguardo (IPW trim 5–95)" = m2_resg_ipw_trim_5_95
      ),
      statistic = "({std.error})",
      stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
      fmt = 3,
      gof_map = c("nobs", "icc", "rmse"),
      output = "output/tablas/tabla_robustez_ipw.html"
    )
    cat("✓ Tabla IPW guardada: output/tablas/tabla_robustez_ipw.html\n")

    modelsummary(
      list(
        "Ctrl IPW original"  = m2_ctrl_ipw,
        "Ctrl IPW trim 1–99" = m2_ctrl_ipw_trim_1_99,
        "Ctrl IPW trim 5–95" = m2_ctrl_ipw_trim_5_95,
        "Resg IPW original"  = m2_resg_ipw,
        "Resg IPW trim 1–99" = m2_resg_ipw_trim_1_99,
        "Resg IPW trim 5–95" = m2_resg_ipw_trim_5_95
      ),
      statistic = "({std.error})",
      stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
      fmt = 3,
      gof_map = c("nobs", "icc", "rmse"),
      output = "output/tablas/tabla_robustez_ipw_trimming.html"
    )
    cat("✓ Tabla IPW trimming guardada: output/tablas/tabla_robustez_ipw_trimming.html\n\n")

    resumen_ipw_trimming |>
      gt(groupname_col = "variable_dependiente") |>
      tab_header(
        title = "Comparación IPW: original vs. pesos truncados",
        subtitle = TERM_DID
      ) |>
      cols_label(
        modelo = "Especificación",
        estimate = "β",
        std.error = "EE",
        p.value = "p",
        signif = "Sig."
      ) |>
      tab_options(
        table.border.top.style = "solid",
        table.border.bottom.style = "solid",
        table.font.size = px(10)
      ) |>
      gtsave("output/tablas/tabla_coef_ipw_trimming.html")
  }
}

# ── 15.4 Placebo real: ola 1 → ola 2 (2016–2018, sin shocks) ─────────────────

cat("--- 15.4 Placebo real: ola 1 → ola 2 (sin shocks) ---\n\n")
cat("Ningún shock en 2016–2018 → τ esperado ≈ 0\n\n")

m_placebo_ctrl_real <- NULL
m_placebo_resg_real <- NULL

if (file.exists("data/subset_placebo_pre.rds")) {
  subset_placebo_pre <- readRDS("data/subset_placebo_pre.rds")
  cat("Placebo pre: N obs =", nrow(subset_placebo_pre),
      "| folios =", n_distinct(subset_placebo_pre$folio), "\n\n")

  m_placebo_ctrl_real <- tryCatch(
    lmer(
      formula_placebo_real("idx_vio_control", controles_base),
      data = subset_placebo_pre,
      REML = FALSE
    ),
    error = function(e) {
      cat("⚠ Error placebo control:", conditionMessage(e), "\n")
      NULL
    }
  )
  m_placebo_resg_real <- tryCatch(
    lmer(
      formula_placebo_real("idx_vio_resguardo", controles_base),
      data = subset_placebo_pre,
      REML = FALSE
    ),
    error = function(e) {
      cat("⚠ Error placebo cambio social:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (!is.null(m_placebo_ctrl_real) && !is.null(m_placebo_resg_real)) {
    cat("Placebo ctrl — τ (T_placebo × indi × zona):\n")
    print(
      broom.mixed::tidy(m_placebo_ctrl_real, effects = "fixed") |>
        filter(term == TERM_PLACEBO_REAL) |>
        select(term, estimate, std.error, p.value)
    )
    cat("\nPlacebo cambio social — τ:\n")
    print(
      broom.mixed::tidy(m_placebo_resg_real, effects = "fixed") |>
        filter(term == TERM_PLACEBO_REAL) |>
        select(term, estimate, std.error, p.value)
    )
    cat("\n")

    p_ctrl <- broom.mixed::tidy(m_placebo_ctrl_real) |>
      filter(term == TERM_PLACEBO_REAL) |> pull(p.value)
    p_resg <- broom.mixed::tidy(m_placebo_resg_real) |>
      filter(term == TERM_PLACEBO_REAL) |> pull(p.value)

    if (all(c(p_ctrl, p_resg) >= 0.1, na.rm = TRUE)) {
      cat("✓ Ambos placebos n.s. (p ≥ .10) → tendencias paralelas plausibles.\n\n")
    } else if (any(c(p_ctrl, p_resg) < 0.05, na.rm = TRUE)) {
      cat("⚠ Al menos un placebo significativo (p < .05). Interpretar DiD con cautela.\n\n")
    } else {
      cat("~ Resultado mixto en placebo → revisar coeficientes.\n\n")
    }

    modelsummary(
      list(
        "Control (placebo real)" = m_placebo_ctrl_real,
        "Cambio social (placebo)" = m_placebo_resg_real
      ),
      statistic = "({std.error})",
      stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
      fmt = 3,
      gof_map = c("nobs", "icc", "rmse"),
      output = "output/tablas/tabla_placebo_pretratamiento.html"
    )
    cat("✓ Tabla placebo guardada: output/tablas/tabla_placebo_pretratamiento.html\n\n")
  }
} else {
  cat("⚠ No se encuentra data/subset_placebo_pre.rds; ejecutar 01_limpieza.R\n\n")
}

# ── 15.4b Robustez: núcleo histórico vs. decreto completo ─────────────────────

cat("--- 15.4b Núcleo histórico del conflicto (olas 3–4) ---\n\n")

datos_nucleo <- subset_data |> filter(ola %in% c(3, 4))

m_nucleo_ctrl <- tryCatch(
  lmer(
    formula_nucleo("idx_vio_control", controles_base),
    data = datos_nucleo,
    REML = FALSE
  ),
  error = function(e) {
    cat("⚠ Error modelo núcleo control:", conditionMessage(e), "\n")
    NULL
  }
)

m_nucleo_resg <- tryCatch(
  lmer(
    formula_nucleo("idx_vio_resguardo", controles_base),
    data = datos_nucleo,
    REML = FALSE
  ),
  error = function(e) {
    cat("⚠ Error modelo núcleo cambio social:", conditionMessage(e), "\n")
    NULL
  }
)

# ── 15.5 Modelos por ítem ─────────────────────────────────────────────────────

cat("--- 15.5 Modelos por ítem ---\n\n")

item_vars <- c(
  "vio_ctrl_carb", "vio_priv_agric",
  "vio_ocup_tierras", "vio_camb_cortes"
)
item_labels <- c(
  vio_ctrl_carb    = "Carabineros / coerción estatal (d3_1 = VD principal)",
  vio_priv_agric   = "Agricultores armados / vigilantismo (d3_2, sensibilidad)",
  vio_ocup_tierras = "Tomas de terrenos / ocupación (d4_2, sensibilidad)",
  vio_camb_cortes  = "Cortes de caminos / protesta (d4_3 = VD principal)"
)

item_models <- list()
for (v in item_vars) {
  if (!v %in% names(subset_data)) {
    cat("⚠ Variable", v, "no encontrada; ítem omitido.\n")
    next
  }
  item_models[[v]] <- tryCatch(
    lmer(
      formula_did(v, controles_base),
      data = subset_data,
      REML = FALSE
    ),
    error = function(e) {
      cat("⚠ Error en ítem", v, ":", conditionMessage(e), "\n")
      NULL
    }
  )
}

item_models <- item_models[!vapply(item_models, is.null, logical(1))]

if (length(item_models) > 0) {
  item_named <- setNames(
    item_models,
    unname(item_labels[names(item_models)])
  )
  coef_ren_item <- if (exists("coef_rename_did")) coef_rename_did else NULL
  modelsummary(
    item_named,
    statistic = "({std.error})",
    stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
    fmt = 3,
    coef_rename = coef_ren_item,
    coef_omit = "Intercept|edad|mujer|urbano|id_chile|id_causa|perc_|apoyo_movil",
    gof_map = c("nobs", "icc", "rmse"),
    output = "output/tablas/tabla_modelos_por_item.html"
  )
  cat("✓ Tabla por ítem guardada: output/tablas/tabla_modelos_por_item.html\n\n")

  for (nm in names(item_models)) {
    cf <- extract_coef(item_models[[nm]], TERM_DID_DECRETO, paste("Ítem:", item_labels[[nm]]), nm)
    cat(item_labels[[nm]], "— DiD decreto (ola 4):",
        round(cf$estimate, 3), cf$signif, "(p =", format.pval(cf$p.value, digits = 3), ")\n")
  }
  cat("\n")
}

# ── 15.5b Sensibilidad apéndice: ítems excluidos de VD (d3_2, d4_2) ───────────
cat("--- 15.5b Sensibilidad: vigilantismo (d3_2) y ocupación (d4_2) ---\n\n")

m_sens_priv <- tryCatch(
  lmer(formula_did("vio_priv_agric", controles_base), data = subset_data, REML = FALSE),
  error = function(e) { cat("⚠ Error sensibilidad d3_2:", conditionMessage(e), "\n"); NULL }
)
m_sens_ocup <- tryCatch(
  lmer(formula_did("vio_ocup_tierras", controles_base), data = subset_data, REML = FALSE),
  error = function(e) { cat("⚠ Error sensibilidad d4_2:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(m_sens_priv)) {
  cf <- extract_coef(m_sens_priv, TERM_DID_DECRETO, "Sensib. vigilantismo (d3_2)", "vio_priv_agric")
  cat("Vigilantismo privado (d3_2) — DiD decreto:",
      round(cf$estimate, 3), cf$signif, "(p =", format.pval(cf$p.value, digits = 3), ")\n")
}
if (!is.null(m_sens_ocup)) {
  cf <- extract_coef(m_sens_ocup, TERM_DID_DECRETO, "Sensib. ocupación (d4_2)", "vio_ocup_tierras")
  cat("Ocupación territorial (d4_2) — DiD decreto:",
      round(cf$estimate, 3), cf$signif, "(p =", format.pval(cf$p.value, digits = 3), ")\n\n")
}

sens_list <- list(
  "Vigilantismo privado d3_2 (apéndice)" = m_sens_priv,
  "Ocupación territorial d4_2 (apéndice)" = m_sens_ocup
)
sens_named <- sens_list[!vapply(sens_list, is.null, logical(1))]

if (length(sens_named) > 0) {
  modelsummary(
    sens_named,
    statistic = "({std.error})",
    stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
    fmt = 3,
    gof_map = c("nobs", "icc", "rmse"),
    output = "output/tablas/tabla_sensibilidad_apendice.html"
  )
  cat("✓ Tabla sensibilidad guardada: output/tablas/tabla_sensibilidad_apendice.html\n\n")
}

# ── 15.5b FE individuales + cluster comuna + DRDID (Fase B/C) ─────────────────

cat("--- 15.5b FE (fixest), cluster comuna, DRDID ---\n\n")

n_comunas_tratadas <- subset_data |>
  dplyr::filter(
    .data$ola == 4,
    .data$indigeneous == "indi",
    .data$cerca_conflicto == "cerca"
  ) |>
  dplyr::summarise(n_comunas = dplyr::n_distinct(.data$comuna_cod)) |>
  dplyr::pull(.data$n_comunas)
cat("Comunas en celda tratado (indi × cerca × ola 4):", n_comunas_tratadas, "\n")

# FE con controles (sensibilidad; distinto del FE magro principal en 03_modelos.R)
f_fe_ctrl <- as.formula(paste(
  "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +", controles_base, "| folio"
))
f_fe_resg <- as.formula(paste(
  "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +", controles_base, "| folio"
))
f_cl_ctrl <- as.formula(paste(
  "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +", controles_base
))
f_cl_resg <- as.formula(paste(
  "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +", controles_base
))

m_fe_ctrl <- tryCatch(
  feols(f_fe_ctrl, data = subset_data, cluster = ~comuna_cod),
  error = function(e) { cat("⚠ FE ctrl:", conditionMessage(e), "\n"); NULL }
)
m_fe_resg <- tryCatch(
  feols(f_fe_resg, data = subset_data, cluster = ~comuna_cod),
  error = function(e) { cat("⚠ FE resg:", conditionMessage(e), "\n"); NULL }
)
m_cl_ctrl <- tryCatch(
  feols(f_cl_ctrl, data = subset_data, cluster = ~comuna_cod),
  error = function(e) { cat("⚠ cluster ctrl:", conditionMessage(e), "\n"); NULL }
)
m_cl_resg <- tryCatch(
  feols(f_cl_resg, data = subset_data, cluster = ~comuna_cod),
  error = function(e) { cat("⚠ cluster resg:", conditionMessage(e), "\n"); NULL }
)

# lmer con (1|folio)+(1|comuna_cod) como sensibilidad RE multinivel
m_re_cl_ctrl <- tryCatch(
  lmer(
    as.formula(paste(
      "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
      controles_base, "+ (1 | folio) + (1 | comuna_cod)"
    )),
    data = subset_data, REML = FALSE
  ),
  error = function(e) { cat("⚠ RE+comuna ctrl:", conditionMessage(e), "\n"); NULL }
)
m_re_cl_resg <- tryCatch(
  lmer(
    as.formula(paste(
      "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
      controles_base, "+ (1 | folio) + (1 | comuna_cod)"
    )),
    data = subset_data, REML = FALSE
  ),
  error = function(e) { cat("⚠ RE+comuna resg:", conditionMessage(e), "\n"); NULL }
)

# DRDID en submuestra indígena: tratado = zona, post = ola 4 (panel balanceado)
# Covariables congeladas en ola 2 (mismo patrón que 03b_drdid.R)
COVARS_DRDID <- c(
  "mujer", "edad", "urbano_rural",
  "id_chile", "id_causa", "perc_desigualdad", "apoyo_movil"
)
LABEL_DRDID_ATT   <- "DR-DiD ATT (solo indi — NO es τ₄)"
LABEL_DRDID_DELTA <- "DR-DiD Δ ATT (indi − no indi) ≈ τ₄"

m_drdid_ctrl <- m_drdid_resg <- NULL
drdid_resumen <- drdid_delta_resumen <- NULL

prep_drdid <- function(vd, ola_pre = 2L, ola_post = 4L) {
  covar_cols_raw <- COVARS_DRDID[COVARS_DRDID %in% names(subset_data)]

  d <- subset_data |>
    dplyr::filter(.data$indigeneous == "indi", .data$ola %in% c(ola_pre, ola_post)) |>
    dplyr::mutate(
      id    = as.integer(factor(.data$folio)),
      year  = as.integer(.data$ola),
      treat = as.integer(.data$cerca_conflicto == "cerca"),
      y     = as.numeric(.data[[vd]])
    )

  for (cv in covar_cols_raw) {
    d[[paste0(cv, "_n")]] <- as.numeric(d[[cv]])
  }
  covar_n <- paste0(covar_cols_raw, "_n")

  # CLAVE: covariables INVARIANTES en el tiempo (baseline ola_pre)
  baseline_covs <- d |>
    dplyr::filter(.data$year == ola_pre) |>
    dplyr::select(dplyr::all_of(c("id", covar_n)))

  d <- d |>
    dplyr::select(-dplyr::all_of(covar_n)) |>
    dplyr::left_join(baseline_covs, by = "id") |>
    dplyr::filter(
      !is.na(.data$y), !is.na(.data$treat),
      dplyr::if_all(dplyr::all_of(covar_n), ~ !is.na(.x))
    ) |>
    dplyr::distinct(.data$id, .data$year, .keep_all = TRUE)

  ids_ok <- d |>
    dplyr::count(.data$id) |>
    dplyr::filter(.data$n == 2L) |>
    dplyr::pull(.data$id)

  d |> dplyr::filter(.data$id %in% ids_ok)
}

if (has_drdid) {
  run_drdid <- function(vd) {
    d <- prep_drdid(vd)
    if (nrow(d) < 50) return(NULL)

    covar_n <- paste0(COVARS_DRDID[COVARS_DRDID %in% names(subset_data)], "_n")
    covar_n <- covar_n[vapply(covar_n, function(v) {
      v %in% names(d) && stats::var(d[[v]], na.rm = TRUE) > 0
    }, logical(1))]

    xf <- if (length(covar_n) > 0) {
      as.formula(paste("~", paste(covar_n, collapse = " + ")))
    } else {
      ~ 1
    }

    tryCatch(
      DRDID::drdid(
        yname = "y", tname = "year", idname = "id",
        dname = "treat", xformla = xf,
        data = as.data.frame(d), panel = TRUE, boot = FALSE
      ),
      error = function(e) {
        cat("⚠ DRDID covars", vd, ":", conditionMessage(e), "→ reintento ~1\n")
        tryCatch(
          DRDID::drdid(
            yname = "y", tname = "year", idname = "id",
            dname = "treat", xformla = ~ 1,
            data = as.data.frame(d), panel = TRUE, boot = FALSE
          ),
          error = function(e2) {
            cat("⚠ DRDID", vd, ":", conditionMessage(e2), "\n")
            NULL
          }
        )
      }
    )
  }

  m_drdid_ctrl <- run_drdid("idx_vio_control")
  m_drdid_resg <- run_drdid("idx_vio_resguardo")

  extract_drdid <- function(obj, vd, label, term = "ATT_DRDID") {
    if (is.null(obj)) {
      return(tibble(
        modelo = label, variable_dependiente = vd, term = term,
        estimate = NA_real_, std.error = NA_real_, p.value = NA_real_, signif = ""
      ))
    }
    att <- as.numeric(obj$ATT)
    se  <- as.numeric(obj$se)
    z   <- if (is.finite(se) && se > 0) att / se else NA_real_
    p   <- if (is.finite(z)) 2 * stats::pnorm(-abs(z)) else NA_real_
    tibble(
      modelo = label, variable_dependiente = vd, term = term,
      estimate = att, std.error = se, p.value = p, signif = signif_stars(p)
    )
  }

  drdid_resumen <- bind_rows(
    extract_drdid(m_drdid_ctrl, "idx_vio_control", LABEL_DRDID_ATT),
    extract_drdid(m_drdid_resg, "idx_vio_resguardo", LABEL_DRDID_ATT)
  )
  print(drdid_resumen)

  # Verificación: ATT 04 debe coincidir (±0.02) con ATT_indi de 03b (Decreto 2→4)
  if (file.exists("data/drdid.rds")) {
    drdid_ref <- readRDS("data/drdid.rds")
    ref_keys <- c(
      idx_vio_control   = "decreto__ola_2_4___idx_vio_control",
      idx_vio_resguardo = "decreto__ola_2_4___idx_vio_resguardo"
    )
    for (vd in names(ref_keys)) {
      ref <- drdid_ref$resultados[[ref_keys[[vd]]]]
      live <- drdid_resumen$estimate[drdid_resumen$variable_dependiente == vd]
      if (!is.null(ref) && length(live) == 1L && is.finite(live)) {
        diff_att <- abs(live - ref$att_indi)
        cat(sprintf(
          "  Verif ATT 04 vs 03b (%s): live=%.3f ref=%.3f Δ=%.3f %s\n",
          vd, live, ref$att_indi, diff_att,
          if (diff_att <= 0.02) "✓" else "⚠ REVISAR"
        ))
      }
    }
    d_ctrl <- prep_drdid("idx_vio_control")
    d_resg <- prep_drdid("idx_vio_resguardo")
    cat(sprintf(
      "  Panel 04 indi 2→4: ctrl nrow=%d n_id=%d | resg nrow=%d n_id=%d\n",
      nrow(d_ctrl), dplyr::n_distinct(d_ctrl$id),
      nrow(d_resg), dplyr::n_distinct(d_resg$id)
    ))
  } else {
    warning(
      "data/drdid.rds no existe — ejecute R/03b_drdid.R antes que R/04_robustez.R ",
      "para verificación ATT y filas Δ ATT en resumen_robustez.",
      call. = FALSE
    )
  }

  # Δ ATT (indi − no indi) desde 03b — estimando ≈ τ₄
  if (file.exists("data/drdid.rds")) {
    drdid_ref <- readRDS("data/drdid.rds")
    delta_keys <- c(
      idx_vio_control   = "decreto__ola_2_4___idx_vio_control",
      idx_vio_resguardo = "decreto__ola_2_4___idx_vio_resguardo"
    )
    drdid_delta_resumen <- purrr::imap_dfr(delta_keys, function(key, vd) {
      x <- drdid_ref$resultados[[key]]
      if (is.null(x)) {
        return(tibble(
          modelo = LABEL_DRDID_DELTA, variable_dependiente = vd,
          term = "DELTA_ATT", estimate = NA_real_, std.error = NA_real_,
          p.value = NA_real_, signif = ""
        ))
      }
      tibble(
        modelo = LABEL_DRDID_DELTA, variable_dependiente = vd,
        term = "DELTA_ATT",
        estimate = x$diff_att, std.error = x$se_diff, p.value = x$p_diff,
        signif = signif_stars(x$p_diff)
      )
    })
    print(drdid_delta_resumen)
  }
} else {
  cat("⚠ Paquete DRDID no disponible; omitido.\n")
}

# ── 15.6 Tabla resumen final de robustez ──────────────────────────────────────

cat("--- 15.6 Tabla resumen de robustez ---\n\n")

if (!exists("mC_ctrl")) {
  mC_ctrl <- m2_ctrl
  mC_resg <- m2_resg
}
if (!exists("mA_ctrl")) {
  mA_ctrl <- mA_resg <- mB_ctrl <- mB_resg <- NULL
}

# Índice dual d3_1+d3_2 (sensibilidad Tabla 15)
m_sensibilidad_control_dual <- NULL
if (all(c("vio_ctrl_carb", "vio_priv_agric") %in% names(subset_data))) {
  subset_data <- subset_data |>
    mutate(
      idx_vio_control_dual = rowMeans(
        cbind(as.numeric(vio_ctrl_carb), as.numeric(vio_priv_agric)),
        na.rm = TRUE
      )
    )
  m_sensibilidad_control_dual <- tryCatch(
    lmer(
      formula_did("idx_vio_control_dual", controles_base),
      data = subset_data, REML = FALSE
    ),
    error = function(e) {
      cat("⚠ Error índice dual control:", conditionMessage(e), "\n")
      NULL
    }
  )
}

if (!exists("mFE_ctrl") || is.null(mFE_ctrl)) {
  if (exists("modelos") && !is.null(modelos$mFE_ctrl)) {
    mFE_ctrl <- modelos$mFE_ctrl
    mFE_resg <- modelos$mFE_resg
  }
}

resumen_robustez <- bind_rows(
  if (!is.null(mFE_ctrl)) {
    extract_coef(mFE_ctrl, TERM_DID_ESTALLIDO, "Principal FE magro", "idx_vio_control")
  },
  if (!is.null(mFE_ctrl)) {
    extract_coef(mFE_ctrl, TERM_DID_DECRETO, "Principal FE magro", "idx_vio_control")
  },
  if (!is.null(mFE_resg)) {
    extract_coef(mFE_resg, TERM_DID_ESTALLIDO, "Principal FE magro", "idx_vio_resguardo")
  },
  if (!is.null(mFE_resg)) {
    extract_coef(mFE_resg, TERM_DID_DECRETO, "Principal FE magro", "idx_vio_resguardo")
  },
  extract_coef(mC_ctrl, TERM_DID_ESTALLIDO, "C — DiD estallido", "idx_vio_control"),
  extract_coef(mC_ctrl, TERM_DID_DECRETO, "C — DiD decreto", "idx_vio_control"),
  extract_coef(mC_resg, TERM_DID_ESTALLIDO, "C — DiD estallido", "idx_vio_resguardo"),
  extract_coef(mC_resg, TERM_DID_DECRETO, "C — DiD decreto", "idx_vio_resguardo"),
  extract_coef(mA_ctrl, TERM_DID_A, "A — Estallido (2→3)", "idx_vio_control"),
  extract_coef(mA_resg, TERM_DID_A, "A — Estallido (2→3)", "idx_vio_resguardo"),
  extract_coef(mB_ctrl, TERM_DID_B, "B — Decreto (3→4)", "idx_vio_control"),
  extract_coef(mB_resg, TERM_DID_B, "B — Decreto (3→4)", "idx_vio_resguardo"),
  extract_coef(m2_ctrl_matched, TERM_DID_DECRETO, "PSM", "idx_vio_control"),
  extract_coef(m2_resg_matched, TERM_DID_DECRETO, "PSM", "idx_vio_resguardo"),
  extract_coef(m2_ctrl_ipw, TERM_DID_DECRETO, "IPW original", "idx_vio_control"),
  extract_coef(m2_resg_ipw, TERM_DID_DECRETO, "IPW original", "idx_vio_resguardo"),
  extract_coef(m2_ctrl_ipw_trim_1_99, TERM_DID_DECRETO, "IPW trim 1–99%", "idx_vio_control"),
  extract_coef(m2_resg_ipw_trim_1_99, TERM_DID_DECRETO, "IPW trim 1–99%", "idx_vio_resguardo"),
  extract_coef(m2_ctrl_ipw_trim_5_95, TERM_DID_DECRETO, "IPW trim 5–95%", "idx_vio_control"),
  extract_coef(m2_resg_ipw_trim_5_95, TERM_DID_DECRETO, "IPW trim 5–95%", "idx_vio_resguardo"),
  extract_coef(m_fe_ctrl, TERM_DID_DECRETO, "FE + controles (fixest)", "idx_vio_control"),
  extract_coef(m_fe_resg, TERM_DID_DECRETO, "FE + controles (fixest)", "idx_vio_resguardo"),
  extract_coef(m_cl_ctrl, TERM_DID_DECRETO, "OLS cluster comuna", "idx_vio_control"),
  extract_coef(m_cl_resg, TERM_DID_DECRETO, "OLS cluster comuna", "idx_vio_resguardo"),
  extract_coef(m_re_cl_ctrl, TERM_DID_DECRETO, "RE folio+comuna", "idx_vio_control"),
  extract_coef(m_re_cl_resg, TERM_DID_DECRETO, "RE folio+comuna", "idx_vio_resguardo"),
  if (!is.null(drdid_resumen)) drdid_resumen else NULL,
  if (!is.null(drdid_delta_resumen)) drdid_delta_resumen else NULL,
  extract_coef(m_placebo_ctrl_real, TERM_PLACEBO_REAL,
               "Placebo real (ola1→2)", "idx_vio_control"),
  extract_coef(m_placebo_resg_real, TERM_PLACEBO_REAL,
               "Placebo real (ola1→2)", "idx_vio_resguardo"),
  extract_coef(m_nucleo_ctrl, TERM_NUCLEO_DID, "Núcleo histórico", "idx_vio_control"),
  extract_coef(m_nucleo_resg, TERM_NUCLEO_DID, "Núcleo histórico", "idx_vio_resguardo"),
  imap_dfr(item_models, ~ extract_coef(
    .x, TERM_DID_DECRETO, paste("Ítem:", item_labels[[.y]]), .y
  ))
)

resumen_robustez <- resumen_robustez |>
  distinct(modelo, variable_dependiente, term, .keep_all = TRUE) |>
  mutate(
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error,
    ic95  = paste0("[", round(ci_lo, 3), ", ", round(ci_hi, 3), "]"),
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4)
  )

write.csv(resumen_robustez, "output/tablas/tabla_resumen_robustez.csv", row.names = FALSE)

resumen_robustez |>
  gt() |>
  tab_header(
    title = "Resumen de robustez",
    subtitle = paste0("Coeficiente de interés: ", TERM_DID, " / placebo")
  ) |>
  cols_label(
    modelo = "Modelo",
    variable_dependiente = "VD",
    term = "Término",
    estimate = "β",
    std.error = "EE",
    ic95 = "IC 95%",
    p.value = "p",
    signif = "Sig."
  ) |>
  tab_options(
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    table.font.size = px(10)
  ) |>
  gtsave("output/tablas/tabla_resumen_robustez.html")

cat("✓ Resumen rxobustez: output/tablas/tabla_resumen_robustez.{csv,html}\n\n")
print(resumen_robustez)

# ── Forest plot comparativo de especificaciones ───────────────────────────────

p_robustez <- resumen_robustez |>
  filter(
    !is.na(estimate),
    !str_starts(modelo, "Ítem:"),
    term == TERM_DID_DECRETO,
    modelo %in% c(
      "Principal FE magro", "C — DiD decreto",
      "FE + controles (fixest)", "OLS cluster comuna", "RE folio+comuna",
      "PSM", "IPW original", "IPW trim 1–99%", "IPW trim 5–95%",
      LABEL_DRDID_DELTA,
      "Placebo real (ola1→2)", "Núcleo histórico"
    )
  ) |>
  mutate(
    variable_dependiente = recode(
      variable_dependiente,
      idx_vio_control = "Control social",
      idx_vio_resguardo = "Cambio social"
    ),
    modelo = factor(modelo, levels = rev(c(
      "Principal FE magro", "C — DiD decreto",
      "FE + controles (fixest)", "OLS cluster comuna", "RE folio+comuna",
      LABEL_DRDID_DELTA,
      "IPW original", "IPW trim 1–99%", "IPW trim 5–95%",
      "PSM", "Placebo real (ola1→2)", "Núcleo histórico"
    ))),
    sig = p.value < 0.05
  ) |>
  ggplot(aes(
    x = estimate, y = modelo,
    xmin = ci_lo, xmax = ci_hi,
    color = variable_dependiente,
    shape = sig
  )) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(position = position_dodge(width = 0.4), size = 0.5) +
  scale_shape_manual(
    values = c("FALSE" = 1, "TRUE" = 16),
    labels = c("n.s.", "p < .05"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Control social" = "#4575B4", "Cambio social" = "#D73027"),
    name = NULL
  ) +
  labs(
    title    = "Estabilidad del efecto DiD a través de especificaciones",
    subtitle = "Coeficiente: Ola 4 × Indígena × Zona excepción · IC 95%",
    x = "Coeficiente estimado", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("output/figuras/fig_robustez_comparativa.png", p_robustez,
       width = 10, height = 6, dpi = 300)
cat("✓ Figura robustez guardada: output/figuras/fig_robustez_comparativa.png\n")

# ── 15.7 Notas de interpretación automáticas ──────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("INTERPRETACIÓN AUTOMÁTICA — ROBUSTEZ\n")
cat(strrep("=", 60), "\n\n")

interpretar_robustez <- function(resumen) {
  main_ctrl <- resumen |>
    filter(modelo == "C — DiD decreto", variable_dependiente == "idx_vio_control")
  main_resg <- resumen |>
    filter(modelo == "C — DiD decreto", variable_dependiente == "idx_vio_resguardo")
  est_ctrl <- resumen |>
    filter(modelo == "C — DiD estallido", variable_dependiente == "idx_vio_control")
  est_resg <- resumen |>
    filter(modelo == "C — DiD estallido", variable_dependiente == "idx_vio_resguardo")

  psm_ok <- resumen |>
    filter(modelo == "PSM", !is.na(estimate)) |>
    mutate(pos_sig = estimate > 0 & p.value < 0.05)
  ipw_ok <- resumen |>
    filter(modelo == "IPW original", !is.na(estimate)) |>
    mutate(pos_sig = estimate > 0 & p.value < 0.05)

  if (nrow(psm_ok) > 0 && all(psm_ok$pos_sig, na.rm = TRUE)) {
    cat("• PSM: Los resultados principales son robustos al ajuste por diferencias",
        "observables en baseline mediante matching.\n")
  } else if (nrow(psm_ok) > 0) {
    cat("• PSM: Los coeficientes en la muestra emparejada no replican plenamente",
        "el patrón principal; interpretar con cautela.\n")
  }

  if (nrow(ipw_ok) > 0 && all(ipw_ok$pos_sig, na.rm = TRUE)) {
    cat("• IPW: Los resultados principales son robustos al ajuste por diferencias",
        "observables en baseline mediante ponderación.\n")
  } else if (nrow(ipw_ok) > 0) {
    cat("• IPW: Los coeficientes ponderados difieren del modelo principal;",
        "revisar balance y pesos extremos.\n")
  }

  plcb <- resumen |>
    filter(modelo == "Placebo real (ola1→2)", !is.na(p.value))
  if (nrow(plcb) == 0) {
    cat("• Placebo real (ola 1→2): No estimado.\n")
  } else if (all(plcb$p.value >= 0.05, na.rm = TRUE)) {
    cat("• Placebo real (ola 1→2): Sin divergencia diferencial en período sin shocks;",
        "tendencias paralelas plausibles.\n")
  } else {
    cat("• Placebo: El placebo pretratamiento detecta diferencias previas",
        "(p < .05 en al menos una VD), por lo que la interpretación causal",
        "debe tratarse con mayor cautela.\n")
  }

  items <- resumen |>
    filter(str_starts(modelo, "Ítem:"), !is.na(estimate)) |>
    mutate(abs_est = abs(estimate))
  if (nrow(items) >= 2) {
    top <- items |> slice_max(abs_est, n = 1)
    cat("• Ítems: El efecto post-tratamiento parece concentrarse en",
        top$modelo, "(β =", top$estimate, top$signif, ").\n")
  }

  if (nrow(est_ctrl) == 1) {
    cat("• DiD estallido (control): β =", est_ctrl$estimate, est_ctrl$signif, "\n")
  }
  if (nrow(est_resg) == 1) {
    cat("• DiD estallido (cambio social): β =", est_resg$estimate, est_resg$signif, "\n")
  }
  if (nrow(main_ctrl) == 1 && !is.na(main_ctrl$p.value) &&
      main_ctrl$p.value < 0.05 && main_ctrl$estimate > 0) {
    cat("• DiD decreto (control social): positivo y significativo (β =",
        main_ctrl$estimate, ").\n")
  }
  if (nrow(main_resg) == 1 && !is.na(main_resg$p.value) &&
      main_resg$p.value < 0.05 && main_resg$estimate > 0) {
    cat("• DiD decreto (cambio social): positivo y significativo (β =",
        main_resg$estimate, ").\n")
  }
}

interpretar_ipw_trimming <- function(resumen_ipw, resumen_principal = NULL) {
  if (is.null(resumen_ipw) || nrow(resumen_ipw) == 0) {
    cat("• IPW trimming: No estimado.\n")
    return(invisible(NULL))
  }

  cat("\n--- Interpretación IPW / trimming ---\n\n")

  for (vd in unique(resumen_ipw$variable_dependiente)) {
    vd_label <- if (vd == "idx_vio_control") {
      "violencia de control social"
    } else {
      "violencia de resguardo territorial"
    }
    sub <- resumen_ipw |>
      filter(variable_dependiente == vd) |>
      mutate(pos_sig = estimate > 0 & p.value < 0.05)

    orig <- sub |> filter(modelo == "IPW original")
    t199 <- sub |> filter(modelo == "IPW trim 1–99%")
    t595 <- sub |> filter(modelo == "IPW trim 5–95%")

    orig_sig <- isTRUE(orig$pos_sig[1])
    t199_sig <- isTRUE(t199$pos_sig[1])
    t595_sig <- isTRUE(t595$pos_sig[1])

    cat("•", vd_label, ":\n")
    cat("  Original: β =", orig$estimate, orig$signif,
        "| Trim 1–99%: β =", t199$estimate, t199$signif,
        "| Trim 5–95%: β =", t595$estimate, t595$signif, "\n")

    if (!is.null(resumen_principal)) {
      pr <- resumen_principal |>
        filter(modelo == "C — DiD decreto", variable_dependiente == vd)
      if (nrow(pr) == 1) {
        cat("  (Modelo principal sin IPW: β =", pr$estimate, pr$signif, ")\n")
      }
    }

    if (orig_sig && t199_sig && t595_sig) {
      cat("  → Los resultados IPW no parecen dominados por pesos extremos",
          "(positivo y significativo en original y truncados).\n")
    } else if (orig_sig && (!t199_sig || !t595_sig)) {
      cat("  → El resultado IPW es sensible a pesos extremos;",
          "interpretar con cautela.\n")
    } else if (!orig_sig) {
      cat("  → El IPW original no es significativo; el trimming no altera",
          "la conclusión principal.\n")
    } else {
      cat("  → Patrón mixto entre especificaciones IPW;",
          "revisar diagnóstico de pesos.\n")
    }

    if (nrow(orig) == 1 && nrow(t595) == 1) {
      cambio_mag <- abs(t595$estimate - orig$estimate) / max(abs(orig$estimate), 0.01)
      if (cambio_mag > 0.5) {
        cat("  → Nota: el trimming 5–95 altera la magnitud del coeficiente en",
            round(100 * cambio_mag, 0), "%, pese a mantener significancia.\n")
      }
    }
  }
}

interpretar_robustez(resumen_robustez)
interpretar_ipw_trimming(resumen_ipw_trimming, resumen_robustez)

# ── Sensibilidad A7.2: restricción a población mapuche ────────────────────────

cat("\n--- Sensibilidad A7.2: solo mapuche (a1 = 1) vs todos indígenas ---\n")

datos_mapuche <- subset_data |>
  filter(
    .data$indigeneous == "no_indi" |
      (.data$indigeneous == "indi" & .data$cat_indi == "mapuche")
  )

m_map_ctrl <- lmer(
  formula_did("idx_vio_control", controles_base),
  data = datos_mapuche, REML = FALSE
)
m_map_resg <- lmer(
  formula_did("idx_vio_resguardo", controles_base),
  data = datos_mapuche, REML = FALSE
)

sens_mapuche_compare <- bind_rows(
  extract_coef(mC_ctrl, TERM_DID_DECRETO, "Todos indígenas", "idx_vio_control"),
  extract_coef(mC_resg, TERM_DID_DECRETO, "Todos indígenas", "idx_vio_resguardo"),
  extract_coef(m_map_ctrl, TERM_DID_DECRETO, "Solo mapuche", "idx_vio_control"),
  extract_coef(m_map_resg, TERM_DID_DECRETO, "Solo mapuche", "idx_vio_resguardo")
) |>
  mutate(
    vd_label = if_else(
      .data$variable_dependiente == "idx_vio_control",
      "Control social", "Cambio social"
    ),
    ic95 = sprintf(
      "[%.3f, %.3f]",
      .data$estimate - 1.96 * .data$std.error,
      .data$estimate + 1.96 * .data$std.error
    )
  )

print(sens_mapuche_compare |> select(modelo, vd_label, estimate, p.value, signif))

# ── 15.8 FASE 3.5: Tratamiento territorial más fino (agenda futura) ───────────
#
# OPCIONAL — solo si los datos lo permiten. Con los datos actuales del panel
# ELRI no hay medidas de intensidad ni de frontera precisa. Se documenta como
# agenda de investigación futura:
#
# (a) INTENSIDAD: construir tratamiento continuo si se dispone de datos de
#     días bajo decreto / despliegue policial / incidentes por comuna.
#     Estimar dosis-respuesta en lugar del binario 0/1.
#     Fuentes posibles: INDH, reportes Ministerio del Interior, datos CIPER.
#
# (b) FRONTERA (RDD geográfico): submuestra de comunas justo dentro vs. justo
#     fuera del límite provincial del decreto (poblaciones similares salvo la
#     línea administrativa). Permitiría un estimador RDD × DiD.
#     Requiere: geolocalización precisa de comunas y datos de distancia al
#     límite del decreto.
#
# Ambas estrategias están fuera del alcance de este paper con los datos
# disponibles. Se reportan como limitación en la sección de discusión.

cat("\n--- 15.8 Tratamiento territorial más fino ---\n")
cat("(OPCIONAL) Intensidad y frontera geográfica documentados como agenda futura.\n")
cat("Datos disponibles: binario zona/fuera (cerca_conflicto). Sin datos de\n")
cat("intensidad por comuna ni geolocalización de límites provinciales.\n\n")

saveRDS(
  list(
    baseline_ola2 = baseline_ola2,
    baseline_cc = baseline_cc,
    m_psm = m_psm,
    w_ipw = w_ipw,
    m2_ctrl_matched = m2_ctrl_matched,
    m2_resg_matched = m2_resg_matched,
    m2_ctrl_ipw = m2_ctrl_ipw,
    m2_resg_ipw = m2_resg_ipw,
    m2_ctrl_ipw_trim_1_99 = m2_ctrl_ipw_trim_1_99,
    m2_resg_ipw_trim_1_99 = m2_resg_ipw_trim_1_99,
    m2_ctrl_ipw_trim_5_95 = m2_ctrl_ipw_trim_5_95,
    m2_resg_ipw_trim_5_95 = m2_resg_ipw_trim_5_95,
    resumen_ipw_trimming = resumen_ipw_trimming,
    diag_pesos = if (exists("diag_pesos")) diag_pesos else NULL,
    m_placebo_ctrl_real = m_placebo_ctrl_real,
    m_placebo_resg_real = m_placebo_resg_real,
    m_nucleo_ctrl = m_nucleo_ctrl,
    m_nucleo_resg = m_nucleo_resg,
    item_models = item_models,
    m_sens_priv = if (exists("m_sens_priv")) m_sens_priv else NULL,
    m_sens_ocup = if (exists("m_sens_ocup")) m_sens_ocup else NULL,
    m2_ctrl_d32 = if (exists("m_sens_priv")) m_sens_priv else NULL,
    m2_resg_d42 = if (exists("m_sens_ocup")) m_sens_ocup else NULL,
    m_sensibilidad_control_dual = if (exists("m_sensibilidad_control_dual")) {
      m_sensibilidad_control_dual
    } else {
      NULL
    },
    resumen_robustez = resumen_robustez,
    sens_mapuche_compare = sens_mapuche_compare,
    controles_base = controles_base,
    m_fe_ctrl = if (exists("m_fe_ctrl")) m_fe_ctrl else NULL,
    m_fe_resg = if (exists("m_fe_resg")) m_fe_resg else NULL,
    m_cl_ctrl = if (exists("m_cl_ctrl")) m_cl_ctrl else NULL,
    m_cl_resg = if (exists("m_cl_resg")) m_cl_resg else NULL,
    m_re_cl_ctrl = if (exists("m_re_cl_ctrl")) m_re_cl_ctrl else NULL,
    m_re_cl_resg = if (exists("m_re_cl_resg")) m_re_cl_resg else NULL,
    m_drdid_ctrl = if (exists("m_drdid_ctrl")) m_drdid_ctrl else NULL,
    m_drdid_resg = if (exists("m_drdid_resg")) m_drdid_resg else NULL,
    drdid_resumen = if (exists("drdid_resumen")) drdid_resumen else NULL,
    n_comunas_tratadas = if (exists("n_comunas_tratadas")) n_comunas_tratadas else NA_integer_,
    ipw_estimator = "feols_weights_cluster_comuna",
    ps_includes_outcomes = FALSE
  ),
  "data/robustez.rds"
)

cat("\n✓ 04_robustez.R ejecutado correctamente.\n")
cat("✓ Objetos guardados: data/robustez.rds\n")
