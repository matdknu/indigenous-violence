# =============================================================================
# 08c_demandas.R — DiD sobre apoyo a demandas indígenas (securitización)
#
# Extensión: mismo diseño período × indígena × zona sobre dos índices:
#   idx_dem_redistrib — redistributivas / representación (z-score)
#   idx_dem_reconoc   — reconocimiento cultural (z-score)
#
# Principal: feols FE magro + cluster comuna
# Sensibilidad: lmer RE, DR-DiD Δ ATT, IPW/PSM, ordinal de ítems
#
# Input:  data/subset_data.rds, data/panel_completo.rds, data/analysis_metadata.rds
# Output: output/tablas/tabla_demandas.html
#         output/figuras/fig_demandas.png
#         data/demandas.rds
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, stringr, haven, gt, ggplot2, forcats,
  fixest, lme4, lmerTest, broom, broom.mixed,
  MatchIt, WeightIt, ordinal, psych, marginaleffects
)

has_drdid <- requireNamespace("DRDID", quietly = TRUE)
if (!has_drdid) {
  tryCatch(install.packages("DRDID", repos = "https://cloud.r-project.org"),
           error = function(e) NULL)
  has_drdid <- requireNamespace("DRDID", quietly = TRUE)
}

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)
source("R/plot_helpers.R")

subset_data    <- readRDS("data/subset_data.rds")
panel_completo <- readRDS("data/panel_completo.rds")
metadata       <- readRDS("data/analysis_metadata.rds")
controles_base <- metadata$controles_base

TERM_DID_DECRETO      <- "periododecreto:indigeneousindi:cerca_conflictocerca"
TERM_DID_POLITIZACION <- "periodoestallido:indigeneousindi:cerca_conflictocerca"

signif_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "", p < .001 ~ "***", p < .01 ~ "**",
    p < .05 ~ "*", p < .1 ~ "+", TRUE ~ ""
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# PASO 2 — Índices z-estandarizados
# ══════════════════════════════════════════════════════════════════════════════

ITEMS_REDIS <- c("e4_2", "e4_4", "e4_5", "e3_5", "e3_4")
ITEMS_REDIS_SENS <- c("e4_2", "e4_4", "e4_5", "e3_5")  # sin e3_4 (doble escaños)
ITEMS_RECON <- c("e5_1", "e5_2", "e5_3", "e5_4", "e4_3")

# Parámetros z = media/DE en subset_data (olas 2–4 agrupadas)
z_params <- lapply(c(ITEMS_REDIS, ITEMS_RECON), function(v) {
  x <- as.numeric(subset_data[[v]])
  list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
})
names(z_params) <- c(ITEMS_REDIS, ITEMS_RECON)

z_item <- function(x, v) {
  p <- z_params[[v]]
  (as.numeric(x) - p$mean) / p$sd
}

add_demandas_indices <- function(dat) {
  for (v in c(ITEMS_REDIS, ITEMS_RECON)) {
    dat[[paste0("z_", v)]] <- z_item(dat[[v]], v)
  }
  Z_R  <- paste0("z_", ITEMS_REDIS)
  Z_RS <- paste0("z_", ITEMS_REDIS_SENS)
  Z_C  <- paste0("z_", ITEMS_RECON)
  # na.rm=FALSE: índice incompleto → NA
  dat$idx_dem_redistrib <- rowMeans(as.data.frame(dat[Z_R]), na.rm = FALSE)
  dat$idx_dem_redistrib_sinescaduplic <- rowMeans(as.data.frame(dat[Z_RS]), na.rm = FALSE)
  dat$idx_dem_reconoc <- rowMeans(as.data.frame(dat[Z_C]), na.rm = FALSE)
  dat
}

subset_data    <- add_demandas_indices(subset_data)
panel_completo <- add_demandas_indices(panel_completo)
subset_placebo <- if (file.exists("data/subset_placebo_pre.rds")) {
  add_demandas_indices(readRDS("data/subset_placebo_pre.rds"))
} else {
  panel_completo |> dplyr::filter(ola %in% 1:2, !is.na(indigeneous))
}

stopifnot(
  is.numeric(subset_data$idx_dem_redistrib),
  is.numeric(subset_data$idx_dem_reconoc),
  !all(is.na(subset_data$idx_dem_redistrib)),
  !all(is.na(subset_data$idx_dem_reconoc))
)

cat("\n", strrep("=", 60), "\n")
cat("PASO 2 — ÍNDICES DE DEMANDAS (z-score sobre subset_data)\n")
cat(strrep("=", 60), "\n\n")

for (vd in c("idx_dem_redistrib", "idx_dem_redistrib_sinescaduplic", "idx_dem_reconoc")) {
  x <- subset_data[[vd]]
  cat(sprintf("  %s: media=%.3f  SD=%.3f  %%NA=%.2f  n=%d\n",
              vd, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
              100 * mean(is.na(x)), sum(!is.na(x))))
}

# Alfa e inter-ítem sobre z-ítems
alpha_redis <- psych::alpha(
  as.data.frame(subset_data[paste0("z_", ITEMS_REDIS)]), check.keys = TRUE
)
alpha_redis_sens <- psych::alpha(
  as.data.frame(subset_data[paste0("z_", ITEMS_REDIS_SENS)]), check.keys = TRUE
)
alpha_recon <- psych::alpha(
  as.data.frame(subset_data[paste0("z_", ITEMS_RECON)]), check.keys = TRUE
)

# Drop-item diagnostics para e3_5
drop_e35 <- tryCatch({
  a0 <- alpha_redis$total$raw_alpha
  a1 <- psych::alpha(
    as.data.frame(subset_data[paste0("z_", setdiff(ITEMS_REDIS, "e3_5"))]),
    check.keys = TRUE
  )$total$raw_alpha
  list(alpha_con = a0, alpha_sin = a1, diluye = a1 > a0)
}, error = function(e) list(alpha_con = NA, alpha_sin = NA, diluye = NA))

cat("\n--- Consistencia interna (ítems z) ---\n")
cat(sprintf("  idx_dem_redistrib (5 ítems, con e3_4): α = %.3f\n",
            alpha_redis$total$raw_alpha))
cat(sprintf("  idx_dem_redistrib_sinescaduplic (sin e3_4): α = %.3f\n",
            alpha_redis_sens$total$raw_alpha))
cat(sprintf("  idx_dem_reconoc (5 ítems): α = %.3f\n",
            alpha_recon$total$raw_alpha))
cat(sprintf("  Nota e3_5: α con=%.3f | α sin e3_5=%.3f | diluye=%s\n",
            drop_e35$alpha_con, drop_e35$alpha_sin,
            as.character(drop_e35$diluye)))

# Correlación e3_4–e4_5 en subset
r_esc <- cor(as.numeric(subset_data$e3_4), as.numeric(subset_data$e4_5),
             use = "pairwise.complete.obs")
cat(sprintf("  r(e3_4, e4_5) en subset = %.3f (doble escaños; sensibilidad sin e3_4)\n", r_esc))

# Actualizar metadata
metadata$vd_demandas <- list(
  idx_dem_redistrib = ITEMS_REDIS,
  idx_dem_redistrib_sinescaduplic = ITEMS_REDIS_SENS,
  idx_dem_reconoc = ITEMS_RECON,
  decision_e3_4 = "incluido (r≈0.54); sensibilidad sin e3_4",
  nota_e3_5 = sprintf(
    "planitud descriptiva; α con=%.3f sin=%.3f; diluye=%s",
    drop_e35$alpha_con, drop_e35$alpha_sin, as.character(drop_e35$diluye)
  ),
  alpha_redis = unname(alpha_redis$total$raw_alpha),
  alpha_redis_sens = unname(alpha_redis_sens$total$raw_alpha),
  alpha_recon = unname(alpha_recon$total$raw_alpha),
  z_params = z_params,
  tipo = "indices_z_estandarizados_media0_de1",
  advertencia = "NO usar como control en modelos de violencia (post-tratamiento)"
)
saveRDS(metadata, "data/analysis_metadata.rds")
saveRDS(subset_data, "data/subset_data.rds")
saveRDS(panel_completo, "data/panel_completo.rds")
cat("✓ Índices guardados en subset_data / panel_completo / metadata\n")

# ══════════════════════════════════════════════════════════════════════════════
# PASO 3 — Estimación DiD
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 60), "\n")
cat("PASO 3 — DiD DEMANDAS (FE magro principal)\n")
cat(strrep("=", 60), "\n\n")

extract_fe_did <- function(model, term) {
  if (is.null(model)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  td <- broom::tidy(model)
  row <- td |> dplyr::filter(.data$term == .env$term)
  if (!nrow(row)) {
    row <- td |> dplyr::filter(grepl("decreto.*indi.*cerca|estallido.*indi.*cerca", term))
  }
  if (!nrow(row)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  tibble(
    estimate = row$estimate[1],
    std.error = row$std.error[1],
    p.value = row$p.value[1]
  )
}

extract_lmer_did <- function(model, term) {
  if (is.null(model)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  td <- broom.mixed::tidy(model, effects = "fixed")
  row <- td |> dplyr::filter(.data$term == .env$term)
  if (!nrow(row)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  tibble(
    estimate = row$estimate[1],
    std.error = row$std.error[1],
    p.value = row$p.value[1]
  )
}

VDS <- c("idx_dem_redistrib", "idx_dem_reconoc")
VD_LABELS <- c(
  idx_dem_redistrib = "Redistributivas / representación",
  idx_dem_reconoc   = "Reconocimiento cultural"
)

# ── 3a FE magro ───────────────────────────────────────────────────────────────
m_fe <- list()
for (vd in VDS) {
  m_fe[[vd]] <- tryCatch(
    feols(
      as.formula(paste0(vd, " ~ periodo * indigeneous * cerca_conflicto | folio")),
      data = subset_data, cluster = ~comuna_cod
    ),
    error = function(e) { cat("⚠ FE", vd, ":", conditionMessage(e), "\n"); NULL }
  )
}
# Sensibilidad sin e3_4
m_fe_sens <- tryCatch(
  feols(
    idx_dem_redistrib_sinescaduplic ~ periodo * indigeneous * cerca_conflicto | folio,
    data = subset_data, cluster = ~comuna_cod
  ),
  error = function(e) NULL
)

cat("FE magro — triple DiD:\n")
for (vd in VDS) {
  for (tm in c(TERM_DID_DECRETO, TERM_DID_POLITIZACION)) {
    r <- extract_fe_did(m_fe[[vd]], tm)
    lab <- if (grepl("decreto", tm)) "decreto" else "proceso"
    cat(sprintf("  %s [%s]: β=%.3f SE=%.3f p=%.3f %s\n",
                vd, lab, r$estimate, r$std.error, r$p.value, signif_stars(r$p.value)))
  }
}
r_sens <- extract_fe_did(m_fe_sens, TERM_DID_DECRETO)
cat(sprintf("  sensibilidad sin e3_4 [decreto]: β=%.3f p=%.3f %s\n",
            r_sens$estimate, r_sens$p.value, signif_stars(r_sens$p.value)))

# ── 3b RE lmer ────────────────────────────────────────────────────────────────
m_re <- list()
for (vd in VDS) {
  m_re[[vd]] <- tryCatch(
    lmer(
      as.formula(paste(
        vd, "~ periodo * indigeneous * cerca_conflicto +",
        controles_base, "+ (1 | folio)"
      )),
      data = subset_data, REML = FALSE
    ),
    error = function(e) { cat("⚠ RE", vd, ":", conditionMessage(e), "\n"); NULL }
  )
}

# ── 3c DR-DiD (helpers alineados con 03b: bootstrap por ID) ───────────────────
COVARS_BASE <- c(
  "mujer", "edad", "urbano_rural",
  "id_chile", "id_causa", "perc_desigualdad", "apoyo_movil"
)

prep_drdid_panel <- function(datos, ola_pre, ola_post, grupo = NULL) {
  d <- datos
  if (!is.null(grupo)) d <- d |> dplyr::filter(.data$indigeneous == grupo)
  d <- d |>
    dplyr::filter(.data$ola %in% c(ola_pre, ola_post)) |>
    dplyr::mutate(
      id = as.integer(factor(.data$folio)),
      year = as.integer(.data$ola),
      treat = as.integer(.data$cerca_conflicto == "cerca")
    )
  covar_cols <- COVARS_BASE[COVARS_BASE %in% names(d)]
  for (cv in covar_cols) d[[paste0(cv, "_n")]] <- as.numeric(d[[cv]])
  covar_n <- paste0(covar_cols, "_n")
  baseline_covs <- d |>
    dplyr::filter(.data$year == ola_pre) |>
    dplyr::select(dplyr::all_of(c("id", covar_n)))
  d <- d |>
    dplyr::select(-dplyr::all_of(covar_n)) |>
    dplyr::left_join(baseline_covs, by = "id") |>
    dplyr::filter(!is.na(.data$treat),
                  dplyr::if_all(dplyr::all_of(covar_n), ~ !is.na(.x))) |>
    dplyr::distinct(.data$id, .data$year, .keep_all = TRUE)
  ids_ok <- d |> dplyr::count(.data$id) |> dplyr::filter(.data$n == 2L) |> dplyr::pull(.data$id)
  d |> dplyr::filter(.data$id %in% ids_ok)
}

run_drdid_one <- function(panel_df, vd) {
  if (nrow(panel_df) < 40) return(NULL)
  panel_df$y <- as.numeric(panel_df[[vd]])
  panel_df <- panel_df[!is.na(panel_df$y), ]
  covar_n <- paste0(COVARS_BASE[COVARS_BASE %in% names(panel_df)], "_n")
  covar_n <- covar_n[vapply(covar_n, function(v) {
    v %in% names(panel_df) && var(panel_df[[v]], na.rm = TRUE) > 0
  }, logical(1))]
  xf <- if (length(covar_n)) as.formula(paste("~", paste(covar_n, collapse = "+"))) else ~1
  tryCatch(
    DRDID::drdid(
      yname = "y", tname = "year", idname = "id", dname = "treat",
      xformla = xf, data = as.data.frame(panel_df), panel = TRUE, boot = FALSE
    ),
    error = function(e) {
      tryCatch(
        DRDID::drdid(
          yname = "y", tname = "year", idname = "id", dname = "treat",
          xformla = ~1, data = as.data.frame(panel_df), panel = TRUE, boot = FALSE
        ),
        error = function(e2) NULL
      )
    }
  )
}

extract_att <- function(obj) {
  if (is.null(obj)) return(list(att = NA_real_, se = NA_real_))
  list(att = as.numeric(obj$ATT), se = as.numeric(obj$se))
}

# Bootstrap por ID (mismo nivel que 03b_drdid.R)
boot_att_diff <- function(datos, vd, ola_pre, ola_post, n_boot = 200L, seed = 2024) {
  set.seed(seed)
  resample_panel <- function(panel_df, samp_ids) {
    idx <- unlist(lapply(samp_ids, function(i) which(panel_df$id == i)), use.names = FALSE)
    out <- panel_df[idx, , drop = FALSE]
    out$id <- rep(seq_along(samp_ids), each = 2L)
    rownames(out) <- NULL
    out
  }
  calc_diff <- function(d_indi, d_noni) {
    att_i <- extract_att(run_drdid_one(d_indi, vd))$att
    att_n <- extract_att(run_drdid_one(d_noni, vd))$att
    if (is.na(att_i) || is.na(att_n)) return(NA_real_)
    att_i - att_n
  }
  p_indi <- prep_drdid_panel(datos, ola_pre, ola_post, "indi")
  p_noni <- prep_drdid_panel(datos, ola_pre, ola_post, "no_indi")
  if (nrow(p_indi) < 40 || nrow(p_noni) < 40) {
    return(list(diff = NA_real_, se = NA_real_, p = NA_real_,
                ci_lo = NA_real_, ci_hi = NA_real_))
  }
  p_indi <- p_indi[order(p_indi$id, p_indi$year), ]
  p_noni <- p_noni[order(p_noni$id, p_noni$year), ]
  att_i <- extract_att(run_drdid_one(p_indi, vd))
  att_n <- extract_att(run_drdid_one(p_noni, vd))
  diff <- att_i$att - att_n$att
  se <- sqrt(att_i$se^2 + att_n$se^2)
  z <- if (is.finite(se) && se > 0) diff / se else NA_real_
  p <- if (is.finite(z)) 2 * pnorm(-abs(z)) else NA_real_
  ids_i <- unique(p_indi$id); ids_n <- unique(p_noni$id)
  boots <- vapply(seq_len(n_boot), function(b) {
    suppressWarnings(calc_diff(
      resample_panel(p_indi, sample(ids_i, length(ids_i), replace = TRUE)),
      resample_panel(p_noni, sample(ids_n, length(ids_n), replace = TRUE))
    ))
  }, numeric(1))
  boots <- boots[is.finite(boots)]
  list(
    diff = diff, se = se, p = p,
    ci_lo = if (length(boots)) unname(quantile(boots, .025)) else NA_real_,
    ci_hi = if (length(boots)) unname(quantile(boots, .975)) else NA_real_
  )
}

drdid_rows <- list()
if (has_drdid) {
  cat("\n--- DR-DiD (bootstrap por ID, n=200; igual que 03b) ---\n")
  trans <- list(
    list(lab = "Decreto (2→4)", pre = 2L, post = 4L, datos = subset_data),
    list(lab = "Proceso (2→3)", pre = 2L, post = 3L, datos = subset_data),
    list(lab = "Placebo (1→2)", pre = 1L, post = 2L, datos = panel_completo)
  )
  for (tr in trans) {
    for (vd in VDS) {
      cat(" ", tr$lab, vd, "…\n")
      set.seed(2024)
      br <- boot_att_diff(tr$datos, vd, tr$pre, tr$post, n_boot = 200L, seed = 2024)
      drdid_rows[[paste(tr$lab, vd)]] <- tibble(
        modelo = paste0("DR-DiD ", tr$lab),
        vd = vd,
        term = "Δ ATT",
        estimate = br$diff,
        std.error = br$se,
        p.value = br$p,
        signif = signif_stars(br$p),
        ic95 = sprintf("[%.3f, %.3f]", br$ci_lo, br$ci_hi)
      )
      cat(sprintf("    Δ ATT=%.3f p=%.3f %s\n", br$diff, br$p, signif_stars(br$p)))
    }
  }
} else {
  cat("⚠ DRDID no disponible\n")
}

# ── 3d IPW + PSM ──────────────────────────────────────────────────────────────
cat("\n--- PSM / IPW ---\n")
baseline <- subset_data |>
  dplyr::filter(ola == 2) |>
  dplyr::mutate(tratado_zona = as.integer(cerca_conflicto == "cerca"))

ps_covars <- c("indigeneous", "mujer", "edad", "urbano_rural",
               "id_chile", "id_causa", "perc_desigualdad")
ps_formula <- as.formula(paste(
  "tratado_zona ~ indigeneous + mujer + edad + urbano_rural +",
  "id_chile + id_causa + perc_desigualdad"
))
baseline_cc <- baseline |> dplyr::filter(dplyr::if_all(dplyr::all_of(ps_covars), ~ !is.na(.x)))

set.seed(2024)
m_psm <- tryCatch(
  matchit(ps_formula, data = baseline_cc, method = "nearest",
          distance = "logit", caliper = 0.2, ratio = 1, replace = FALSE),
  error = function(e) NULL
)
m_ipw_fe <- list()
m_psm_fe <- list()
if (!is.null(m_psm)) {
  folios_m <- unique(match.data(m_psm)$folio)
  dat_m <- subset_data |> dplyr::filter(folio %in% folios_m)
  for (vd in VDS) {
    m_psm_fe[[vd]] <- tryCatch(
      feols(as.formula(paste0(vd, " ~ periodo * indigeneous * cerca_conflicto | folio")),
            data = dat_m, cluster = ~comuna_cod),
      error = function(e) NULL
    )
  }
}

set.seed(2024)
w_ipw <- tryCatch(
  weightit(ps_formula, data = baseline_cc, method = "ps", estimand = "ATE"),
  error = function(e) NULL
)
if (!is.null(w_ipw)) {
  pesos <- baseline_cc |> dplyr::mutate(w_ipw = w_ipw$weights) |> dplyr::select(folio, w_ipw)
  dat_w <- subset_data |> dplyr::left_join(pesos, by = "folio") |>
    dplyr::filter(!is.na(w_ipw), w_ipw > 0)
  for (vd in VDS) {
    m_ipw_fe[[vd]] <- tryCatch(
      feols(as.formula(paste0(vd, " ~ periodo * indigeneous * cerca_conflicto +", controles_base)),
            data = dat_w, weights = ~w_ipw, cluster = ~comuna_cod),
      error = function(e) NULL
    )
  }
}

# ── 3e Ordinal clmm sobre ítems representativos ───────────────────────────────
cat("\n--- Ordinal clmm (ítems, no índice z) ---\n")
collapse_sym <- function(x) {
  factor(
    dplyr::case_when(
      x %in% 1:2 ~ "Bajo",
      x == 3 ~ "Medio",
      x %in% 4:5 ~ "Alto",
      TRUE ~ NA_character_
    ),
    levels = c("Bajo", "Medio", "Alto"), ordered = TRUE
  )
}
dat_ord <- subset_data |>
  dplyr::mutate(
    e4_4_ord = collapse_sym(e4_4),
    e4_5_ord = collapse_sym(e4_5),
    e5_2_ord = collapse_sym(e5_2)
  )

form_clmm <- function(y) {
  as.formula(paste(
    y, "~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  ))
}

m_clmm <- list()
for (it in c("e4_4_ord", "e4_5_ord", "e5_2_ord")) {
  m_clmm[[it]] <- tryCatch(
    clmm(form_clmm(it), data = dat_ord),
    error = function(e) {
      cat("⚠ clmm", it, ":", conditionMessage(e), "\n"); NULL
    }
  )
}

extract_clmm <- function(model, term = TERM_DID_DECRETO) {
  if (is.null(model)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  td <- broom::tidy(model)
  row <- td |> dplyr::filter(.data$term == .env$term)
  if (!nrow(row)) {
    return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  }
  tibble(estimate = row$estimate[1], std.error = row$std.error[1], p.value = row$p.value[1])
}

# ══════════════════════════════════════════════════════════════════════════════
# PASO 4 — Tabla y figura
# ══════════════════════════════════════════════════════════════════════════════

mk_row <- function(modelo, vd, term_lab, est) {
  tibble(
    modelo = modelo,
    vd = vd,
    term = term_lab,
    estimate = round(est$estimate, 3),
    std.error = round(est$std.error, 3),
    p.value = round(est$p.value, 4),
    signif = signif_stars(est$p.value),
    ic95 = sprintf("[%.3f, %.3f]",
                   est$estimate - 1.96 * est$std.error,
                   est$estimate + 1.96 * est$std.error)
  )
}

tabla_rows <- list()
for (vd in VDS) {
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "FE magro (principal)", vd, "Decreto", extract_fe_did(m_fe[[vd]], TERM_DID_DECRETO))
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "FE magro (principal)", vd, "Proceso", extract_fe_did(m_fe[[vd]], TERM_DID_POLITIZACION))
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "RE lmer", vd, "Decreto", extract_lmer_did(m_re[[vd]], TERM_DID_DECRETO))
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "RE lmer", vd, "Proceso", extract_lmer_did(m_re[[vd]], TERM_DID_POLITIZACION))
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "PSM", vd, "Decreto", extract_fe_did(m_psm_fe[[vd]], TERM_DID_DECRETO))
  tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
    "IPW", vd, "Decreto", extract_fe_did(m_ipw_fe[[vd]], TERM_DID_DECRETO))
}
# DRDID
if (length(drdid_rows)) {
  for (r in drdid_rows) {
    term_lab <- dplyr::case_when(
      grepl("Decreto", r$modelo) ~ "Decreto",
      grepl("Proceso", r$modelo) ~ "Proceso",
      grepl("Placebo", r$modelo) ~ "Placebo",
      TRUE ~ "Δ ATT"
    )
    tabla_rows[[length(tabla_rows) + 1]] <- r |>
      dplyr::mutate(term = term_lab, modelo = "DR-DiD Δ ATT")
  }
}
# Sensibilidad sin e3_4
tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
  "FE magro (sin e3_4)", "idx_dem_redistrib", "Decreto",
  extract_fe_did(m_fe_sens, TERM_DID_DECRETO)
)
# Ordinal ítems
tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
  "clmm e4_4 (tierras)", "ítem redistrib", "Decreto", extract_clmm(m_clmm$e4_4_ord))
tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
  "clmm e4_5 (escaños)", "ítem redistrib", "Decreto", extract_clmm(m_clmm$e4_5_ord))
tabla_rows[[length(tabla_rows) + 1]] <- mk_row(
  "clmm e5_2 (símbolos)", "ítem reconoc", "Decreto", extract_clmm(m_clmm$e5_2_ord))

tabla_demandas <- dplyr::bind_rows(tabla_rows) |>
  dplyr::mutate(
    vd_label = dplyr::recode(vd,
      idx_dem_redistrib = "Redistributivas / representación",
      idx_dem_reconoc = "Reconocimiento cultural",
      .default = vd
    )
  )

cat("\n=== TABLA TRIPLE DiD DEMANDAS ===\n\n")
print(as.data.frame(tabla_demandas |> dplyr::select(
  modelo, vd_label, term, estimate, std.error, p.value, signif, ic95
)), row.names = FALSE)

tabla_demandas |>
  dplyr::select(modelo, vd_label, term, estimate, std.error, p.value, signif, ic95) |>
  gt(groupname_col = "vd_label") |>
  tab_header(
    title = "DiD demandas indígenas — triple interacción",
    subtitle = paste0(
      "Coeficientes: período × indígena × zona. ",
      "FE magro = principal. Índices z-score (media 0, DE 1). ",
      "α redistrib=", round(alpha_redis$total$raw_alpha, 3),
      " | α reconoc=", round(alpha_recon$total$raw_alpha, 3),
      " | r(e3_4,e4_5)=", round(r_esc, 3)
    )
  ) |>
  cols_label(
    modelo = "Especificación", term = "Shock",
    estimate = "β", std.error = "SE", p.value = "p",
    signif = "", ic95 = "IC 95%"
  ) |>
  tab_footnote(
    footnote = paste0(
      "Placebo DR-DiD ola 1→2 factible (ítems E en ola 1). ",
      "Bootstrap Δ ATT por ID (igual 03b). ",
      "Sensibilidad FE sin e3_4 (evita doble conteo escaños). ",
      "clmm sobre ítems 1–2/3/4–5, no sobre índice z. ",
      "Nota e3_5: ", metadata$vd_demandas$nota_e3_5
    )
  ) |>
  opt_stylize(style = 1, color = "blue") |>
  gtsave("output/tablas/tabla_demandas.html")
cat("✓ Tabla: output/tablas/tabla_demandas.html\n")

# ── Figura: medias observadas + IC 95% (misma escala en ambos paneles) ────────
dat_fig <- subset_data |>
  dplyr::select(folio, ola, indigeneous, cerca_conflicto,
                idx_dem_redistrib, idx_dem_reconoc) |>
  tidyr::pivot_longer(c(idx_dem_redistrib, idx_dem_reconoc),
                      names_to = "indice", values_to = "z") |>
  dplyr::mutate(
    grupo = paste(indigeneous, cerca_conflicto, sep = " / "),
    grupo = forcats::fct_recode(
      factor(grupo),
      "No indígena / lejos" = "no_indi / lejos",
      "No indígena / zona"  = "no_indi / cerca",
      "Indígena / lejos"    = "indi / lejos",
      "Indígena / zona"     = "indi / cerca"
    ),
    grupo = forcats::fct_relevel(
      grupo,
      "No indígena / lejos", "No indígena / zona",
      "Indígena / lejos", "Indígena / zona"
    ),
    indice = factor(
      indice,
      levels = c("idx_dem_redistrib", "idx_dem_reconoc"),
      labels = c("Redistributivas / representación", "Reconocimiento cultural")
    ),
    ola = factor(ola, labels = c("Ola 2\n(2018)", "Ola 3\n(2021)", "Ola 4\n(2023)"))
  )

resumen_fig <- dat_fig |>
  dplyr::filter(!is.na(z)) |>
  dplyr::group_by(indice, grupo, ola) |>
  dplyr::summarise(
    m = mean(z), se = sd(z) / sqrt(dplyr::n()), n = dplyr::n(),
    lo = m - 1.96 * se, hi = m + 1.96 * se, .groups = "drop"
  )

n_min <- resumen_fig |>
  dplyr::filter(grupo == "Indígena / zona") |>
  dplyr::pull(n) |>
  min()

# Escala común fija; rango ampliado vs borrador (-0.80, 0.12) para no cortar
# medias indígenas (hasta ~0.32) ni bandas IC (~-0.70…0.36).
p_dem <- ggplot(resumen_fig, aes(ola, m, color = grupo, fill = grupo,
                                 linetype = grupo, group = grupo)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ indice) +
  scale_y_continuous(limits = c(-0.75, 0.40)) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed", "solid")) +
  labs(
    title = "Apoyo a demandas indígenas — medias por grupo (IC 95%)",
    subtitle = paste0(
      "Índices z-score · trayectorias paralelas en redistributivas → ",
      "efecto de ciclo nacional, no territorial"
    ),
    x = NULL, y = "Media del índice (z)",
    color = NULL, fill = NULL, linetype = NULL,
    caption = paste0(
      "Medias observadas con IC 95%; misma escala en ambos paneles. ",
      "La celda Indígena/zona (n≈", n_min, ") es la más pequeña y volátil; ",
      "la interacción triple período×indígena×zona NO es significativa (ver tabla)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, color = "gray35", size = 9),
    panel.grid.minor = element_blank()
  )

ggsave("output/figuras/fig_demandas.png", p_dem, width = 11, height = 5, dpi = 300)
cat("✓ Figura: output/figuras/fig_demandas.png\n")

# MDE (potencia 80%, α .05 dos colas) a partir de SE del FE / DR-DiD
mde_z <- function(se, power = .80, alpha = .05) {
  (qnorm(1 - alpha / 2) + qnorm(power)) * se
}
se_fe_r <- extract_fe_did(m_fe$idx_dem_redistrib, TERM_DID_DECRETO)$std.error
se_fe_c <- extract_fe_did(m_fe$idx_dem_reconoc, TERM_DID_DECRETO)$std.error
cat(sprintf("MDE FE redistrib (SE=%.3f): %.3f z-SD\n", se_fe_r, mde_z(se_fe_r)))
cat(sprintf("MDE FE reconoc   (SE=%.3f): %.3f z-SD\n", se_fe_c, mde_z(se_fe_c)))
cat(sprintf("MDE DR-DiD Δ (SE≈0.156): %.3f z-SD\n", mde_z(0.156)))

# ── Nota interpretativa automática ────────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("INTERPRETACIÓN AUTOMÁTICA — SECURITIZACIÓN\n")
cat(strrep("=", 60), "\n\n")

fe_dec_r <- extract_fe_did(m_fe$idx_dem_redistrib, TERM_DID_DECRETO)
fe_dec_c <- extract_fe_did(m_fe$idx_dem_reconoc, TERM_DID_DECRETO)
fe_pro_r <- extract_fe_did(m_fe$idx_dem_redistrib, TERM_DID_POLITIZACION)
fe_pro_c <- extract_fe_did(m_fe$idx_dem_reconoc, TERM_DID_POLITIZACION)

sig_neg <- function(r) isTRUE(r$estimate < 0 && !is.na(r$p.value) && r$p.value < .1)
sig_pos <- function(r) isTRUE(r$estimate > 0 && !is.na(r$p.value) && r$p.value < .1)
ns <- function(r) isTRUE(is.na(r$p.value) || r$p.value >= .1)

if (sig_neg(fe_dec_r) && ns(fe_dec_c)) {
  cat("→ Patrón: redistrib ↓ significativo y reconoc ≈ 0\n")
  cat("  SECURITIZACIÓN SELECTIVA: se erosiona la demanda política-redistributiva,\n")
  cat("  no la cultural.\n")
} else if (sig_neg(fe_dec_r) && sig_neg(fe_dec_c)) {
  cat("→ Patrón: ambos ↓\n")
  cat("  DESGASTE GENERAL del ciclo de demandas.\n")
} else if (ns(fe_dec_r) && ns(fe_dec_c)) {
  cat("→ Triple ≈ 0 en ambos índices.\n")
  cat("  Si hay caída agregada (efecto de período, no triple) → fenómeno\n")
  cat("  MACRO-NACIONAL (securitización de país), no efecto territorial del decreto.\n")
  # Chequeo efecto período agregado
  if (!is.null(m_fe$idx_dem_redistrib)) {
    td <- broom::tidy(m_fe$idx_dem_redistrib)
    per <- td |> dplyr::filter(term %in% c("periododecreto", "periodoestallido"))
    print(per |> dplyr::select(term, estimate, p.value))
  }
} else {
  cat("→ Patrón mixto / positivo. Revisar coeficientes manualmente.\n")
}
cat(sprintf("\nFE decreto redistrib: β=%.3f p=%.3f %s\n",
            fe_dec_r$estimate, fe_dec_r$p.value, signif_stars(fe_dec_r$p.value)))
cat(sprintf("FE decreto reconoc:   β=%.3f p=%.3f %s\n",
            fe_dec_c$estimate, fe_dec_c$p.value, signif_stars(fe_dec_c$p.value)))
cat(sprintf("FE proceso redistrib: β=%.3f p=%.3f %s\n",
            fe_pro_r$estimate, fe_pro_r$p.value, signif_stars(fe_pro_r$p.value)))
cat(sprintf("FE proceso reconoc:   β=%.3f p=%.3f %s\n",
            fe_pro_c$estimate, fe_pro_c$p.value, signif_stars(fe_pro_c$p.value)))

# Grep-control: índices NO deben usarse como controles de violencia
stopifnot(!grepl("idx_dem_", controles_base))

saveRDS(
  list(
    tabla_demandas = tabla_demandas,
    m_fe = m_fe, m_re = m_re, m_fe_sens = m_fe_sens,
    m_psm_fe = m_psm_fe, m_ipw_fe = m_ipw_fe, m_clmm = m_clmm,
    alpha_redis = alpha_redis$total$raw_alpha,
    alpha_redis_sens = alpha_redis_sens$total$raw_alpha,
    alpha_recon = alpha_recon$total$raw_alpha,
    r_e34_e45 = r_esc,
    drop_e35 = drop_e35,
    z_params = z_params,
    items_redis = ITEMS_REDIS,
    items_recon = ITEMS_RECON,
    bootstrap_cluster_level = "ID (folio) — igual que 03b_drdid.R"
  ),
  "data/demandas.rds"
)

cat("\n✓ 08c_demandas.R ejecutado correctamente.\n")
cat("✓ Objetos: data/demandas.rds\n")
