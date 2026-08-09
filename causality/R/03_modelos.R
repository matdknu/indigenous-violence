# =============================================================================
# 03_modelos.R — Modelos multinivel DiD (cuasi-experimental exploratorio)
#
# Propósito: estimar modelos DiD para las dos VD de ÍTEM ÚNICO del proyecto:
#            idx_vio_control = d3_1 (fuerza de Carabineros / coerción estatal)
#            idx_vio_resguardo = d4_3 (cortes de camino / protesta disruptiva)
#            Modelo PRINCIPAL: feols FE individuo (magro) + cluster por comuna.
#            Sensibilidad: lmer RE. El análisis electoral fue eliminado — FASE 2.
# Input:     data/subset_data.rds
# Output:    output/tablas/tabla_modelos.html
#            output/figuras/fig_coeficientes.png
#
# Modelos DiD (dos shocks secuenciales):
#   A — transición estallido (ola 2→3): T1_estallido
#   B — transición decreto/Apruebo (ola 3→4): T2_decreto
#   C — tres períodos (principal): periodo × indígena × zona
# VDs: idx_vio_control (status quo) e idx_vio_resguardo (cambio social)
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, performance,
  broom.mixed, modelsummary, marginaleffects, ggplot2, stringr, fixest
)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
source("R/plot_helpers.R")
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")

# ── Test de colinealidad urbano_rural × cerca_conflicto ───────────────────────

cat("\n", strrep("=", 60), "\n")
cat("TEST COLINEALIDAD: urbano_rural × cerca_conflicto\n")
cat(strrep("=", 60), "\n\n")

cor_ur_cc <- cor(
  as.numeric(subset_data$urbano_rural),
  as.numeric(subset_data$cerca_conflicto),
  use = "complete.obs"
)
incluir_urbano_rural <- abs(cor_ur_cc) <= 0.5

cat("Correlación urbano_rural ~ cerca_conflicto: r =", round(cor_ur_cc, 3), "\n")
cat(
  "Decisión:",
  if (incluir_urbano_rural) {
    "INCLUIR urbano_rural (r <= .5, colinealidad aceptable)"
  } else {
    "EXCLUIR urbano_rural (r > .5, absorbe efecto zona)"
  },
  "\n\n"
)
cat("Distribución urbano_rural × cerca_conflicto:\n")
print(table(subset_data$urbano_rural, subset_data$cerca_conflicto, useNA = "ifany"))
cat("\n")

if (file.exists("data/analysis_metadata.rds")) {
  metadata <- readRDS("data/analysis_metadata.rds")
  controles_base <- metadata$controles_base
  cat("Controles (desde analysis_metadata.rds):", controles_base, "\n\n")
} else {
  controles_base <- if (incluir_urbano_rural) {
    "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
  } else {
    "mujer + edad + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
  }
  cat("Controles en modelos:", controles_base, "\n\n")
}

# ── Coeficientes de interés (DiD triple) ────────────────────────────────────────

TERM_DID_ESTALLIDO <- "periodoestallido:indigeneousindi:cerca_conflictocerca"
TERM_DID_DECRETO   <- "periododecreto:indigeneousindi:cerca_conflictocerca"
TERM_DID_A         <- "T1_estallido:indigeneousindi:cerca_conflictocerca"
TERM_DID_B         <- "T2_decreto:indigeneousindi:cerca_conflictocerca"

coef_rename_did <- c(
  "(Intercept)"          = "Intercepto",
  "periodoestallido"     = "Ola 3 — Proceso de politización (2019–2021)",
  "periododecreto"         = "Ola 4 — Decreto + cierre institucional",
  "T1_estallido"         = "Transición proceso politización (ola 2→3)",
  "T2_decreto"           = "Transición decreto (ola 3→4)",
  "indigeneousindi"      = "Indígena",
  "cerca_conflictocerca" = "Zona excepción (decreto)",
  "mujermujer"           = "Mujer",
  "urbano_rural2"        = "Rural",
  "urbano_rural"         = "Rural",
  "edad25_34"            = "Edad 25–34",
  "edad35_44"            = "Edad 35–44",
  "edad45_54"            = "Edad 45–54",
  "edad55_64"            = "Edad 55–64",
  "edad65+"              = "Edad 65+",
  "id_chile"             = "Id. con Chile",
  "id_causa"             = "Id. causa indígena",
  "perc_desigualdad"     = "Perc. desigualdad",
  "periodoestallido:indigeneousindi"      = "Ola 3 (proceso) × Indígena",
  "periododecreto:indigeneousindi"          = "Ola 4 (decreto) × Indígena",
  "periodoestallido:cerca_conflictocerca" = "Ola 3 (proceso) × Zona excepción",
  "periododecreto:cerca_conflictocerca"     = "Ola 4 (decreto) × Zona excepción",
  "indigeneousindi:cerca_conflictocerca"  = "Indígena × Zona excepción",
  "T1_estallido:indigeneousindi"          = "Proceso × Indígena",
  "T2_decreto:indigeneousindi"            = "Decreto × Indígena",
  "T1_estallido:cerca_conflictocerca"     = "Proceso × Zona excepción",
  "T2_decreto:cerca_conflictocerca"       = "Decreto × Zona excepción",
  "periodoestallido:indigeneousindi:cerca_conflictocerca" =
    "Ola 3 × Indígena × Zona [DiD proceso]",
  "periododecreto:indigeneousindi:cerca_conflictocerca" =
    "Ola 4 × Indígena × Zona [DiD decreto]",
  "T1_estallido:indigeneousindi:cerca_conflictocerca" =
    "Proceso × Indígena × Zona [DiD]",
  "T2_decreto:indigeneousindi:cerca_conflictocerca" =
    "Decreto × Indígena × Zona [DiD]"
)

print_did <- function(model, label) {
  cat(label, "— coeficientes DiD:\n")
  print(
    broom.mixed::tidy(model, effects = "fixed") |>
      filter(str_detect(term, "indigeneous") & str_detect(term, "cerca_conflicto")) |>
      filter(str_detect(term, "estallido|decreto|T1|T2|periodo"))
  )
  cat("\n")
}

# ── MODELO PRINCIPAL: FE individuo (magro) + clúster por comuna ───────────────
#
# Interpretación: con FE de individuo (folio) dejamos de comparar indígenas
# vs. no indígenas (nivel confundido por factores estructurales) y pasamos a
# comparar el CAMBIO INTRA-PERSONA alrededor del decreto. La identificación
# descansa en el timing del D.S. 418 (12 oct 2021), plausiblemente exógeno
# a las actitudes individuales. El estimando es un EFECTO TERRITORIAL/
# CONTEXTUAL de vivir en territorio militarizado.
#
# NO se incluyen controles: los términos invariantes se absorben por FE;
# los actitudinales variables (apoyo_movil, id_causa, etc.) son potencialmente
# post-tratamiento y su inclusión introduciría sesgo (bad controls).
#
# Términos omitidos por colinealidad con el FE (se esperan warnings):
#   indigeneous, cerca_conflicto, indigeneous:cerca_conflicto

cat("\n", strrep("=", 60), "\n")
cat("MODELO PRINCIPAL: FE individuo + clúster por comuna\n")
cat(strrep("=", 60), "\n\n")

# ── Verificación de movers (individuo en >1 comuna) ──────────────────────────
movers <- subset_data |>
  dplyr::group_by(.data$folio) |>
  dplyr::summarise(n_comunas = dplyr::n_distinct(.data$comuna_cod), .groups = "drop") |>
  dplyr::filter(.data$n_comunas > 1)

n_movers <- nrow(movers)
cat("Movers (folio con >1 comuna entre olas):", n_movers, "\n")

# Si hay movers, añadir FE de comuna; si no, | folio basta (comuna anidada en folio)
fe_spec <- if (n_movers > 0) {
  cat("→ FE spec: | folio + comuna_cod (hay movers)\n\n")
  "folio + comuna_cod"
} else {
  cat("→ FE spec: | folio  (sin movers; cluster comuna cubre Moulton)\n\n")
  "folio"
}

n_comunas_tratadas_03 <- subset_data |>
  dplyr::filter(.data$ola == 4, .data$indigeneous == "indi",
                .data$cerca_conflicto == "cerca") |>
  dplyr::summarise(n = dplyr::n_distinct(.data$comuna_cod)) |>
  dplyr::pull(.data$n)
cat("Comunas en celda tratada (indi×cerca×ola4):", n_comunas_tratadas_03, "\n")
if (n_comunas_tratadas_03 < 30) {
  cat("⚠  Comunas tratadas <30 — SE cluster pueden ser imprecisos;\n")
  cat("   considerar wild cluster bootstrap (ver 04_robustez.R).\n")
}
cat("\n")

# ── Modelo FE tres períodos (principal) ───────────────────────────────────────

mFE_ctrl <- tryCatch(
  feols(
    as.formula(paste0(
      "idx_vio_control ~ periodo * indigeneous * cerca_conflicto | ", fe_spec
    )),
    data    = subset_data,
    cluster = ~comuna_cod
  ),
  error = function(e) {
    cat("⚠ feols FE ctrl:", conditionMessage(e), "\n"); NULL
  }
)

mFE_resg <- tryCatch(
  feols(
    as.formula(paste0(
      "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto | ", fe_spec
    )),
    data    = subset_data,
    cluster = ~comuna_cod
  ),
  error = function(e) {
    cat("⚠ feols FE resg:", conditionMessage(e), "\n"); NULL
  }
)

# ── Coeficientes DiD del modelo FE ───────────────────────────────────────────

print_did_fe <- function(model, label, pattern_tiempo = "periodo") {
  if (is.null(model)) { cat(label, "— modelo no disponible\n\n"); return(invisible(NULL)) }
  cat(label, "— coeficientes DiD (FE folio + cluster comuna):\n")
  td <- broom::tidy(model, conf.int = TRUE)
  print(
    td |> dplyr::filter(
      stringr::str_detect(term, "indigeneous") &
      stringr::str_detect(term, "cerca_conflicto") &
      stringr::str_detect(term, pattern_tiempo)
    )
  )
  cat("\n")
}

print_did_fe(mFE_ctrl, "Control social (status quo)")
print_did_fe(mFE_resg, "Cambio social")

# ── Tabla FE (principal) ──────────────────────────────────────────────────────

if (!is.null(mFE_ctrl) && !is.null(mFE_resg)) {
  modelsummary(
    list(
      "Control social — FE magro" = mFE_ctrl,
      "Cambio social — FE magro"  = mFE_resg
    ),
    statistic = "({std.error})",
    stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
    fmt = 3,
    coef_rename = coef_rename_did,
    coef_omit = "Intercept",
    gof_map = c("nobs", "r.squared"),
    notes = paste0(
      "Modelo principal: FE individuo (folio) + clúster por comuna (", fe_spec, "). ",
      "D.S. 418, 12 oct 2021. ",
      "Estimando: cambio diferencial compatible con efecto territorial del decreto. ",
      "Términos invariantes absorbidos por FE (indigeneous, zona omitidos)."
    ),
    output = "output/tablas/tabla_fe_principal.html"
  )
  cat("✓ Tabla FE principal guardada: output/tablas/tabla_fe_principal.html\n\n")
}

# ── Modelo FE transición decreto (ola 3 → 4, especificación B) ────────────────

mFE_B_ctrl <- tryCatch(
  feols(
    as.formula(paste0(
      "idx_vio_control ~ T2_decreto * indigeneous * cerca_conflicto | ", fe_spec
    )),
    data    = subset_data |> dplyr::filter(ola %in% c(3, 4)),
    cluster = ~comuna_cod
  ),
  error = function(e) { cat("⚠ feols FE-B ctrl:", conditionMessage(e), "\n"); NULL }
)

mFE_B_resg <- tryCatch(
  feols(
    as.formula(paste0(
      "idx_vio_resguardo ~ T2_decreto * indigeneous * cerca_conflicto | ", fe_spec
    )),
    data    = subset_data |> dplyr::filter(ola %in% c(3, 4)),
    cluster = ~comuna_cod
  ),
  error = function(e) { cat("⚠ feols FE-B resg:", conditionMessage(e), "\n"); NULL }
)

cat("FE-B (solo transición decreto ola 3→4):\n")
print_did_fe(mFE_B_ctrl, "Control social", pattern_tiempo = "T2_decreto")
print_did_fe(mFE_B_resg, "Cambio social",  pattern_tiempo = "T2_decreto")

# ── Modelo A: transición proceso de politización (ola 2 → ola 3) ──────────────

cat("\n", strrep("=", 60), "\n")
cat("SENSIBILIDAD: Efectos aleatorios por individuo (lmer)\n")
cat(strrep("=", 60), "\n")
cat("(Nota: RE como sensibilidad al FE principal; sin cluster en lmer — ver 04_robustez.R)\n\n")

cat("--- Modelo A: Proceso de politización (ola 2 → ola 3) ---\n\n")

datos_A <- subset_data |> filter(ola %in% c(2, 3))

mA_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ T1_estallido * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = datos_A, REML = FALSE
)

mA_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ T1_estallido * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = datos_A, REML = FALSE
)

icc_A <- performance::icc(
  lmer(idx_vio_control ~ 1 + (1 | folio), data = datos_A, REML = TRUE)
)$ICC_adjusted
cat("ICC estallido (ctrl):", round(as.numeric(icc_A), 3), "\n\n")
print_did(mA_ctrl, "Control social (status quo)")
print_did(mA_resg, "Cambio social")

# ── Modelo B: transición decreto/Apruebo (ola 3 → ola 4) ──────────────────────

cat("--- Modelo B: Transición decreto/cierre institucional (ola 3 → ola 4) ---\n\n")

datos_B <- subset_data |> filter(ola %in% c(3, 4))

mB_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ T2_decreto * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = datos_B, REML = FALSE
)

mB_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ T2_decreto * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = datos_B, REML = FALSE
)

print_did(mB_ctrl, "Control social (status quo)")
print_did(mB_resg, "Cambio social")

# ── Modelo C: tres períodos (principal) ───────────────────────────────────────

cat("--- Modelo C: Tres períodos (principal del paper) ---\n\n")

mC_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

mC_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("ICC control (Modelo C):",
    round(as.numeric(performance::icc(
      lmer(idx_vio_control ~ 1 + (1 | folio), data = subset_data, REML = TRUE)
    )$ICC_adjusted), 3), "\n\n")
print_did(mC_ctrl, "Control social (status quo)")
print_did(mC_resg, "Cambio social")

# Aliases para scripts que esperan m2_*
m2_ctrl <- mC_ctrl
m2_resg <- mC_resg

# ── Tabla principal — Modelo C ────────────────────────────────────────────────

modelsummary(
  list(
    "Control social (status quo)" = mC_ctrl,
    "Cambio social"               = mC_resg
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_did,
  coef_omit = "edad|mujer|urbano|id_chile|id_causa|perc_",
  gof_map = c("nobs", "icc", "rmse"),
  notes = paste0(
    "Modelo C: tres períodos, ref. = ola 2 (2018). ",
    "Coeficientes de interés: DiD estallido (ola 3) y DiD decreto (ola 4). ",
    "Efectos aleatorios por individuo (folio)."
  ),
  output = "output/tablas/tabla_modelos.html"
)
cat("✓ Tabla principal guardada: output/tablas/tabla_modelos.html\n")

modelsummary(
  list(
    "Ctrl — Estallido (A)" = mA_ctrl,
    "Cambio — Estallido (A)" = mA_resg,
    "Ctrl — Decreto (B)"   = mB_ctrl,
    "Cambio — Decreto (B)" = mB_resg,
    "Ctrl — Tres períodos (C)" = mC_ctrl,
    "Cambio — Tres períodos (C)" = mC_resg
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_did,
  gof_map = c("nobs", "icc", "rmse"),
  output = "output/tablas/tabla_apendice_m1_m2.html"
)
cat("✓ Tabla apéndice (A/B/C) guardada: output/tablas/tabla_apendice_m1_m2.html\n")

modelsummary(
  list(
    "Ctrl — Estallido (A)" = mA_ctrl,
    "Cambio — Estallido (A)" = mA_resg,
    "Ctrl — Decreto (B)"   = mB_ctrl,
    "Cambio — Decreto (B)" = mB_resg
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_did,
  gof_map = c("nobs", "icc", "rmse"),
  output = "output/tablas/tabla_modelos_AB.html"
)
cat("✓ Tabla modelos A/B guardada: output/tablas/tabla_modelos_AB.html\n")

# ── Figura 2 — Medias predichas (Modelo C) ────────────────────────────────────

PERIODO_LABELS <- c(
  "pre"       = "Ola 2\n(2018)",
  "estallido" = "Ola 3\n(2021)",
  "decreto"   = "Ola 4\n(2023)"
)
pred_grupo <- function(model, vd_label, periodo_levels) {
  marginaleffects::predictions(
    model,
    newdata = datagrid(
      periodo         = periodo_levels,
      indigeneous     = c("no_indi", "indi"),
      cerca_conflicto = c("lejos", "cerca")
    )
  ) |>
    as_tibble() |>
    mutate(
      vd = vd_label,
      grupo = factor(
        paste0(indigeneous, " — ", cerca_conflicto),
        levels = c(
          "no_indi — lejos", "no_indi — cerca",
          "indi — lejos",    "indi — cerca"
        ),
        labels = c(
          "No indígena / lejos", "No indígena / zona excepción",
          "Indígena / lejos",    "Indígena / zona excepción"
        )
      ),
      periodo_num = match(periodo, periodo_levels),
      periodo = factor(periodo, levels = periodo_levels, labels = PERIODO_LABELS[periodo_levels])
    )
}

pred_C <- bind_rows(
  pred_grupo(mC_ctrl, "Control social (status quo)", c("pre", "estallido", "decreto")),
  pred_grupo(mC_resg, "Cambio social", c("pre", "estallido", "decreto"))
) |>
  mutate(
    vd = factor(vd, levels = c("Control social (status quo)", "Cambio social"))
  )

p_medias_C <- ggplot(pred_C,
                     aes(x = periodo_num, y = estimate,
                         color = grupo, linetype = grupo, group = grupo)) +
  annotate("rect",
           xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "#FFE0E0", alpha = 0.4) +
  annotate("text", x = 3, y = Inf, label = "Decreto +\nCierre inst.",
           vjust = 1.4, size = 3, color = "#B22222") +
  geom_line(linewidth = 0.9, show.legend = TRUE) +
  geom_point(size = 2.5, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.08, linewidth = 0.4, show.legend = FALSE
  ) +
  facet_wrap(~ vd, scales = "fixed", ncol = 2) +
  scale_x_continuous(
    breaks = 1:3,
    labels = unname(PERIODO_LABELS[c("pre", "estallido", "decreto")])
  ) +
  scale_y_likert_shared() +
  labs(
    title    = "Medias predichas por grupo — Modelo C (tres períodos)",
    subtitle = paste0(
      "Ola 2 = baseline (2018) · Ola 3 = proceso de politización (2019–2021) · ",
      "Ola 4 = decreto + cierre institucional (2023)\n",
      "Zona sombreada = post-decreto · IC 95%"
    ),
    x = NULL, y = "Media predicha (escala 1–5)",
    caption  = "Línea sólida = zona de excepción (53 comunas decreto) · punteada = lejos"
  )

p_medias_C <- add_scale_grupo_trajectory(p_medias_C) +
  theme_trajectory(base_size = 11)

ggsave("output/figuras/fig_medias_predichas.png", p_medias_C,
       width = 12, height = 6, dpi = 300)
cat("✓ Figura medias predichas guardada: output/figuras/fig_medias_predichas.png\n")

# ── Figura apéndice — Coeficientes Modelo C ───────────────────────────────────

coefs_all <- bind_rows(
  broom.mixed::tidy(mC_ctrl, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Control social (status quo)"),
  broom.mixed::tidy(mC_resg, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Cambio social")
) |>
  filter(term != "(Intercept)") |>
  mutate(
    es_did = str_detect(term, ":"),
    modelo = factor(modelo, levels = c("Control social (status quo)", "Cambio social")),
    term_label = recode(term, !!!coef_rename_did),
    term_label = if_else(is.na(term_label), term, term_label)
  )

p_coef <- ggplot(coefs_all,
                 aes(x = estimate,
                     y = reorder(term_label, estimate),
                     xmin = conf.low, xmax = conf.high,
                     color = es_did, alpha = es_did)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(linewidth = 0.5) +
  facet_wrap(~ modelo, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c("FALSE" = "grey50", "TRUE" = "#D73027"),
    labels = c("Efecto principal", "Interacción DiD"),
    name = NULL
  ) +
  scale_alpha_manual(values = c("FALSE" = 0.6, "TRUE" = 1.0), guide = "none") +
  labs(
    title    = "Efectos fijos — Modelo C (tres períodos)",
    subtitle = "Rojo = interacciones DiD; IC 95%",
    x = "Coeficiente estimado", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold")
  )

ggsave("output/figuras/fig_coeficientes.png", p_coef,
       width = 13, height = 9, dpi = 300)
cat("✓ Figura coeficientes guardada: output/figuras/fig_coeficientes.png\n")

cat("\n✓ 03_modelos.R ejecutado correctamente.\n")

saveRDS(
  list(
    # Modelo principal: FE magro + cluster por comuna
    mFE_ctrl   = mFE_ctrl,   mFE_resg   = mFE_resg,
    mFE_B_ctrl = mFE_B_ctrl, mFE_B_resg = mFE_B_resg,
    fe_spec    = fe_spec,
    n_comunas_tratadas = n_comunas_tratadas_03,
    # Sensibilidad: efectos aleatorios por individuo (lmer)
    mC_ctrl = mC_ctrl, mC_resg = mC_resg,
    mA_ctrl = mA_ctrl, mA_resg = mA_resg,
    mB_ctrl = mB_ctrl, mB_resg = mB_resg,
    m2_ctrl = mC_ctrl, m2_resg = mC_resg,
    controles_base = controles_base,
    incluir_urbano_rural = incluir_urbano_rural,
    coef_rename_did = coef_rename_did,
    TERM_DID_ESTALLIDO = TERM_DID_ESTALLIDO,
    TERM_DID_DECRETO = TERM_DID_DECRETO,
    TERM_DID_A = TERM_DID_A,
    TERM_DID_B = TERM_DID_B
  ),
  "data/modelos.rds"
)
cat("✓ Modelos guardados: data/modelos.rds\n")
