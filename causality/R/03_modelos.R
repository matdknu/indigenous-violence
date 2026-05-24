# =============================================================================
# 03_modelos.R — Modelos multinivel DiD y plebiscito 2022
#
# Propósito: estimar modelos DiD multinivel, modelos logísticos de voto
#            y exportar tablas/figuras para el paper.
# Input:     data/subset_data.rds
# Output:    output/tablas/tabla_modelos.html
#            output/tablas/tabla_plebiscito.html
#            output/figuras/fig_coeficientes.png
#            output/figuras/fig_prob_rechazo.png
#
# Modelos DiD:
#   M0 — nulo (ICC)
#   M1 — efectos principales
#   M2 — interacción triple (DiD)
# VDs: idx_vio_control (control social) e idx_vio_resguardo (resguardo territorial)
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, performance,
  broom.mixed, modelsummary, marginaleffects, ggplot2, stringr
)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
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

controles_base <- if (incluir_urbano_rural) {
  "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + perc_injusticia"
} else {
  "mujer + edad + id_chile + id_causa + perc_desigualdad + perc_injusticia"
}
cat("Controles en modelos:", controles_base, "\n\n")

# ── Modelos multinivel DiD — Violencia de control ─────────────────────────────

cat("--- Modelo: Justificación de violencia de CONTROL ---\n\n")

m0_ctrl <- lmer(
  idx_vio_control ~ 1 + (1 | folio),
  data = subset_data, REML = TRUE
)
cat("ICC violencia control:", round(as.numeric(performance::icc(m0_ctrl)$ICC_adjusted), 3), "\n\n")

m1_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous + cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

m2_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("Comparación M1 vs M2 (LRT):\n")
print(anova(m1_ctrl, m2_ctrl))
cat("\nResumen M2 (DiD) — Violencia de control:\n")
print(summary(m2_ctrl))

# ── Modelos multinivel DiD — Violencia de resguardo territorial ────────────────

cat("\n--- Modelo: Justificación de violencia de RESGUARDO territorial ---\n\n")

m0_resg <- lmer(
  idx_vio_resguardo ~ 1 + (1 | folio),
  data = subset_data, REML = TRUE
)
cat("ICC resguardo territorial:", round(as.numeric(performance::icc(m0_resg)$ICC_adjusted), 3), "\n\n")

m1_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous + cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

m2_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("Comparación M1 vs M2 (LRT):\n")
print(anova(m1_resg, m2_resg))
cat("\nResumen M2 (DiD) — Resguardo territorial:\n")
print(summary(m2_resg))

# ── Tabla 3 — Modelos DiD principales ─────────────────────────────────────────

coef_rename_did <- c(
  "(Intercept)"          = "Intercepto",
  "periodotratamiento"   = "Ola 3 (tratamiento)",
  "periodopost"          = "Ola 4 (post)",
  "indigeneousindi"      = "Indígena",
  "cerca_conflictocerca" = "Zona excepción",
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
  "perc_injusticia"      = "Perc. injusticia",
  "periodotratamiento:indigeneousindi" = "Ola 3 × Indígena",
  "periodopost:indigeneousindi"        = "Ola 4 × Indígena",
  "periodotratamiento:cerca_conflictocerca" = "Ola 3 × Zona excepción",
  "periodopost:cerca_conflictocerca"        = "Ola 4 × Zona excepción",
  "indigeneousindi:cerca_conflictocerca"    = "Indígena × Zona excepción",
  "periodotratamiento:indigeneousindi:cerca_conflictocerca" = "Ola 3 × Indígena × Zona [DiD]",
  "periodopost:indigeneousindi:cerca_conflictocerca"        = "Ola 4 × Indígena × Zona [DiD]"
)

modelsummary(
  list(
    "Control social (M1)"         = m1_ctrl,
    "Control social (DiD)"        = m2_ctrl,
    "Cambio social (M1)"        = m1_resg,
    "Cambio social (DiD)"        = m2_resg
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_did,
  gof_map = c("nobs", "icc", "rmse"),
  output = "output/tablas/tabla_modelos.html"
)
cat("✓ Tabla 3 guardada: output/tablas/tabla_modelos.html\n")

# Tabla A1 (apéndice): mismos modelos, etiquetas para apéndice
modelsummary(
  list(
    "Control social (M1)" = m1_ctrl,
    "Control social (M2)" = m2_ctrl,
    "Cambio social (M1)"  = m1_resg,
    "Cambio social (M2)"  = m2_resg
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_did,
  gof_map = c("nobs", "icc", "rmse"),
  output = "output/tablas/tabla_apendice_m1_m2.html"
)
cat("✓ Tabla A1 (apéndice) guardada: output/tablas/tabla_apendice_m1_m2.html\n")

# ── Figura 2 — Medias predichas por grupo (modelos M2) ────────────────────────

pred_grid <- function(model, vd_label) {
  marginaleffects::predictions(
    model,
    newdata = datagrid(
      periodo         = c("pre", "tratamiento", "post"),
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
      )
    )
}

pred_medias <- bind_rows(
  pred_grid(m2_ctrl, "Control social (status quo)"),
  pred_grid(m2_resg, "Cambio social")
) |>
  mutate(
    vd = factor(vd, levels = c("Control social (status quo)", "Cambio social")),
    periodo = factor(
      periodo,
      levels = c("pre", "tratamiento", "post"),
      labels = c("Ola 2\n(Pre)", "Ola 3\n(Tratamiento)", "Ola 4\n(Post)")
    )
  )

p_medias <- ggplot(pred_medias,
                   aes(x = periodo, y = estimate,
                       color = grupo, linetype = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, linewidth = 0.5) +
  facet_wrap(~ vd, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c(
      "No indígena / lejos"          = "#4575B4",
      "No indígena / zona excepción" = "#74ADD1",
      "Indígena / lejos"             = "#D73027",
      "Indígena / zona excepción"    = "#F46D43"
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "No indígena / lejos"          = "dashed",
      "No indígena / zona excepción" = "solid",
      "Indígena / lejos"             = "dashed",
      "Indígena / zona excepción"    = "solid"
    ),
    name = NULL
  ) +
  labs(
    title    = "Medias predichas por grupo (modelos DiD, M2)",
    subtitle = "Controles en valores de referencia · IC 95%",
    x = NULL, y = "Media predicha (escala 1–5)",
    caption  = "Línea sólida = zona de excepción · Línea punteada = lejos del conflicto"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_medias_predichas.png", p_medias,
       width = 11, height = 5.5, dpi = 300)
cat("✓ Figura 2 guardada: output/figuras/fig_medias_predichas.png\n")

# ── Figura 3 — Coeficientes con IC 95% ────────────────────────────────────────

coefs_all <- bind_rows(
  broom.mixed::tidy(m2_ctrl, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Control social (status quo)"),
  broom.mixed::tidy(m2_resg, effects = "fixed", conf.int = TRUE) |>
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
    title    = "Efectos fijos — Modelos DiD multinivel (ELRI)",
    subtitle = "Rojo = interacciones DiD extendida; IC 95%",
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
cat("✓ Figura 3 guardada: output/figuras/fig_coeficientes.png\n")

# ── Modelos plebiscito 2022 (ola 4) ───────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("MOVILIZACIÓN Y VOTO RECHAZO — PLEBISCITO 2022 (ola 4)\n")
cat(strrep("=", 60), "\n\n")

subset_ola4 <- subset_data |>
  filter(ola == 4) |>
  mutate(
    voto_si = case_when(
      voto_participa == 1 ~ 1L,
      voto_participa == 2 ~ 0L,
      TRUE                ~ NA_integer_
    ),
    voto_rechazo = case_when(
      voto_opcion == 2 ~ 1L,
      voto_opcion == 1 ~ 0L,
      voto_opcion == 3 ~ 0L,
      TRUE             ~ NA_integer_
    ),
    voto_rechazo_strict = case_when(
      voto_opcion == 2 ~ 1L,
      voto_opcion == 1 ~ 0L,
      TRUE             ~ NA_integer_
    )
  )

cat("--- Distribución del voto en ola 4 ---\n")
cat("¿Votó?\n")
print(table(subset_ola4$voto_si, subset_ola4$indigeneous, useNA = "ifany"))
cat("\nOpción de voto (entre votantes):\n")
print(table(subset_ola4$voto_opcion, subset_ola4$indigeneous, useNA = "ifany"))
cat("\nVoto Rechazo × zona × identidad:\n")
print(table(
  subset_ola4$voto_rechazo, subset_ola4$cerca_conflicto,
  subset_ola4$indigeneous, useNA = "ifany"
))

# ── M_movil: participación electoral ──────────────────────────────────────────

cat("\n--- M_movil: Predicción de participación electoral ---\n")

m_movil <- glm(
  voto_si ~ indigeneous * cerca_conflicto + mujer + edad +
    idx_vio_control + idx_vio_resguardo + idx_just_proc +
    apoyo_movil + id_chile + id_causa,
  data   = subset_ola4,
  family = binomial(link = "logit")
)

cat("\nOdds ratios — Movilización:\n")
print(round(exp(cbind(OR = coef(m_movil), confint(m_movil))), 3))

# ── M_rechazo: voto Rechazo entre votantes ────────────────────────────────────

cat("\n--- M_rechazo: Predicción de voto Rechazo (entre votantes) ---\n")

m_rechazo <- glm(
  voto_rechazo ~ indigeneous * cerca_conflicto + mujer + edad +
    idx_vio_control + idx_vio_resguardo + idx_just_proc +
    apoyo_movil + id_chile + id_causa,
  data   = subset_ola4 |> filter(voto_si == 1),
  family = binomial(link = "logit")
)

cat("\nOdds ratios — Voto Rechazo:\n")
print(round(exp(cbind(OR = coef(m_rechazo), confint(m_rechazo))), 3))

# ── M_rechazo_strict: Rechazo vs Apruebo ──────────────────────────────────────

cat("\n--- M_rechazo_strict: Rechazo vs Apruebo (excluye nulos/blancos) ---\n")

m_rechazo_strict <- glm(
  voto_rechazo_strict ~ indigeneous * cerca_conflicto + mujer + edad +
    idx_vio_control + idx_vio_resguardo + idx_just_proc +
    apoyo_movil + id_chile + id_causa,
  data   = subset_ola4 |> filter(!is.na(voto_rechazo_strict)),
  family = binomial(link = "logit")
)

cat("\nOdds ratios — Rechazo vs Apruebo (estricto):\n")
print(round(exp(cbind(OR = coef(m_rechazo_strict), confint(m_rechazo_strict))), 3))

# ── Tabla 4 — Modelos plebiscito ──────────────────────────────────────────────

coef_rename_pleb <- c(
  "(Intercept)"          = "Intercepto",
  "indigeneousindi"      = "Indígena",
  "cerca_conflictocerca" = "Zona excepción",
  "mujermujer"           = "Mujer",
  "edad25_34"            = "Edad 25–34",
  "edad35_44"            = "Edad 35–44",
  "edad45_54"            = "Edad 45–54",
  "edad55_64"            = "Edad 55–64",
  "edad65+"              = "Edad 65+",
  "idx_vio_control"      = "Justif. vio. control social",
  "idx_vio_resguardo"    = "Justif. vio. cambio social",
  "idx_just_proc"        = "Justicia procedimental",
  "apoyo_movil"          = "Apoyo movilizaciones",
  "id_chile"             = "Id. con Chile",
  "id_causa"             = "Id. causa indígena",
  "indigeneousindi:cerca_conflictocerca" = "Indígena × Zona excepción"
)

modelsummary(
  list(
    "Participó (votó)"   = m_movil,
    "Voto Rechazo"       = m_rechazo,
    "Rechazo vs Apruebo" = m_rechazo_strict
  ),
  exponentiate = TRUE,
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_pleb,
  gof_map = c("nobs", "aic"),
  output = "output/tablas/tabla_plebiscito.html"
)
cat("✓ Tabla 4 guardada: output/tablas/tabla_plebiscito.html\n")

# ── Figura 4 — Probabilidades predichas de voto Rechazo ────────────────────────

pred_rechazo <- marginaleffects::predictions(
  m_rechazo,
  newdata = datagrid(
    indigeneous     = c("no_indi", "indi"),
    cerca_conflicto = c("lejos", "cerca")
  )
) |>
  as_tibble() |>
  mutate(
    grupo = factor(
      paste0(indigeneous, " / ", cerca_conflicto),
      levels = c("no_indi / lejos", "no_indi / cerca", "indi / lejos", "indi / cerca"),
      labels = c(
        "No indígena / lejos", "No indígena / zona excepción",
        "Indígena / lejos",    "Indígena / zona excepción"
      )
    )
  )

p_pred <- ggplot(pred_rechazo,
                 aes(x = grupo, y = estimate,
                     ymin = conf.low, ymax = conf.high,
                     color = indigeneous)) +
  geom_pointrange(size = 0.8, linewidth = 1) +
  scale_color_manual(
    values = c("no_indi" = "#4575B4", "indi" = "#D73027"),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title    = "Probabilidad predicha de voto Rechazo",
    subtitle = "Plebiscito 4 de septiembre 2022 — ELRI ola 4",
    x = NULL, y = "P(Rechazo)",
    caption  = "IC 95% · Resto de variables en sus medias"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/figuras/fig_prob_rechazo.png", p_pred,
       width = 7, height = 5, dpi = 300)
cat("✓ Figura 4 guardada: output/figuras/fig_prob_rechazo.png\n")

cat("\n✓ 03_modelos.R ejecutado correctamente.\n")

saveRDS(
  list(
    m0_ctrl = m0_ctrl, m1_ctrl = m1_ctrl, m2_ctrl = m2_ctrl,
    m0_resg = m0_resg, m1_resg = m1_resg, m2_resg = m2_resg,
    m_movil = m_movil, m_rechazo = m_rechazo,
    m_rechazo_strict = m_rechazo_strict,
    controles_base = controles_base,
    incluir_urbano_rural = incluir_urbano_rural,
    coef_rename_did = coef_rename_did,
    coef_rename_pleb = coef_rename_pleb
  ),
  "data/modelos.rds"
)
cat("✓ Modelos guardados: data/modelos.rds\n")
