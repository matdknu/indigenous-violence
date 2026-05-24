# =============================================================================
# 05_mecanismo.R — Análisis de mecanismo: justicia procedimental como mediadora
#
# Propósito: explorar si idx_just_proc media parcialmente el efecto del
#            estado de excepción sobre la justificación de violencia.
#            Tres pasos (Baron & Kenny adaptado a panel):
#              Paso 1: tratamiento → just_proc (M_just)
#              Paso 2: just_proc_lag → VD (modelos con mediador)
#              Paso 3: comparar coef. DiD con/sin just_proc (atenuación)
#
# Limitación explícita: just_proc se mide en la misma ola que la VD en
# algunos períodos. La aproximación más limpia usa just_proc de ola t-1
# como predictor de VD en ola t (rezago de un período).
#
# Input:  data/subset_data.rds, data/modelos.rds
# Output: output/figuras/fig_trayectorias_justproc.png
#         output/figuras/fig_mediacion_coefs.png
#         output/tablas/tabla_mecanismo.html
#         data/mecanismo.rds
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, performance,
  broom.mixed, modelsummary, ggplot2, stringr
)

if (!dir.exists("output/tablas"))  dir.create("output/tablas",  recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")
modelos     <- readRDS("data/modelos.rds")
controles_base       <- modelos$controles_base
incluir_urbano_rural <- modelos$incluir_urbano_rural

if (!"idx_just_proc" %in% names(subset_data)) {
  stop("idx_just_proc no está en subset_data. Ejecutar 01_limpieza.R primero.")
}

# ── Justicia procedimental rezagada (ola t-1 → ola t) ─────────────────────────

just_lag <- subset_data |>
  select(folio, ola, idx_just_proc) |>
  mutate(ola_next = ola + 1L) |>
  rename(just_proc_lag = idx_just_proc) |>
  select(folio, ola = ola_next, just_proc_lag)

subset_med <- subset_data |>
  left_join(just_lag, by = c("folio", "ola"))

cat("--- Correlaciones just_proc_lag con VDs ---\n")
cat("just_proc_lag × idx_vio_control:",
    round(cor(subset_med$just_proc_lag, subset_med$idx_vio_control,
              use = "pairwise.complete.obs"), 3), "\n")
cat("just_proc_lag × idx_vio_resguardo:",
    round(cor(subset_med$just_proc_lag, subset_med$idx_vio_resguardo,
              use = "pairwise.complete.obs"), 3), "\n")
cat("N válidos just_proc_lag:", sum(!is.na(subset_med$just_proc_lag)), "\n\n")

# ── Figura 3 — Trayectorias idx_just_proc ─────────────────────────────────────

tray_just <- subset_med |>
  filter(!is.na(indigeneous)) |>
  group_by(indigeneous, cerca_conflicto, periodo) |>
  summarise(
    media = mean(idx_just_proc, na.rm = TRUE),
    se    = sd(idx_just_proc, na.rm = TRUE) / sqrt(sum(!is.na(idx_just_proc))),
    ci_lo = media - 1.96 * se,
    ci_hi = media + 1.96 * se,
    .groups = "drop"
  ) |>
  mutate(
    grupo = factor(
      paste0(indigeneous, " — ", cerca_conflicto),
      levels = c("no_indi — lejos", "no_indi — cerca",
                 "indi — lejos",    "indi — cerca"),
      labels = c("No indígena / lejos", "No indígena / zona excepción",
                 "Indígena / lejos",    "Indígena / zona excepción")
    )
  )

p_just <- ggplot(tray_just,
                 aes(x = periodo, y = media,
                     color = grupo, linetype = grupo, group = grupo)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = grupo),
              alpha = 0.08, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(
    values = c(
      "No indígena / lejos"          = "#4575B4",
      "No indígena / zona excepción" = "#74ADD1",
      "Indígena / lejos"             = "#D73027",
      "Indígena / zona excepción"    = "#F46D43"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "No indígena / lejos"          = "#4575B4",
      "No indígena / zona excepción" = "#74ADD1",
      "Indígena / lejos"             = "#D73027",
      "Indígena / zona excepción"    = "#F46D43"
    ),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "dashed", "solid"), name = NULL
  ) +
  scale_x_discrete(labels = c(
    "pre"         = "Ola 2\n(Pre)",
    "tratamiento" = "Ola 3\n(Tratamiento)",
    "post"        = "Ola 4\n(Post)"
  )) +
  labs(
    title    = "Trayectorias de justicia procedimental por grupo",
    subtitle = "ELRI — índice d5_1 + d5_2 · IC 95% sombreado",
    x = NULL, y = "Media (escala 1–5)",
    caption  = "Línea sólida = zona de excepción · punteada = lejos"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_trayectorias_justproc.png", p_just,
       width = 8, height = 5, dpi = 300)
cat("✓ Figura 3 guardada: output/figuras/fig_trayectorias_justproc.png\n")

# ── Paso 1: Tratamiento → just_proc ───────────────────────────────────────────

m_just <- lmer(
  as.formula(paste(
    "idx_just_proc ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m0_just <- lmer(idx_just_proc ~ 1 + (1 | folio), data = subset_med, REML = TRUE)
cat("\n--- Paso 1: Tratamiento → Justicia procedimental ---\n")
cat("ICC just_proc:", round(as.numeric(performance::icc(m0_just)$ICC_adjusted), 3), "\n\n")
print(summary(m_just))

# ── Pasos 2 y 3: DiD con y sin just_proc_lag ──────────────────────────────────

m2_ctrl_sin <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m2_resg_sin <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m2_ctrl_med <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ just_proc_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m2_resg_med <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ just_proc_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

TERM_DID <- "periodopost:indigeneousindi:cerca_conflictocerca"

extraer_did <- function(m, nombre) {
  td <- broom.mixed::tidy(m, effects = "fixed")
  row <- td |> filter(.data$term == .env$TERM_DID)
  if (nrow(row) == 0) {
    cat(nombre, "— coeficiente DiD no encontrado\n")
    return(tibble(
      term = TERM_DID, estimate = NA_real_, std.error = NA_real_,
      p.value = NA_real_, modelo = nombre
    ))
  }
  cat(nombre, "— β DiD (ola 4):", round(row$estimate, 3),
      " SE:", round(row$std.error, 3),
      " p:", format.pval(row$p.value, digits = 3), "\n")
  row |> mutate(modelo = nombre)
}

cat("\n--- Paso 3: Comparación coef. DiD con y sin mediador ---\n\n")

r_ctrl_sin <- extraer_did(m2_ctrl_sin, "Vio. control  SIN just_proc")
r_ctrl_med <- extraer_did(m2_ctrl_med, "Vio. control  CON just_proc_lag")
r_resg_sin <- extraer_did(m2_resg_sin, "Vio. resguardo SIN just_proc")
r_resg_med <- extraer_did(m2_resg_med, "Vio. resguardo CON just_proc_lag")

ate_ctrl <- if (!is.na(r_ctrl_sin$estimate) && r_ctrl_sin$estimate != 0) {
  (r_ctrl_sin$estimate - r_ctrl_med$estimate) / abs(r_ctrl_sin$estimate) * 100
} else NA_real_

ate_resg <- if (!is.na(r_resg_sin$estimate) && r_resg_sin$estimate != 0) {
  (r_resg_sin$estimate - r_resg_med$estimate) / abs(r_resg_sin$estimate) * 100
} else NA_real_

cat("\nAtenuación del DiD al incluir just_proc_lag:\n")
cat("  Vio. control:", round(ate_ctrl, 1), "%\n")
cat("  Vio. resguardo:", round(ate_resg, 1), "%\n")
cat("\nNota: atenuación >10% y <100% → evidencia de mediación parcial\n")
cat("      atenuación ~0%           → just_proc no media el efecto\n")
cat("      atenuación >100%         → posible supresión; revisar\n")

# ── Figura comparativa — coefs DiD ────────────────────────────────────────────

comp_coefs <- bind_rows(r_ctrl_sin, r_ctrl_med, r_resg_sin, r_resg_med) |>
  filter(!is.na(estimate)) |>
  mutate(
    vd  = if_else(
      str_detect(modelo, "control"),
      "Control social (status quo)",
      "Cambio social"
    ),
    med = if_else(str_detect(modelo, "CON"), "Con just_proc_lag", "Sin just_proc_lag"),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error
  )

p_med <- ggplot(comp_coefs,
                aes(x = estimate, y = med,
                    xmin = ci_lo, xmax = ci_hi,
                    color = vd)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(position = position_dodge(width = 0.4),
                  linewidth = 0.8, size = 0.6) +
  facet_wrap(~ vd, ncol = 2) +
  scale_color_manual(
    values = c(
      "Control social (status quo)" = "#4575B4",
      "Cambio social"               = "#D73027"
    ),
    guide = "none"
  ) +
  labs(
    title    = "Mediación parcial: efecto DiD con y sin justicia procedimental",
    subtitle = "Coeficiente: Ola 4 × Indígena × Zona excepción · IC 95%",
    x = "Coeficiente estimado", y = NULL,
    caption  = "just_proc_lag = valor de justicia procedimental en ola anterior"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/figuras/fig_mediacion_coefs.png", p_med,
       width = 9, height = 4, dpi = 300)
cat("✓ Figura mediación guardada: output/figuras/fig_mediacion_coefs.png\n")

# ── Tabla A6 — Modelos mecanismo ──────────────────────────────────────────────

coef_rename_med <- c(
  "periodotratamiento"   = "Ola 3 (tratamiento)",
  "periodopost"          = "Ola 4 (post)",
  "indigeneousindi"      = "Indígena",
  "cerca_conflictocerca" = "Zona excepción",
  "just_proc_lag"        = "Just. proc. (rezagada)",
  "id_chile"             = "Id. con Chile",
  "id_causa"             = "Id. causa indígena",
  "perc_desigualdad"     = "Perc. desigualdad",
  "perc_injusticia"      = "Perc. injusticia",
  "periodotratamiento:indigeneousindi:cerca_conflictocerca" = "Ola 3 × Indígena × Zona [DiD]",
  "periodopost:indigeneousindi:cerca_conflictocerca"        = "Ola 4 × Indígena × Zona [DiD]"
)

modelsummary(
  list(
    "Paso 1: Just. proc."   = m_just,
    "Vio. ctrl. (sin med.)" = m2_ctrl_sin,
    "Vio. ctrl. (con med.)" = m2_ctrl_med,
    "Vio. resg. (sin med.)" = m2_resg_sin,
    "Vio. resg. (con med.)" = m2_resg_med
  ),
  statistic  = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_med,
  coef_omit   = "edad|mujer|urbano",
  gof_map = c("nobs", "icc", "rmse"),
  notes = "Coeficientes de edad, sexo y urbano_rural omitidos por espacio. Ver Tabla A1.",
  output = "output/tablas/tabla_mecanismo.html"
)
cat("✓ Tabla A6 guardada: output/tablas/tabla_mecanismo.html\n")

saveRDS(
  list(
    m_just      = m_just,
    m2_ctrl_sin = m2_ctrl_sin,
    m2_ctrl_med = m2_ctrl_med,
    m2_resg_sin = m2_resg_sin,
    m2_resg_med = m2_resg_med,
    ate_ctrl    = ate_ctrl,
    ate_resg    = ate_resg,
    comp_coefs  = comp_coefs
  ),
  "data/mecanismo.rds"
)
cat("✓ Objetos guardados: data/mecanismo.rds\n")
cat("\n✓ 05_mecanismo.R ejecutado correctamente.\n")
