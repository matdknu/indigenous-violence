# =============================================================================
# 05_mecanismo.R — Mecanismo de mediación: justicia procedimental
#                  ingroup/outgroup como canal del efecto DiD
#
# Hipótesis:
#   H4a: just_proc_ingroup media el efecto sobre justificación de violencia
#        — deterioro del trato percibido al propio grupo
#   H4b: brecha_just_proc (outgroup − ingroup) como indicador de
#        discriminación relativa percibida
#   H4c: el mecanismo opera asimétricamente por tipo de violencia
#
# Análisis en 4 pasos:
#   Paso 0: descriptivos de trayectorias ingroup/outgroup/brecha
#   Paso 1: tratamiento → mediadores (just_proc_ingroup, brecha)
#   Paso 2: mediadores → VDs (controlando tratamiento)
#   Paso 3: atenuación del DiD al incluir mediadores
#
# Input:  data/subset_data.rds, data/analysis_metadata.rds
# Output: output/figuras/fig_trayectorias_justproc_inout.png
#         output/figuras/fig_brecha_justproc.png
#         output/figuras/fig_mediacion_ingroup.png
#         output/tablas/tabla_mecanismo_ingroup.html
#         data/mecanismo.rds
# =============================================================================

set.seed(2024)
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, performance,
  broom.mixed, modelsummary, ggplot2, patchwork
)

if (!dir.exists("output/tablas"))  dir.create("output/tablas",  recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")
metadata    <- readRDS("data/analysis_metadata.rds")
controles_base       <- metadata$controles_base
incluir_urbano_rural <- metadata$incluir_urbano_rural

# ══════════════════════════════════════════════════════════════════════════════
# PASO 0: Descriptivos — trayectorias ingroup/outgroup/brecha por grupo
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PASO 0: Trayectorias descriptivas justicia procedimental\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Tabla de medias
desc_just <- subset_data |>
  filter(!is.na(indigeneous)) |>
  group_by(periodo, indigeneous, zona_decreto) |>
  summarise(
    ingroup_media   = mean(just_proc_ingroup, na.rm = TRUE),
    ingroup_se      = sd(just_proc_ingroup, na.rm = TRUE) /
                      sqrt(sum(!is.na(just_proc_ingroup))),
    outgroup_media  = mean(just_proc_outgroup, na.rm = TRUE),
    outgroup_se     = sd(just_proc_outgroup, na.rm = TRUE) /
                      sqrt(sum(!is.na(just_proc_outgroup))),
    brecha_media    = mean(brecha_just_proc, na.rm = TRUE),
    brecha_se       = sd(brecha_just_proc, na.rm = TRUE) /
                      sqrt(sum(!is.na(brecha_just_proc))),
    n = n(),
    .groups = "drop"
  )

cat("\nMedias por grupo y período:\n")
print(desc_just, n = 30)

# ── Figura: Trayectorias ingroup vs outgroup ──────────────────────────────────

tray_long <- subset_data |>
  filter(!is.na(indigeneous)) |>
  pivot_longer(
    cols = c(just_proc_ingroup, just_proc_outgroup),
    names_to  = "tipo_just",
    values_to = "valor_just"
  ) |>
  mutate(
    tipo_just = factor(
      tipo_just,
      levels = c("just_proc_ingroup", "just_proc_outgroup"),
      labels = c("Trato a MI grupo (ingroup)", "Trato al OTRO grupo (outgroup)")
    )
  ) |>
  group_by(periodo, indigeneous, zona_decreto, tipo_just) |>
  summarise(
    media = mean(valor_just, na.rm = TRUE),
    se    = sd(valor_just, na.rm = TRUE) / sqrt(sum(!is.na(valor_just))),
    ci_lo = media - 1.96 * se,
    ci_hi = media + 1.96 * se,
    .groups = "drop"
  ) |>
  mutate(
    grupo = factor(
      paste0(indigeneous, " — ", zona_decreto),
      levels = c("no_indi — fuera", "no_indi — decreto",
                 "indi — fuera",    "indi — decreto"),
      labels = c("No indígena / fuera", "No indígena / zona decreto",
                 "Indígena / fuera",    "Indígena / zona decreto")
    )
  )

p_inout <- ggplot(tray_long,
    aes(x = periodo, y = media,
        color = grupo, linetype = grupo, group = grupo)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = grupo),
              alpha = 0.06, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ tipo_just, ncol = 2) +
  scale_color_manual(
    values = c(
      "No indígena / fuera"        = "#4575B4",
      "No indígena / zona decreto" = "#74ADD1",
      "Indígena / fuera"           = "#D73027",
      "Indígena / zona decreto"    = "#F46D43"
    ), name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "No indígena / fuera"        = "#4575B4",
      "No indígena / zona decreto" = "#74ADD1",
      "Indígena / fuera"           = "#D73027",
      "Indígena / zona decreto"    = "#F46D43"
    ), guide = "none"
  ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "dashed", "solid"), name = NULL
  ) +
  scale_x_discrete(labels = c(
    "pre" = "Ola 2\n(2018)",
    "estallido" = "Ola 3\n(2021)",
    "decreto" = "Ola 4\n(2023)"
  )) +
  labs(
    title = "Justicia procedimental: percepción ingroup vs. outgroup",
    subtitle = paste0(
      "Ingroup = trato percibido a MI grupo étnico · ",
      "Outgroup = trato percibido al OTRO grupo\n",
      "IC 95% sombreado · Escala 1 (no respeta) – 5 (respeta mucho)"
    ),
    x = NULL, y = "Media percepción de respeto",
    caption = "Línea sólida = zona decreto · punteada = fuera"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_trayectorias_justproc_inout.png", p_inout,
       width = 12, height = 6, dpi = 300)
cat("✓ Figura trayectorias ingroup/outgroup guardada\n")

# ── Figura: Brecha percibida por grupo ────────────────────────────────────────

tray_brecha <- subset_data |>
  filter(!is.na(indigeneous)) |>
  group_by(periodo, indigeneous, zona_decreto) |>
  summarise(
    media = mean(brecha_just_proc, na.rm = TRUE),
    se    = sd(brecha_just_proc, na.rm = TRUE) /
            sqrt(sum(!is.na(brecha_just_proc))),
    ci_lo = media - 1.96 * se,
    ci_hi = media + 1.96 * se,
    .groups = "drop"
  ) |>
  mutate(
    grupo = factor(
      paste0(indigeneous, " — ", zona_decreto),
      levels = c("no_indi — fuera", "no_indi — decreto",
                 "indi — fuera",    "indi — decreto"),
      labels = c("No indígena / fuera", "No indígena / zona decreto",
                 "Indígena / fuera",    "Indígena / zona decreto")
    )
  )

p_brecha <- ggplot(tray_brecha,
    aes(x = periodo, y = media,
        color = grupo, linetype = grupo, group = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
             linewidth = 0.5) +
  annotate("text", x = 0.6, y = 0.15,
           label = "Tratan mejor\nal outgroup →",
           size = 2.5, color = "grey40", hjust = 0, fontface = "italic") +
  annotate("text", x = 0.6, y = -0.15,
           label = "← Tratan mejor\na mi grupo",
           size = 2.5, color = "grey40", hjust = 0, fontface = "italic") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = grupo),
              alpha = 0.08, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(
    values = c(
      "No indígena / fuera"        = "#4575B4",
      "No indígena / zona decreto" = "#74ADD1",
      "Indígena / fuera"           = "#D73027",
      "Indígena / zona decreto"    = "#F46D43"
    ), name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "No indígena / fuera"        = "#4575B4",
      "No indígena / zona decreto" = "#74ADD1",
      "Indígena / fuera"           = "#D73027",
      "Indígena / zona decreto"    = "#F46D43"
    ), guide = "none"
  ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "dashed", "solid"), name = NULL
  ) +
  scale_x_discrete(labels = c(
    "pre" = "Ola 2\n(2018)",
    "estallido" = "Ola 3\n(2021)",
    "decreto" = "Ola 4\n(2023)"
  )) +
  labs(
    title = "Brecha percibida de justicia procedimental (outgroup − ingroup)",
    subtitle = paste0(
      "Valores positivos = perciben mejor trato al OTRO grupo (agravio)\n",
      "Valores negativos = perciben mejor trato a MI grupo (privilegio percibido)"
    ),
    x = NULL, y = "Brecha (outgroup − ingroup)",
    caption = "Línea sólida = zona decreto · punteada = fuera"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_brecha_justproc.png", p_brecha,
       width = 10, height = 6, dpi = 300)
cat("✓ Figura brecha justicia procedimental guardada\n")

# ══════════════════════════════════════════════════════════════════════════════
# PASO 1: Tratamiento → Mediadores
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PASO 1: ¿El tratamiento afecta los mediadores?\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Modelo 1a: Tratamiento → just_proc_ingroup
m1_ingroup <- lmer(
  as.formula(paste(
    "just_proc_ingroup ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

# Modelo 1b: Tratamiento → just_proc_outgroup
m1_outgroup <- lmer(
  as.formula(paste(
    "just_proc_outgroup ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

# Modelo 1c: Tratamiento → brecha_just_proc
m1_brecha <- lmer(
  as.formula(paste(
    "brecha_just_proc ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

# Extraer coeficientes DiD para mediadores
TERM_DID_EST <- "periodoestallido:indigeneousindi:zona_decretodecreto"
TERM_DID_DEC <- "periododecreto:indigeneousindi:zona_decretodecreto"

extraer_did <- function(modelo, term, nombre) {
  td <- broom.mixed::tidy(modelo, effects = "fixed")
  row <- td |> filter(term == !!term)
  if (nrow(row) == 0) {
    cat("  ", nombre, "— término no encontrado:", term, "\n")
    return(tibble(term=term, estimate=NA, std.error=NA, p.value=NA, modelo=nombre))
  }
  cat("  ", nombre, "— β =", round(row$estimate, 3),
      " SE =", round(row$std.error, 3),
      " p =", format.pval(row$p.value, digits = 3), "\n")
  row |> mutate(modelo = nombre)
}

cat("\nEfecto DiD ESTALLIDO (ola 3 × indi × zona) sobre mediadores:\n")
extraer_did(m1_ingroup,  TERM_DID_EST, "Just. proc. ingroup")
extraer_did(m1_outgroup, TERM_DID_EST, "Just. proc. outgroup")
extraer_did(m1_brecha,   TERM_DID_EST, "Brecha just. proc.")

cat("\nEfecto DiD DECRETO (ola 4 × indi × zona) sobre mediadores:\n")
extraer_did(m1_ingroup,  TERM_DID_DEC, "Just. proc. ingroup")
extraer_did(m1_outgroup, TERM_DID_DEC, "Just. proc. outgroup")
extraer_did(m1_brecha,   TERM_DID_DEC, "Brecha just. proc.")

# ══════════════════════════════════════════════════════════════════════════════
# PASO 2: Crear mediadores rezagados (just_proc de ola t−1)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PASO 2: Variables rezagadas + efecto mediador → VD\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Crear rezagos: valor en ola anterior como predictor
just_lag <- subset_data |>
  select(folio, ola, just_proc_ingroup, just_proc_outgroup, brecha_just_proc) |>
  mutate(ola_next = ola + 1) |>
  rename(
    ingroup_lag  = just_proc_ingroup,
    outgroup_lag = just_proc_outgroup,
    brecha_lag   = brecha_just_proc
  ) |>
  select(folio, ola = ola_next, ingroup_lag, outgroup_lag, brecha_lag)

subset_med <- subset_data |>
  left_join(just_lag, by = c("folio", "ola"))

cat("Obs con ingroup_lag disponible:",
    sum(!is.na(subset_med$ingroup_lag)), "/", nrow(subset_med), "\n")
cat("(Ola 2 no tiene lag → NA esperado para ola 2)\n")

# ══════════════════════════════════════════════════════════════════════════════
# PASO 3: Comparar DiD con y sin mediadores — atenuación
# ══════════════════════════════════════════════════════════════════════════════

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PASO 3: ¿Se atenúa el DiD al incluir mediadores?\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# ── Modelos SIN mediador (baseline) ──────────────────────────────────────────

m_ctrl_sin <- lmer(
  as.formula(paste(
    "idx_vio_control_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m_resg_sin <- lmer(
  as.formula(paste(
    "idx_vio_resguardo_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

# ── Modelos CON ingroup_lag ──────────────────────────────────────────────────

m_ctrl_ingroup <- lmer(
  as.formula(paste(
    "idx_vio_control_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ ingroup_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m_resg_ingroup <- lmer(
  as.formula(paste(
    "idx_vio_resguardo_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ ingroup_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

# ── Modelos CON brecha_lag ───────────────────────────────────────────────────

m_ctrl_brecha <- lmer(
  as.formula(paste(
    "idx_vio_control_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ brecha_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m_resg_brecha <- lmer(
  as.formula(paste(
    "idx_vio_resguardo_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ brecha_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

# ── Modelos CON ambos (ingroup_lag + brecha_lag) ─────────────────────────────

m_ctrl_ambos <- lmer(
  as.formula(paste(
    "idx_vio_control_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ ingroup_lag + brecha_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

m_resg_ambos <- lmer(
  as.formula(paste(
    "idx_vio_resguardo_ord ~ periodo * indigeneous * zona_decreto +",
    controles_base, "+ ingroup_lag + brecha_lag + (1 | folio)"
  )),
  data = subset_med, REML = FALSE
)

# ── Extraer y comparar coeficientes DiD ──────────────────────────────────────

extraer_comparar <- function(m_sin, m_con, vd_label, med_label, term) {
  td_sin <- broom.mixed::tidy(m_sin, effects="fixed") |> filter(term == !!term)
  td_con <- broom.mixed::tidy(m_con, effects="fixed") |> filter(term == !!term)

  if (nrow(td_sin) == 0 | nrow(td_con) == 0) return(NULL)

  ate <- (td_sin$estimate - td_con$estimate) / abs(td_sin$estimate) * 100

  tibble(
    vd          = vd_label,
    mediador    = med_label,
    b_sin       = td_sin$estimate,
    se_sin      = td_sin$std.error,
    p_sin       = td_sin$p.value,
    b_con       = td_con$estimate,
    se_con      = td_con$std.error,
    p_con       = td_con$p.value,
    atenuacion  = ate
  )
}

cat("\n--- Comparación DiD DECRETO (τ₄): con y sin mediadores ---\n\n")

comp <- bind_rows(
  extraer_comparar(m_ctrl_sin, m_ctrl_ingroup,
    "Vio. control", "Ingroup lag", TERM_DID_DEC),
  extraer_comparar(m_ctrl_sin, m_ctrl_brecha,
    "Vio. control", "Brecha lag",  TERM_DID_DEC),
  extraer_comparar(m_ctrl_sin, m_ctrl_ambos,
    "Vio. control", "Ambos",       TERM_DID_DEC),
  extraer_comparar(m_resg_sin, m_resg_ingroup,
    "Vio. resguardo", "Ingroup lag", TERM_DID_DEC),
  extraer_comparar(m_resg_sin, m_resg_brecha,
    "Vio. resguardo", "Brecha lag",  TERM_DID_DEC),
  extraer_comparar(m_resg_sin, m_resg_ambos,
    "Vio. resguardo", "Ambos",       TERM_DID_DEC)
)

print(comp |>
  mutate(across(where(is.numeric), ~ round(., 3))) |>
  as.data.frame()
)

cat("\nInterpretación:\n")
cat("  Atenuación 10–30% → mediación parcial (canal contribuye)\n")
cat("  Atenuación 30–60% → mediación sustancial\n")
cat("  Atenuación > 60%  → mediación fuerte (canal principal)\n")
cat("  Atenuación ≈ 0%   → canal no opera\n")
cat("  Atenuación < 0%   → supresión (mediador tiene efecto contrario)\n")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURAS Y TABLAS FINALES
# ══════════════════════════════════════════════════════════════════════════════

# ── Figura: Coeficientes DiD con y sin mediadores ─────────────────────────────

comp_fig <- comp |>
  pivot_longer(
    cols = c(b_sin, b_con),
    names_to = "especificacion",
    values_to = "estimate"
  ) |>
  mutate(
    se = if_else(especificacion == "b_sin", se_sin, se_con),
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se,
    especificacion = factor(
      especificacion,
      levels = c("b_sin", "b_con"),
      labels = c("Sin mediador", "Con mediador")
    ),
    label = paste0(vd, "\n(", mediador, ")")
  )

p_mediacion <- ggplot(comp_fig,
    aes(x = estimate, y = label,
        xmin = ci_lo, xmax = ci_hi,
        color = especificacion)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(
    position = position_dodge(width = 0.5),
    linewidth = 0.7, size = 0.5
  ) +
  facet_wrap(~ vd, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("Sin mediador" = "#D73027", "Con mediador" = "#4575B4"),
    name = NULL
  ) +
  labs(
    title = "Mediación: atenuación del DiD al incluir justicia procedimental",
    subtitle = paste0(
      "Coeficiente τ₄ (Decreto × Indígena × Zona) · IC 95%\n",
      "Mediadores rezagados (valor en ola anterior)"
    ),
    x = "Coeficiente DiD estimado", y = NULL,
    caption = paste0(
      "Rojo = sin mediador · Azul = con mediador\n",
      "Si azul se acerca a 0 respecto a rojo → mediación parcial"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/figuras/fig_mediacion_ingroup.png", p_mediacion,
       width = 10, height = 8, dpi = 300)
cat("\n✓ Figura mediación guardada\n")

# ── Tabla completa de modelos de mecanismo ────────────────────────────────────

coef_rename_mec <- c(
  "periodoestallido"   = "Ola 3 — Resabio estallido",
  "periododecreto"     = "Ola 4 — Decreto + Apruebo",
  "indigeneousindi"    = "Indígena",
  "zona_decretodecreto"= "Zona excepción",
  "ingroup_lag"        = "Just. proc. ingroup (lag)",
  "outgroup_lag"       = "Just. proc. outgroup (lag)",
  "brecha_lag"         = "Brecha just. proc. (lag)",
  "just_proc_ingroup"  = "Just. proc. ingroup",
  "just_proc_outgroup" = "Just. proc. outgroup",
  "brecha_just_proc"   = "Brecha just. proc.",
  "periodoestallido:indigeneousindi:zona_decretodecreto" =
    "Ola 3 × Indígena × Zona [DiD estallido]",
  "periododecreto:indigeneousindi:zona_decretodecreto" =
    "Ola 4 × Indígena × Zona [DiD decreto]"
)

modelsummary(
  list(
    "Paso 1:\nIngroup"  = m1_ingroup,
    "Paso 1:\nOutgroup" = m1_outgroup,
    "Paso 1:\nBrecha"   = m1_brecha,
    "Ctrl\nsin med."    = m_ctrl_sin,
    "Ctrl +\ningroup"   = m_ctrl_ingroup,
    "Ctrl +\nbrecha"    = m_ctrl_brecha,
    "Resg\nsin med."    = m_resg_sin,
    "Resg +\ningroup"   = m_resg_ingroup,
    "Resg +\nbrecha"    = m_resg_brecha
  ),
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  coef_rename = coef_rename_mec,
  coef_omit = "edad|mujer|urbano|malestar|apoyo",
  gof_map = c("nobs", "icc", "rmse"),
  notes = paste0(
    "Paso 1: efecto del tratamiento sobre mediadores. ",
    "Columnas 4–9: modelos DiD con y sin mediadores rezagados. ",
    "Controles sociodem. y sustantivos omitidos. ",
    "Mediadores rezagados = valor en ola anterior. ",
    "+ p<.1, * p<.05, ** p<.01, *** p<.001."
  ),
  output = "output/tablas/tabla_mecanismo_ingroup.html"
)
cat("✓ Tabla mecanismo guardada\n")

# ── Guardar todos los objetos ─────────────────────────────────────────────────

saveRDS(
  list(
    # Paso 1: tratamiento → mediadores
    m1_ingroup  = m1_ingroup,
    m1_outgroup = m1_outgroup,
    m1_brecha   = m1_brecha,
    # Paso 3: modelos con y sin mediadores
    m_ctrl_sin     = m_ctrl_sin,
    m_ctrl_ingroup = m_ctrl_ingroup,
    m_ctrl_brecha  = m_ctrl_brecha,
    m_ctrl_ambos   = m_ctrl_ambos,
    m_resg_sin     = m_resg_sin,
    m_resg_ingroup = m_resg_ingroup,
    m_resg_brecha  = m_resg_brecha,
    m_resg_ambos   = m_resg_ambos,
    # Tabla comparativa
    comparacion_atenuacion = comp,
    # Descriptivos
    desc_just = desc_just
  ),
  "data/mecanismo.rds"
)
cat("✓ Objetos guardados: data/mecanismo.rds\n")

cat("\n✓ 05_mecanismo.R ejecutado correctamente.\n")
