# =============================================================================
# 08_identidad_ingroup.R — Exploración identidad ingroup/outgroup
#
# Propósito: diagnosticar la estructura de a4, a5, a6 por identidad
#            étnica y decidir si una descomposición ingroup/outgroup
#            es viable o si conviene usar predominancia como moderador.
#
# Input:  data/subset_data.rds
# Output: output/tablas/tabla_identidad_diagnostico.html
#         output/figuras/fig_identidad_distribucion.png
#         output/figuras/fig_predominancia_trayectorias.png
#         Consola: tablas de diagnóstico para decisión
# =============================================================================

set.seed(2024)
pacman::p_load(
  dplyr, tidyverse, ggplot2, patchwork, gt, scales
)

if (!dir.exists("output/tablas"))  dir.create("output/tablas",  recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")

# Cargar panel raw para obtener a5 (no está en panel_completo ni subset_data)
load("data/BBDD_ELRI_LONG.RData")

# Función para recodificar missing (copiada de 01_limpieza.R)
recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  xv <- if (inherits(x, "haven_labelled")) {
    as.integer(haven::zap_labels(x))
  } else {
    as.integer(x)
  }
  if_else(xv %in% miss_vals, NA_integer_, xv)
}

# Extraer a5 del panel raw y limpiar missing
a5_data <- BBDD_ELRI_LONG |>
  select(folio, ola, a5) |>
  mutate(a5 = recode_missing(a5))

# Unir a5 a subset_data
subset_data <- subset_data |>
  left_join(a5_data, by = c("folio", "ola"))

# ── 1. DIAGNÓSTICO BÁSICO (ola 2, baseline) ──────────────────────────────────

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("DIAGNÓSTICO IDENTIDAD: a4 (id_indi), a5, a6 (id_chile) por grupo étnico\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Medias por grupo
cat("\n--- Medias baseline (ola 2) ---\n")
diag_medias <- subset_data |>
  filter(ola == 2) |>
  group_by(indigeneous) |>
  summarise(
    a4_id_pueblo_media = mean(as.numeric(id_indi), na.rm = TRUE),
    a4_sd              = sd(as.numeric(id_indi), na.rm = TRUE),
    a5_importancia_media = mean(as.numeric(a5), na.rm = TRUE),
    a5_sd              = sd(as.numeric(a5), na.rm = TRUE),
    a6_id_chile_media  = mean(as.numeric(id_chile), na.rm = TRUE),
    a6_sd              = sd(as.numeric(id_chile), na.rm = TRUE),
    predominancia_media = mean(as.numeric(id_indi) - as.numeric(id_chile), na.rm = TRUE),
    predominancia_sd   = sd(as.numeric(id_indi) - as.numeric(id_chile), na.rm = TRUE),
    cor_a4_a6          = cor(as.numeric(id_indi), as.numeric(id_chile), use = "complete.obs"),
    cor_a4_a5          = cor(as.numeric(id_indi), as.numeric(a5), use = "complete.obs"),
    n = n(),
    .groups = "drop"
  )
print(diag_medias)

# Distribución de a4 (id_indi) por grupo (¿hay varianza entre no indígenas?)
cat("\n--- Distribución a4 (id. pueblo originario = id_indi) por grupo ---\n")
cat("\nIndígenas:\n")
print(table(subset_data |> filter(ola == 2, indigeneous == "indi") |>
  pull(id_indi), useNA = "ifany"))

cat("\nNo indígenas:\n")
print(table(subset_data |> filter(ola == 2, indigeneous == "no_indi") |>
  pull(id_indi), useNA = "ifany"))

# Distribución de a6 (id_chile) por grupo
cat("\n--- Distribución a6 (id. Chile = id_chile) por grupo ---\n")
cat("\nIndígenas:\n")
print(table(subset_data |> filter(ola == 2, indigeneous == "indi") |>
  pull(id_chile), useNA = "ifany"))

cat("\nNo indígenas:\n")
print(table(subset_data |> filter(ola == 2, indigeneous == "no_indi") |>
  pull(id_chile), useNA = "ifany"))

# Tabla cruzada a4 × a6 entre indígenas
cat("\n--- Tabla cruzada id_indi × id_chile entre INDÍGENAS (ola 2) ---\n")
cat("Filas = id_indi (id pueblo), Columnas = id_chile (id Chile)\n\n")
print(table(
  id_indi = subset_data |> filter(ola == 2, indigeneous == "indi") |> pull(id_indi),
  id_chile = subset_data |> filter(ola == 2, indigeneous == "indi") |> pull(id_chile),
  useNA = "ifany"
))

# Correlación a4 + a5 (¿forman un índice?)
cat("\n--- Correlación id_indi–a5 (¿forman un índice?) ---\n")
r_a4_a5 <- cor(
  as.numeric(subset_data$id_indi),
  as.numeric(subset_data$a5),
  use = "complete.obs"
)
cat("r(id_indi, a5) =", round(r_a4_a5, 3), "\n")
if (r_a4_a5 > 0.7) {
  cat("→ Alta correlación: se pueden promediar en idx_id_etnica\n")
} else {
  cat("→ Correlación moderada: mantener separadas o explorar\n")
}

# ── 2. CREAR VARIABLES DERIVADAS ─────────────────────────────────────────────

subset_data <- subset_data |>
  mutate(
    a4_num = as.numeric(id_indi),
    a5_num = as.numeric(a5),
    a6_num = as.numeric(id_chile),

    # Índice de identidad étnica (si a4 y a5 correlacionan alto)
    idx_id_etnica = rowMeans(pick(a4_num, a5_num), na.rm = TRUE),

    # Identidad nacional
    id_nacional = a6_num,

    # Predominancia identitaria: étnica − nacional
    # Positivo = predomina identidad étnica
    # Negativo = predomina identidad nacional
    # Cero = equilibrio
    predominancia_id = idx_id_etnica - id_nacional,

    # Identidad dual: promedio de ambas
    # Alto = se identifica fuerte con AMBAS
    # Bajo = baja identificación con ambas
    id_dual = (idx_id_etnica + id_nacional) / 2,

    # Ingroup/outgroup identitario
    id_ingroup = case_when(
      indigeneous == "indi"    ~ idx_id_etnica,
      indigeneous == "no_indi" ~ id_nacional,
      TRUE ~ NA_real_
    ),
    id_outgroup = case_when(
      indigeneous == "indi"    ~ id_nacional,
      indigeneous == "no_indi" ~ idx_id_etnica,
      TRUE ~ NA_real_
    ),
    brecha_id = id_ingroup - id_outgroup
  )

# Verificar
cat("\n--- Variables derivadas (ola 2) ---\n")
subset_data |>
  filter(ola == 2) |>
  group_by(indigeneous) |>
  summarise(
    id_ingroup_media   = mean(id_ingroup, na.rm = TRUE),
    id_outgroup_media  = mean(id_outgroup, na.rm = TRUE),
    brecha_id_media    = mean(brecha_id, na.rm = TRUE),
    predominancia_media = mean(predominancia_id, na.rm = TRUE),
    id_dual_media      = mean(id_dual, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  print()

cat("\nInterpretación esperada:\n")
cat("  Indígenas: brecha_id > 0 (se identifican más con pueblo que con Chile)\n")
cat("  No indígenas: brecha_id < 0 (se identifican más con Chile que con pueblo)\n")

# ── 3. FIGURAS DIAGNÓSTICAS ──────────────────────────────────────────────────

# Figura 1: Distribución de id_indi y id_chile por grupo (histogramas)
p_dist <- subset_data |>
  filter(ola == 2) |>
  pivot_longer(cols = c(a4_num, a6_num),
    names_to = "variable",
    values_to = "valor") |>
  mutate(
    variable = recode(variable,
      "a4_num" = "Id. pueblo originario (id_indi)",
      "a6_num" = "Id. con Chile (id_chile)"
    )
  ) |>
  ggplot(aes(x = factor(valor), fill = indigeneous)) +
  geom_bar(position = "dodge", color = "white", linewidth = 0.2) +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_manual(
    values = c("no_indi" = "#4575B4", "indi" = "#D73027"),
    labels = c("No indígena", "Indígena"),
    name = NULL
  ) +
  labs(
    title = "Distribución de identidad étnica y nacional (ola 2, baseline)",
    x = "Escala 1–5", y = "N respondentes",
    caption = "¿Hay varianza de id_indi entre no indígenas?"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

ggsave("output/figuras/fig_identidad_distribucion.png", p_dist,
       width = 10, height = 5, dpi = 300)
cat("✓ Figura distribución identidad guardada\n")

# Figura 2: Trayectorias de predominancia por grupo y zona
tray_predom <- subset_data |>
  filter(!is.na(indigeneous)) |>
  group_by(periodo, indigeneous, zona_decreto) |>
  summarise(
    media = mean(predominancia_id, na.rm = TRUE),
    se    = sd(predominancia_id, na.rm = TRUE) /
            sqrt(sum(!is.na(predominancia_id))),
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

p_predom <- ggplot(tray_predom,
    aes(x = periodo, y = media,
        color = grupo, linetype = grupo, group = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.6, y = 0.3,
           label = "Predomina\nid. étnica →",
           size = 2.5, color = "grey40", hjust = 0, fontface = "italic") +
  annotate("text", x = 0.6, y = -0.3,
           label = "← Predomina\nid. nacional",
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
    title = "Predominancia identitaria (étnica − nacional) por grupo",
    subtitle = paste0(
      "Positivo = predomina identidad étnica · ",
      "Negativo = predomina identidad nacional\n",
      "IC 95% sombreado"
    ),
    x = NULL, y = "Predominancia (id_étnica − id_Chile)",
    caption = "Línea sólida = zona decreto · punteada = fuera"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_predominancia_trayectorias.png", p_predom,
       width = 10, height = 6, dpi = 300)
cat("✓ Figura predominancia guardada\n")

# Figura 3: Trayectorias ingroup vs outgroup identitario
tray_id_long <- subset_data |>
  filter(!is.na(indigeneous)) |>
  pivot_longer(
    cols = c(id_ingroup, id_outgroup),
    names_to  = "tipo_id",
    values_to = "valor_id"
  ) |>
  mutate(
    tipo_id = factor(
      tipo_id,
      levels = c("id_ingroup", "id_outgroup"),
      labels = c("Identidad con MI grupo", "Identidad con el OTRO grupo")
    )
  ) |>
  group_by(periodo, indigeneous, zona_decreto, tipo_id) |>
  summarise(
    media = mean(valor_id, na.rm = TRUE),
    se    = sd(valor_id, na.rm = TRUE) / sqrt(sum(!is.na(valor_id))),
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

p_id_inout <- ggplot(tray_id_long,
    aes(x = periodo, y = media,
        color = grupo, linetype = grupo, group = grupo)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = grupo),
              alpha = 0.06, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ tipo_id, ncol = 2) +
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
    title = "Identidad ingroup vs. outgroup por grupo y zona",
    subtitle = paste0(
      "Ingroup: para indígenas = id pueblo (id_indi+a5), para no indígenas = id Chile (id_chile)\n",
      "Outgroup: viceversa · IC 95%"
    ),
    x = NULL, y = "Media (escala 1–5)",
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

ggsave("output/figuras/fig_identidad_ingroup_outgroup.png", p_id_inout,
       width = 12, height = 6, dpi = 300)
cat("✓ Figura identidad ingroup/outgroup guardada\n")

# ── 4. ANÁLISIS DE HETEROGENEIDAD ────────────────────────────────────────────
# ¿El efecto DiD varía según predominancia identitaria en baseline?

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("ANÁLISIS DE HETEROGENEIDAD POR PREDOMINANCIA IDENTITARIA\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Fijar predominancia al baseline (ola 2) para evitar post-tratamiento
predom_base <- subset_data |>
  filter(ola == 2) |>
  select(folio, predominancia_base = predominancia_id)

subset_data <- subset_data |>
  left_join(predom_base, by = "folio")

cat("\nDistribución predominancia baseline:\n")
cat("  Indígenas:\n")
summary(subset_data |> filter(ola == 2, indigeneous == "indi") |>
  pull(predominancia_base)) |> print()
cat("  No indígenas:\n")
summary(subset_data |> filter(ola == 2, indigeneous == "no_indi") |>
  pull(predominancia_base)) |> print()

# Crear terciles de predominancia por grupo
subset_data <- subset_data |>
  group_by(indigeneous) |>
  mutate(
    predom_tercil = ntile(predominancia_base, 3),
    predom_cat = factor(
      case_when(
        predom_tercil == 1 ~ "Nacional",     # predomina id. nacional
        predom_tercil == 2 ~ "Equilibrio",   # mixto
        predom_tercil == 3 ~ "Étnica"        # predomina id. étnica
      ),
      levels = c("Nacional", "Equilibrio", "Étnica")
    )
  ) |>
  ungroup()

cat("\nTerciles de predominancia (ola 2):\n")
print(table(subset_data |> filter(ola == 2) |>
  select(indigeneous, predom_cat)))

# Modelos DiD separados por tercil de predominancia
metadata <- readRDS("data/analysis_metadata.rds")
controles_base <- metadata$controles_base

pacman::p_load(lme4, lmerTest, broom.mixed)

TERM_DID_DEC <- "periododecreto:indigeneousindi:zona_decretodecreto"

cat("\n--- Modelos DiD por tercil de predominancia (Modelo C) ---\n\n")

resultados_hetero <- list()
for (vd in c("idx_vio_control", "idx_vio_resguardo")) {
  for (tercil in c("Nacional", "Equilibrio", "Étnica")) {
    datos_t <- subset_data |> filter(predom_cat == tercil)

    m <- tryCatch(
      lmer(
        as.formula(paste(
          vd, "~ periodo * indigeneous * zona_decreto +",
          controles_base, "+ (1 | folio)"
        )),
        data = datos_t, REML = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(m)) {
      td <- tidy(m, effects = "fixed") |> filter(term == TERM_DID_DEC)
      if (nrow(td) > 0) {
        cat(sprintf("  %s | Tercil: %-11s | β = %6.3f | SE = %5.3f | p = %s\n",
          if_else(vd == "idx_vio_control", "Control ", "Cambio  "),
          tercil,
          td$estimate, td$std.error,
          format.pval(td$p.value, digits = 3)
        ))
        resultados_hetero[[paste(vd, tercil)]] <- td |>
          mutate(vd = vd, tercil = tercil)
      }
    }
  }
  cat("\n")
}

# Modelo con interacción continua (predominancia × DiD)
cat("\n--- Modelo con predominancia continua como moderador ---\n\n")

for (vd in c("idx_vio_control", "idx_vio_resguardo")) {
  m_mod <- tryCatch(
    lmer(
      as.formula(paste(
        vd, "~ periodo * indigeneous * zona_decreto * predominancia_base +",
        controles_base, "+ (1 | folio)"
      )),
      data = subset_data, REML = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(m_mod)) {
    td_mod <- tidy(m_mod, effects = "fixed") |>
      filter(str_detect(term, "predominancia_base"))
    cat(vd, "— términos con predominancia:\n")
    print(td_mod |> select(term, estimate, std.error, p.value) |>
      mutate(across(where(is.numeric), ~ round(., 3))))
    cat("\n")
  }
}

# ── 5. TABLA RESUMEN PARA DECISIÓN ───────────────────────────────────────────

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("RESUMEN PARA DECISIÓN\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("
PREGUNTAS A RESPONDER CON ESTE OUTPUT:

1. ¿id_indi (a4) tiene varianza entre no indígenas?
   Si >80% responden 1 o 2 → ingroup/outgroup NO viable
   Si hay dispersión → ingroup/outgroup viable

2. ¿id_indi (a4) y a5 correlacionan alto (r > 0.7)?
   Si sí → promediar en idx_id_etnica
   Si no → usar id_indi sola

3. ¿La predominancia (étnica − nacional) varía por zona?
   Si sí → la zona puede afectar identidades → cuidado con
   post-tratamiento

4. ¿El efecto DiD varía por tercil de predominancia?
   Si sí → análisis de heterogeneidad viable y publicable
   Si no → la identidad no modera el efecto

5. ¿La interacción cuádruple (DiD × predominancia) es significativa?
   Si sí → la predominancia identitaria en baseline condiciona
   la respuesta al decreto
   Si no → el efecto es homogéneo por perfil identitario
")

# Guardar resultados de heterogeneidad para paper_results.R
hetero_identidad <- list(
  resultados = bind_rows(resultados_hetero),
  predom_baseline = subset_data |>
    filter(ola == 2) |>
    group_by(indigeneous) |>
    summarise(
      brecha_id = mean(brecha_id, na.rm = TRUE),
      predominancia = mean(predominancia_id, na.rm = TRUE),
      .groups = "drop"
    )
)
saveRDS(hetero_identidad, "data/hetero_identidad.rds")

cat("\n✓ 08_identidad_ingroup.R ejecutado correctamente.\n")
