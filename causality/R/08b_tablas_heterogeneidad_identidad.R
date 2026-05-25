# =============================================================================
# 08b_tablas_heterogeneidad_identidad.R
#
# Propósito: Generar tablas publicables con resultados de heterogeneidad
#            por predominancia identitaria baseline
#
# Input:  data/subset_data.rds
# Output: output/tablas/tabla_heterog_predominancia.html
#         output/tablas/tabla_heterog_predominancia.docx
# =============================================================================

set.seed(2024)
pacman::p_load(
  dplyr, tidyverse, lme4, lmerTest, broom.mixed,
  gt, gtExtras, flextable, officer
)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# ── 1. CARGAR DATOS Y RECREAR VARIABLES ──────────────────────────────────────

subset_data <- readRDS("data/subset_data.rds")

# Cargar a5 desde panel raw
load("data/BBDD_ELRI_LONG.RData")

recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  xv <- if (inherits(x, "haven_labelled")) {
    as.integer(haven::zap_labels(x))
  } else {
    as.integer(x)
  }
  if_else(xv %in% miss_vals, NA_integer_, xv)
}

a5_data <- BBDD_ELRI_LONG |>
  select(folio, ola, a5) |>
  mutate(a5 = recode_missing(a5))

subset_data <- subset_data |>
  left_join(a5_data, by = c("folio", "ola"), relationship = "many-to-many") |>
  mutate(
    a4_num = as.numeric(id_indi),
    a5_num = as.numeric(a5),
    a6_num = as.numeric(id_chile),
    idx_id_etnica = rowMeans(pick(a4_num, a5_num), na.rm = TRUE),
    id_nacional = a6_num,
    predominancia_id = idx_id_etnica - id_nacional
  )

# Fijar predominancia al baseline
predom_base <- subset_data |>
  filter(ola == 2) |>
  select(folio, predominancia_base = predominancia_id)

subset_data <- subset_data |>
  left_join(predom_base, by = "folio") |>
  group_by(indigeneous) |>
  mutate(
    predom_tercil = ntile(predominancia_base, 3),
    predom_cat = factor(
      case_when(
        predom_tercil == 1 ~ "Nacional",
        predom_tercil == 2 ~ "Equilibrio",
        predom_tercil == 3 ~ "Étnica"
      ),
      levels = c("Nacional", "Equilibrio", "Étnica")
    )
  ) |>
  ungroup()

metadata <- readRDS("data/analysis_metadata.rds")
controles_base <- metadata$controles_base

# ── 2. MODELOS DiD POR TERCIL ─────────────────────────────────────────────────

TERM_DID_DEC <- "periododecreto:indigeneousindi:zona_decretodecreto"

resultados <- list()

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
      td <- tidy(m, effects = "fixed", conf.int = TRUE) |>
        filter(term == TERM_DID_DEC)
      
      if (nrow(td) > 0) {
        resultados[[paste(vd, tercil)]] <- td |>
          mutate(
            vd = vd,
            tercil = tercil,
            n_individuos = n_distinct(datos_t$folio),
            n_obs = nrow(datos_t)
          )
      }
    }
  }
}

# Consolidar
df_resultados <- bind_rows(resultados) |>
  mutate(
    vd_label = case_when(
      vd == "idx_vio_control" ~ "Justifica violencia de control",
      vd == "idx_vio_resguardo" ~ "Justifica violencia de cambio"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

# ── 3. TABLA GT (HTML) ────────────────────────────────────────────────────────

tabla_gt <- df_resultados |>
  select(
    vd_label, tercil, estimate, std.error, conf.low, conf.high,
    statistic, p.value, sig, n_individuos, n_obs
  ) |>
  gt(groupname_col = "vd_label") |>
  tab_header(
    title = "Heterogeneidad del efecto DiD por predominancia identitaria baseline",
    subtitle = "Modelos mixtos DiD (Modelo C) estratificados por tercil de predominancia"
  ) |>
  cols_label(
    tercil = "Tercil predominancia",
    estimate = "β",
    std.error = "SE",
    conf.low = "IC 95% inf.",
    conf.high = "IC 95% sup.",
    statistic = "t",
    p.value = "p",
    sig = "",
    n_individuos = "N indiv.",
    n_obs = "N obs."
  ) |>
  fmt_number(
    columns = c(estimate, std.error, conf.low, conf.high, statistic),
    decimals = 3
  ) |>
  fmt_number(
    columns = p.value,
    decimals = 4
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = estimate,
      rows = p.value < 0.05
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(
      rows = tercil == "Equilibrio"
    )
  ) |>
  tab_footnote(
    footnote = paste0(
      "Predominancia = (id_étnica − id_nacional) fijada en baseline (ola 2). ",
      "Tercil 1 (Nacional) = predomina identidad nacional; ",
      "Tercil 2 (Equilibrio) = identidad mixta; ",
      "Tercil 3 (Étnica) = predomina identidad étnica. ",
      "Controles: ", controles_base, ". ",
      "*** p<0.001, ** p<0.01, * p<0.05"
    )
  ) |>
  tab_source_note(
    source_note = "Fuente: ELRI panel balanceado (olas 2-4). Término DiD: periodo×indigeneous×zona_decreto"
  )

gtsave(tabla_gt, "output/tablas/tabla_heterog_predominancia.html")
cat("✓ Tabla HTML guardada: output/tablas/tabla_heterog_predominancia.html\n")

# ── 4. TABLA FLEXTABLE (DOCX) ─────────────────────────────────────────────────

ft <- df_resultados |>
  select(
    vd_label, tercil, estimate, std.error, conf.low, conf.high,
    p.value, sig, n_individuos
  ) |>
  mutate(
    ci_95 = sprintf("[%.3f, %.3f]", conf.low, conf.high),
    est_se = sprintf("%.3f (%.3f)%s", estimate, std.error, sig)
  ) |>
  select(vd_label, tercil, est_se, ci_95, p.value, n_individuos) |>
  flextable() |>
  set_header_labels(
    vd_label = "Variable dependiente",
    tercil = "Tercil predominancia",
    est_se = "β (SE)",
    ci_95 = "IC 95%",
    p.value = "p-valor",
    n_individuos = "N"
  ) |>
  merge_v(j = "vd_label") |>
  align(align = "center", part = "all") |>
  align(j = 1:2, align = "left", part = "body") |>
  fontsize(size = 10, part = "all") |>
  bold(j = "est_se", i = ~ p.value < 0.05) |>
  add_header_lines(
    values = c(
      "Heterogeneidad del efecto DiD por predominancia identitaria baseline",
      "Modelos mixtos DiD (Modelo C) estratificados por tercil"
    )
  ) |>
  add_footer_lines(
    values = c(
      "Nota: Predominancia = (id_étnica − id_nacional) fijada en baseline (ola 2).",
      "Tercil 1 (Nacional) = predomina id. nacional; Tercil 2 (Equilibrio) = mixta; Tercil 3 (Étnica) = predomina id. étnica.",
      sprintf("Controles: %s. *** p<0.001, ** p<0.01, * p<0.05", controles_base),
      "Fuente: ELRI panel balanceado (olas 2-4). Término DiD: periodo×indigeneous×zona_decreto"
    )
  ) |>
  autofit()

save_as_docx(
  ft,
  path = "output/tablas/tabla_heterog_predominancia.docx"
)
cat("✓ Tabla DOCX guardada: output/tablas/tabla_heterog_predominancia.docx\n")

# ── 5. TABLA DESCRIPTIVA: PREDOMINANCIA POR GRUPO ─────────────────────────────

desc_predom <- subset_data |>
  filter(ola == 2) |>
  group_by(indigeneous) |>
  summarise(
    n = n(),
    media = mean(predominancia_base, na.rm = TRUE),
    sd = sd(predominancia_base, na.rm = TRUE),
    min = min(predominancia_base, na.rm = TRUE),
    q25 = quantile(predominancia_base, 0.25, na.rm = TRUE),
    mediana = median(predominancia_base, na.rm = TRUE),
    q75 = quantile(predominancia_base, 0.75, na.rm = TRUE),
    max = max(predominancia_base, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    indigeneous = if_else(indigeneous == "indi", "Indígena", "No indígena")
  )

gt_desc <- desc_predom |>
  gt() |>
  tab_header(
    title = "Distribución de predominancia identitaria baseline (ola 2)",
    subtitle = "Predominancia = (id_étnica − id_nacional)"
  ) |>
  cols_label(
    indigeneous = "Grupo",
    n = "N",
    media = "Media",
    sd = "SD",
    min = "Mín.",
    q25 = "Q1",
    mediana = "Mediana",
    q75 = "Q3",
    max = "Máx."
  ) |>
  fmt_number(
    columns = c(media, sd, min, q25, mediana, q75, max),
    decimals = 2
  ) |>
  tab_footnote(
    footnote = paste0(
      "Predominancia positiva = predomina identidad étnica sobre nacional. ",
      "Predominancia negativa = predomina identidad nacional sobre étnica. ",
      "id_étnica = promedio de a4 (identificación con pueblo) y a5 (importancia). ",
      "id_nacional = a6 (identificación con Chile)."
    )
  )

gtsave(gt_desc, "output/tablas/tabla_predominancia_descriptiva.html")
cat("✓ Tabla descriptiva guardada: output/tablas/tabla_predominancia_descriptiva.html\n")

# ── 6. TABLA CRUZADA: TERCILES POR GRUPO ─────────────────────────────────────

tercil_grupo <- subset_data |>
  filter(ola == 2) |>
  count(indigeneous, predom_cat) |>
  group_by(indigeneous) |>
  mutate(
    prop = n / sum(n),
    label = sprintf("%d (%.1f%%)", n, prop * 100)
  ) |>
  ungroup() |>
  select(indigeneous, predom_cat, label) |>
  pivot_wider(
    names_from = predom_cat,
    values_from = label,
    values_fill = "0 (0.0%)"
  ) |>
  mutate(
    indigeneous = if_else(indigeneous == "indi", "Indígena", "No indígena")
  )

gt_tercil <- tercil_grupo |>
  gt() |>
  tab_header(
    title = "Distribución de respondentes por tercil de predominancia (baseline)",
    subtitle = "N (%) por grupo étnico y tercil de predominancia identitaria"
  ) |>
  cols_label(
    indigeneous = "Grupo",
    Nacional = "Tercil 1: Nacional",
    Equilibrio = "Tercil 2: Equilibrio",
    Étnica = "Tercil 3: Étnica"
  ) |>
  tab_footnote(
    footnote = "Los terciles se calculan por separado dentro de cada grupo étnico."
  )

gtsave(gt_tercil, "output/tablas/tabla_terciles_grupo.html")
cat("✓ Tabla terciles guardada: output/tablas/tabla_terciles_grupo.html\n")

cat("\n✓ 08b_tablas_heterogeneidad_identidad.R ejecutado correctamente.\n")
