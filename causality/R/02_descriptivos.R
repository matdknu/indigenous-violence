# =============================================================================
# 02_descriptivos.R — Tablas y figuras descriptivas para el paper
#
# Propósito: producir Tablas 1–2 y Figuras 1–2 a partir del subset analítico.
# Input:     data/subset_data.rds
# Output:    output/tablas/tabla_socdem.{html,docx}
#            output/tablas/tabla_descriptivos.html
#            output/tablas/tabla_consistencia_interna.html
#            output/tablas/tabla_operacionalizacion.html
#            output/figuras/fig_trayectorias.png
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, gt, gtsummary, ggplot2, viridis, patchwork, psych, cardx)
library(cardx)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")

# ── Descriptivos por grupo y período (consola) ────────────────────────────────

desc_grupo <- subset_data |>
  group_by(indigeneous, periodo) |>
  summarise(
    n                = n(),
    vio_control_m   = round(mean(idx_vio_control, na.rm = TRUE), 2),
    vio_resguardo_m    = round(mean(idx_vio_resguardo,  na.rm = TRUE), 2),
    perc_desig_m     = round(mean(perc_desigualdad,  na.rm = TRUE), 2),
    perc_injust_m    = round(mean(perc_injusticia,   na.rm = TRUE), 2),
    apoyo_movil_m    = round(mean(apoyo_movil,       na.rm = TRUE), 2),
    id_causa_m        = round(mean(id_causa,          na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- Descriptivos por identidad y período ---\n")
print(desc_grupo)

desc_zona <- subset_data |>
  group_by(cerca_conflicto, periodo) |>
  summarise(
    n               = n(),
    vio_control_m   = round(mean(idx_vio_control, na.rm = TRUE), 2),
    vio_resguardo_m    = round(mean(idx_vio_resguardo,  na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- Descriptivos por zona y período ---\n")
print(desc_zona)

# ── Tabla 1 — Características sociodemográficas (baseline ola 2) ──────────────

baseline <- subset_data |> filter(ola == 2)

tabla_socdem <- baseline |>
  select(
    indigeneous, mujer, edad, urbano_rural, cerca_conflicto,
    id_chile, id_indi, id_causa
  ) |>
  tbl_summary(
    by = indigeneous,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous()  ~ "{mean} ± {sd}"
    ),
    missing = "no"
  ) |>
  add_p() |>
  add_overall() |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(all_stat_cols() ~ "**Identidad étnica**") |>
  bold_labels()

gt_socdem <- tabla_socdem |>
  as_gt() |>
  gt::opt_stylize(style = 1)

gt_socdem |> gt::gtsave("output/tablas/tabla_socdem.html")
gt_socdem |> gt::gtsave("output/tablas/tabla_socdem.docx")
cat("✓ Tabla 1 guardada: output/tablas/tabla_socdem.{html,docx}\n")

# ── Tabla 2 — Distribución de variables clave ─────────────────────────────────

vars_desc <- c(
  "idx_vio_control", "idx_vio_resguardo",
  "perc_desigualdad", "perc_injusticia", "apoyo_movil",
  "id_chile", "id_causa"
)

tabla_descriptivos <- subset_data |>
  filter(!is.na(indigeneous)) |>
  select(indigeneous, periodo, all_of(vars_desc)) |>
  pivot_longer(all_of(vars_desc), names_to = "variable", values_to = "valor") |>
  group_by(indigeneous, periodo, variable) |>
  summarise(
    N     = sum(!is.na(valor)),
    Media = mean(valor, na.rm = TRUE),
    SD    = sd(valor, na.rm = TRUE),
    Min   = min(valor, na.rm = TRUE),
    Max   = max(valor, na.rm = TRUE),
    pct_na = round(100 * mean(is.na(valor)), 1),
    .groups = "drop"
  ) |>
  mutate(
    variable = factor(
      variable,
      levels = vars_desc,
      labels = c(
        "Justif. vio. control social (status quo)",
        "Justif. vio. cambio social",
        "Perc. desigualdad", "Perc. injusticia", "Apoyo movilizaciones",
        "Id. con Chile", "Id. causa indígena"
      )
    ),
    Media = round(Media, 2),
    SD    = round(SD, 2)
  )

tabla_descriptivos <- tabla_descriptivos |>
  mutate(
    grupo = paste0(
      ifelse(indigeneous == "indi", "Indígena", "No indígena"),
      " — ",
      case_when(
        periodo == "pre"         ~ "Ola 2 (pre)",
        periodo == "tratamiento" ~ "Ola 3 (tratamiento)",
        periodo == "post"        ~ "Ola 4 (post)",
        TRUE                     ~ as.character(periodo)
      )
    )
  ) |>
  select(-indigeneous, -periodo)

gt_desc <- tabla_descriptivos |>
  gt(groupname_col = "grupo", rowname_col = "variable") |>
  cols_label(
    variable = "Variable",
    N = "N", Media = "Media", SD = "SD",
    Min = "Mín", Max = "Máx", pct_na = "% NA"
  ) |>
  tab_header(
    title = "Distribución de variables dependientes e independientes clave",
    subtitle = "Por identidad étnica y período (ELRI, olas 2–4)"
  ) |>
  fmt_number(columns = c(Media, SD, Min, Max), decimals = 2) |>
  opt_stylize(style = 1)

gt_desc |> gtsave("output/tablas/tabla_descriptivos.html")
cat("✓ Tabla 2 guardada: output/tablas/tabla_descriptivos.html\n")

# ── Figura 1 — Trayectorias longitudinales ─────────────────────────────────────

tray_long <- subset_data |>
  filter(!is.na(indigeneous)) |>
  group_by(indigeneous, cerca_conflicto, periodo) |>
  summarise(
    vio_control   = mean(idx_vio_control, na.rm = TRUE),
    vio_resguardo = mean(idx_vio_resguardo, na.rm = TRUE),
    vio_control_lo = mean(idx_vio_control, na.rm = TRUE) -
      1.96 * sd(idx_vio_control, na.rm = TRUE) / sqrt(sum(!is.na(idx_vio_control))),
    vio_control_hi = mean(idx_vio_control, na.rm = TRUE) +
      1.96 * sd(idx_vio_control, na.rm = TRUE) / sqrt(sum(!is.na(idx_vio_control))),
    vio_resguardo_lo = mean(idx_vio_resguardo, na.rm = TRUE) -
      1.96 * sd(idx_vio_resguardo, na.rm = TRUE) / sqrt(sum(!is.na(idx_vio_resguardo))),
    vio_resguardo_hi = mean(idx_vio_resguardo, na.rm = TRUE) +
      1.96 * sd(idx_vio_resguardo, na.rm = TRUE) / sqrt(sum(!is.na(idx_vio_resguardo))),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(vio_control, vio_resguardo),
    names_to = "indice",
    values_to = "media"
  ) |>
  mutate(
    lo = if_else(indice == "vio_control", vio_control_lo, vio_resguardo_lo),
    hi = if_else(indice == "vio_control", vio_control_hi, vio_resguardo_hi)
  ) |>
  select(-vio_control_lo, -vio_control_hi, -vio_resguardo_lo, -vio_resguardo_hi) |>
  mutate(
    indice = factor(
      indice,
      levels = c("vio_control", "vio_resguardo"),
      labels = c(
        "Justificación de la violencia\npor el control social (status quo)",
        "Justificación de la violencia\npor el cambio social"
      )
    ),
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

p_tray <- ggplot(tray_long,
                 aes(x = periodo, y = media,
                     color = grupo, linetype = grupo, group = grupo,
                     fill = grupo)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  facet_wrap(~ indice, scales = "free_y", ncol = 2) +
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
  scale_x_discrete(
    labels = c(
      "pre"         = "Ola 2\n(Pre)",
      "tratamiento" = "Ola 3\n(Tratamiento)",
      "post"        = "Ola 4\n(Post)"
    )
  ) +
  labs(
    title    = "Trayectorias longitudinales por grupo identitario y zona",
    subtitle = "ELRI — Olas 2 (pre), 3 (estado de excepción), 4 (post)",
    x = NULL, y = "Media (escala 1–5)",
    caption  = "Línea sólida = zona de excepción · Línea punteada = lejos del conflicto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave("output/figuras/fig_trayectorias.png", p_tray,
       width = 11, height = 5, dpi = 300)
cat("✓ Figura 1 guardada: output/figuras/fig_trayectorias.png\n")

# ── Figura 2 (apéndice) — Consistencia interna ────────────────────────────────

alpha_ctrl <- psych::alpha(
  subset_data[, c("vio_ctrl_carb", "vio_ctrl_agric")], check.keys = TRUE
)
alpha_resg <- psych::alpha(
  subset_data[, c("vio_camb_tierras", "vio_camb_cortes")], check.keys = TRUE
)

r_ctrl <- cor(subset_data$vio_ctrl_carb, subset_data$vio_ctrl_agric,
              use = "complete.obs")
r_resg <- cor(subset_data$vio_camb_tierras, subset_data$vio_camb_cortes,
              use = "complete.obs")

cat("\n--- Consistencia interna ---\n")
cat("  Vio. control: α =", round(alpha_ctrl$total$raw_alpha, 3),
    "| r =", round(r_ctrl, 3), "\n")
cat("  Vio. resguardo: α =", round(alpha_resg$total$raw_alpha, 3),
    "| r =", round(r_resg, 3), "\n")

tabla_consistencia <- tibble(
  Indice = c(
    "Control social (status quo)",
    "Cambio social"
  ),
  Alpha  = c(
    round(alpha_ctrl$total$raw_alpha, 3),
    round(alpha_resg$total$raw_alpha, 3)
  ),
  `Correlacion inter-item` = c(round(r_ctrl, 3), round(r_resg, 3))
)

gt_consist <- tabla_consistencia |>
  gt() |>
  tab_header(
    title = "Consistencia interna de índices compuestos",
    subtitle = "Alfa de Cronbach y correlación inter-ítem (apéndice)"
  ) |>
  fmt_number(columns = c(Alpha, `Correlacion inter-item`), decimals = 3) |>
  opt_stylize(style = 1)

gt_consist |> gtsave("output/tablas/tabla_consistencia_interna.html")
cat("✓ Tabla apéndice guardada: output/tablas/tabla_consistencia_interna.html\n")

# ── Tabla operacionalización de variables (paper) ───────────────────────────────

alpha_just <- psych::alpha(
  subset_data[, c("just_proc_indi", "just_proc_noindi")],
  check.keys = TRUE
)

tabla_variables <- tibble::tribble(
  ~Variable,              ~Items,                                        ~Escala,       ~Fuente,
  "Control social (status quo)", "d3_1 + d3_2 (Carabineros/agricultores)", "1–5 (α=.77)", "ELRI D",
  "Cambio social",               "d4_2 + d4_3 (tierras/carreteras)",       "1–5 (α=.75)", "ELRI D",
  "Justicia proc.",       "d5_1 + d5_2 (trato Carabineros)",            "1–5 (α=.83)", "ELRI D",
  "Id. causa indígena",  "d6_1",                                        "1–5",         "ELRI D",
  "Id. con Chile",        "a6",                                          "1–5",         "ELRI A",
  "Perc. desigualdad",    "c22 (invertida)",                             "1–5",         "ELRI C",
  "Perc. injusticia",     "c23 (invertida)",                             "1–5",         "ELRI C",
  "Apoyo movilizaciones", "c25",                                         "1–5",         "ELRI C"
) |>
  mutate(
    Escala = case_when(
      Variable == "Justicia proc." ~ paste0("1–5 (α=", round(alpha_just$total$raw_alpha, 2), ")"),
      TRUE ~ Escala
    )
  ) |>
  gt() |>
  cols_label(Variable = "Variable", Items = "Ítem(s)", Escala = "Escala", Fuente = "Módulo") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid",
    table_body.hlines.style = "none",
    table.font.size = px(11)
  )

tabla_variables |> gtsave("output/tablas/tabla_operacionalizacion.html")
cat("✓ Tabla operacionalización guardada: output/tablas/tabla_operacionalizacion.html\n")
