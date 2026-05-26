# =============================================================================
# 07_likert_collapse.R — Colapso ordinal Likert 1–5 → 3 categorías (A vs B)
#
# Propósito: comparar dos esquemas de recodificación para justificación de
#            violencia (control social y cambio social), visualizar distribuciones
#            y contrastar ajuste de modelos DiD (continuo vs ordinal).
#
# Esquema A (simétrico):     1–2 Rechaza | 3 Neutral | 4–5 Justifica
# Esquema B (intensidad):     1–2 Rechaza | 3–4 Moderado | 5 Apoya totalmente
#
# Input:  data/subset_data.rds
# Output: output/figuras/fig_likert_*.png
#         output/tablas/tabla_likert_*.html
#         data/likert_collapse.rds
# =============================================================================



set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, ggplot2, patchwork, stringr,
  broom.mixed, modelsummary, gt, lme4, lmerTest, ordinal, performance
)

source("R/plot_helpers.R")

if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")

# ── Funciones de colapso ──────────────────────────────────────────────────────

collapse_sym_item <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2)     ~ 1L,
    x == 3             ~ 2L,
    x %in% c(4, 5)     ~ 3L,
    TRUE               ~ NA_integer_
  )
}

collapse_int_item <- function(x) {
  dplyr::case_when(
    x %in% c(1, 2)     ~ 1L,
    x %in% c(3, 4)     ~ 2L,
    x == 5             ~ 3L,
    TRUE               ~ NA_integer_
  )
}

collapse_sym_idx <- function(x) {
  dplyr::case_when(
    x <= 2             ~ 1L,
    x > 2 & x < 4      ~ 2L,
    x >= 4             ~ 3L,
    TRUE               ~ NA_integer_
  )
}

collapse_int_idx <- function(x) {
  dplyr::case_when(
    x <= 2             ~ 1L,
    x > 2 & x < 5      ~ 2L,
    x >= 5             ~ 3L,
    TRUE               ~ NA_integer_
  )
}

lab_ord_A <- c("1" = "Rechaza", "2" = "Neutral", "3" = "Justifica")
lab_ord_B <- c("1" = "Rechaza", "2" = "Moderado", "3" = "Apoya totalmente")

factor_ord <- function(x, labels) {
  factor(x, levels = 1:3, labels = labels, ordered = TRUE)
}

# ── Recodificación (ítem + índice) ────────────────────────────────────────────

dat <- subset_data |>
  mutate(
    # Ítems colapsados
    vio_ctrl_carb_A = collapse_sym_item(vio_ctrl_carb),
    vio_ctrl_agric_A = collapse_sym_item(vio_ctrl_agric),
    vio_camb_tierras_A = collapse_sym_item(vio_camb_tierras),
    vio_camb_cortes_A = collapse_sym_item(vio_camb_cortes),
    vio_ctrl_carb_B = collapse_int_item(vio_ctrl_carb),
    vio_ctrl_agric_B = collapse_int_item(vio_ctrl_agric),
    vio_camb_tierras_B = collapse_int_item(vio_camb_tierras),
    vio_camb_cortes_B = collapse_int_item(vio_camb_cortes),
    # Índice: media de ítems colapsados → redondeo a categoría 1–3
    idx_ctrl_mean_A = rowMeans(
      cbind(vio_ctrl_carb_A, vio_ctrl_agric_A), na.rm = TRUE
    ),
    idx_resg_mean_A = rowMeans(
      cbind(vio_camb_tierras_A, vio_camb_cortes_A), na.rm = TRUE
    ),
    idx_ctrl_mean_B = rowMeans(
      cbind(vio_ctrl_carb_B, vio_ctrl_agric_B), na.rm = TRUE
    ),
    idx_resg_mean_B = rowMeans(
      cbind(vio_camb_tierras_B, vio_camb_cortes_B), na.rm = TRUE
    ),
    idx_vio_control_A = factor_ord(pmax(1L, pmin(3L, round(idx_ctrl_mean_A))), lab_ord_A),
    idx_vio_resguardo_A = factor_ord(pmax(1L, pmin(3L, round(idx_resg_mean_A))), lab_ord_A),
    idx_vio_control_B = factor_ord(pmax(1L, pmin(3L, round(idx_ctrl_mean_B))), lab_ord_B),
    idx_vio_resguardo_B = factor_ord(pmax(1L, pmin(3L, round(idx_resg_mean_B))), lab_ord_B),
    # Colapso directo sobre media continua 1–5 (sensibilidad)
    idx_vio_control_A_idx = factor_ord(collapse_sym_idx(idx_vio_control), lab_ord_A),
    idx_vio_resguardo_A_idx = factor_ord(collapse_sym_idx(idx_vio_resguardo), lab_ord_A),
    idx_vio_control_B_idx = factor_ord(collapse_int_idx(idx_vio_control), lab_ord_B),
    idx_vio_resguardo_B_idx = factor_ord(collapse_int_idx(idx_vio_resguardo), lab_ord_B)
  )

cat("\n", strrep("=", 60), "\n")
cat("COLAPSO LIKERT — distribución esquema A vs B (índice redondeado)\n")
cat(strrep("=", 60), "\n\n")

tab_ctrl_A <- dat |>
  count(periodo, idx_vio_control_A, name = "n") |>
  group_by(periodo) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup()
tab_ctrl_B <- dat |>
  count(periodo, idx_vio_control_B, name = "n") |>
  group_by(periodo) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup()

cat("--- Control social — Esquema A (simétrico) ---\n")
print(tab_ctrl_A)
cat("\n--- Control social — Esquema B (intensidad) ---\n")
print(tab_ctrl_B)

conc_ctrl <- dat |>
  count(
    A = as.character(idx_vio_control_A),
    B = as.character(idx_vio_control_B),
    name = "n"
  )
conc_resg <- dat |>
  count(
    A = as.character(idx_vio_resguardo_A),
    B = as.character(idx_vio_resguardo_B),
    name = "n"
  )
cat("\n--- Tabla cruzada A × B — Control social ---\n")
print(conc_ctrl)
cat("\n--- Tabla cruzada A × B — Cambio social ---\n")
print(conc_resg)

# ── Controles (misma regla que 03_modelos.R) ────────────────────────────────────

cor_ur_cc <- cor(
  as.numeric(dat$urbano_rural),
  as.numeric(dat$cerca_conflicto),
  use = "complete.obs"
)
incluir_urbano_rural <- abs(cor_ur_cc) <= 0.5
if (file.exists("data/analysis_metadata.rds")) {
  controles_base <- readRDS("data/analysis_metadata.rds")$controles_base
} else {
  controles_base <- if (incluir_urbano_rural) {
    "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
  } else {
    "mujer + edad + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
  }
}

form_did <- function(y) {
  as.formula(paste(
    y, "~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  ))
}

TERM_DID_DECRETO <- "periododecreto:indigeneousindi:cerca_conflictocerca"

extract_did <- function(model, term = TERM_DID_DECRETO, spec, vd, family) {
  if (inherits(model, "clmm")) {
    td <- broom::tidy(model, conf.int = TRUE)
  } else {
    td <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  }
  row <- td |> filter(.data$term == .env$term)
  if (nrow(row) == 0) {
    return(tibble(
      spec = spec, vd = vd, family = family,
      estimate = NA_real_, std.error = NA_real_, p.value = NA_real_,
      AIC = NA_real_, BIC = NA_real_, logLik = NA_real_
    ))
  }
  tibble(
    spec = spec,
    vd = vd,
    family = family,
    estimate = row$estimate[1],
    std.error = row$std.error[1],
    p.value = row$p.value[1],
    AIC = tryCatch(AIC(model), error = function(e) NA_real_),
    BIC = tryCatch(BIC(model), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  )
}

cat("\n", strrep("=", 60), "\n")
cat("MODELOS DiD — continuo (1–5) vs ordinal A vs ordinal B\n")
cat(strrep("=", 60), "\n\n")

fit_specs <- list()

# Continuo
fit_specs[["ctrl_cont"]] <- lmer(
  form_did("idx_vio_control"), data = dat, REML = FALSE
)
fit_specs[["resg_cont"]] <- lmer(
  form_did("idx_vio_resguardo"), data = dat, REML = FALSE
)

# Ordinal A (índice redondeado post-ítem)
fit_specs[["ctrl_ordA"]] <- clmm(
  form_did("idx_vio_control_A"), data = dat
)
fit_specs[["resg_ordA"]] <- clmm(
  form_did("idx_vio_resguardo_A"), data = dat
)

# Ordinal B
fit_specs[["ctrl_ordB"]] <- clmm(
  form_did("idx_vio_control_B"), data = dat
)
fit_specs[["resg_ordB"]] <- clmm(
  form_did("idx_vio_resguardo_B"), data = dat
)

# Lineal en códigos 1–3 (aprox. robustez)
dat <- dat |>
  mutate(
    idx_vio_control_A_num = as.numeric(idx_vio_control_A),
    idx_vio_resguardo_A_num = as.numeric(idx_vio_resguardo_A),
    idx_vio_control_B_num = as.numeric(idx_vio_control_B),
    idx_vio_resguardo_B_num = as.numeric(idx_vio_resguardo_B)
  )

fit_specs[["ctrl_linA"]] <- lmer(
  form_did("idx_vio_control_A_num"), data = dat, REML = FALSE
)
fit_specs[["resg_linA"]] <- lmer(
  form_did("idx_vio_resguardo_A_num"), data = dat, REML = FALSE
)
fit_specs[["ctrl_linB"]] <- lmer(
  form_did("idx_vio_control_B_num"), data = dat, REML = FALSE
)
fit_specs[["resg_linB"]] <- lmer(
  form_did("idx_vio_resguardo_B_num"), data = dat, REML = FALSE
)

resumen_modelos <- bind_rows(
  extract_did(fit_specs$ctrl_cont, spec = "Continuo 1–5", vd = "Control social", family = "Gaussian"),
  extract_did(fit_specs$resg_cont, spec = "Continuo 1–5", vd = "Cambio social", family = "Gaussian"),
  extract_did(fit_specs$ctrl_ordA, spec = "Ordinal A (simétrico)", vd = "Control social", family = "CLMM"),
  extract_did(fit_specs$resg_ordA, spec = "Ordinal A (simétrico)", vd = "Cambio social", family = "CLMM"),
  extract_did(fit_specs$ctrl_ordB, spec = "Ordinal B (intensidad)", vd = "Control social", family = "CLMM"),
  extract_did(fit_specs$resg_ordB, spec = "Ordinal B (intensidad)", vd = "Cambio social", family = "CLMM"),
  extract_did(fit_specs$ctrl_linA, spec = "Lineal cód. A", vd = "Control social", family = "Gaussian (1–3)"),
  extract_did(fit_specs$resg_linA, spec = "Lineal cód. A", vd = "Cambio social", family = "Gaussian (1–3)"),
  extract_did(fit_specs$ctrl_linB, spec = "Lineal cód. B", vd = "Control social", family = "Gaussian (1–3)"),
  extract_did(fit_specs$resg_linB, spec = "Lineal cód. B", vd = "Cambio social", family = "Gaussian (1–3)")
) |>
  mutate(
    sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ "+",
      TRUE ~ ""
    )
  )

cat("Coeficiente DiD decreto (ola 4 × indígena × zona):\n")
print(resumen_modelos |> select(spec, vd, estimate, std.error, p.value, AIC, BIC, sig))

# Mejor ajuste por VD (menor AIC entre especificaciones comparables)
mejor_ajuste <- resumen_modelos |>
  filter(!is.na(AIC)) |>
  group_by(vd) |>
  slice_min(AIC, n = 1, with_ties = FALSE) |>
  ungroup()

cat("\n--- Menor AIC por variable dependiente ---\n")
print(mejor_ajuste |> select(vd, spec, AIC, BIC, estimate, p.value))

# ── Tablas HTML ───────────────────────────────────────────────────────────────

prep_distrib <- function(var, vd_label, esquema_label) {
  dat |>
    count(periodo, categoria = as.character(.data[[var]]), name = "n") |>
    group_by(periodo) |>
    mutate(
      pct = round(100 * n / sum(n), 1),
      vd = vd_label,
      esquema = esquema_label
    ) |>
    ungroup()
}

tab_distrib <- bind_rows(
  prep_distrib("idx_vio_control_A", "Control social", "A (simétrico)"),
  prep_distrib("idx_vio_control_B", "Control social", "B (intensidad)"),
  prep_distrib("idx_vio_resguardo_A", "Cambio social", "A (simétrico)"),
  prep_distrib("idx_vio_resguardo_B", "Cambio social", "B (intensidad)")
)

tab_distrib |>
  gt(groupname_col = c("vd", "esquema", "periodo")) |>
  cols_label(categoria = "Categoría", n = "N", pct = "%") |>
  tab_header(
    title = "Distribución de categorías colapsadas",
    subtitle = "Índice = media de ítems colapsados, redondeada a 1–3"
  ) |>
  opt_stylize(style = 1) |>
  gtsave("output/tablas/tabla_likert_distribucion.html")

resumen_modelos |>
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = if_else(is.na(p.value), NA, round(p.value, 4)),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1)
  ) |>
  gt() |>
  tab_header(
    title = "Comparación de modelos DiD por codificación",
    subtitle = "Coeficiente de interés: Ola 4 × Indígena × Zona excepción"
  ) |>
  fmt_missing(columns = everything(), missing_text = "—") |>
  opt_stylize(style = 1) |>
  gtsave("output/tablas/tabla_likert_modelos.html")

# ── Figura 1 — Densidad escala continua + cortes A y B ────────────────────────

dat_long_dens <- dat |>
  select(periodo, idx_vio_control, idx_vio_resguardo) |>
  pivot_longer(
    -periodo,
    names_to = "variable",
    values_to = "valor"
  ) |>
  mutate(
    vd = if_else(variable == "idx_vio_control", "Control social", "Cambio social")
  )

p_dens <- ggplot(dat_long_dens, aes(x = valor, fill = periodo, color = periodo)) +
  geom_density(alpha = 0.25, linewidth = 0.5) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "#2166AC", linewidth = 0.7) +
  geom_vline(xintercept = 3.5, linetype = "solid", color = "#B22222", linewidth = 0.7) +
  geom_vline(xintercept = 2.5, linetype = "dotted", color = "#4DAF4A", linewidth = 0.6) +
  geom_vline(xintercept = 4.5, linetype = "dotdash", color = "#984EA3", linewidth = 0.6) +
  facet_wrap(~ vd, ncol = 2) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) +
  labs(
    title = "Densidad de la escala Likert continua (1–5)",
    subtitle = "Líneas A: 2.5 y 3.5 · Líneas B: 2.5 y 4.5 (cortes sobre media continua del índice)",
    x = "Puntuación índice (media ítems)", y = "Densidad",
    fill = "Período", color = "Período",
    caption = "1=Nunca · 2=No · 3=A veces · 4=Se justifica · 5=Siempre"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/figuras/fig_likert_density.png", p_dens,
       width = 10, height = 5, dpi = 300)

# ── Figura 2 — Barras apiladas % por período (A vs B) ─────────────────────────

prep_stack <- function(data, var, vd_label, esquema_label) {
  data |>
    count(periodo, categoria = as.character(.data[[var]]), name = "n") |>
    group_by(periodo) |>
    mutate(
      pct = 100 * n / sum(n),
      vd = vd_label,
      esquema = esquema_label
    ) |>
    ungroup()
}

stack_dat <- bind_rows(
  prep_stack(dat, "idx_vio_control_A", "Control social", "A — Simétrico"),
  prep_stack(dat, "idx_vio_resguardo_A", "Cambio social", "A — Simétrico"),
  prep_stack(dat, "idx_vio_control_B", "Control social", "B — Intensidad"),
  prep_stack(dat, "idx_vio_resguardo_B", "Cambio social", "B — Intensidad")
)

pal_A <- c("Rechaza" = "#2166AC", "Neutral" = "#FDB863", "Justifica" = "#D73027")
pal_B <- c("Rechaza" = "#2166AC", "Moderado" = "#FDB863", "Apoya totalmente" = "#D73027")

p_stack <- ggplot(stack_dat, aes(x = periodo, y = pct, fill = categoria)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = if_else(pct >= 8, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.8, color = "grey15"
  ) +
  facet_grid(esquema ~ vd) +
  scale_fill_manual(
    values = c(
      "Rechaza" = "#2166AC",
      "Neutral" = "#FDB863",
      "Justifica" = "#D73027",
      "Moderado" = "#FDB863",
      "Apoya totalmente" = "#D73027"
    ),
    name = "Categoría"
  ) +
  scale_x_discrete(
    labels = c(
      "pre" = "Ola 2\n(2018)",
      "estallido" = "Ola 3\n(2021)",
      "decreto" = "Ola 4\n(2023)"
    )
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Distribución de categorías colapsadas por período",
    subtitle = "A: 1–2 Rechaza · 3 Neutral · 4–5 Justifica  |  B: 1–2 Rechaza · 3–4 Moderado · 5 Apoya totalmente",
    x = NULL, y = "% dentro del período"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("output/figuras/fig_likert_distrib.png", p_stack,
       width = 11, height = 7, dpi = 300)

# ── Figura 3 — Medias (continuo vs códigos 1–3) por período ───────────────────

medias_cmp <- dat |>
  group_by(periodo) |>
  summarise(
    across(
      c(
        idx_vio_control, idx_vio_resguardo,
        idx_vio_control_A_num, idx_vio_resguardo_A_num,
        idx_vio_control_B_num, idx_vio_resguardo_B_num
      ),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  pivot_longer(-periodo, names_to = "var", values_to = "media") |>
  mutate(
    vd = if_else(str_detect(var, "control"), "Control social", "Cambio social"),
    esquema = case_when(
      str_detect(var, "_A_num") ~ "A (1–3)",
      str_detect(var, "_B_num") ~ "B (1–3)",
      TRUE ~ "Continuo (1–5)"
    ),
    periodo_num = match(periodo, c("pre", "estallido", "decreto"))
  )

p_medias <- ggplot(medias_cmp,
                   aes(x = periodo_num, y = media,
                       color = esquema, linetype = esquema, group = esquema)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ vd, ncol = 2, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:3,
    labels = c("Ola 2\n(2018)", "Ola 3\n(2021)", "Ola 4\n(2023)")
  ) +
  scale_color_manual(values = c(
    "Continuo (1–5)" = "#333333",
    "A (1–3)" = "#2166AC",
    "B (1–3)" = "#D73027"
  )) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  labs(
    title = "Medias por período: escala continua vs códigos colapsados",
    subtitle = "Códigos A/B = media de ítems recodificados (1–3), redondeada",
    x = NULL, y = "Media",
    color = "Codificación", linetype = "Codificación"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(nrow = 1, override.aes = list(linetype = c("solid", "dashed", "dotdash"))),
    linetype = "none"
  )

ggsave("output/figuras/fig_likert_medias.png", p_medias,
       width = 10, height = 5, dpi = 300)

# ── Figura 4 — Concordancia A vs B (heatmap) ───────────────────────────────────

conc_plot2 <- bind_rows(
  dat |>
    count(cat_A = idx_vio_control_A, cat_B = idx_vio_control_B, name = "n") |>
    mutate(vd = "Control social"),
  dat |>
    count(cat_A = idx_vio_resguardo_A, cat_B = idx_vio_resguardo_B, name = "n") |>
    mutate(vd = "Cambio social")
) |>
  group_by(vd) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

p_conc <- ggplot(conc_plot2, aes(x = cat_A, y = cat_B, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), size = 3) +
  facet_wrap(~ vd, ncol = 2) +
  scale_fill_viridis_c(option = "magma", name = "% obs.") +
  labs(
    title = "Concordancia entre esquemas A y B",
    x = "Esquema A (simétrico)", y = "Esquema B (intensidad)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave("output/figuras/fig_likert_concordancia.png", p_conc,
       width = 9, height = 4.5, dpi = 300)

# ── Guardar objetos ───────────────────────────────────────────────────────────

saveRDS(
  list(
    dat = dat,
    resumen_modelos = resumen_modelos,
    mejor_ajuste = mejor_ajuste,
    tab_distrib = tab_distrib,
    concordancia = conc_plot2,
    fit_specs = fit_specs,
    controles_base = controles_base
  ),
  "data/likert_collapse.rds"
)

cat("\n✓ Figuras guardadas:\n")
cat("  output/figuras/fig_likert_density.png\n")
cat("  output/figuras/fig_likert_distrib.png\n")
cat("  output/figuras/fig_likert_medias.png\n")
cat("  output/figuras/fig_likert_concordancia.png\n")
cat("✓ Tablas: output/tablas/tabla_likert_{distribucion,modelos}.html\n")
cat("✓ Objetos: data/likert_collapse.rds\n")
cat("\n✓ 07_likert_collapse.R ejecutado correctamente.\n")
