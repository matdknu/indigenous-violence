# =============================================================================
# 02_descriptivos.R — Tablas y figuras descriptivas para el paper
#
# Propósito: producir Tablas 1–2 y Figuras 1–2 a partir del subset analítico.
# Input:     data/subset_data.rds
# Output:    output/tablas/tabla_socdem.{html,docx}
#            output/tablas/tabla_descriptivos.html
#            output/tablas/tabla_consistencia_interna.html
#            output/tablas/tabla_operacionalizacion.html
#            output/figuras/fig_timeline.png
#            output/figuras/fig_trayectorias.png
#            output/figuras/fig_trayectorias_justifica.png
#            output/figuras/fig_cohorte_transicion_justifica.png
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, gt, gtsummary, ggplot2, viridis, patchwork, psych, cardx)
library(cardx)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)
if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

subset_data <- readRDS("data/subset_data.rds")
source(if (file.exists("R/plot_helpers.R")) "R/plot_helpers.R" else "causality/R/plot_helpers.R")

PERIODO_LABELS <- c(
  "pre"       = "Ola 2\n(2018)",
  "estallido" = "Ola 3\n(2021)",
  "decreto"   = "Ola 4\n(2023)"
)

# ── Figura 0 — Timeline histórico ─────────────────────────────────────────────

eventos <- tibble::tribble(
  ~fecha,  ~evento,                           ~tipo,
  2018,    "Ola 2\n(baseline)",               "ola",
  2019.75, "Estallido social\n(oct 2019)",    "shock",
  2020.5,  "Pandemia\n(2020)",                "shock",
  2020.92, "Ola 3\n(dic 2020 –\nmay 2021)",   "ola",
  2021.79, "Estado de excepción\n(oct 2021)", "decreto",
  2022.67, "Plebiscito Rechazo\n(sep 2022)", "shock",
  2023,    "Ola 4\n(2023)",                   "ola"
)

p_timeline <- ggplot(eventos, aes(x = fecha, y = 0)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.5) +
  geom_point(aes(color = tipo, shape = tipo, size = tipo)) +
  geom_text(aes(label = evento, color = tipo), vjust = -1.2,
            size = 3, lineheight = 0.85, fontface = "bold") +
  scale_color_manual(values = c(
    "ola" = "#2166AC", "shock" = "#D73027", "decreto" = "#B22222"
  ), guide = "none") +
  scale_shape_manual(values = c("ola" = 16, "shock" = 17, "decreto" = 18), guide = "none") +
  scale_size_manual(values = c("ola" = 4, "shock" = 3.5, "decreto" = 5), guide = "none") +
  scale_x_continuous(
    breaks = c(2018, 2019.75, 2021.79, 2022.67, 2023),
    labels = c("2018", "Oct\n2019", "Oct\n2021", "Sep\n2022", "2023")
  ) +
  ylim(-0.5, 0.5) +
  labs(
    title    = "Timeline del diseño cuasi-experimental",
    subtitle = "Shocks históricos y olas de medición ELRI",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_blank(),
    panel.grid  = element_blank(),
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(size = 9)
  )

ggsave("output/figuras/fig_timeline.png", p_timeline, width = 10, height = 3.5, dpi = 300)
cat("✓ Timeline guardado: output/figuras/fig_timeline.png\n")

# ── Descriptivos por grupo y período (consola) ────────────────────────────────

desc_grupo <- subset_data |>
  group_by(indigeneous, periodo) |>
  summarise(
    n                = n(),
    vio_control_m   = round(mean(idx_vio_control, na.rm = TRUE), 2),
    vio_resguardo_m    = round(mean(idx_vio_resguardo,  na.rm = TRUE), 2),
    perc_desig_m     = round(mean(perc_desigualdad,  na.rm = TRUE), 2),
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
    id_chile, id_causa
  ) |>
  tbl_summary(
    by = indigeneous,
    label = list(
      id_chile ~ "Identificación con Chile",
      id_causa ~ "Identificación con la causa indígena"
    ),
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
  "perc_desigualdad", "apoyo_movil",
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
        "Perc. desigualdad", "Apoyo movilizaciones",
        "Identificación con Chile",
        "Identificación con la causa indígena"
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
        periodo == "pre"       ~ "Ola 2 (2018)",
        periodo == "estallido" ~ "Ola 3 (2021)",
        periodo == "decreto"   ~ "Ola 4 (2023)",
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
    periodo_num = match(periodo, c("pre", "estallido", "decreto")),
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
        "Indígena / lejos",     "Indígena / zona excepción"
      )
    )
  )

p_tray <- ggplot(tray_long,
                 aes(x = periodo_num, y = media,
                     color = grupo, linetype = grupo, group = grupo)) +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "#FFE0E0", alpha = 0.35) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = grupo),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9, show.legend = TRUE) +
  geom_point(size = 2.8, show.legend = FALSE) +
  facet_wrap(~ indice, scales = "fixed", ncol = 2) +
  scale_x_continuous(breaks = 1:3, labels = unname(PERIODO_LABELS)) +
  scale_y_likert_shared() +
  labs(
    title    = "Trayectorias longitudinales por grupo identitario y zona",
    subtitle = "ELRI — Ola 2 (2018) · Ola 3 resabio estallido (2021) · Ola 4 decreto + Apruebo (2023)",
    x = NULL, y = "Media (escala 1–5)",
    caption  = "Línea sólida = zona de excepción · Línea punteada = lejos del conflicto"
  )

p_tray <- add_scale_grupo_trajectory(p_tray) +
  theme_trajectory()

ggsave("output/figuras/fig_trayectorias.png", p_tray,
       width = 11, height = 5, dpi = 300)
cat("✓ Figura 1 guardada: output/figuras/fig_trayectorias.png\n")

# ── Figura 1b — Evolución longitudinal: % que justifica (3–5) ─────────────────
# Solo descriptivo / paper. No escribe .rds ni altera subset_data; los modelos
# (03–09) siguen usando idx_vio_* continuo (escala 1–5).

bin_justifica <- function(x) {
  case_when(is.na(x) ~ NA_integer_, x >= 3 ~ 1L, TRUE ~ 0L)
}

tray_just <- subset_data |>
  filter(!is.na(indigeneous)) |>
  mutate(
    just_ctrl = bin_justifica(idx_vio_control),
    just_resg = bin_justifica(idx_vio_resguardo)
  ) |>
  group_by(indigeneous, cerca_conflicto, periodo) |>
  summarise(
    n_ctrl = sum(!is.na(just_ctrl)),
    n_resg = sum(!is.na(just_resg)),
    pct_just_ctrl = mean(just_ctrl, na.rm = TRUE),
    pct_just_resg = mean(just_resg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(pct_just_ctrl, pct_just_resg),
    names_to = "indice",
    values_to = "pct"
  ) |>
  mutate(
    n = if_else(indice == "pct_just_ctrl", n_ctrl, n_resg),
    se = ifelse(n > 0, sqrt(pct * (1 - pct) / n), NA_real_),
    lo = pmax(pct - 1.96 * se, 0),
    hi = pmin(pct + 1.96 * se, 1),
    periodo_num = match(periodo, c("pre", "estallido", "decreto")),
    indice = factor(
      indice,
      levels = c("pct_just_ctrl", "pct_just_resg"),
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
        "Indígena / lejos",     "Indígena / zona excepción"
      )
    )
  ) |>
  select(-n_ctrl, -n_resg, -n, -se)

p_tray_just <- ggplot(tray_just,
                      aes(x = periodo_num, y = pct,
                          color = grupo, linetype = grupo, group = grupo)) +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "#FFE0E0", alpha = 0.35) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = grupo),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9, show.legend = TRUE) +
  geom_point(size = 2.8, show.legend = FALSE) +
  facet_wrap(~ indice, ncol = 2) +
  scale_x_continuous(
    breaks = 1:3,
    labels = unname(PERIODO_LABELS),
    name = "Tiempo (olas de medición)"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0.02, 0.05)),
    name = "% que justifica (3–5)"
  ) +
  labs(
    title    = "Evolución longitudinal: proporción que justifica la violencia",
    subtitle = "Solo respuestas 3–5 en escala 1–5 · ELRI olas 2–4",
    caption  = "Línea sólida = zona de excepción · Línea punteada = lejos del conflicto · IC 95 %"
  )

p_tray_just <- add_scale_grupo_trajectory(p_tray_just) +
  theme_trajectory()

ggsave("output/figuras/fig_trayectorias_justifica.png", p_tray_just,
       width = 11, height = 5, dpi = 300)
cat("✓ Figura longitudinal justifica guardada: output/figuras/fig_trayectorias_justifica.png\n")

cat("\n--- % justifica (3–5) a lo largo del tiempo ---\n")
tray_just |>
  mutate(
    tiempo = factor(periodo_num, levels = 1:3, labels = unname(PERIODO_LABELS))
  ) |>
  select(grupo, tiempo, indice, pct) |>
  mutate(pct = round(100 * pct, 1)) |>
  arrange(indice, grupo, tiempo) |>
  print(n = Inf)

# ── Figura 1c — Cohortes ola 2: transición hacia la otra dimensión de violencia ─
# Solo descriptivo. Cohorte fijada en ola 2 (1–2 = no justifica); se sigue en el
# tiempo la otra VD: % que justifica (3–5) y % que intensifica (Δ≥1 o cruza a 3–5).

UMBRAL_JUST <- 3L

baseline_ola2 <- subset_data |>
  filter(ola == 2, !is.na(indigeneous)) |>
  transmute(
    folio,
    ctrl_b = idx_vio_control,
    resg_b = idx_vio_resguardo,
    no_just_ctrl_b = !is.na(idx_vio_control) & idx_vio_control < UMBRAL_JUST,
    no_just_resg_b = !is.na(idx_vio_resguardo) & idx_vio_resguardo < UMBRAL_JUST
  )

metricas_cohorte <- function(datos, var_out, var_base, etiqueta_dir) {
  datos |>
    filter(!is.na(.data[[var_out]]), !is.na(.data[[var_base]])) |>
    mutate(
      justifica_out = .data[[var_out]] >= UMBRAL_JUST,
      # Intensificación solo post-baseline (olas 3–4), no en ola 2
      intensifica = periodo != "pre" & (
        (.data[[var_out]] >= UMBRAL_JUST & .data[[var_base]] < UMBRAL_JUST) |
          (.data[[var_out]] - .data[[var_base]] >= 1)
      )
    ) |>
    group_by(indigeneous, cerca_conflicto, periodo) |>
    summarise(
      n = n(),
      pct_justifica = mean(justifica_out, na.rm = TRUE),
      pct_intensifica = mean(intensifica, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(pct_justifica, pct_intensifica),
      names_to = "metrica",
      values_to = "pct"
    ) |>
    mutate(
      direccion = etiqueta_dir,
      metrica = recode(
        metrica,
        pct_justifica = "Justifica (3–5)",
        pct_intensifica = "Intensifica (Δ≥1 o pasa a 3–5)"
      ),
      metrica = factor(
        metrica,
        levels = c("Justifica (3–5)", "Intensifica (Δ≥1 o pasa a 3–5)")
      )
    )
}

long_cohorte <- subset_data |>
  filter(!is.na(indigeneous), ola %in% c(2, 3, 4)) |>
  inner_join(baseline_ola2, by = "folio")

cohorte_tray <- bind_rows(
  long_cohorte |>
    filter(no_just_ctrl_b) |>
    metricas_cohorte(
      var_out = "idx_vio_resguardo",
      var_base = "resg_b",
      etiqueta_dir = "ctrl_a_cambio"
    ),
  long_cohorte |>
    filter(no_just_resg_b) |>
    metricas_cohorte(
      var_out = "idx_vio_control",
      var_base = "ctrl_b",
      etiqueta_dir = "cambio_a_ctrl"
    )
) |>
  mutate(
    periodo_num = match(periodo, c("pre", "estallido", "decreto")),
    trayectoria = factor(
      direccion,
      levels = c("ctrl_a_cambio", "cambio_a_ctrl"),
      labels = c(
        "Si no justificas control (ola 2) → ¿justifican cambio social?",
        "Si no justifican cambio (ola 2) → ¿justifican control social?"
      )
    ),
    iteracion = factor(
      periodo_num,
      levels = 1:3,
      labels = c(
        "Iteración 1 · Ola 2 (2018)",
        "Iteración 2 · Ola 3 (2021)",
        "Iteración 3 · Ola 4 (2023)"
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
        "Indígena / lejos",     "Indígena / zona excepción"
      )
    ),
    # Intensificación sin sentido en iteración 1 (es el baseline de la cohorte)
    pct = if_else(
      metrica == "Intensifica (Δ≥1 o pasa a 3–5)" & periodo == "pre",
      NA_real_,
      pct
    )
  )

LABELS_COHORTE <- c(
  "ctrl_a_cambio" = "Si no justificas control (ola 2) → ¿justifican cambio social?",
  "cambio_a_ctrl" = "Si no justifican cambio (ola 2) → ¿justifican control social?"
)

FILL_METRICA <- c(
  "Justifica (3–5)" = "#636363",
  "Intensifica (Δ≥1 o pasa a 3–5)" = "#D73027"
)

# McNemar pareado: ¿cambió la proporción vs la ola anterior? (p < .05 → *)
mcnemar_vs_ola_anterior <- function(datos, var_out, var_base, tray_id) {
  star_p <- function(p) if (!is.na(p) && p < 0.05) "*" else ""

  ind <- datos |>
    mutate(
      y_just = !is.na(.data[[var_out]]) & .data[[var_out]] >= UMBRAL_JUST,
      y_intens = periodo != "pre" & (
        (.data[[var_out]] >= UMBRAL_JUST & .data[[var_base]] < UMBRAL_JUST) |
          (.data[[var_out]] - .data[[var_base]] >= 1)
      )
    )

  comparar <- function(ola_t, ola_prev, metrica, yvar) {
    purrr::pmap_dfr(
      expand.grid(
        indigeneous = levels(ind$indigeneous),
        cerca_conflicto = levels(ind$cerca_conflicto),
        stringsAsFactors = FALSE
      ),
      function(indigeneous, cerca_conflicto) {
        par <- ind |>
          filter(
            .data$indigeneous == .env$indigeneous,
            .data$cerca_conflicto == .env$cerca_conflicto,
            .data$ola %in% c(ola_prev, ola_t)
          ) |>
          select(folio, ola, y = !!sym(yvar)) |>
          tidyr::pivot_wider(names_from = ola, values_from = y) |>
          filter(!is.na(.data[[as.character(ola_prev)]]), !is.na(.data[[as.character(ola_t)]]))

        p_val <- NA_real_
        if (nrow(par) >= 10) {
          before <- par[[as.character(ola_prev)]]
          after  <- par[[as.character(ola_t)]]
          tab <- table(
            factor(after, levels = c(FALSE, TRUE)),
            factor(before, levels = c(FALSE, TRUE))
          )
          if (all(dim(tab) == c(2L, 2L)) && sum(tab[1, 2], tab[2, 1]) > 0) {
            p_val <- tryCatch(
              stats::mcnemar.test(tab, correct = TRUE)$p.value,
              error = function(e) NA_real_
            )
          }
        }

        tibble(
          direccion = tray_id,
          indigeneous = indigeneous,
          cerca_conflicto = cerca_conflicto,
          ola = ola_t,
          metrica = metrica,
          p_vs_prev = p_val,
          sig_star = star_p(p_val)
        )
      }
    )
  }

  bind_rows(
    comparar(3, 2, "Justifica (3–5)", "y_just"),
    comparar(4, 3, "Justifica (3–5)", "y_just"),
    comparar(3, 2, "Intensifica (Δ≥1 o pasa a 3–5)", "y_intens"),
    comparar(4, 3, "Intensifica (Δ≥1 o pasa a 3–5)", "y_intens")
  )
}

sig_cohorte <- bind_rows(
  mcnemar_vs_ola_anterior(
    long_cohorte |> filter(no_just_ctrl_b),
    "idx_vio_resguardo", "resg_b", "ctrl_a_cambio"
  ),
  mcnemar_vs_ola_anterior(
    long_cohorte |> filter(no_just_resg_b),
    "idx_vio_control", "ctrl_b", "cambio_a_ctrl"
  )
) |>
  mutate(
    # Alinear con cohorte_tray: 1 = ola 2, 2 = ola 3, 3 = ola 4
    periodo_num = match(ola, c(2L, 3L, 4L)),
    grupo = factor(
      paste0(indigeneous, " — ", cerca_conflicto),
      levels = c(
        "no_indi — lejos", "no_indi — cerca",
        "indi — lejos",    "indi — cerca"
      ),
      labels = c(
        "No indígena / lejos", "No indígena / zona excepción",
        "Indígena / lejos",     "Indígena / zona excepción"
      )
    )
  )

cohorte_plot <- cohorte_tray |>
  left_join(
    sig_cohorte |>
      select(direccion, grupo, periodo_num, metrica, sig_star, p_vs_prev),
    by = c("direccion", "grupo", "periodo_num", "metrica")
  ) |>
  filter(!is.na(pct)) |>
  mutate(
    label_bar = ifelse(
      pct >= 0.08,
      paste0(
        scales::percent(pct, accuracy = 1),
        ifelse(is.na(sig_star) | sig_star == "", "", sig_star)
      ),
      ""
    )
  )

p_cohorte <- ggplot(
  cohorte_plot,
  aes(x = grupo, y = pct, fill = metrica)
) +
  geom_col(
    position = position_dodge(width = 0.82),
    width = 0.72,
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = label_bar, group = metrica),
    position = position_dodge(width = 0.82),
    vjust = -0.35,
    size = 2.6,
    color = "grey20"
  ) +
  facet_grid(
    trayectoria ~ iteracion,
    scales = "free_y",
    labeller = labeller(
      trayectoria = LABELS_COHORTE,
      iteracion = label_value
    )
  ) +
  scale_fill_manual(values = FILL_METRICA, name = NULL) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12)),
    name = "% de la cohorte (ola 2)"
  ) +
  labs(
    title    = "En la medida que no justificas una violencia, ¿aparece la otra?",
    subtitle = paste(
      "Cohorte fijada en ola 2 (respuestas 1–2); cada iteración muestra la otra dimensión.",
      "Solo descriptivo — no altera modelos DiD."
    ),
    x = NULL,
    caption  = paste(
      "Barra gris = justifica (3–5) · Barra roja = intensifica (Δ≥1 o cruza a 3–5 desde ola 2).",
      "* = cambio significativo vs ola anterior (McNemar pareado, p < .05)."
    )
  ) +
  theme_trajectory() +
  theme(
    strip.text.y = element_text(size = 9, face = "bold"),
    strip.text.x = element_text(size = 9),
    axis.text.x = element_text(size = 8, angle = 35, hjust = 1),
    legend.position = "bottom",
    panel.spacing = grid::unit(0.9, "lines")
  )

ggsave("output/figuras/fig_cohorte_transicion_justifica.png", p_cohorte,
       width = 13, height = 8.5, dpi = 300)
cat("✓ Figura cohortes/transición guardada: output/figuras/fig_cohorte_transicion_justifica.png\n")

write.csv(
  cohorte_plot |>
    mutate(
      pct_pct = round(100 * pct, 1),
      trayectoria = as.character(trayectoria),
      grupo = as.character(grupo),
      iteracion = as.character(iteracion)
    ) |>
    select(trayectoria, iteracion, grupo, n, metrica, pct_pct, sig_star, p_vs_prev),
  "output/tablas/tabla_cohorte_transicion.csv",
  row.names = FALSE
)

gt_cohorte <- cohorte_tray |>
  filter(!is.na(pct)) |>
  mutate(pct_lab = scales::percent(pct, accuracy = 1)) |>
  select(trayectoria, iteracion, grupo, metrica, n, pct_lab) |>
  gt(groupname_col = "trayectoria") |>
  cols_label(
    iteracion = "Iteración",
    grupo = "Grupo",
    metrica = "Indicador",
    n = "N cohorte",
    pct_lab = "%"
  ) |>
  tab_header(
    title = "Transición descriptiva entre dimensiones de violencia justificada",
    subtitle = "Cohorte anclada en ola 2 (1–2 en dimensión origen); lectura por iteración"
  ) |>
  tab_footnote(
    footnote = "Intensifica: Δ≥1 o alcanza 3–5 desde baseline ola 2 (no definido en ola 2)."
  ) |>
  opt_stylize(style = 1)

gt_cohorte |> gtsave("output/tablas/tabla_cohorte_transicion.html")
cat("✓ Tabla cohortes: output/tablas/tabla_cohorte_transicion.{csv,html}\n")

cat("\n--- Cohortes ola 2: tamaño y transición ola 2 → ola 4 ---\n")
trans_24 <- subset_data |>
  filter(ola == 2, !is.na(indigeneous)) |>
  select(folio, indigeneous, cerca_conflicto, ctrl_b = idx_vio_control, resg_b = idx_vio_resguardo) |>
  left_join(
    subset_data |>
      filter(ola == 4) |>
      select(folio, ctrl_4 = idx_vio_control, resg_4 = idx_vio_resguardo),
    by = "folio"
  ) |>
  mutate(
    no_just_ctrl_b = !is.na(ctrl_b) & ctrl_b < UMBRAL_JUST,
    no_just_resg_b = !is.na(resg_b) & resg_b < UMBRAL_JUST
  )

resumen_cohorte <- bind_rows(
  trans_24 |>
    filter(no_just_ctrl_b) |>
    group_by(indigeneous, cerca_conflicto) |>
    summarise(
      direccion = "No justifica control → intensifica cambio (ola 4)",
      n_cohorte = n(),
      pct = mean(
        (!is.na(resg_4) & resg_4 >= UMBRAL_JUST & resg_b < UMBRAL_JUST) |
          (!is.na(resg_4) & !is.na(resg_b) & resg_4 - resg_b >= 1),
        na.rm = TRUE
      ),
      .groups = "drop"
    ),
  trans_24 |>
    filter(no_just_resg_b) |>
    group_by(indigeneous, cerca_conflicto) |>
    summarise(
      direccion = "No justifica cambio → intensifica control (ola 4)",
      n_cohorte = n(),
      pct = mean(
        (!is.na(ctrl_4) & ctrl_4 >= UMBRAL_JUST & ctrl_b < UMBRAL_JUST) |
          (!is.na(ctrl_4) & !is.na(ctrl_b) & ctrl_4 - ctrl_b >= 1),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
) |>
  mutate(pct = round(100 * pct, 1))

print(resumen_cohorte)

cat("\n--- Cuadrícula cohorte (%, redondeado) ---\n")
cohorte_tray |>
  mutate(pct = round(100 * pct, 1)) |>
  select(trayectoria, iteracion, grupo, metrica, pct, n) |>
  arrange(trayectoria, iteracion, grupo, metrica) |>
  print(n = Inf)

# ── Figura 2 (apéndice) — Nota de medición (VD = ítem único) ───────────────────
# Ambas VD principales son ÍTEMS ÚNICOS (validez de contenido).
# No aplica alfa de Cronbach ni correlación inter-ítem.

cat("\n--- Nota de medición VD ---\n")
cat("  Control social estatal (idx_vio_control) = d3_1 (Carabineros) — ítem único\n")
cat("  Cambio social (idx_vio_resguardo)        = d4_3 (cortes de camino) — ítem único\n")
cat("  Excluidos de VD (sensibilidad apéndice):\n")
cat("    vio_priv_agric (d3_2) = vigilantismo privado\n")
cat("    vio_ocup_tierras (d4_2) = ocupación territorial\n")
cat("  → No alfa de Cronbach para estas VD.\n")

tabla_medicion <- tibble::tribble(
  ~VD, ~Item, ~Contenido, ~Tipo, ~Excluido_del_indice,
  "Control social estatal", "d3_1", "Fuerza de Carabineros (coerción estatal)", "Ítem único", "d3_2 (vigilantismo privado)",
  "Cambio social", "d4_3", "Cortes de camino (protesta disruptiva)", "Ítem único", "d4_2 (ocupación territorial)"
)

gt_medicion <- tabla_medicion |>
  gt() |>
  tab_header(
    title = "Nota de medición — Variables dependientes",
    subtitle = paste0(
      "Ítems únicos elegidos por validez de contenido. ",
      "No se reporta alfa de Cronbach (un ítem no tiene consistencia interna)."
    )
  ) |>
  cols_label(
    VD = "Variable dependiente",
    Item = "Ítem ELRI",
    Contenido = "Contenido",
    Tipo = "Tipo",
    Excluido_del_indice = "Excluido (sensibilidad apéndice)"
  ) |>
  tab_footnote(
    footnote = paste0(
      "d3_2 (vio_priv_agric) y d4_2 (vio_ocup_tierras) se modelan solo en ",
      "apéndice/sensibilidad (modelos por ítem), nunca dentro de las VD principales."
    )
  ) |>
  opt_stylize(style = 1)

gt_medicion |> gtsave("output/tablas/tabla_consistencia_interna.html")
gt_medicion |> gtsave("output/tablas/tabla_nota_medicion_vd.html")
cat("✓ Nota de medición guardada: output/tablas/tabla_nota_medicion_vd.html\n")
cat("✓ (también como tabla_consistencia_interna.html — reemplazo sin α)\n")

# ── Tabla operacionalización de variables (paper) ───────────────────────────────

alpha_just <- psych::alpha(
  subset_data[, c("just_proc_indi", "just_proc_noindi")],
  check.keys = TRUE
)

tabla_variables <- tibble::tribble(
  ~Variable,              ~Items,                                        ~Escala,       ~Fuente,
  "Control social estatal", "d3_1 (fuerza de Carabineros)", "1–5 (ítem único)", "ELRI D",
  "Cambio social",          "d4_3 (cortes de camino)",                    "1–5 (ítem único)", "ELRI D",
  "Justicia proc.",       "d5_1 + d5_2 (trato Carabineros)",            "1–5 (α=.83)", "ELRI D",
  "Id. causa indígena",  "d6_1",                                        "1–5",         "ELRI D",
  "Id. con Chile",        "a6",                                          "1–5",         "ELRI A",
  "Perc. desigualdad",    "c22 (invertida)",                             "1–5",         "ELRI C",
  "Apoyo movilizaciones", "c25",                                         "1–5",         "ELRI C",
  "Sensib.: vigilantismo", "d3_2 (agricultores armados)", "1–5 (apéndice)", "ELRI D",
  "Sensib.: ocupación",    "d4_2 (tomas de terrenos)", "1–5 (apéndice)", "ELRI D"
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
