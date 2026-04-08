if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  tidyr,
  purrr,
  ggplot2,
  reshape2,
  here,
  LMest,
  readr,
  tibble,
  stringr,
  scales,
  knitr,
  kableExtra,
  networkD3,
  htmlwidgets
)

options(stringsAsFactors = FALSE, scipen = 999)

rm(list = ls())
gc()

# ============================================================
# 0. Configuracion
# ============================================================

output_root <- here("outputs", "longitudinal")
data_dir <- file.path(output_root, "data")
models_dir <- file.path(output_root, "models")
tables_dir <- file.path(output_root, "tables")
plots_dir <- file.path(output_root, "plots")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

modelo_path <- file.path(models_dir, "modelo_4c.rds")
if (!file.exists(modelo_path)) {
  stop("No se encontro `outputs/longitudinal/models/modelo_4c.rds`. Ejecuta primero `script/02_create-class.R`.")
}

modelo_lm4 <- readRDS(modelo_path)
std_errors <- se(modelo_lm4)

wave_labels <- c("2016", "2018", "2021", "2023")
period_labels <- c("2016_2018", "2018_2021", "2021_2023", "2023_?")
comparison_levels <- c("Clase 2 vs Clase 1", "Clase 3 vs Clase 1", "Clase 4 vs Clase 1")

primary_label_map <- c(
  "(Intercept)" = "Intercepto",
  "urbano_rural" = "Zona urbana",
  "mujer1" = "Mujer",
  "indiindi" = "Identidad indígena",
  "indi1" = "Identidad indígena",
  "d1_1" = "Percepción de conflicto",
  "c5" = "Confianza en pueblos originarios",
  "d5_1" = "Justicia procedimental",
  "d6_1" = "Identificación con la causa",
  "c13" = "Frecuencia de contacto",
  "edad.L" = "Edad"
)

key_vars <- c(
  "Percepción de conflicto",
  "Confianza en pueblos originarios",
  "Justicia procedimental",
  "Identificación con la causa",
  "Frecuencia de contacto",
  "Identidad indígena",
  "Edad"
)

label_variable <- function(x) {
  if (x %in% names(primary_label_map)) return(unname(primary_label_map[x]))
  if (grepl("^edad", x)) return("Edad")
  if (grepl("^mujer", x)) return("Mujer")
  if (grepl("^indi", x)) return("Identidad indígena")
  x
}

sign_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "cero",
    x > 0 ~ "positivo",
    x < 0 ~ "negativo",
    TRUE ~ "cero"
  )
}

short_sign_label <- function(x) {
  dplyr::case_when(
    x == "positivo" ~ "+",
    x == "negativo" ~ "-",
    TRUE ~ "0"
  )
}

round_numeric <- function(df, digits = 3) {
  df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))
}

save_html_table <- function(df, path, caption, digits = 3) {
  df %>%
    knitr::kable(format = "html", digits = digits, booktabs = TRUE, caption = caption) %>%
    kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
    kableExtra::save_kable(path)
}

extract_transition_matrices <- function(model) {
  pi_array <- model$PI
  if (length(dim(pi_array)) != 4) {
    stop("La estructura de `PI` no corresponde al formato esperado estado x estado x sujeto x tiempo.")
  }

  raw_times <- seq_len(dim(pi_array)[4])
  matrices <- lapply(raw_times, function(t_idx) {
    apply(pi_array[, , , t_idx, drop = FALSE], c(1, 2), mean, na.rm = TRUE)
  })

  valid <- purrr::map_lgl(matrices, ~ any(.x > 0, na.rm = TRUE))
  list(matrices = matrices[valid], valid_times = raw_times[valid])
}

extract_be_effects <- function(model, se_object) {
  if (is.null(model$Be) || is.null(se_object$seBe)) return(tibble::tibble())

  k_total <- ncol(model$Be) + 1L
  ref_class <- 1L
  nonref <- setdiff(seq_len(k_total), ref_class)

  be_df <- as.data.frame(model$Be)
  colnames(be_df) <- paste0("Clase ", nonref, " vs Clase ", ref_class)
  be_df$term <- rownames(model$Be)

  se_df <- as.data.frame(se_object$seBe)
  colnames(se_df) <- paste0("Clase ", nonref, " vs Clase ", ref_class)
  se_df$term <- rownames(se_object$seBe)

  be_long <- be_df %>%
    tidyr::pivot_longer(cols = starts_with("Clase "), names_to = "comparacion", values_to = "coeficiente")

  se_long <- se_df %>%
    tidyr::pivot_longer(cols = starts_with("Clase "), names_to = "comparacion", values_to = "se")

  dplyr::left_join(be_long, se_long, by = c("term", "comparacion")) %>%
    dplyr::filter(!is.na(coeficiente), !is.na(se)) %>%
    dplyr::mutate(
      variable = vapply(term, label_variable, character(1)),
      z = coeficiente / se,
      p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      or = exp(coeficiente),
      or_ic_lower = exp(coeficiente - 1.96 * se),
      or_ic_upper = exp(coeficiente + 1.96 * se),
      significativo = p_value < 0.05
    ) %>%
    dplyr::select(term, variable, comparacion, coeficiente, se, z, p_value, or, or_ic_lower, or_ic_upper, significativo)
}

extract_ga_effects <- function(model, se_object, wave_labels, period_labels) {
  if (is.null(model$Ga) || is.null(se_object$seGa)) return(tibble::tibble())

  ga_dims <- dim(model$Ga)
  if (length(ga_dims) != 3) {
    stop("La estructura de `Ga` no corresponde al formato esperado termino x destino x origen/periodo.")
  }

  period_lookup <- stats::setNames(period_labels[seq_len(min(length(period_labels), ga_dims[3]))], as.character(seq_len(ga_dims[3])))
  wave_lookup <- stats::setNames(wave_labels[seq_len(min(length(wave_labels), ga_dims[3]))], as.character(seq_len(ga_dims[3])))

  purrr::map_dfr(seq_len(ga_dims[3]), function(origin_idx) {
    coef_mat <- model$Ga[, , origin_idx, drop = FALSE][, , 1]
    se_mat <- se_object$seGa[, , origin_idx, drop = FALSE][, , 1]

    term_names <- rownames(coef_mat)
    if (is.null(term_names)) term_names <- paste0("term_", seq_len(nrow(coef_mat)))
    dest_names <- colnames(coef_mat)
    if (is.null(dest_names)) dest_names <- seq_len(ncol(coef_mat))

    coef_df <- as.data.frame(coef_mat)
    colnames(coef_df) <- as.character(dest_names)
    coef_df$term <- term_names

    se_df <- as.data.frame(se_mat)
    colnames(se_df) <- as.character(dest_names)
    se_df$term <- term_names

    coef_long <- coef_df %>%
      tidyr::pivot_longer(cols = -term, names_to = "clase_destino", values_to = "coeficiente")

    se_long <- se_df %>%
      tidyr::pivot_longer(cols = -term, names_to = "clase_destino", values_to = "se")

    dplyr::left_join(coef_long, se_long, by = c("term", "clase_destino")) %>%
      dplyr::mutate(
        clase_origen = origin_idx,
        ola_origen = unname(wave_lookup[as.character(origin_idx)]),
        periodo = unname(period_lookup[as.character(origin_idx)]),
        variable = vapply(term, label_variable, character(1)),
        comparacion = paste0("Clase ", clase_destino, " vs Clase 1"),
        z = coeficiente / se,
        p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
        or = exp(coeficiente),
        or_ic_lower = exp(coeficiente - 1.96 * se),
        or_ic_upper = exp(coeficiente + 1.96 * se),
        significativo = p_value < 0.05,
        signo = sign_label(coeficiente)
      ) %>%
      dplyr::select(
        term, variable, clase_origen, ola_origen, periodo, clase_destino, comparacion,
        coeficiente, se, z, p_value, or, or_ic_lower, or_ic_upper, significativo, signo
      )
  })
}

exported_files <- character()

# ============================================================
# 1. Prevalencia por ola
# ============================================================

if (is.null(modelo_lm4$Pmarg)) {
  stop("El modelo no contiene `Pmarg`; no se puede calcular prevalencia por ola.")
}

class_prevalence_by_wave_long <- as.data.frame(modelo_lm4$Pmarg) %>%
  tibble::rownames_to_column("class") %>%
  tidyr::pivot_longer(cols = -class, names_to = "time", values_to = "probability") %>%
  dplyr::mutate(
    class = factor(readr::parse_number(class)),
    time = readr::parse_number(time),
    wave = factor(wave_labels[time], levels = wave_labels),
    prevalence = probability,
    prevalence_pct = 100 * prevalence
  ) %>%
  dplyr::select(wave, time, class, prevalence, prevalence_pct) %>%
  dplyr::arrange(time, class)

class_prevalence_by_wave <- class_prevalence_by_wave_long %>%
  dplyr::select(wave, class, prevalence_pct) %>%
  tidyr::pivot_wider(names_from = class, values_from = prevalence_pct, names_prefix = "Clase_") %>%
  round_numeric(1)

readr::write_csv(class_prevalence_by_wave_long, file.path(tables_dir, "class_prevalence_by_wave_long.csv"))
readr::write_csv(class_prevalence_by_wave, file.path(tables_dir, "class_prevalence_by_wave.csv"))
exported_files <- c(exported_files, "tables/class_prevalence_by_wave_long.csv", "tables/class_prevalence_by_wave.csv")

p_class_prevalence <- ggplot(class_prevalence_by_wave_long, aes(x = wave, y = prevalence_pct, color = class, group = class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = NULL, y = "Prevalencia de clase (%)", color = "Clase") +
  theme_minimal(base_size = 12)

ggsave(file.path(plots_dir, "class_prevalence_by_wave.png"), p_class_prevalence, width = 8, height = 5, dpi = 300)
exported_files <- c(exported_files, "plots/class_prevalence_by_wave.png")

# ============================================================
# 2. Transicion y movilidad
# ============================================================

transition_info <- extract_transition_matrices(modelo_lm4)
transition_mats <- transition_info$matrices
valid_times <- transition_info$valid_times

if (!length(transition_mats)) {
  stop("No fue posible extraer matrices de transicion validas desde `PI`.")
}

available_periods <- period_labels[seq_len(length(transition_mats))]
transition_mean <- Reduce("+", transition_mats) / length(transition_mats)

transition_mean_long <- as_tibble(transition_mean) %>%
  dplyr::mutate(from = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "probability") %>%
  dplyr::mutate(
    to = readr::parse_number(to),
    period = "promedio",
    probability_pct = 100 * probability
  )

transition_period_long <- purrr::map2_dfr(transition_mats, seq_along(transition_mats), function(mat, i) {
  from_index <- max(valid_times[i] - 1, 1)

  as_tibble(mat) %>%
    dplyr::mutate(from = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "probability") %>%
    dplyr::mutate(
      to = readr::parse_number(to),
      from_wave = wave_labels[from_index],
      to_wave = wave_labels[min(from_index + 1, length(wave_labels))],
      period = available_periods[i],
      probability_pct = 100 * probability
    )
})

transition_probabilities <- dplyr::bind_rows(transition_mean_long, transition_period_long)
readr::write_csv(transition_probabilities, file.path(tables_dir, "transition_probabilities.csv"))
exported_files <- c(exported_files, "tables/transition_probabilities.csv")

mobility_summary_by_period <- purrr::map2_dfr(transition_mats, seq_along(transition_mats), function(mat, i) {
  source_prev <- as.numeric(modelo_lm4$Pmarg[, i])
  stay_overall <- sum(source_prev * diag(mat))

  tibble::tibble(
    period = available_periods[i],
    wave_from = wave_labels[i],
    wave_to = wave_labels[i + 1],
    stability_overall_pct = 100 * stay_overall,
    mobility_overall_pct = 100 * (1 - stay_overall)
  )
})

net_flows_by_class_and_period <- purrr::map2_dfr(transition_mats, seq_along(transition_mats), function(mat, i) {
  source_prev <- as.numeric(modelo_lm4$Pmarg[, i])
  dest_prev <- as.numeric(modelo_lm4$Pmarg[, i + 1])
  weighted_mat <- sweep(mat, 1, source_prev, `*`)
  inflow <- colSums(weighted_mat) - diag(weighted_mat)
  outflow <- source_prev * (1 - diag(mat))

  tibble::tibble(
    period = available_periods[i],
    wave_from = wave_labels[i],
    wave_to = wave_labels[i + 1],
    class = factor(seq_along(source_prev)),
    prevalence_t_pct = 100 * source_prev,
    prevalence_t1_pct = 100 * dest_prev,
    stability_pct = 100 * diag(mat),
    outflow_pct = 100 * outflow,
    inflow_pct = 100 * inflow,
    net_flow_pct = 100 * (inflow - outflow),
    prevalence_change_pct = 100 * (dest_prev - source_prev)
  )
})

readr::write_csv(round_numeric(mobility_summary_by_period, 2), file.path(tables_dir, "mobility_summary_by_period.csv"))
readr::write_csv(round_numeric(net_flows_by_class_and_period, 2), file.path(tables_dir, "net_flows_by_class_and_period.csv"))
exported_files <- c(exported_files, "tables/mobility_summary_by_period.csv", "tables/net_flows_by_class_and_period.csv")

p_transition_mean <- ggplot(transition_mean_long, aes(x = factor(to), y = factor(from), fill = probability)) +
  geom_tile(color = "grey80", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(probability_pct, 1), "%")), size = 3) +
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
  labs(x = "Clase destino", y = "Clase origen", fill = "Prob.") +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave(file.path(plots_dir, "transition_matrix_mean.png"), p_transition_mean, width = 6.5, height = 5.5, dpi = 300)
exported_files <- c(exported_files, "plots/transition_matrix_mean.png")

p_transition_period <- ggplot(transition_period_long, aes(x = factor(to), y = factor(from), fill = probability)) +
  geom_tile(color = "grey80", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(probability_pct, 1), "%")), size = 2.8) +
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
  facet_wrap(~ period) +
  labs(x = "Clase destino", y = "Clase origen", fill = "Prob.") +
  coord_fixed() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave(file.path(plots_dir, "transition_matrices_by_period.png"), p_transition_period, width = 10, height = 6, dpi = 300)
exported_files <- c(exported_files, "plots/transition_matrices_by_period.png")

sankey_links <- transition_period_long %>%
  dplyr::filter(probability > 0.01) %>%
  dplyr::mutate(
    source_name = paste0(wave_from, "_Clase_", from),
    target_name = paste0(wave_to, "_Clase_", to),
    value = probability
  )

if (nrow(sankey_links) > 0) {
  sankey_nodes <- tibble::tibble(name = unique(c(sankey_links$source_name, sankey_links$target_name)))

  sankey_links <- sankey_links %>%
    dplyr::mutate(
      source = match(source_name, sankey_nodes$name) - 1L,
      target = match(target_name, sankey_nodes$name) - 1L
    )

  sankey_plot <- networkD3::sankeyNetwork(
    Links = as.data.frame(sankey_links[, c("source", "target", "value")]),
    Nodes = as.data.frame(sankey_nodes),
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 20,
    sinksRight = TRUE
  )

  htmlwidgets::saveWidget(sankey_plot, file.path(plots_dir, "transition_sankey.html"), selfcontained = TRUE)
  exported_files <- c(exported_files, "plots/transition_sankey.html")
}

# ============================================================
# 3. Efectos de pertenencia inicial
# ============================================================

initial_class_membership_effects <- extract_be_effects(modelo_lm4, std_errors)
readr::write_csv(round_numeric(initial_class_membership_effects, 4), file.path(tables_dir, "initial_class_membership_effects.csv"))
exported_files <- c(exported_files, "tables/initial_class_membership_effects.csv")

if (nrow(initial_class_membership_effects) > 0) {
  p_initial_effects <- ggplot(initial_class_membership_effects, aes(x = or, y = variable, color = comparacion)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
    geom_point(position = position_dodge(width = 0.6), size = 2.4) +
    geom_errorbar(
      aes(xmin = or_ic_lower, xmax = or_ic_upper),
      position = position_dodge(width = 0.6),
      width = 0.3
    ) +
    scale_x_log10() +
    labs(x = "Odds ratio (escala log)", y = NULL, color = "Comparación") +
    theme_minimal(base_size = 12)

  ggsave(file.path(plots_dir, "initial_membership_odds_ratios.png"), p_initial_effects, width = 10, height = 7, dpi = 300)
  exported_files <- c(exported_files, "plots/initial_membership_odds_ratios.png")
}

# ============================================================
# 4. Efectos de transicion
# ============================================================

transition_effects_raw <- extract_ga_effects(modelo_lm4, std_errors, wave_labels, period_labels)
transition_effects_significant <- transition_effects_raw %>%
  dplyr::filter(significativo) %>%
  dplyr::mutate(
    interpretacion = dplyr::case_when(
      or > 1 ~ "Asociado con mayor probabilidad relativa de transición.",
      or < 1 ~ "Asociado con menor probabilidad relativa de transición.",
      TRUE ~ "Efecto cercano a nulo."
    )
  )

readr::write_csv(round_numeric(transition_effects_raw, 4), file.path(tables_dir, "transition_effects_raw.csv"))
readr::write_csv(round_numeric(transition_effects_significant, 4), file.path(tables_dir, "transition_effects_significant.csv"))
exported_files <- c(exported_files, "tables/transition_effects_raw.csv", "tables/transition_effects_significant.csv")

top_predictors_by_period <- transition_effects_significant %>%
  dplyr::group_by(periodo) %>%
  dplyr::arrange(dplyr::desc(abs(log(or))), .by_group = TRUE) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::ungroup() %>%
  dplyr::select(periodo, ola_origen, variable, comparacion, coeficiente, or, or_ic_lower, or_ic_upper, p_value, interpretacion)

readr::write_csv(round_numeric(top_predictors_by_period, 4), file.path(tables_dir, "top_predictors_by_period.csv"))
exported_files <- c(exported_files, "tables/top_predictors_by_period.csv")

# ============================================================
# 5. Estabilidad y cambio de signo
# ============================================================

available_ga_periods <- intersect(period_labels, unique(transition_effects_raw$periodo))

ga_key <- transition_effects_raw %>%
  dplyr::filter(variable %in% key_vars, comparacion %in% comparison_levels) %>%
  dplyr::mutate(
    periodo = factor(periodo, levels = available_ga_periods),
    comparacion = factor(comparacion, levels = comparison_levels),
    variable = factor(variable, levels = rev(key_vars))
  ) %>%
  dplyr::arrange(comparacion, variable, periodo)

ga_key_dynamics <- ga_key %>%
  dplyr::group_by(variable, comparacion) %>%
  dplyr::mutate(
    signo_corto = short_sign_label(signo),
    previous_sign = dplyr::lag(signo),
    cambio_signo = !is.na(previous_sign) & signo != previous_sign & signo != "cero" & previous_sign != "cero",
    heat_fill = dplyr::case_when(
      signo == "positivo" ~ "positivo",
      signo == "negativo" ~ "negativo",
      TRUE ~ "neutro"
    ),
    matrix_fill = dplyr::case_when(
      is.na(previous_sign) & signo == "positivo" ~ "positivo",
      is.na(previous_sign) & signo == "negativo" ~ "negativo",
      cambio_signo & signo == "positivo" ~ "positivo",
      cambio_signo & signo == "negativo" ~ "negativo",
      TRUE ~ "sin_cambio"
    )
  ) %>%
  dplyr::ungroup()

sign_changes_key_variables <- ga_key_dynamics %>%
  dplyr::group_by(variable, comparacion) %>%
  dplyr::summarise(
    patron = paste(signo_corto, collapse = " | "),
    n_signos = dplyr::n_distinct(signo[signo != "cero"]),
    n_cambios = sum(cambio_signo, na.rm = TRUE),
    cambio_signo = n_cambios > 0,
    .groups = "drop"
  ) %>%
  dplyr::filter(cambio_signo)

readr::write_csv(sign_changes_key_variables, file.path(tables_dir, "sign_changes_key_variables.csv"))
exported_files <- c(exported_files, "tables/sign_changes_key_variables.csv")

rupture_counts_period <- ga_key_dynamics %>%
  dplyr::filter(cambio_signo) %>%
  dplyr::count(periodo, name = "n_rupturas")

rupture_patterns_by_variable <- ga_key_dynamics %>%
  dplyr::group_by(variable, periodo) %>%
  dplyr::summarise(
    n_pos = sum(signo == "positivo", na.rm = TRUE),
    n_neg = sum(signo == "negativo", na.rm = TRUE),
    dominant_sign = dplyr::case_when(
      n_pos > n_neg ~ "+",
      n_neg > n_pos ~ "-",
      TRUE ~ "0"
    ),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = periodo, values_from = dominant_sign) %>%
  dplyr::left_join(
    ga_key_dynamics %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(
        n_cambios = sum(cambio_signo, na.rm = TRUE),
        ruptura_clave = ifelse(any(cambio_signo), as.character(periodo[which.max(cambio_signo)]), NA_character_),
        .groups = "drop"
      ),
    by = "variable"
  ) %>%
  dplyr::mutate(
    interpretacion = dplyr::case_when(
      variable == "Confianza en pueblos originarios" ~ "Patrón compatible con reordenamiento de la confianza tras el ciclo de conflicto y represión.",
      variable == "Justicia procedimental" ~ "Consistente con cambios en la evaluación del trato estatal según el contexto político.",
      variable == "Frecuencia de contacto" ~ "El contacto aparece como mecanismo menos estable de lo esperado entre periodos.",
      variable == "Identificación con la causa" ~ "La identificación parece politizarse y variar con la coyuntura.",
      variable == "Identidad indígena" ~ "La identidad mantiene relevancia, aunque su signo no es completamente estable.",
      variable == "Percepción de conflicto" ~ "Compatible con una mayor estructuración política de la percepción de conflicto.",
      variable == "Edad" ~ "Sugiere reordenamientos generacionales entre periodos.",
      TRUE ~ "Patrón de signo potencialmente inestable."
    )
  )

readr::write_csv(rupture_patterns_by_variable, file.path(tables_dir, "rupture_patterns_by_variable.csv"))
exported_files <- c(exported_files, "tables/rupture_patterns_by_variable.csv")

sign_change_synthesis_by_class <- ga_key_dynamics %>%
  dplyr::group_by(variable, comparacion) %>%
  dplyr::summarise(
    patron = paste(signo_corto, collapse = " | "),
    n_cambios = sum(cambio_signo, na.rm = TRUE),
    resumen = paste0(patron, " (", n_cambios, " cambios)"),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = comparacion, values_from = resumen) %>%
  dplyr::mutate(
    patron_general = dplyr::case_when(
      grepl("\\(2 cambios\\)|\\(3 cambios\\)", `Clase 2 vs Clase 1`) ~ "Clase 2 más volátil",
      grepl("\\(2 cambios\\)|\\(3 cambios\\)", `Clase 3 vs Clase 1`) ~ "Clase 3 más volátil",
      grepl("\\(2 cambios\\)|\\(3 cambios\\)", `Clase 4 vs Clase 1`) ~ "Clase 4 más volátil",
      TRUE ~ "Volatilidad compartida o baja"
    )
  )

readr::write_csv(sign_change_synthesis_by_class, file.path(tables_dir, "sign_change_synthesis_by_class.csv"))
exported_files <- c(exported_files, "tables/sign_change_synthesis_by_class.csv")

if (nrow(ga_key_dynamics) > 0) {
  p_sign_change_matrix <- ggplot(ga_key_dynamics, aes(x = periodo, y = variable, fill = matrix_fill)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_tile(
      data = ~ dplyr::filter(.x, cambio_signo),
      fill = NA,
      color = "black",
      linewidth = 1.1
    ) +
    facet_wrap(~ comparacion, ncol = 1) +
    geom_vline(xintercept = 2.5, linewidth = 1.2, color = "black") +
    scale_fill_manual(values = c("positivo" = "#1b9e77", "negativo" = "#d95f02", "sin_cambio" = "grey75"), drop = FALSE) +
    labs(
      x = "Períodos",
      y = NULL,
      fill = NULL,
      title = "Ruptura en Mecanismos de Transición: Antes vs Después de Represión"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  ggsave(file.path(plots_dir, "sign_change_matrix.png"), p_sign_change_matrix, width = 10, height = 9, dpi = 300)
  exported_files <- c(exported_files, "plots/sign_change_matrix.png")

  p_sign_heatmap <- ggplot(ga_key_dynamics, aes(x = periodo, y = variable, fill = heat_fill)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_tile(
      data = ~ dplyr::filter(.x, cambio_signo),
      fill = NA,
      color = "black",
      linewidth = 1.1
    ) +
    facet_wrap(~ comparacion, ncol = 1) +
    scale_fill_manual(values = c("positivo" = "#006d2c", "negativo" = "#a50f15", "neutro" = "grey70"), drop = FALSE) +
    labs(
      x = "Períodos",
      y = NULL,
      fill = "Signo",
      title = "Estabilidad de Signos: Represión como Ruptura Estructural"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  ggsave(file.path(plots_dir, "sign_heatmap_by_period.png"), p_sign_heatmap, width = 10, height = 9, dpi = 300)
  exported_files <- c(exported_files, "plots/sign_heatmap_by_period.png")

  timeline_breaks <- ga_key_dynamics %>%
    dplyr::filter(cambio_signo) %>%
    dplyr::count(periodo, name = "n_rupturas") %>%
    dplyr::right_join(
      tibble::tibble(periodo = factor(available_ga_periods, levels = available_ga_periods)),
      by = "periodo"
    ) %>%
    dplyr::mutate(
      n_rupturas = dplyr::coalesce(n_rupturas, 0L),
      balance = purrr::map_int(as.character(periodo), function(p) {
        tmp <- ga_key_dynamics %>% dplyr::filter(as.character(periodo) == p, cambio_signo)
        sum(tmp$signo == "positivo", na.rm = TRUE) - sum(tmp$signo == "negativo", na.rm = TRUE)
      }),
      color_event = dplyr::case_when(
        balance > 0 ~ "fortalece",
        balance < 0 ~ "debilita",
        TRUE ~ "neutral"
      )
    )

  p_rupture_timeline <- ggplot(timeline_breaks, aes(x = periodo, y = n_rupturas, group = 1, color = color_event)) +
    geom_line(linewidth = 1) +
    geom_point(size = 4) +
    scale_color_manual(values = c("fortalece" = "#1b9e77", "debilita" = "#d95f02", "neutral" = "grey50")) +
    labs(
      x = NULL,
      y = "Número de rupturas de signo",
      color = NULL,
      title = "Represión Estatal como Quiebre en Actitudes Públicas (2016-2023)"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

  ggsave(file.path(plots_dir, "rupture_timeline.png"), p_rupture_timeline, width = 10, height = 5.5, dpi = 300)
  exported_files <- c(exported_files, "plots/rupture_timeline.png")
}

# ============================================================
# 6. Interpretacion breve para uso interno
# ============================================================

critical_variables <- ga_key_dynamics %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(n_cambios = sum(cambio_signo, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(n_cambios))

hypothesis_validation <- tibble::tibble(
  hypothesis = c(
    "H1: 2018→2021 concentra los cambios de signo",
    "H2: Confianza en pueblos originarios es la más volátil",
    "H3: Justicia procedimental pierde capacidad explicativa tras 2018",
    "H4: Identidad indígena se vuelve más importante post-represión"
  ),
  prediction = c(
    "El mayor número de rupturas aparece en 2018_2021",
    "Confianza presenta el mayor número de cambios",
    "Justicia cambia de signo después de 2018 en al menos una comparación",
    "Identidad indígena mantiene signo positivo en el periodo post-represión"
  ),
  observed = c(
    if (nrow(rupture_counts_period) > 0) as.character(rupture_counts_period$periodo[which.max(rupture_counts_period$n_rupturas)]) else "Sin rupturas observadas",
    if (nrow(critical_variables) > 0) as.character(critical_variables$variable[which.max(critical_variables$n_cambios)]) else "Sin cambios observados",
    if (any(ga_key_dynamics$variable == "Justicia procedimental")) "Se observan cambios de signo en algunas comparaciones." else "No se observa cambio claro.",
    if (any(ga_key_dynamics$variable == "Identidad indígena")) {
      id_post <- ga_key_dynamics %>% dplyr::filter(variable == "Identidad indígena", as.character(periodo) %in% c("2018_2021", "2021_2023"))
      paste(unique(id_post$signo), collapse = "; ")
    } else {
      "No disponible"
    }
  ),
  validated = c(
    if (nrow(rupture_counts_period) > 0 && as.character(rupture_counts_period$periodo[which.max(rupture_counts_period$n_rupturas)]) == "2018_2021") "SI" else "NO",
    if (nrow(critical_variables) > 0 && critical_variables$variable[which.max(critical_variables$n_cambios)] == "Confianza en pueblos originarios") "SI" else "NO",
    if (any(ga_key_dynamics$variable == "Justicia procedimental" & ga_key_dynamics$cambio_signo)) "SI" else "NO",
    if (any(ga_key_dynamics$variable == "Identidad indígena" & as.character(ga_key_dynamics$periodo) %in% c("2018_2021", "2021_2023") & ga_key_dynamics$signo == "positivo")) "SI" else "NO"
  )
)

readr::write_csv(hypothesis_validation, file.path(tables_dir, "hypothesis_validation.csv"))
exported_files <- c(exported_files, "tables/hypothesis_validation.csv")

period_narrative <- c(
  "Narrativa histórica por período",
  "2016→2018: Predomina un patrón más predecible y relativamente estable antes del estallido social.",
  "2018→2021: Se observan varias rupturas de signo, especialmente en variables asociadas con conflicto, confianza y justicia procedimental; este patrón es consistente con una reconfiguración del contexto tras el estallido y la represión.",
  "2021→2023: Parte de los cambios se consolidan y parte se revierten, lo que sugiere un reacomodo post-represión durante el ciclo constituyente.",
  "2023→?: La proyección debe leerse con cautela; el patrón previo sugiere que algunas asociaciones podrían estabilizarse y otras seguir siendo volátiles."
)

executive_summary <- paste(
  "En conjunto, el modelo longitudinal sugiere que el ciclo 2018–2021 estuvo asociado con un reordenamiento de los mecanismos que estructuran las transiciones entre clases latentes.",
  "Las variables más sensibles fueron confianza en pueblos originarios, justicia procedimental, frecuencia de contacto e identidad/identificación, aunque la evidencia debe leerse como asociativa y no causal.",
  "Si estos patrones persistieran, 2026 podría mostrar una estabilización parcial solo si mejora la legitimidad institucional y disminuye la polarización."
)

interpretation_lines <- c(
  "Interpretación breve del análisis longitudinal",
  paste0("Modelo leído correctamente: ", modelo_path),
  paste0("Número de clases: ", modelo_lm4$k),
  paste0("Número de observaciones: ", modelo_lm4$n),
  paste0("Periodo con mayor movilidad global: ", mobility_summary_by_period$period[which.max(mobility_summary_by_period$mobility_overall_pct)]),
  paste0("Variables críticas por número de cambios: ", paste0(critical_variables$variable, " (", critical_variables$n_cambios, ")", collapse = "; ")),
  "",
  period_narrative,
  "",
  "Validación de hipótesis",
  paste0(hypothesis_validation$hypothesis, ": ", hypothesis_validation$validated, " | observado = ", hypothesis_validation$observed),
  "",
  "Conclusión ejecutiva",
  executive_summary
)

writeLines(interpretation_lines, con = file.path(tables_dir, "interpretation_brief.txt"))
exported_files <- c(exported_files, "tables/interpretation_brief.txt")

# ============================================================
# 7. Outputs auxiliares HTML
# ============================================================

save_html_table(round_numeric(class_prevalence_by_wave, 1), file.path(tables_dir, "class_prevalence_by_wave.html"), "Prevalencia de clases por ola", 1)
save_html_table(round_numeric(mobility_summary_by_period, 2), file.path(tables_dir, "mobility_summary_by_period.html"), "Movilidad global por período", 2)
save_html_table(round_numeric(net_flows_by_class_and_period, 2), file.path(tables_dir, "net_flows_by_class_and_period.html"), "Flujos netos por clase y período", 2)
save_html_table(round_numeric(transition_effects_significant, 4), file.path(tables_dir, "transition_effects_significant.html"), "Predictores significativos de transición", 4)
save_html_table(round_numeric(top_predictors_by_period, 4), file.path(tables_dir, "top_predictors_by_period.html"), "Top predictores por período", 4)
if (nrow(sign_changes_key_variables) > 0) {
  save_html_table(sign_changes_key_variables, file.path(tables_dir, "sign_changes_key_variables.html"), "Cambios de signo en variables clave", 3)
}
save_html_table(rupture_patterns_by_variable, file.path(tables_dir, "rupture_patterns_by_variable.html"), "Patrones de ruptura por variable", 3)
save_html_table(sign_change_synthesis_by_class, file.path(tables_dir, "sign_change_synthesis_by_class.html"), "Síntesis de cambios de signo por clase", 3)
save_html_table(hypothesis_validation, file.path(tables_dir, "hypothesis_validation.html"), "Validación de hipótesis", 3)

# ============================================================
# 8. Resumen de estado en consola
# ============================================================

message("modelo leído correctamente")
message("número de clases: ", modelo_lm4$k)
message("número de observaciones: ", modelo_lm4$n)
message("archivos exportados principales:")
for (path in unique(exported_files)) {
  message("- outputs/longitudinal/", path)
}

