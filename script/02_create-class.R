if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  tidyr,
  purrr,
  ggplot2,
  haven,
  here,
  LMest,
  readr,
  tibble,
  reshape2
)

options(stringsAsFactors = FALSE, scipen = 999)

rm(list = ls())
gc()

# ============================================================
# 0. Configuracion general
# ============================================================

# Politica de missing del pipeline:
# 1) recodificar codigos especiales a NA;
# 2) restringir a panel balanceado de 4 olas;
# 3) imputar solo d5_1 con la mediana del panel balanceado;
# 4) aplicar complete-case sobre el resto de variables del analisis.

use_saved_model <- TRUE
use_saved_candidates <- TRUE
selected_k <- 4L
candidate_k <- 1:6
set.seed(1234)

output_root <- here("outputs", "longitudinal")
data_dir <- file.path(output_root, "data")
models_dir <- file.path(output_root, "models")
tables_dir <- file.path(output_root, "tables")
plots_dir <- file.path(output_root, "plots")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

candidate_models_path <- file.path(models_dir, "candidate_models_k1_6.rds")
final_model_path <- file.path(models_dir, "modelo_4c.rds")
legacy_candidate_models_path <- here("outputs", "modelos_k_1_6_lmest.rds")
legacy_final_model_path <- here("outputs", "modelo_4c.rds")

# ============================================================
# 1. Carga de datos y helpers
# ============================================================

load(here("data", "BBDD_ELRI_LONG.RData"))

na_codes <- c(66, 88, 99, 8888, 9999)
response_vars_raw <- c("d3_1", "d3_2", "d4_2", "d4_3")
response_vars <- paste0(response_vars_raw, "_red")
covariates <- c(
  "urbano_rural", "mujer", "indi", "edad",
  "d1_1", "c5", "d5_1", "d6_1", "c13"
)
analysis_vars <- c("folio", "ola", covariates, response_vars)

item_descriptions <- c(
  "1" = "Uso de fuerza policial en protestas indígenas",
  "2" = "Uso de armas por agricultores",
  "3" = "Toma de terrenos por grupos indígenas",
  "4" = "Corte de carreteras por grupos indígenas"
)

mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

reclassify_violence <- function(x) {
  dplyr::case_when(
    x <= 2 ~ 1L,
    x >= 3 ~ 2L,
    TRUE ~ NA_integer_
  )
}

extract_metric <- function(x, candidates) {
  for (nm in candidates) {
    if (!is.null(x[[nm]]) && length(x[[nm]]) >= 1) {
      return(as.numeric(x[[nm]])[1])
    }
  }
  NA_real_
}

as_model_selection_table <- function(candidate_object, candidate_k) {
  if (is.list(candidate_object) && all(paste0("k", candidate_k) %in% names(candidate_object))) {
    return(
      purrr::map_dfr(candidate_object, function(model) {
        tibble::tibble(
          k = as.integer(model$k),
          logLik = extract_metric(model, c("lk", "lkv", "Lk")),
          AIC = extract_metric(model, c("aic", "Aic")),
          BIC = extract_metric(model, c("bic", "Bic")),
          n = extract_metric(model, c("n")),
          TT = extract_metric(model, c("TT"))
        )
      }) %>%
        dplyr::arrange(k)
    )
  }

  tibble::tibble(
    k = if (!is.null(candidate_object$k)) as.integer(candidate_object$k) else candidate_k,
    logLik = if (!is.null(candidate_object$lk)) as.numeric(candidate_object$lk) else NA_real_,
    AIC = if (!is.null(candidate_object$aic)) as.numeric(candidate_object$aic) else NA_real_,
    BIC = if (!is.null(candidate_object$bic)) as.numeric(candidate_object$bic) else NA_real_,
    n = if (!is.null(candidate_object$n)) as.numeric(candidate_object$n)[1] else NA_real_,
    TT = if (!is.null(candidate_object$TT)) as.numeric(candidate_object$TT)[1] else NA_real_
  ) %>%
    dplyr::arrange(k)
}

fit_lmest_model <- function(k, data) {
  lmest(
    responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
    latentFormula =~ urbano_rural + mujer + indi + edad + d1_1 + c5 + d5_1 + d6_1 + c13,
    index = c("folio", "time4"),
    data = data,
    k = k,
    start = 0,
    modBasic = 3,
    modManifest = "FM",
    paramLatent = "multilogit",
    output = TRUE,
    out_se = TRUE,
    seed = 1234
  )
}

propose_class_label <- function(control_mean, change_mean) {
  if (is.na(control_mean) || is.na(change_mean)) {
    return(c("Perfil no rotulado", "No fue posible resumir las probabilidades condicionales."))
  }
  if (control_mean < 0.35 && change_mean < 0.35) {
    return(c("Rechazo amplio de la violencia", "Baja justificación tanto de violencia de control como de cambio."))
  }
  if (control_mean >= 0.60 && change_mean < 0.40) {
    return(c("Pro control coercitivo", "Alta justificación del control y bajo apoyo a acciones de cambio."))
  }
  if (control_mean < 0.40 && change_mean >= 0.60) {
    return(c("Pro acción indígena", "Baja justificación del control y alta legitimación de acciones de cambio indígena."))
  }
  if (control_mean >= 0.60 && change_mean >= 0.60) {
    return(c("Legitimación amplia de la violencia", "Alta justificación tanto del control como de acciones de cambio."))
  }
  c("Perfil mixto o ambivalente", "Combinación intermedia entre apoyo y rechazo a distintos repertorios.")
}

# ============================================================
# 2. Preparacion de datos
# ============================================================

a1_num <- as.numeric(haven::zap_labels(BBDD_ELRI_LONG$a1))
a1_num[a1_num %in% na_codes] <- NA_real_

BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::mutate(
    a1_num = a1_num,
    indi = dplyr::case_when(
      a1_num %in% 1:11 ~ "indi",
      a1_num == 12 ~ "no_indi",
      TRUE ~ NA_character_
    )
  )

indi_global_mode <- mode_chr(BBDD_ELRI_LONG$indi)

BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::group_by(folio) %>%
  dplyr::mutate(indi = ifelse(is.na(indi), mode_chr(indi), indi)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    indi = ifelse(is.na(indi), indi_global_mode, indi),
    indi = factor(indi, levels = c("no_indi", "indi"))
  )

panel_base <- BBDD_ELRI_LONG %>%
  dplyr::mutate(
    mujer = dplyr::case_when(
      g2 == 1 ~ "0",
      g2 == 2 ~ "1",
      TRUE ~ NA_character_
    ),
    edad = dplyr::case_when(
      g18 %in% 18:24 ~ "18_24",
      g18 %in% 25:34 ~ "25_34",
      g18 %in% 35:44 ~ "35_44",
      g18 %in% 45:54 ~ "45_54",
      g18 %in% 55:64 ~ "55_64",
      g18 %in% 65:89 ~ "65+",
      TRUE ~ NA_character_
    ),
    ola = as.integer(ola)
  ) %>%
  dplyr::filter(ola %in% 1:4) %>%
  dplyr::select(
    folio, ola,
    d3_1, d3_2, d4_2, d4_3,
    d1_1, c5, d5_1, d6_1, c13,
    urbano_rural, mujer, indi, edad
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(d3_1, d3_2, d4_2, d4_3, d1_1, c5, d5_1, c13, urbano_rural),
      ~ as.numeric(haven::zap_labels(.x))
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ dplyr::if_else(.x %in% na_codes, NA_real_, .x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(d3_1, d3_2, d4_2, d4_3),
      reclassify_violence,
      .names = "{.col}_red"
    ),
    mujer = factor(mujer, levels = c("0", "1"), labels = c("Hombre", "Mujer")),
    edad = factor(
      edad,
      levels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+"),
      ordered = TRUE
    )
  ) %>%
  dplyr::select(dplyr::all_of(analysis_vars))

# ============================================================
# 3. Muestra panel balanceada
# ============================================================

panel_balanced <- panel_base %>%
  dplyr::group_by(folio) %>%
  dplyr::filter(dplyr::n_distinct(ola) == 4) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(folio, ola)

missing_before_imputation <- tibble::tibble(
  variable = names(panel_balanced),
  missing_balanced_panel = sapply(panel_balanced, function(x) sum(is.na(x)))
)

panel_balanced <- panel_balanced %>%
  dplyr::mutate(
    d5_1 = dplyr::if_else(
      is.na(d5_1),
      stats::median(d5_1, na.rm = TRUE),
      d5_1
    )
  )

missing_after_imputation <- tibble::tibble(
  variable = names(panel_balanced),
  missing_after_imputation = sapply(panel_balanced, function(x) sum(is.na(x)))
)

complete_subjects <- panel_balanced %>%
  dplyr::mutate(row_complete = stats::complete.cases(dplyr::pick(dplyr::all_of(names(panel_balanced))))) %>%
  dplyr::group_by(folio) %>%
  dplyr::summarise(all_complete = all(row_complete), .groups = "drop") %>%
  dplyr::filter(all_complete) %>%
  dplyr::pull(folio)

panel_analysis_sample <- panel_balanced %>%
  dplyr::filter(folio %in% complete_subjects) %>%
  dplyr::mutate(
    time4 = as.integer(factor(ola, levels = sort(unique(ola)))),
    folio = as.integer(as.character(folio))
  ) %>%
  dplyr::arrange(folio, time4) %>%
  as.data.frame()

missing_final <- tibble::tibble(
  variable = names(panel_analysis_sample),
  missing_final_sample = sapply(panel_analysis_sample, function(x) sum(is.na(x)))
)

panel_missingness_summary <- missing_before_imputation %>%
  dplyr::left_join(missing_after_imputation, by = "variable") %>%
  dplyr::left_join(missing_final, by = "variable")

stage_summary <- tibble::tibble(
  stage = c("balanced_panel_rows", "balanced_panel_subjects", "analysis_rows", "analysis_subjects"),
  value = c(
    nrow(panel_balanced),
    dplyr::n_distinct(panel_balanced$folio),
    nrow(panel_analysis_sample),
    dplyr::n_distinct(panel_analysis_sample$folio)
  )
)

readr::write_csv(panel_analysis_sample, file.path(data_dir, "panel_analysis_sample.csv"))
readr::write_csv(panel_missingness_summary, file.path(data_dir, "panel_missingness_summary.csv"))
readr::write_csv(stage_summary, file.path(data_dir, "panel_stage_summary.csv"))

# ============================================================
# 4. Modelos candidatos
# ============================================================

if (use_saved_candidates && file.exists(candidate_models_path)) {
  candidate_models <- readRDS(candidate_models_path)
} else if (use_saved_candidates && file.exists(legacy_candidate_models_path)) {
  candidate_models <- readRDS(legacy_candidate_models_path)
  saveRDS(candidate_models, candidate_models_path)
} else {
  candidate_models <- purrr::map(candidate_k, ~ fit_lmest_model(.x, panel_analysis_sample))
  names(candidate_models) <- paste0("k", candidate_k)
  saveRDS(candidate_models, candidate_models_path)
}

model_selection_fit_indices <- as_model_selection_table(candidate_models, candidate_k) %>%
  dplyr::mutate(
    delta_BIC = BIC - dplyr::lag(BIC),
    delta_AIC = AIC - dplyr::lag(AIC)
  )

selected_model_stats <- model_selection_fit_indices %>%
  dplyr::filter(k == selected_k)

model_selection_summary <- tibble::tibble(
  selected_k = selected_k,
  k_with_min_bic = model_selection_fit_indices$k[which.min(model_selection_fit_indices$BIC)],
  selected_model_BIC = selected_model_stats$BIC,
  selected_model_AIC = selected_model_stats$AIC,
  selected_model_logLik = selected_model_stats$logLik,
  sample_subjects = dplyr::n_distinct(panel_analysis_sample$folio),
  sample_rows = nrow(panel_analysis_sample),
  balanced_waves = length(unique(panel_analysis_sample$time4)),
  modBasic = 3,
  modManifest = "FM",
  paramLatent = "multilogit",
  missing_policy = "Balanced 4-wave panel; median imputation only for d5_1; complete-case on remaining variables"
)

readr::write_csv(model_selection_fit_indices, file.path(tables_dir, "model_selection_fit_indices.csv"))
readr::write_csv(model_selection_summary, file.path(tables_dir, "model_selection_summary.csv"))

p_bic <- ggplot(model_selection_fit_indices, aes(x = k, y = BIC)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = candidate_k) +
  labs(x = "Numero de clases", y = "BIC") +
  theme_minimal(base_size = 12)

ggsave(file.path(plots_dir, "model_selection_bic.png"), p_bic, width = 7, height = 4.5, dpi = 300)

# ============================================================
# 5. Modelo final
# ============================================================

if (use_saved_model && file.exists(final_model_path)) {
  modelo_4c <- readRDS(final_model_path)
} else if (use_saved_model && file.exists(legacy_final_model_path)) {
  modelo_4c <- readRDS(legacy_final_model_path)
  saveRDS(modelo_4c, final_model_path)
} else if (is.list(candidate_models) && !is.null(candidate_models[[paste0("k", selected_k)]])) {
  modelo_4c <- candidate_models[[paste0("k", selected_k)]]
  saveRDS(modelo_4c, final_model_path)
} else {
  modelo_4c <- fit_lmest_model(selected_k, panel_analysis_sample)
  saveRDS(modelo_4c, final_model_path)
}

# ============================================================
# 6. Outputs basicos del modelo final
# ============================================================

psi_long <- reshape2::melt(modelo_4c$Psi, level = 1) %>%
  dplyr::rename(
    class = state,
    item = item,
    response_category = category,
    probability = value
  ) %>%
  dplyr::mutate(
    class = factor(class),
    item = factor(item, levels = sort(unique(item))),
    probability_pct = round(100 * probability, 1)
  )

readr::write_csv(psi_long, file.path(tables_dir, "conditional_response_probabilities.csv"))

class_prevalence_overall <- tibble::tibble(
  class = factor(seq_len(ncol(modelo_4c$Piv))),
  prevalence_initial_mean = colMeans(modelo_4c$Piv),
  prevalence_average_marginal = rowMeans(modelo_4c$Pmarg)
) %>%
  dplyr::mutate(
    prevalence_initial_mean_pct = round(100 * prevalence_initial_mean, 1),
    prevalence_average_marginal_pct = round(100 * prevalence_average_marginal, 1)
  )

readr::write_csv(class_prevalence_overall, file.path(tables_dir, "class_prevalence_overall.csv"))

p_conditional <- ggplot(psi_long, aes(x = item, y = probability_pct, fill = factor(response_category))) +
  geom_col(position = "stack") +
  facet_wrap(~ class, ncol = 1) +
  scale_x_discrete(labels = item_descriptions) +
  scale_fill_manual(values = c("#1f2041", "#ffc857"), labels = c("No", "Si")) +
  labs(x = NULL, y = "Probabilidad (%)", fill = "Respuesta") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.85),
    panel.spacing = grid::unit(1, "lines")
  )

ggsave(file.path(plots_dir, "conditional_probabilities_by_class.png"), p_conditional, width = 11, height = 9, dpi = 300)

endorsement_prob <- psi_long %>%
  dplyr::mutate(response_category_num = suppressWarnings(as.numeric(as.character(response_category)))) %>%
  dplyr::filter(response_category_num == max(response_category_num, na.rm = TRUE)) %>%
  dplyr::mutate(
    item_group = dplyr::case_when(
      as.character(item) %in% c("1", "2") ~ "control",
      as.character(item) %in% c("3", "4") ~ "cambio",
      TRUE ~ "otro"
    )
  )

class_labels_proposed <- endorsement_prob %>%
  dplyr::group_by(class, item_group) %>%
  dplyr::summarise(mean_prob = mean(probability), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = item_group, values_from = mean_prob) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    proposed_label = propose_class_label(control, cambio)[1],
    short_description = propose_class_label(control, cambio)[2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    mean_control_endorsement = round(100 * control, 1),
    mean_change_endorsement = round(100 * cambio, 1)
  ) %>%
  dplyr::select(class, proposed_label, short_description, mean_control_endorsement, mean_change_endorsement)

readr::write_csv(class_labels_proposed, file.path(tables_dir, "class_labels_proposed.csv"))

message("Script 02 finalizado.")
message("Muestra analitica: ", dplyr::n_distinct(panel_analysis_sample$folio), " sujetos y ", nrow(panel_analysis_sample), " filas.")
message("Modelo final guardado en: ", final_model_path)
