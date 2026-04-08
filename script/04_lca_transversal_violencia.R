if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  tidyr,
  purrr,
  ggplot2,
  haven,
  here,
  poLCA,
  readr,
  nnet,
  broom
)

options(stringsAsFactors = FALSE, scipen = 999)

rm(list = ls())
gc()

# Flujo sugerido:
# 1) Este script estima una LCA transversal solo en la ultima ola (`ola == 4`).
# 2) No reemplaza el analisis longitudinal.
# 3) Para el longitudinal, usar `script/2_create-clase2.0.R` y `script/03_predictors.R`.

dir.create(here("output", "transversal"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "transversal", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "transversal", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "transversal", "modelos"), recursive = TRUE, showWarnings = FALSE)

load(here("data", "BBDD_ELRI_LONG.RData"))

na_codes <- c(66, 88, 99, 8888, 9999)
item_vars <- c("d3_1", "d3_2", "d4_2", "d4_3")
item_labels <- c(
  d3_1_red = "Uso de fuerza policial en protestas indigenas",
  d3_2_red = "Uso de armas por agricultores",
  d4_2_red = "Toma de terrenos por grupos indigenas",
  d4_3_red = "Corte de carreteras por grupos indigenas"
)

reclasificar <- function(x) {
  dplyr::case_when(
    x <= 2 ~ 1L,
    x >= 3 ~ 2L,
    TRUE ~ NA_integer_
  )
}

mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

a1_num <- as.numeric(haven::zap_labels(BBDD_ELRI_LONG$a1))
a1_num[a1_num %in% na_codes] <- NA_real_

indi_global_mode <- dplyr::case_when(
  sum(a1_num %in% 1:11, na.rm = TRUE) >= sum(a1_num == 12, na.rm = TRUE) ~ "indi",
  TRUE ~ "no_indi"
)

predictor_vars <- c("d1_1", "c5", "d5_1", "d6_1", "c13", "urbano_rural")

db_lca <- BBDD_ELRI_LONG %>%
  dplyr::mutate(
    a1_num = a1_num,
    indi = dplyr::case_when(
      a1_num %in% 1:11 ~ "indi",
      a1_num == 12 ~ "no_indi",
      TRUE ~ NA_character_
    ),
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
    )
  ) %>%
  dplyr::group_by(folio) %>%
  dplyr::mutate(indi = ifelse(is.na(indi), mode_chr(indi), indi)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    indi = ifelse(is.na(indi), indi_global_mode, indi),
    ola = as.integer(ola)
  ) %>%
  dplyr::filter(ola == 4) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(item_vars),
      ~ as.numeric(haven::zap_labels(.x))
    ),
    dplyr::across(
      dplyr::all_of(predictor_vars),
      ~ as.numeric(haven::zap_labels(.x))
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(item_vars),
      ~ dplyr::if_else(.x %in% na_codes, NA_real_, .x)
    ),
    dplyr::across(
      dplyr::all_of(predictor_vars),
      ~ dplyr::if_else(.x %in% na_codes, NA_real_, .x)
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(item_vars),
      reclasificar,
      .names = "{.col}_red"
    ),
    ola_label = dplyr::case_when(
      ola == 1 ~ "2016",
      ola == 2 ~ "2018",
      ola == 3 ~ "2021",
      ola == 4 ~ "2023",
      TRUE ~ as.character(ola)
    )
  ) %>%
  dplyr::select(
    folio, ola, ola_label, indi, mujer, edad,
    dplyr::all_of(predictor_vars),
    dplyr::ends_with("_red")
  ) %>%
  tidyr::drop_na()

response_vars <- c("d3_1_red", "d3_2_red", "d4_2_red", "d4_3_red")

db_lca <- db_lca %>%
  dplyr::mutate(
    id_obs = dplyr::row_number(),
    dplyr::across(
      dplyr::all_of(response_vars),
      ~ factor(.x, levels = c(1, 2), labels = c("No", "Si"))
    ),
    ola_label = factor(ola_label, levels = c("2016", "2018", "2021", "2023")),
    indi = factor(indi, levels = c("no_indi", "indi")),
    mujer = factor(mujer, levels = c("0", "1"), labels = c("Hombre", "Mujer")),
    edad = factor(
      edad,
      levels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+"),
      ordered = TRUE
    )
  )

message("N analitico (ola 4): ", nrow(db_lca))
message("Patrones observados: ", dplyr::n_distinct(db_lca[response_vars]))

item_dist <- db_lca %>%
  dplyr::select(dplyr::all_of(response_vars)) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "item",
    values_to = "respuesta"
  ) %>%
  dplyr::count(item, respuesta, name = "n") %>%
  dplyr::group_by(item) %>%
  dplyr::mutate(pct = 100 * n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(item_label = dplyr::recode(item, !!!item_labels))

readr::write_csv(item_dist, here("output", "transversal", "tables", "item_distribution.csv"))

f_lca <- stats::as.formula(
  paste("cbind(", paste(response_vars, collapse = ", "), ") ~ 1")
)

safe_polca <- function(k, data, nrep = 10, maxiter = 1500) {
  set.seed(1234 + k)
  poLCA::poLCA(
    formula = f_lca,
    data = data,
    nclass = k,
    nrep = ifelse(k == 1, 1, nrep),
    maxiter = maxiter,
    verbose = FALSE,
    calc.se = TRUE,
    graphs = FALSE,
    na.rm = FALSE
  )
}

k_seq <- 1:5
fits <- purrr::map(k_seq, safe_polca, data = db_lca)
names(fits) <- paste0("k", k_seq)

fit_tbl <- tibble::tibble(
  K = k_seq,
  N = nrow(db_lca),
  logLik = purrr::map_dbl(fits, ~ as.numeric(.x$llik)),
  npar = purrr::map_dbl(fits, ~ as.numeric(.x$npar)),
  AIC = purrr::map_dbl(fits, ~ as.numeric(.x$aic)),
  BIC = purrr::map_dbl(fits, ~ as.numeric(.x$bic)),
  Gsq = purrr::map_dbl(fits, ~ as.numeric(.x$Gsq)),
  Chisq = purrr::map_dbl(fits, ~ as.numeric(.x$Chisq))
) %>%
  dplyr::mutate(
    dAIC = AIC - dplyr::lag(AIC),
    dBIC = BIC - dplyr::lag(BIC),
    LRT = 2 * (logLik - dplyr::lag(logLik)),
    df_LRT = npar - dplyr::lag(npar),
    p_LRT = dplyr::if_else(
      !is.na(LRT) & df_LRT > 0,
      stats::pchisq(LRT, df = df_LRT, lower.tail = FALSE),
      NA_real_
    )
  )

readr::write_csv(fit_tbl, here("output", "transversal", "tables", "fit_indices.csv"))

fit_long <- fit_tbl %>%
  dplyr::select(K, AIC, BIC) %>%
  tidyr::pivot_longer(-K, names_to = "criterion", values_to = "value")

p_fit <- ggplot(fit_long, aes(x = K, y = value, color = criterion)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = k_seq) +
  labs(
    x = "Numero de clases (K)",
    y = "Indice de ajuste",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = here("output", "transversal", "plots", "fit_indices.png"),
  plot = p_fit,
  width = 7,
  height = 4.5,
  dpi = 300
)

k_final <- 4L
if (!k_final %in% k_seq) {
  k_final <- fit_tbl$K[which.min(fit_tbl$BIC)]
}

fit_final <- fits[[which(k_seq == k_final)]]
saveRDS(
  fit_final,
  here("output", "transversal", "modelos", paste0("lca_k", k_final, ".rds"))
)

probs_long <- purrr::imap_dfr(fit_final$probs, function(mat, item_name) {
  as.data.frame(mat) %>%
    tibble::rownames_to_column("class") %>%
    dplyr::mutate(item = item_name) %>%
    tidyr::pivot_longer(
      cols = -c(class, item),
      names_to = "category",
      values_to = "prob"
    )
})

endorsement_category <- if ("Pr(2)" %in% unique(probs_long$category)) {
  "Pr(2)"
} else if ("Si" %in% unique(probs_long$category)) {
  "Si"
} else {
  unique(probs_long$category)[length(unique(probs_long$category))]
}

p_endorse <- probs_long %>%
  dplyr::filter(category == endorsement_category) %>%
  dplyr::mutate(
    class = readr::parse_number(class),
    class = factor(class, levels = seq_len(k_final)),
    item = factor(item, levels = response_vars),
    item_label = factor(
      dplyr::recode(as.character(item), !!!item_labels),
      levels = unname(item_labels[response_vars])
    ),
    prob = as.numeric(prob)
  )

# Alias defensivo por si en otros bloques se usa el nombre con typo.
p_endore <- p_endorse

class_prev <- tibble::tibble(
  class = factor(seq_len(k_final), levels = seq_len(k_final)),
  pi = as.numeric(fit_final$P),
  pi_pct = round(100 * pi, 1)
)

report_probs <- p_endorse %>%
  dplyr::ungroup() %>%
  dplyr::select(item_label, class, prob) %>%
  dplyr::distinct() %>%
  dplyr::mutate(class = paste0("Clase_", class)) %>%
  tidyr::pivot_wider(
    names_from = class,
    values_from = prob,
    values_fn = ~ mean(.x, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("Clase_"),
      ~ round(as.numeric(.x), 3)
    )
  )

prev_lab <- class_prev %>%
  dplyr::transmute(
    old = paste0("Clase_", class),
    new = paste0("Clase_", class, "_", pi_pct, "pct")
  )

for (i in seq_len(nrow(prev_lab))) {
  names(report_probs)[names(report_probs) == prev_lab$old[i]] <- prev_lab$new[i]
}

readr::write_csv(report_probs, here("output", "transversal", "tables", "conditional_probabilities.csv"))
readr::write_csv(class_prev, here("output", "transversal", "tables", "class_prevalence.csv"))

p_profile <- ggplot(
  p_endorse,
  aes(x = item_label, y = prob, group = class, color = class)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = NULL,
    y = "P(Se justifica | clase)",
    color = "Clase"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "top"
  )

ggsave(
  filename = here("output", "transversal", "plots", paste0("profiles_k", k_final, ".png")),
  plot = p_profile,
  width = 8,
  height = 5,
  dpi = 300
)

posterior_df <- as.data.frame(fit_final$posterior)
prob_cols <- names(posterior_df)

class_assign <- db_lca %>%
  dplyr::bind_cols(posterior_df) %>%
  dplyr::mutate(
    class_hat = max.col(as.matrix(dplyr::select(., dplyr::all_of(prob_cols))), ties.method = "first"),
    prob_max = do.call(
      pmax,
      c(dplyr::select(., dplyr::all_of(prob_cols)), list(na.rm = TRUE))
    )
  )

wave_class <- class_assign %>%
  dplyr::count(ola_label, class_hat, name = "n") %>%
  dplyr::group_by(ola_label) %>%
  dplyr::mutate(pct = 100 * n / sum(n)) %>%
  dplyr::ungroup()

indi_class <- class_assign %>%
  dplyr::count(indi, class_hat, name = "n") %>%
  dplyr::group_by(indi) %>%
  dplyr::mutate(pct = 100 * n / sum(n)) %>%
  dplyr::ungroup()

readr::write_csv(wave_class, here("output", "transversal", "tables", "class_by_wave.csv"))
readr::write_csv(indi_class, here("output", "transversal", "tables", "class_by_indi.csv"))
saveRDS(class_assign, here("output", "transversal", "db_lca_class_assignments.rds"))

# Predictores de pertenencia a clase usando el mismo set de 02_create-class.R
class_assign <- class_assign %>%
  dplyr::mutate(class_hat = factor(class_hat))

multinom_fit <- nnet::multinom(
  class_hat ~ urbano_rural + mujer + indi + edad + d1_1 + c5 + d5_1 + d6_1 + c13,
  data = class_assign,
  trace = FALSE
)

multinom_tidy <- broom::tidy(multinom_fit, conf.int = TRUE) %>%
  dplyr::mutate(
    odds_ratio = exp(estimate),
    or_conf.low = exp(conf.low),
    or_conf.high = exp(conf.high),
    p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  )

readr::write_csv(
  multinom_tidy,
  here("output", "transversal", "tables", "class_membership_predictors.csv")
)
saveRDS(
  multinom_fit,
  here("output", "transversal", "modelos", "class_membership_multinom.rds")
)

message("Modelo final guardado con K = ", k_final)
print(fit_tbl)
print(class_prev)
