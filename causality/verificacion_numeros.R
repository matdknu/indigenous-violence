# Verificación de consistencia de números

# Cargar datos
subset_data <- readRDS("data/subset_data.rds")
load("data/BBDD_ELRI_LONG.RData")

# Función missing
recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  xv <- if (inherits(x, "haven_labelled")) {
    as.integer(haven::zap_labels(x))
  } else {
    as.integer(x)
  }
  if_else(xv %in% miss_vals, NA_integer_, xv)
}

# Extraer a5
a5_data <- BBDD_ELRI_LONG |>
  dplyr::select(folio, ola, a5) |>
  dplyr::mutate(a5 = recode_missing(a5))

subset_data <- subset_data |>
  dplyr::left_join(a5_data, by = c("folio", "ola"), relationship = "many-to-many")

# Calcular variables
subset_data <- subset_data |>
  dplyr::mutate(
    a4_num = as.numeric(id_indi),
    a5_num = as.numeric(a5),
    a6_num = as.numeric(id_chile),
    idx_id_etnica = rowMeans(dplyr::pick(a4_num, a5_num), na.rm = TRUE),
    id_nacional = a6_num,
    predominancia_id = idx_id_etnica - id_nacional,
    id_ingroup = dplyr::case_when(
      indigeneous == "indi" ~ idx_id_etnica,
      indigeneous == "no_indi" ~ id_nacional,
      TRUE ~ NA_real_
    ),
    id_outgroup = dplyr::case_when(
      indigeneous == "indi" ~ id_nacional,
      indigeneous == "no_indi" ~ idx_id_etnica,
      TRUE ~ NA_real_
    ),
    brecha_id = id_ingroup - id_outgroup
  )

cat("\n=== VERIFICACIÓN DE NÚMEROS ===\n\n")

# 1. Distribución id_indi no indígenas
cat("1. Distribución id_indi (a4) NO INDÍGENAS (ola 2):\n")
tabla_noindi <- table(subset_data |> 
  dplyr::filter(ola == 2, indigeneous == "no_indi") |> 
  dplyr::pull(id_indi), useNA = "ifany")
print(tabla_noindi)

total_noindi <- sum(tabla_noindi[1:5])
cat("\nTotal (sin NAs):", total_noindi, "\n")
cat("1-2:", sum(tabla_noindi[1:2]), "=", 
    round(sum(tabla_noindi[1:2]) / total_noindi * 100, 1), "%\n")
cat("3-5:", sum(tabla_noindi[3:5]), "=", 
    round(sum(tabla_noindi[3:5]) / total_noindi * 100, 1), "%\n")

# 2. Medias baseline
cat("\n2. Medias por grupo (ola 2):\n")
medias <- subset_data |>
  dplyr::filter(ola == 2) |>
  dplyr::group_by(indigeneous) |>
  dplyr::summarise(
    id_indi_media = mean(a4_num, na.rm = TRUE),
    a5_media = mean(a5_num, na.rm = TRUE),
    id_chile_media = mean(a6_num, na.rm = TRUE),
    predominancia = mean(predominancia_id, na.rm = TRUE),
    brecha_id = mean(brecha_id, na.rm = TRUE),
    .groups = "drop"
  )
print(medias)

# 3. Correlación
cat("\n3. Correlación id_indi × a5:\n")
r_a4_a5 <- cor(subset_data$a4_num, subset_data$a5_num, use = "complete.obs")
cat("r =", round(r_a4_a5, 3), "\n")

# 4. Verificar que id_chile SD es correcto
cat("\n4. Desviaciones estándar id_chile (ola 2):\n")
sds <- subset_data |>
  dplyr::filter(ola == 2) |>
  dplyr::group_by(indigeneous) |>
  dplyr::summarise(
    sd_id_chile = sd(a6_num, na.rm = TRUE),
    .groups = "drop"
  )
print(sds)

cat("\n✓ Verificación completada.\n")
