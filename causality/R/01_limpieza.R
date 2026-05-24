# =============================================================================
# 01_limpieza.R — Limpieza y preparación del panel ELRI
#
# ELRI — Análisis longitudinal espejo: justificación de la violencia
# y voto Rechazo plebiscito 2022
#
# Propósito: cargar datos crudos, recodificar missings, fijar identidad étnica
#            desde ola 1 y construir el subset analítico (olas 2–4).
# Input:     data/BBDD_ELRI_LONG.RData
# Output:    data/panel_completo.rds
#            data/subset_data.rds
#
# Notas metodológicas:
#   · Missing: 88/99 (olas 1-2) y 8888/9999 (olas 3-4), más 77/66
#   · VDs DiD: idx_vio_control (control social / status quo; d3_1+d3_2)
#             idx_vio_resguardo (cambio social; d4_2+d4_3)
#   · d3_3, d4_1 excluidos (solo olas 1-2); d5_4 excluido (solo olas 1-2)
#   · Identidad étnica fijada desde ola 1 (time-invariant)
#   · Análisis: olas 2 (pre), 3 (tratamiento), 4 (post)
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, stringr, haven, psych, labelled)

# ── Carga de datos ────────────────────────────────────────────────────────────

load("data/BBDD_ELRI_LONG.RData")
data <- BBDD_ELRI_LONG

# ── Comunas en zona de excepción constitucional ───────────────────────────────
# Estado de excepción decretado en octubre 2021 (activo durante ola 3).

comunas_conflicto <- c(
  "09201", "08202", "08203", "09121", "09203", "08205",
  "09104", "09204", "09105", "10104", "09106", "09107",
  "09108", "08201", "09109", "09111", "09205", "09112",
  "09101", "08207", "09211", "09119"
)

data <- data |>
  mutate(
    comuna = str_pad(as.character(comuna), width = 5, pad = "0"),
    cerca_conflicto = factor(
      if_else(comuna %in% comunas_conflicto, "cerca", "lejos"),
      levels = c("lejos", "cerca")   # "lejos" = referencia
    )
  )

# ── Panel balanceado (4 olas completas) ─────────────────────────────────────

data <- data |>
  group_by(folio) |>
  filter(n_distinct(ola) == 4) |>
  ungroup()

# ── Recodificación de missings ────────────────────────────────────────────────

vars_sustantivas <- c(
  "a4", "a5", "a6", "a7",
  "comuna",
  "c1", "c2", "c3_1", "c3_2", "c4", "c5", "c6_1", "c6_2",
  "c7_1", "c7_2", "c7_3", "c8", "c9", "c10", "c11",
  "c12", "c13", "c14", "c15", "c16",
  "c17_1", "c17_2", "c17_4", "c18", "c19",
  "c21_2", "c21_4",
  "c22", "c23", "c24", "c25",
  "c26_1", "c26_2", "c26_3", "c27_1", "c27_3",
  "c28_1", "c28_2", "c28_3", "c28_4", "c28_5", "c28_6",
  "c29_1", "c30_1", "c31_1", "c31_2", "c32_1", "c32_2",
  "c33_1", "c33_2", "c34_1", "c35", "c36",
  "d1_1", "d1_2", "d1_3", "d2_1", "d2_3",
  "d3_1", "d3_2", "d4_2", "d4_3",
  "d5_1", "d5_2", "d5_4", "d6_1",   # d5_*: justicia procedimental
  "d13", "d14"
)

recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  if_else(x %in% miss_vals, NA_integer_, as.integer(x))
}

data <- data |>
  mutate(across(
    all_of(intersect(vars_sustantivas, names(data))),
    recode_missing
  ))

# ── Identidad étnica fijada desde ola 1 (time-invariant) ──────────────────────

identidad_ola1 <- data |>
  filter(ola == 1) |>
  transmute(
    folio,
    indigeneous = factor(
      case_when(
        a1 %in% 1:11 ~ "indi",
        a1 == 12     ~ "no_indi",
        TRUE         ~ NA_character_
      ),
      levels = c("no_indi", "indi")
    ),
    cat_indi = factor(
      case_when(
        a1 == 1                       ~ "mapuche",
        a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
        a1 == 12                      ~ "chileno_noindig",
        TRUE                          ~ "otro_indigena"
      ),
      levels = c("chileno_noindig", "mapuche", "andino", "otro_indigena")
    ),
    mujer = factor(
      if_else(g2 == 2, "mujer", "hombre"),
      levels = c("hombre", "mujer")
    ),
    cerca_conflicto,
    urbano_rural = as.factor(as.character(urbano_rural))
  )

data <- data |>
  select(-any_of(c("indigeneous", "cat_indi", "mujer", "cerca_conflicto", "urbano_rural"))) |>
  left_join(identidad_ola1, by = "folio")

# ── Variables derivadas (panel completo, olas 1–4) ─────────────────────────────

data <- data |>
  mutate(
    edad = factor(
      case_when(
        g18 %in% 18:24 ~ "18_24",
        g18 %in% 25:34 ~ "25_34",
        g18 %in% 35:44 ~ "35_44",
        g18 %in% 45:54 ~ "45_54",
        g18 %in% 55:64 ~ "55_64",
        g18 %in% 65:89 ~ "65+"
      ),
      levels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+")
    ),
    periodo = factor(
      case_when(
        ola == 2 ~ "pre",
        ola == 3 ~ "tratamiento",
        ola == 4 ~ "post",
        TRUE     ~ NA_character_
      ),
      levels = c("pre", "tratamiento", "post")
    ),
    tratamiento = as.integer(ola == 3 & cerca_conflicto == "cerca"),
    ola_num = as.integer(ola),
    idx_vio_control   = rowMeans(pick(d3_1, d3_2), na.rm = TRUE),
    idx_vio_resguardo = rowMeans(pick(d4_2, d4_3), na.rm = TRUE),
    idx_just_proc     = rowMeans(pick(d5_1, d5_2), na.rm = TRUE),
    perc_desigualdad  = 6L - as.integer(c22),
    perc_injusticia   = 6L - as.integer(c23),
    malestar_diferen  = c24,
    apoyo_movil       = c25
  )

panel_completo <- data |>
  select(
    folio, ola, ola_num, periodo, tratamiento,
    indigeneous, cat_indi, cerca_conflicto, urbano_rural, mujer, edad,
    id_chile = a6, id_indi = a4,
    vio_ctrl_carb = d3_1, vio_ctrl_agric = d3_2,
    vio_camb_tierras = d4_2, vio_camb_cortes = d4_3,
    just_proc_indi = d5_1, just_proc_noindi = d5_2,
    id_causa = d6_1,
    voto_participa = d13, voto_opcion = d14,
    perc_desigualdad, perc_injusticia, malestar_diferen, apoyo_movil,
    idx_vio_control, idx_vio_resguardo, idx_just_proc, comuna
  )

saveRDS(panel_completo, "data/panel_completo.rds")
cat("✓ panel_completo guardado:", nrow(panel_completo), "obs\n")

# ── Subset analítico: olas 2, 3 y 4 ───────────────────────────────────────────
# Ola 2 = pre | Ola 3 = tratamiento (estado de excepción) | Ola 4 = post

subset_data <- panel_completo |>
  filter(ola %in% c(2, 3, 4))

# ── Verificación básica ───────────────────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("VERIFICACIÓN DEL SUBSET ANALÍTICO\n")
cat(strrep("=", 60), "\n\n")
cat("N individuos únicos:", n_distinct(subset_data$folio), "\n")
cat("N observaciones totales:", nrow(subset_data), "\n\n")
cat("--- Distribución por período e identidad ---\n")
print(table(subset_data$periodo, subset_data$indigeneous, useNA = "ifany"))
cat("\n--- Distribución por período y zona ---\n")
print(table(subset_data$periodo, subset_data$cerca_conflicto, useNA = "ifany"))
cat("\n--- Tabla 2×2 DiD: identidad × zona ---\n")
print(table(subset_data$indigeneous, subset_data$cerca_conflicto, useNA = "ifany"))
cat("\n--- NAs por variable de interés ---\n")
vars_check <- c(
  "idx_vio_control", "idx_vio_resguardo", "idx_just_proc",
  "perc_desigualdad", "perc_injusticia", "apoyo_movil",
  "id_causa", "id_chile"
)
print(colSums(is.na(subset_data[, vars_check])))

cat("\n--- Verificación post-inversión c22/c23 ---\n")
cat("NAs perc_desigualdad:", sum(is.na(subset_data$perc_desigualdad)), "\n")
cat("NAs perc_injusticia: ", sum(is.na(subset_data$perc_injusticia)), "\n")
cat("Rango perc_desigualdad:", paste(range(subset_data$perc_desigualdad, na.rm = TRUE), collapse = " – "), "\n")
cat("Rango perc_injusticia: ", paste(range(subset_data$perc_injusticia, na.rm = TRUE), collapse = " – "), "\n")

# ── Guardar ───────────────────────────────────────────────────────────────────

saveRDS(subset_data, "data/subset_data.rds")
cat("\n✓ subset_data guardado:", nrow(subset_data), "obs ×", ncol(subset_data), "variables\n")
