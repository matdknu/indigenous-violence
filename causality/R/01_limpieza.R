# =============================================================================
# 01_limpieza.R — Limpieza y preparación del subset analítico ELRI
#
# Timeline:
#   Ola 1 (2016): baseline pre-todo → solo placebo
#   Ola 2 (2018): baseline pre-estallido → REFERENCIA DiD
#   Ola 3 (dic 2020 – may 2021): resabio estallido social → TRATAMIENTO 1
#   Ola 4 (2023): decreto prolongado + derrota Apruebo → TRATAMIENTO 2
#
# Decreto D.S. N°418/2021 (12 oct 2021): 53 comunas en 4 provincias
#   La Araucanía: Cautín (091) + Malleco (092)
#   Biobío:       Arauco (082) + Biobío-provincia (083)
#
# Codificación ordinal A (1–2 / 3 / 4–5) en ítems → índice principal ordinal
#
# Output:
#   data/panel_completo.rds
#   data/subset_data.rds
#   data/subset_placebo_pre.rds
#   data/analysis_metadata.rds
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, stringr, haven)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# ── 1. Cargar datos ────────────────────────────────────────────────────────────

load("data/BBDD_ELRI_LONG.RData")
panel_raw <- BBDD_ELRI_LONG

cat("Dimensiones:", nrow(panel_raw), "×", ncol(panel_raw), "\n")
cat("Olas disponibles:", sort(unique(panel_raw$ola)), "\n\n")

# ── 2. Definiciones geográficas ───────────────────────────────────────────────

# CAPA 1: Decreto D.S. N°418/2021 — 53 comunas (4 provincias completas)
comunas_decreto <- c(
  # Provincia de Cautín — La Araucanía (21 comunas)
  "09101", "09102", "09103", "09104", "09105", "09106", "09107",
  "09108", "09109", "09110", "09111", "09112", "09113", "09114",
  "09115", "09116", "09117", "09118", "09119", "09120", "09121",
  # Provincia de Malleco — La Araucanía (11 comunas)
  "09201", "09202", "09203", "09204", "09205", "09206", "09207",
  "09208", "09209", "09210", "09211",
  # Provincia de Arauco — Biobío (7 comunas)
  "08201", "08202", "08203", "08204", "08205", "08206", "08207",
  # Provincia de Biobío — Biobío (14 comunas)
  "08301", "08302", "08303", "08304", "08305", "08306", "08307",
  "08308", "08309", "08310", "08311", "08312", "08313", "08314"
)

# Alias para compatibilidad con scripts y documentación previa
comunas_excepcion <- comunas_decreto

# CAPA 2: Spillover — decreto + provincia de Valdivia (Los Ríos)
comunas_valdivia <- c(
  "14101", "14102", "14103", "14104", "14105",
  "14106", "14107", "14108", "14109"
)
comunas_decreto_ampliada <- c(comunas_decreto, comunas_valdivia)

# CAPA 3: Núcleo histórico de alta intensidad del conflicto mapuche (21 comunas)
comunas_nucleo <- c(
  "09201", "08202", "08203", "09121", "09203", "08205", "09104",
  "09204", "09105", "09106", "09107", "09108", "08201", "09109",
  "09205", "09111", "09112", "09101", "08207", "09211", "09119"
)
comunas_nucleo_conflicto <- comunas_nucleo

cat("Comunas decreto estricto:", length(comunas_decreto), "\n")
cat("Comunas decreto ampliado:", length(comunas_decreto_ampliada), "\n")
cat("Comunas núcleo histórico:", length(comunas_nucleo), "\n\n")

# ── 3. Recodificación de missing ───────────────────────────────────────────────

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
  "d5_1", "d5_2", "d6_1",
  "d13", "d14"
)

recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  xv <- if (inherits(x, "haven_labelled")) {
    as.integer(haven::zap_labels(x))
  } else {
    as.integer(x)
  }
  if_else(xv %in% miss_vals, NA_integer_, xv)
}

panel <- panel_raw |>
  mutate(across(
    all_of(intersect(vars_sustantivas, names(panel_raw))),
    recode_missing
  ))

cat("Missing recodificados.\n")

# ── 4. Panel balanceado (4 olas) ─────────────────────────────────────────────

panel <- panel |>
  group_by(folio) |>
  filter(n_distinct(ola) == 4) |>
  ungroup()

# ── 5. Identidad y sociodemografía fijadas desde ola 1 ────────────────────────

identidad_ola1 <- panel |>
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
    urbano_rural = as.factor(as.character(urbano_rural)),
    comuna_cod = str_pad(as.character(comuna), 5, pad = "0")
  )

# ── 6. Colapso ordinal A (ítem → índice) ──────────────────────────────────────

likert_sym_item <- function(x) {
  case_when(
    x %in% c(1, 2)    ~ 1L,
    x == 3            ~ 2L,
    x %in% c(4, 5)    ~ 3L,
    TRUE              ~ NA_integer_
  )
}

lab_ord_A <- c("1" = "Rechaza", "2" = "Neutral", "3" = "Justifica")

factor_ord_A <- function(x) {
  factor(
    pmax(1L, pmin(3L, round(x))),
    levels = 1:3,
    labels = lab_ord_A,
    ordered = TRUE
  )
}

# ── 7. Panel completo con variables derivadas ─────────────────────────────────

panel_completo <- panel |>
  select(-any_of(c(
    "indigeneous", "cat_indi", "mujer", "urbano_rural"
  ))) |>
  left_join(identidad_ola1, by = "folio") |>
  mutate(
    # Geografía — 4 capas
    zona_decreto = factor(
      if_else(comuna_cod %in% comunas_decreto, "decreto", "fuera"),
      levels = c("fuera", "decreto")
    ),
    zona_decreto_ampliada = factor(
      case_when(
        comuna_cod %in% comunas_decreto  ~ "decreto",
        comuna_cod %in% comunas_valdivia ~ "adyacente",
        TRUE                             ~ "fuera"
      ),
      levels = c("fuera", "adyacente", "decreto")
    ),
    nucleo_conflicto = factor(
      if_else(comuna_cod %in% comunas_nucleo, "nucleo", "periferia"),
      levels = c("periferia", "nucleo")
    ),
    region_conflicto = factor(
      case_when(
        str_sub(comuna_cod, 1, 2) == "08" ~ "Biobío",
        str_sub(comuna_cod, 1, 2) == "09" ~ "La Araucanía",
        str_sub(comuna_cod, 1, 2) == "14" ~ "Los Ríos",
        TRUE                              ~ "Otras regiones"
      ),
      levels = c("Otras regiones", "Biobío", "La Araucanía", "Los Ríos")
    ),
    # Compatibilidad scripts 02–05 (lejos/cerca = fuera/decreto)
    cerca_conflicto = factor(
      if_else(comuna_cod %in% comunas_decreto, "cerca", "lejos"),
      levels = c("lejos", "cerca")
    ),

    # Edad (g18)
    edad = factor(
      case_when(
        g18 %in% 18:24 ~ "18_24",
        g18 %in% 25:34 ~ "25_34",
        g18 %in% 35:44 ~ "35_44",
        g18 %in% 45:54 ~ "45_54",
        g18 %in% 55:64 ~ "55_64",
        g18 %in% 65:89 ~ "65+",
        TRUE           ~ NA_character_
      ),
      levels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+")
    ),

    # Controles sustantivos
    perc_desigualdad = 6L - as.integer(c22),
    perc_injusticia  = 6L - as.integer(c23),
    malestar_diferen = as.numeric(c24),
    apoyo_movil      = as.numeric(c25),
    id_chile         = as.numeric(a6),
    id_causa         = as.numeric(d6_1),

    # Índices continuos 1–5
    # Represión estatal: ítem único d3_1 (Carabineros repriman); d3_2 excluido (vigilantismo)
    idx_represion_estatal = as.numeric(d3_1),
    idx_vio_control       = as.numeric(d3_1),  # alias para scripts/paper
    idx_vio_resguardo     = rowMeans(pick(d4_2, d4_3), na.rm = TRUE),
    # Solo apéndice A7 — índice dual d3_1 + d3_2 (sensibilidad)
    idx_vio_control_dual  = rowMeans(pick(d3_1, d3_2), na.rm = TRUE),

    # ── Justicia procedimental: estructura ingroup/outgroup ────────────
    #
    # d5_1 = "Carabineros tratan a INDÍGENAS con respeto"
    # d5_2 = "Carabineros tratan a NO INDÍGENAS con respeto"
    #
    # Escala original: 1 = muy en desacuerdo ... 5 = muy de acuerdo
    # Mayor valor = percibe MÁS respeto/justicia

    # Percepción de justicia hacia MI grupo
    just_proc_ingroup = case_when(
      indigeneous == "indi"    ~ as.numeric(d5_1),
      indigeneous == "no_indi" ~ as.numeric(d5_2),
      TRUE                     ~ NA_real_
    ),

    # Percepción de justicia hacia el OTRO grupo
    just_proc_outgroup = case_when(
      indigeneous == "indi"    ~ as.numeric(d5_2),
      indigeneous == "no_indi" ~ as.numeric(d5_1),
      TRUE                     ~ NA_real_
    ),

    # Brecha percibida: outgroup − ingroup
    # Positivo = "tratan MEJOR al otro grupo que al mío" (agravio)
    # Negativo = "tratan MEJOR a mi grupo" (privilegio percibido)
    # Cero     = "tratan igual"
    brecha_just_proc = just_proc_outgroup - just_proc_ingroup,

    # Mantener idx_just_proc promediado para comparabilidad
    # (pero ya NO es el mediador principal)
    idx_just_proc = rowMeans(pick(d5_1, d5_2), na.rm = TRUE),

    # Ítems colapsados — esquema A
    d3_1_ord = likert_sym_item(d3_1),
    d3_2_ord = likert_sym_item(d3_2),
    d4_2_ord = likert_sym_item(d4_2),
    d4_3_ord = likert_sym_item(d4_3),

    # Índices ordinales (1–3); control = ítem único d3_1
    idx_vio_control_ord   = as.numeric(d3_1_ord),
    idx_vio_resguardo_ord = rowMeans(pick(d4_2_ord, d4_3_ord), na.rm = TRUE),

    # Categorías ordenadas (redondeo post-ítem)
    justifica_control_cat   = factor_ord_A(idx_vio_control_ord),
    justifica_resguardo_cat = factor_ord_A(idx_vio_resguardo_ord),

    # Categorías desde índice continuo (tablas descriptivas / sensibilidad)
    justifica_control_cont = factor(
      case_when(
        idx_vio_control <= 2   ~ "Rechaza",
        idx_vio_control == 3   ~ "Neutral",
        idx_vio_control >= 4   ~ "Justifica",
        TRUE                   ~ NA_character_
      ),
      levels = c("Rechaza", "Neutral", "Justifica"),
      ordered = TRUE
    ),
    justifica_resguardo_cont = factor(
      case_when(
        idx_vio_resguardo <= 2 ~ "Rechaza",
        idx_vio_resguardo == 3 ~ "Neutral",
        idx_vio_resguardo >= 4 ~ "Justifica",
        TRUE                   ~ NA_character_
      ),
      levels = c("Rechaza", "Neutral", "Justifica"),
      ordered = TRUE
    ),

    # Período (solo olas 2–4; NA en ola 1)
    periodo = factor(
      case_when(
        ola == 2 ~ "pre",
        ola == 3 ~ "estallido",
        ola == 4 ~ "decreto",
        TRUE     ~ NA_character_
      ),
      levels = c("pre", "estallido", "decreto")
    ),
    T1_estallido = as.integer(ola == 3),
    T2_decreto   = as.integer(ola == 4),
    post_decreto = as.integer(ola == 4),
    tratamiento  = as.integer(ola == 4 & cerca_conflicto == "cerca"),
    ola_num      = as.integer(ola)
  ) |>
  select(
    folio, ola, ola_num, periodo, tratamiento,
    T1_estallido, T2_decreto, post_decreto,
    indigeneous, cat_indi, mujer, edad, urbano_rural,
    comuna, comuna_cod,
    zona_decreto, zona_decreto_ampliada,
    cerca_conflicto, nucleo_conflicto, region_conflicto,
    id_chile, id_indi = a4,
    vio_ctrl_carb = d3_1, vio_ctrl_agric = d3_2,
    vio_camb_tierras = d4_2, vio_camb_cortes = d4_3,
    d3_1_ord, d3_2_ord, d4_2_ord, d4_3_ord,
    just_proc_indi = d5_1, just_proc_noindi = d5_2,
    just_proc_ingroup, just_proc_outgroup, brecha_just_proc,
    id_causa = d6_1,
    voto_participa = d13, voto_opcion = d14,
    perc_desigualdad, perc_injusticia, malestar_diferen, apoyo_movil,
    idx_represion_estatal, idx_vio_control, idx_vio_control_dual,
    idx_vio_resguardo, idx_just_proc,
    idx_vio_control_ord, idx_vio_resguardo_ord,
    justifica_control_cat, justifica_resguardo_cat,
    justifica_control_cont, justifica_resguardo_cont
  )

# ── Verificación inversión c22/c23 ────────────────────────────────────────────

cat("\n--- Verificación inversión c22/c23 ---\n")
cat("Rango perc_desigualdad:",
    paste(range(panel_completo$perc_desigualdad, na.rm = TRUE), collapse = " – "), "\n")
cat("Rango perc_injusticia: ",
    paste(range(panel_completo$perc_injusticia, na.rm = TRUE), collapse = " – "), "\n")
cat("Esperado: rango 1–5 (mayor = más desigualdad / injusticia percibida)\n")

saveRDS(panel_completo, "data/panel_completo.rds")
cat("\n✓ Panel completo guardado:", nrow(panel_completo), "obs,",
    ncol(panel_completo), "vars\n")

# ── 8. Subset analítico: olas 2–4 ─────────────────────────────────────────────

subset_data <- panel_completo |>
  filter(ola %in% c(2, 3, 4)) |>
  filter(!is.na(indigeneous))

# Colinealidad urbano_rural × zona (usa cerca_conflicto = alias decreto)
cor_ur_zd <- cor(
  as.numeric(subset_data$urbano_rural),
  as.numeric(subset_data$cerca_conflicto),
  use = "complete.obs"
)
incluir_urbano_rural <- abs(cor_ur_zd) <= 0.5

controles_base <- if (incluir_urbano_rural) {
  "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
} else {
  "mujer + edad + id_chile + id_causa + perc_desigualdad + malestar_diferen + apoyo_movil"
}

cat("\nCorrelación urbano_rural × zona_decreto:", round(cor_ur_zd, 3), "\n")
cat("Incluir urbano_rural en modelos:", incluir_urbano_rural, "\n")
cat("Controles base:", controles_base, "\n")

saveRDS(subset_data, "data/subset_data.rds")
cat("✓ Subset analítico guardado:", nrow(subset_data), "obs\n")

# ── 9. Subset placebo: olas 1–2 ───────────────────────────────────────────────

subset_placebo_pre <- panel_completo |>
  filter(ola %in% c(1, 2)) |>
  filter(!is.na(indigeneous)) |>
  mutate(
    periodo_placebo = factor(
      if_else(ola == 1, "pre1", "pre2"),
      levels = c("pre1", "pre2")
    ),
    T_placebo = as.integer(ola == 2)
  ) |>
  select(
    folio, ola, periodo_placebo, T_placebo,
    indigeneous, cat_indi, mujer, edad, urbano_rural,
    comuna_cod, zona_decreto, cerca_conflicto, nucleo_conflicto,
    region_conflicto,
    idx_vio_control, idx_vio_resguardo, idx_just_proc,
    idx_vio_control_ord, idx_vio_resguardo_ord,
    justifica_control_cat, justifica_resguardo_cat,
    just_proc_ingroup, just_proc_outgroup, brecha_just_proc,
    id_chile, id_causa,
    perc_desigualdad, perc_injusticia, malestar_diferen, apoyo_movil
  )

saveRDS(subset_placebo_pre, "data/subset_placebo_pre.rds")
cat("✓ Placebo pre guardado:", nrow(subset_placebo_pre), "obs\n")

# ── 10. Metadata para scripts posteriores ───────────────────────────────────────

analysis_metadata <- list(
  controles_base         = controles_base,
  incluir_urbano_rural   = incluir_urbano_rural,
  comunas_decreto        = comunas_decreto,
  comunas_decreto_ampliada = comunas_decreto_ampliada,
  comunas_nucleo         = comunas_nucleo,
  comunas_valdivia       = comunas_valdivia,
  referencia_periodo     = "pre",
  referencia_indigeneous = "no_indi",
  referencia_zona        = "fuera",
  referencia_cerca       = "lejos",
  esquema_ordinal        = "A_simetrico_1-2_3_4-5",
  timeline = tibble::tribble(
    ~ola, ~ano_campo,              ~rol,
    1,    "2016",                  "Placebo pre-todo",
    2,    "2018",                  "Baseline DiD [REF]",
    3,    "dic2020–may2021",       "Resabio estallido",
    4,    "2023",                  "Decreto + derrota Apruebo"
  )
)

saveRDS(analysis_metadata, "data/analysis_metadata.rds")
cat("✓ Metadata guardada: data/analysis_metadata.rds\n")

# ── 11. Resumen final ───────────────────────────────────────────────────────────

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("RESUMEN 01_limpieza.R\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat("\n--- Timeline del diseño ---\n")
cat("Ola 1 (2016):          N =",
    sum(panel_completo$ola == 1), "— Pre-todo (solo placebo)\n")
cat("Ola 2 (2018):          N =",
    sum(subset_data$ola == 2), "— Baseline pre-estallido [REF]\n")
cat("Ola 3 (dic2020-may21): N =",
    sum(subset_data$ola == 3), "— Resabio estallido social\n")
cat("Ola 4 (2023):          N =",
    sum(subset_data$ola == 4), "— Decreto + derrota Apruebo\n")

cat("\n--- CAPA 1 — Decreto estricto (53 comunas) ---\n")
print(table(subset_data$zona_decreto, subset_data$ola))

cat("\n--- CAPA 2 — Decreto ampliado (+ Valdivia) ---\n")
print(table(subset_data$zona_decreto_ampliada, subset_data$ola))

cat("\n--- CAPA 3 — Núcleo histórico (21 comunas) ---\n")
print(table(subset_data$nucleo_conflicto, subset_data$ola))

cat("\n--- CAPA 4 — Región del conflicto ---\n")
print(table(subset_data$region_conflicto, subset_data$ola))

cat("\n--- Distribución ordinal A (ola 2, baseline) — control ---\n")
print(round(prop.table(table(
  subset_data |> filter(ola == 2) |> pull(justifica_control_cat),
  useNA = "ifany"
)), 3))

cat("\n--- Distribución ordinal A (ola 2) — resguardo ---\n")
print(round(prop.table(table(
  subset_data |> filter(ola == 2) |> pull(justifica_resguardo_cat),
  useNA = "ifany"
)), 3))

cat("\n--- Panel balanceado ---\n")
cat("Individuos en las 3 olas analíticas:",
    subset_data |> count(folio) |> filter(n == 3) |> nrow(), "\n")
cat("N variables en subset_data:", ncol(subset_data), "\n")

cat("\n--- Justicia procedimental ingroup/outgroup (baseline ola 2) ---\n")
subset_data |>
  filter(ola == 2) |>
  group_by(indigeneous) |>
  summarise(
    just_ingroup_media  = mean(just_proc_ingroup, na.rm = TRUE),
    just_outgroup_media = mean(just_proc_outgroup, na.rm = TRUE),
    brecha_media        = mean(brecha_just_proc, na.rm = TRUE),
    brecha_sd           = sd(brecha_just_proc, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  print()

cat("\nInterpretación esperada:\n")
cat("  Indígenas: brecha > 0 (perciben mejor trato al outgroup)\n")
cat("  No indígenas: brecha ≈ 0 o < 0 (perciben trato igual o mejor a su grupo)\n")

cat("\n✓ 01_limpieza.R ejecutado correctamente.\n")
