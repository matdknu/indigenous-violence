# =============================================================================
# ELRI — Análisis longitudinal espejo: justificación de la violencia
# y voto Rechazo plebiscito 2022
# Versión 2 — corregida con libro de códigos completo
#
# Notas de corrección:
#   · Valores missing: 88/99 (olas 1-2) Y 8888/9999 (olas 3-4), más 77/66
#
#   VIOLENCIA DE CONTROL SOCIAL (Estado/civiles sobre indígenas):
#   · d3_1 = Carabineros disuelven protestas indígenas  → CONTROL, todas las olas
#   · d3_2 = Agricultores usan armas contra indígenas   → CONTROL, todas las olas
#   · d3_3 = Carabineros allanan comunidades            → EXCLUIDO (solo olas 1-2)
#
#   VIOLENCIA DE RESGUARDO SOCIAL (indígenas como agentes):
#   · d4_2 = Tomas de terrenos por indígenas            → RESGUARDO, todas las olas
#   · d4_3 = Bloqueo/corte de carreteras por indígenas  → RESGUARDO, todas las olas
#   · d4_1 = Ataques incendiarios por indígenas         → EXCLUIDO (solo olas 1-2)
#
#   VARIABLES DEPENDIENTES (modelos multinivel DiD):
#   · idx_vio_control  = d3_1 + d3_2  (violencia de control social)
#   · idx_vio_resguardo= d4_2 + d4_3  (violencia de resguardo social)
#
#   VARIABLE INDEPENDIENTE como VI en modelo de voto:
#   · idx_just_proc    = d5_1 + d5_2  (justicia procedimental Carabineros)
#
#   CONTROLES en modelos DiD:
#   · mujer, edad, id_chile (a6), id_causa (d6_1)
#   · c22_inv: percepción de desigualdad (invertida: 1=igual, 5=mucho peor)
#   · c23_inv: percepción de injusticia  (invertida: 1=muy justa, 5=muy injusta)
#   · urbano_rural: se testea colinealidad con cerca_conflicto antes de incluir
#
#   MODELO DE VOTO (logística, ola 4):
#   · VIs: idx_vio_control, idx_vio_resguardo, idx_just_proc,
#          c25 (apoyo movilizaciones), id_chile, id_causa,
#          indigeneous × cerca_conflicto, mujer, edad
#
#   · Identidad étnica fijada desde ola 1 (time-invariant)
#   · Análisis: olas 2 (pre), 3 (tratamiento), 4 (post)
# =============================================================================


# ── 0. PAQUETES ───────────────────────────────────────────────────────────────

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyverse, stringr, haven,
  lme4, lmerTest,        # modelos multinivel
  performance,           # ICC
  psych,                 # alfa de Cronbach
  sjlabelled, sjmisc,    # etiquetas SPSS/Stata
  ggplot2, viridis,      # visualización
  broom.mixed,           # tidy() para lme4
  modelsummary           # tablas exportables
)

cat("\014"); rm(list = ls()); gc()

# Crear carpeta de outputs si no existe
if (!dir.exists("output")) dir.create("output")


# ── 1. CARGAR DATOS ───────────────────────────────────────────────────────────

load("data/BBDD_ELRI_LONG.RData")
data <- BBDD_ELRI_LONG


# ── 2. COMUNAS EN ZONA DE CONFLICTO / ESTADO DE EXCEPCIÓN ────────────────────
# Estado de excepción constitucional de emergencia decretado en octubre 2021
# (activo durante la ola 3). Comunas de La Araucanía y zonas aledañas.

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
      levels = c("lejos", "cerca")   # "lejos" = referencia en modelos
    )
  )


# ── 3. PANEL BALANCEADO: solo individuos presentes en las 4 olas ──────────────

data <- data |>
  group_by(folio) |>
  filter(n_distinct(ola) == 4) |>
  ungroup()


# ── 4. RECODIFICAR VALORES MISSING ───────────────────────────────────────────
# El libro de códigos muestra inconsistencia entre olas:
#   Olas 1-2: missing = 88 / 99   (2 dígitos)
#   Olas 3-4: missing = 8888/9999 (4 dígitos)
# Adicionalmente:
#   77 = "no aplica"       → NA
#   66 = "no tiene amigos" → NA (para variables de contacto)
# Se neutralizan todos con una sola pasada.

vars_sustantivas <- c(
  # Módulo A: Identidad y lengua
  "a4", "a5", "a6", "a7",
  # Módulo C: Relaciones interculturales
  "c1", "c2", "c3_1", "c3_2",
  "c4", "c5", "c6_1", "c6_2",
  "c7_1", "c7_2", "c7_3",
  "c8", "c9", "c10", "c11",
  "c12", "c13", "c14", "c15", "c16",
  "c17_1", "c17_2", "c17_4",
  "c18", "c19",
  "c21_2", "c21_4",
  "c22", "c23", "c24", "c25",
  "c26_1", "c26_2", "c26_3",
  "c27_1", "c27_3",
  "c28_1", "c28_2", "c28_3", "c28_4", "c28_5", "c28_6",
  "c29_1", "c30_1",
  "c31_1", "c31_2",
  "c32_1", "c32_2",
  "c33_1", "c33_2",
  "c34_1", "c35", "c36",
  # Módulo D: Conflicto e ideología — variables clave del análisis
  "d1_1", "d1_2", "d1_3",
  "d2_1", "d2_3",
  "d3_1",   # VIO_CONTROL: fuerza Carabineros en protestas indígenas
  "d3_2",   # VIO_CONTROL: agricultores usan armas contra indígenas
  "d4_2",   # VIO_RESGUARDO: tomas de terrenos
  "d4_3",   # VIO_RESGUARDO: bloqueo/corte de carreteras
  "d5_1",   # JUST_PROC: Carabineros tratan bien a indígenas
  "d5_2",   # JUST_PROC: Carabineros tratan bien a no-indígenas
  "d5_4",   # OBEDIENCIA: debo acatar decisiones de Carabineros
  "d6_1",   # ID_CAUSA: identificación con la causa indígena
  # Percepción de desigualdad y movilización (Módulo C)
  "c22",    # condiciones de vida indígenas vs no-indígenas (1=mucho peor, 5=mucho mejor)
  "c23",    # diferencia justa o injusta (1=muy injusta, 5=muy justa)
  "c24",    # malestar ante diferencias (1=nada molesto, 5=muy molesto)
  "c25",    # apoyo a movilizaciones indígenas (1=nada, 5=mucho)
  # Participación y voto plebiscito (olas 3-4)
  "d13",    # ¿Votó en el plebiscito de salida? (1=Sí, 2=No)
  "d14"     # ¿Por cuál opción? (1=Apruebo, 2=Rechazo, 3=Nulo/Blanco) — solo ola 4
)

# Función que convierte TODOS los códigos de missing a NA
recode_missing <- function(x) {
  miss_vals <- c(66L, 77L, 88L, 99L, 8888L, 9999L)
  if_else(x %in% miss_vals, NA_integer_, as.integer(x))
}

data <- data |>
  mutate(across(
    all_of(intersect(vars_sustantivas, names(data))),
    recode_missing
  ))


# ── 5. FIJAR IDENTIDAD ÉTNICA DESDE OLA 1 (time-invariant) ───────────────────
# Algunos respondentes cambian de categoría étnica entre olas.
# Se ancla en ola 1 como decisión metodológica estándar en estudios
# de identidad con diseño longitudinal.

identidad_ola1 <- data |>
  filter(ola == 1) |>
  transmute(
    folio,
    
    # Indígena vs. no indígena (variable espejo)
    indigeneous = factor(
      case_when(
        a1 %in% 1:11 ~ "indi",
        a1 == 12     ~ "no_indi",
        TRUE         ~ NA_character_
      ),
      levels = c("no_indi", "indi")   # no_indi = referencia
    ),
    
    # Categoría étnica más detallada
    cat_indi = factor(case_when(
      a1 == 1                       ~ "mapuche",
      a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
      a1 == 12                      ~ "chileno_noindig",
      TRUE                          ~ "otro_indigena"
    ), levels = c("chileno_noindig", "mapuche", "andino", "otro_indigena")),
    
    # Sexo (time-invariant)
    mujer = factor(
      if_else(g2 == 2, "mujer", "hombre"),
      levels = c("hombre", "mujer")   # hombre = referencia
    ),
    
    # Zona y urbano/rural (time-invariant)
    cerca_conflicto,
    urbano_rural
  )

# Reemplazar versión time-variant por la fija desde ola 1
data <- data |>
  select(-any_of(c("indigeneous", "cat_indi", "mujer"))) |>
  left_join(identidad_ola1,
            by = c("folio", "cerca_conflicto", "urbano_rural"))


# ── 6. SUBSET ANALÍTICO: OLAS 2, 3 Y 4 ──────────────────────────────────────
# Ola 2 = pre  (baseline, antes del estallido y estado de excepción)
# Ola 3 = tratamiento activo (estallido social + estado de excepción, 2021)
# Ola 4 = post (seguimiento post-tratamiento)

subset_data <- data |>
  filter(ola %in% c(2, 3, 4)) |>
  mutate(
    
    # Edad time-variant (puede cambiar entre olas, es válido)
    edad = factor(case_when(
      g18 %in% 18:24 ~ "18_24",
      g18 %in% 25:34 ~ "25_34",
      g18 %in% 35:44 ~ "35_44",
      g18 %in% 45:54 ~ "45_54",
      g18 %in% 55:64 ~ "55_64",
      g18 %in% 65:89 ~ "65+"
    ), levels = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+")),
    
    # Período para DiD — ola 2 es la referencia
    periodo = factor(
      case_when(
        ola == 2 ~ "pre",
        ola == 3 ~ "tratamiento",
        ola == 4 ~ "post"
      ),
      levels = c("pre", "tratamiento", "post")
    ),
    
    # Dummy de tratamiento activo:
    # 1 = individuo en zona de excepción en ola 3 (estado de excepción activo)
    # 0 = todo lo demás
    tratamiento = as.integer(ola == 3 & cerca_conflicto == "cerca"),
    
    # ── Índices compuestos ───────────────────────────────────────────────────
    
    # Violencia de CONTROL social (Estado/civiles justificando violencia sobre indígenas)
    # d3_1: uso de fuerza de Carabineros para disolver protestas indígenas
    # d3_2: agricultores que usan armas para enfrentar grupos indígenas
    # [d3_3 excluido: allanamiento de comunidades, solo en olas 1-2]
    idx_vio_control = rowMeans(pick(d3_1, d3_2), na.rm = TRUE),
    
    # Violencia de RESGUARDO social (indígenas como agentes de la violencia)
    # d4_2: tomas de terrenos por grupos indígenas
    # d4_3: bloqueo o corte de carreteras por grupos indígenas
    # [d4_1 excluido: ataques incendiarios, solo en olas 1-2]
    idx_vio_resguardo = rowMeans(pick(d4_2, d4_3), na.rm = TRUE),
    
    # Justicia PROCEDIMENTAL de Carabineros (trato igualitario entre grupos)
    # d5_1: Carabineros tratan con respeto a personas indígenas
    # d5_2: Carabineros tratan con respeto a personas no-indígenas
    idx_just_proc = rowMeans(pick(d5_1, d5_2), na.rm = TRUE),
    
    # ── Variables de desigualdad y movilización ────────────────────────────
    # c22: invertir para que mayor valor = mayor desigualdad percibida
    # original: 1=mucho peor, 5=mucho mejor → invertido: 1=igual/mejor, 5=mucho peor
    c22_inv = 6L - as.integer(c22),   # percepción desigualdad (1=mínima, 5=máxima)
    
    # c23: invertir para que mayor valor = mayor percepción de injusticia
    # original: 1=muy injusta, 5=muy justa → invertido: 1=muy justa, 5=muy injusta
    c23_inv = 6L - as.integer(c23),   # percepción injusticia (1=mínima, 5=máxima)
    
    # c24 ya está en sentido intuitivo: 1=nada molesto, 5=muy molesto
    # c25 ya está en sentido intuitivo: 1=nada de apoyo, 5=mucho apoyo
    
    # Justicia procedimental — pasa a ser VI (no VD) en modelo de voto
    idx_just_proc = rowMeans(pick(d5_1, d5_2), na.rm = TRUE),
    
    # Número de ola como entero (útil en modelos de tendencia)
    ola_num = as.integer(ola)
    
  ) |>
  select(
    folio, ola, ola_num, periodo, tratamiento,
    indigeneous, cat_indi, cerca_conflicto, urbano_rural,
    mujer, edad,
    
    # Identidad
    id_chile = a6,           # identificación con Chile (1=muy poco, 5=mucho)
    id_indi  = a4,           # identificación con pueblo originario
    
    # Variables clave de violencia (ítems individuales)
    vio_ctrl_carb    = d3_1, # fuerza Carabineros en protestas indígenas
    vio_ctrl_agric   = d3_2, # agricultores usan armas contra indígenas
    vio_camb_tierras = d4_2, # tomas de terrenos
    vio_camb_cortes  = d4_3, # bloqueo/corte de carreteras
    
    # Justicia procedimental
    # Nota: d5_4 (obediencia institucional) solo existe en olas 1-2,
    # genera 3210 NAs en el panel olas 2-3-4 → excluida del análisis
    just_proc_indi   = d5_1, # Carabineros respetan a indígenas
    just_proc_noindi = d5_2, # Carabineros respetan a no-indígenas
    
    # Identificación con la causa indígena
    id_causa = d6_1,
    
    # Contacto intergrupal (variables de control)
    # contacto_noind = c7_3,  # disponible pero no en modelos principales
    # contacto_ind   = c14,   # disponible pero no en modelos principales
    
    # Participación y voto plebiscito (d13 = olas 3-4; d14 = solo ola 4)
    voto_participa = d13,  # ¿Votó en el plebiscito de salida? (1=Sí, 2=No)
    voto_opcion    = d14,  # Opción (1=Apruebo, 2=Rechazo, 3=Nulo/Blanco)
    
    # Percepción de desigualdad, injusticia, malestar y movilización
    perc_desigualdad = c22_inv,  # 1=mínima desigualdad percibida, 5=máxima
    perc_injusticia  = c23_inv,  # 1=muy justa, 5=muy injusta
    malestar_diferen = c24,      # 1=nada molesto, 5=muy molesto
    apoyo_movil      = c25,      # 1=nada de apoyo, 5=mucho apoyo
    
    # Índices compuestos
    idx_vio_control, idx_vio_resguardo, idx_just_proc
  )


# ── 7. VERIFICACIÓN BÁSICA ────────────────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("VERIFICACIÓN DEL SUBSET ANALÍTICO\n")
cat(strrep("=", 60), "\n\n")

cat("N individuos únicos:", n_distinct(subset_data$folio), "\n")
cat("N observaciones totales:", nrow(subset_data), "\n\n")

cat("--- Distribución por período e identidad ---\n")
print(table(subset_data$periodo, subset_data$indigeneous, useNA = "ifany"))

cat("\n--- Distribución por período y zona ---\n")
print(table(subset_data$periodo, subset_data$cerca_conflicto, useNA = "ifany"))

cat("\n--- Tabla 2×2 DiD: identidad × zona (referencia para DiD) ---\n")
print(table(subset_data$indigeneous, subset_data$cerca_conflicto, useNA = "ifany"))

cat("\n--- NAs por variable de interés ---\n")
vars_check <- c("idx_vio_control", "idx_vio_resguardo", "idx_just_proc",
                "perc_desigualdad", "perc_injusticia", "apoyo_movil",
                "id_causa", "id_chile")
print(colSums(is.na(subset_data[, vars_check])))


# ── 8. ESTADÍSTICOS DESCRIPTIVOS POR GRUPO Y PERÍODO ─────────────────────────

desc_grupo <- subset_data |>
  group_by(indigeneous, periodo) |>
  summarise(
    n                  = n(),
    vio_control_m      = round(mean(idx_vio_control,   na.rm = TRUE), 2),
    vio_control_sd     = round(sd(idx_vio_control,     na.rm = TRUE), 2),
    vio_resguardo_m    = round(mean(idx_vio_resguardo, na.rm = TRUE), 2),
    vio_resguardo_sd   = round(sd(idx_vio_resguardo,   na.rm = TRUE), 2),
    just_proc_m        = round(mean(idx_just_proc,     na.rm = TRUE), 2),
    just_proc_sd       = round(sd(idx_just_proc,       na.rm = TRUE), 2),
    # obediencia excluida (d5_4 no disponible en olas 2-3-4)
    id_causa_m         = round(mean(id_causa,          na.rm = TRUE), 2),
    perc_desig_m       = round(mean(perc_desigualdad, na.rm = TRUE), 2),
    perc_injust_m      = round(mean(perc_injusticia,  na.rm = TRUE), 2),
    apoyo_movil_m      = round(mean(apoyo_movil,      na.rm = TRUE), 2),
    just_proc_m        = round(mean(idx_just_proc,    na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- Descriptivos por identidad y período ---\n")
print(desc_grupo)

desc_zona <- subset_data |>
  group_by(cerca_conflicto, periodo) |>
  summarise(
    n               = n(),
    vio_control_m   = round(mean(idx_vio_control,   na.rm = TRUE), 2),
    vio_resguardo_m = round(mean(idx_vio_resguardo, na.rm = TRUE), 2),
    just_proc_m     = round(mean(idx_just_proc,     na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- Descriptivos por zona y período ---\n")
print(desc_zona)


# ── 9. CONSISTENCIA INTERNA (alfa de Cronbach) ────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("CONSISTENCIA INTERNA (alfa de Cronbach)\n")
cat(strrep("=", 60), "\n\n")

cat("Violencia de CONTROL social (d3_1 + d3_2):\n")
alpha_ctrl <- psych::alpha(
  subset_data[, c("vio_ctrl_carb", "vio_ctrl_agric")], check.keys = TRUE
)
cat("  alfa =", round(alpha_ctrl$total$raw_alpha, 3), "\n\n")

cat("Violencia de RESGUARDO social (d4_2 + d4_3):\n")
alpha_resg <- psych::alpha(
  subset_data[, c("vio_camb_tierras", "vio_camb_cortes")], check.keys = TRUE
)
cat("  alfa =", round(alpha_resg$total$raw_alpha, 3), "\n\n")

cat("Justicia procedimental (d5_1 + d5_2):\n")
alpha_just <- psych::alpha(
  subset_data[, c("just_proc_indi", "just_proc_noindi")], check.keys = TRUE
)
cat("  alfa =", round(alpha_just$total$raw_alpha, 3), "\n\n")

cat("Nota: Con solo 2 ítems, alfa equivale a la correlación entre ítems.\n")
cat("Correlaciones entre ítems:\n")
cat("  Vio. control:   r =",
    round(cor(subset_data$vio_ctrl_carb, subset_data$vio_ctrl_agric,
              use = "complete.obs"), 3), "\n")
cat("  Vio. resguardo: r =",
    round(cor(subset_data$vio_camb_tierras, subset_data$vio_camb_cortes,
              use = "complete.obs"), 3), "\n")
cat("  Just. proc.:    r =",
    round(cor(subset_data$just_proc_indi, subset_data$just_proc_noindi,
              use = "complete.obs"), 3), "\n")


# ── 10. VISUALIZACIÓN: TRAYECTORIAS POR GRUPO ────────────────────────────────

tray_long <- subset_data |>
  filter(!is.na(indigeneous)) |>  # excluir NA identidad del gráfico
  group_by(indigeneous, cerca_conflicto, periodo) |>
  summarise(
    vio_control   = mean(idx_vio_control,   na.rm = TRUE),
    vio_resguardo = mean(idx_vio_resguardo, na.rm = TRUE),
    just_proc     = mean(idx_just_proc,     na.rm = TRUE),
    apoyo_movil   = mean(apoyo_movil,       na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(vio_control, vio_resguardo, just_proc, apoyo_movil),
    names_to = "indice", values_to = "media"
  ) |>
  mutate(
    indice = factor(indice,
                    levels = c("vio_control", "vio_resguardo", "just_proc", "apoyo_movil"),
                    labels = c(
                      "Justif. violencia\n(control social)",
                      "Justif. violencia\n(resguardo social)",
                      "Justicia\nprocedimental (VI)",
                      "Apoyo a\nmovilizaciones"
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
                     color = grupo, linetype = grupo, group = grupo)) +
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
    labels = c("pre" = "Ola 2\n(Pre)",
               "tratamiento" = "Ola 3\n(Tratamiento)",
               "post" = "Ola 4\n(Post)")
  ) +
  labs(
    title    = "Trayectorias longitudinales por grupo identitario y zona",
    subtitle = "ELRI — Olas 2 (pre), 3 (estado de excepción), 4 (post)",
    x = NULL, y = "Media (escala 1–5)",
    caption  = "Línea sólida = zona de excepción · Línea punteada = lejos del conflicto"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2))

print(p_tray)
ggsave("output/fig_trayectorias.png", p_tray,
       width = 11, height = 8, dpi = 300)

cat("✓ Figura guardada: output/fig_trayectorias.png\n")


# ── 11. MODELOS MULTINIVEL CON DiD ────────────────────────────────────────────
# Estructura: mediciones (nivel 1) anidadas en individuos (nivel 2)
# Referencia: no_indi, pre, lejos_conflicto, hombre, 18_24
#
# Modelos:
#   M0: nulo (solo ICC)
#   M1: efectos principales de período e identidad + controles
#   M2: DiD — interacción triple período × identidad × zona
#
# La interacción triple captura si el estado de excepción (ola 3, zona cerca)
# afecta de forma diferencial a indígenas vs. no indígenas.

cat("\n", strrep("=", 60), "\n")
cat("MODELOS MULTINIVEL (lme4 / lmerTest)\n")
cat(strrep("=", 60), "\n\n")


# ── 11a. Violencia de CONTROL (Estado) ────────────────────────────────────────

cat("--- Modelo: Justificación de violencia de CONTROL ---\n\n")

m0_ctrl <- lmer(idx_vio_control ~ 1 + (1 | folio),
                data = subset_data, REML = TRUE)
cat("ICC violencia control:", round(as.numeric(performance::icc(m0_ctrl)$ICC_adjusted), 3), "\n\n")

# Controles base — se añade urbano_rural condicionalmente
# ── TEST DE COLINEALIDAD: urbano_rural × cerca_conflicto ─────────────────────
# Las comunas del estado de excepción son predominantemente rurales.
# Si la correlación es alta (|r| > .5), urbano_rural absorbe parte del
# efecto de cerca_conflicto y debe excluirse del modelo.

cat("\n", strrep("=", 60), "\n")
cat("TEST COLINEALIDAD: urbano_rural × cerca_conflicto\n")
cat(strrep("=", 60), "\n\n")

ur_num  <- as.numeric(subset_data$urbano_rural)
cc_num  <- as.numeric(subset_data$cerca_conflicto)
cor_ur_cc <- cor(ur_num, cc_num, use = "complete.obs")
cat("Correlación urbano_rural ~ cerca_conflicto: r =", round(cor_ur_cc, 3), "\n")

incluir_urbano_rural <- abs(cor_ur_cc) <= 0.5
cat("Decisión:", ifelse(incluir_urbano_rural,
                        "INCLUIR urbano_rural (r <= .5, colinealidad aceptable)",
                        "EXCLUIR urbano_rural (r > .5, absorbe efecto zona)"), "\n\n")

cat("Distribución urbano_rural × cerca_conflicto:\n")
print(table(subset_data$urbano_rural, subset_data$cerca_conflicto, useNA = "ifany"))
cat("\n")

# Controles base — se añade urbano_rural condicionalmente
controles_base <- if (incluir_urbano_rural) {
  "mujer + edad + urbano_rural + id_chile + id_causa + perc_desigualdad + perc_injusticia"
} else {
  "mujer + edad + id_chile + id_causa + perc_desigualdad + perc_injusticia"
}
cat("Controles en modelos:", controles_base, "\n\n")

m1_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous + cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

m2_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("Comparación M1 vs M2 (LRT):\n")
print(anova(m1_ctrl, m2_ctrl))

cat("\nResumen M2 (DiD) — Violencia de control:\n")
print(summary(m2_ctrl))


# ── 11b. Violencia de RESGUARDO social ────────────────────────────────────────

cat("\n--- Modelo: Justificación de violencia de RESGUARDO social ---\n\n")

m0_resg <- lmer(idx_vio_resguardo ~ 1 + (1 | folio),
                data = subset_data, REML = TRUE)
cat("ICC violencia resguardo:", round(as.numeric(performance::icc(m0_resg)$ICC_adjusted), 3), "\n\n")

m1_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous + cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

m2_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("Comparación M1 vs M2 (LRT):\n")
print(anova(m1_resg, m2_resg))

cat("\nResumen M2 (DiD) — Violencia de resguardo:\n")
print(summary(m2_resg))


# ── 11c. Justicia procedimental ───────────────────────────────────────────────

# ── 11c. Justicia procedimental — exploratoria, no VD principal ──────────────
# idx_just_proc se usa como VI en el modelo de voto. Se estima aquí de forma
# exploratoria para describir su trayectoria, pero no forma parte de las VD
# principales del análisis DiD.

cat("\n--- Modelo exploratorio: Justicia procedimental (VI en voto) ---\n\n")

m0_just <- lmer(idx_just_proc ~ 1 + (1 | folio),
                data = subset_data, REML = TRUE)
cat("ICC justicia proc.:", round(as.numeric(performance::icc(m0_just)$ICC_adjusted), 3), "\n")
cat("(Nota: idx_just_proc es VI en modelo de voto, no VD principal)\n\n")

m2_just <- lmer(
  as.formula(paste(
    "idx_just_proc ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)

cat("Resumen exploratorio — Justicia procedimental:\n")
print(summary(m2_just))


# ── 11d. Obediencia institucional ────────────────────────────────────────────
# d5_4 solo aparece en olas 1-2 del cuestionario ELRI.
# En el panel olas 2-3-4 tiene 3210 NAs → modelo no estimable.
# Se excluye del análisis longitudinal.
# Si se desea explorar en ola 2 únicamente:
#   m_obed_ola2 <- lm(d5_4 ~ indigeneous * cerca_conflicto + mujer + edad,
#                     data = filter(data, ola == 2))


# ── 12. TABLA RESUMEN EXPORTABLE ──────────────────────────────────────────────

modelsummary(
  list(
    "Vio. control (M1)"      = m1_ctrl,
    "Vio. control (DiD)"     = m2_ctrl,
    "Vio. resguardo (M1)"    = m1_resg,
    "Vio. resguardo (DiD)"   = m2_resg,
    "Just. proc. (DiD)"      = m2_just
  ),
  statistic  = "({std.error})",
  stars      = TRUE,
  fmt        = 3,
  gof_omit   = "AIC|BIC|Log|REML|Num.Obs|R2",
  output     = "output/tabla_modelos.html"   # html no requiere pandoc
)

cat("✓ Tabla exportada: output/tabla_modelos.html\n")


# ── 13. VISUALIZACIÓN DE COEFICIENTES ─────────────────────────────────────────

coefs_all <- bind_rows(
  broom.mixed::tidy(m2_ctrl, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Vio. control"),
  broom.mixed::tidy(m2_resg, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Vio. resguardo"),
  broom.mixed::tidy(m2_just, effects = "fixed", conf.int = TRUE) |>
    mutate(modelo = "Just. proc.")
) |>
  filter(term != "(Intercept)") |>
  # Resaltar interacciones DiD (las más relevantes teóricamente)
  mutate(
    es_did = str_detect(term, ":"),
    modelo = factor(modelo,
                    levels = c("Vio. control", "Vio. resguardo",
                               "Just. proc."))
  )

p_coef <- ggplot(coefs_all,
                 aes(x = estimate,
                     y = reorder(term, estimate),
                     xmin = conf.low, xmax = conf.high,
                     color = es_did, alpha = es_did)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(position = position_dodge(width = 0.5), linewidth = 0.5) +
  facet_wrap(~ modelo, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c("FALSE" = "grey50", "TRUE" = "#D73027"),
    labels = c("Efecto principal", "Interacción DiD"),
    name = NULL
  ) +
  scale_alpha_manual(
    values = c("FALSE" = 0.6, "TRUE" = 1.0), guide = "none"
  ) +
  labs(
    title   = "Efectos fijos — Modelos DiD multinivel (ELRI)",
    subtitle = "Rojo = interacciones; IC 95%",
    x = "Coeficiente estimado", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold")
  )

print(p_coef)
ggsave("output/fig_coeficientes.png", p_coef,
       width = 13, height = 9, dpi = 300)

cat("✓ Figura guardada: output/fig_coeficientes.png\n")


# ── 14. MOVILIZACIÓN Y VOTO RECHAZO — PLEBISCITO 2022 ───────────────────────
# Hipótesis: el estado de excepción (ola 3, zona cerca) movilizó más a la
# población → esa movilización se tradujo en voto Rechazo en ola 4.
#
# Variables:
#   d13 (voto_participa): ¿Votó en el plebiscito? — olas 3 y 4
#                          1 = Sí  |  2 = No
#   d14 (voto_opcion):    ¿Por cuál opción?        — SOLO ola 4
#                          1 = Apruebo  |  2 = Rechazo  |  3 = Nulo/Blanco
#
# Dos modelos:
#   M_movil   : logística — ¿el tratamiento (DiD) predice haber votado?
#   M_rechazo : logística — entre quienes votaron, ¿predice voto Rechazo?
#
# Ambos se estiman solo con ola 4 (única ola con d14 disponible).
# voto_participa en ola 4 actúa como filtro para M_rechazo y como
# VD en M_movil.

cat("
", strrep("=", 60), "
")
cat("MOVILIZACIÓN Y VOTO RECHAZO — PLEBISCITO 2022 (ola 4)
")
cat(strrep("=", 60), "

")

# ── Preparar subset ola 4 ────────────────────────────────────────────────────

subset_ola4 <- subset_data |>
  filter(ola == 4) |>
  mutate(
    
    # ¿Votó en el plebiscito? (1=Sí → 1; 2=No → 0)
    voto_si = case_when(
      voto_participa == 1 ~ 1L,
      voto_participa == 2 ~ 0L,
      TRUE                ~ NA_integer_
    ),
    
    # Voto Rechazo (entre quienes votaron)
    # 1 = Rechazo | 0 = Apruebo o Nulo/Blanco
    voto_rechazo = case_when(
      voto_opcion == 2 ~ 1L,   # Rechazo
      voto_opcion == 1 ~ 0L,   # Apruebo
      voto_opcion == 3 ~ 0L,   # Nulo/Blanco (agrupado con No-Rechazo)
      TRUE             ~ NA_integer_
    ),
    
    # Rechazo solo vs Apruebo (excluye nulos, alternativa más restrictiva)
    voto_rechazo_strict = case_when(
      voto_opcion == 2 ~ 1L,
      voto_opcion == 1 ~ 0L,
      TRUE             ~ NA_integer_   # Nulo y No-votante → NA
    )
  )

cat("--- Distribución del voto en ola 4 ---
")
cat("¿Votó?
")
print(table(subset_ola4$voto_si, subset_ola4$indigeneous, useNA = "ifany"))
cat("
Opción de voto (entre votantes):
")
print(table(subset_ola4$voto_opcion, subset_ola4$indigeneous, useNA = "ifany"))
cat("
Voto Rechazo × zona × identidad:
")
print(table(subset_ola4$voto_rechazo, subset_ola4$cerca_conflicto,
            subset_ola4$indigeneous, useNA = "ifany"))


# ── Modelo 1: ¿El tratamiento predice movilización (haber votado)? ──────────
# VD: voto_si (1=votó, 0=no votó)
# La hipótesis es que vivir en zona de excepción siendo indígena aumenta
# la probabilidad de haber votado (movilización diferencial).

cat("
--- M_movil: Predicción de participación electoral ---
")

m_movil <- glm(
  voto_si ~
    indigeneous * cerca_conflicto +
    mujer + edad +
    idx_vio_control + idx_vio_resguardo +
    idx_just_proc + apoyo_movil +
    id_chile + id_causa,
  data   = subset_ola4,
  family = binomial(link = "logit")
)

cat("
Odds ratios — Movilización:
")
or_movil <- round(exp(cbind(OR = coef(m_movil), confint(m_movil))), 3)
print(or_movil)

cat("
Resumen completo:
")
print(summary(m_movil))


# ── Modelo 2: ¿El tratamiento predice voto Rechazo (entre votantes)? ────────
# VD: voto_rechazo (1=Rechazo, 0=Apruebo/Nulo)
# Solo entre quienes efectivamente votaron (voto_si == 1).
# Se incluyen los índices de violencia como mediadores potenciales.

cat("
--- M_rechazo: Predicción de voto Rechazo (entre votantes) ---
")

m_rechazo <- glm(
  voto_rechazo ~
    indigeneous * cerca_conflicto +
    mujer + edad +
    idx_vio_control + idx_vio_resguardo +
    idx_just_proc + apoyo_movil +
    id_chile + id_causa,
  data   = subset_ola4 |> filter(voto_si == 1),
  family = binomial(link = "logit")
)

cat("
Odds ratios — Voto Rechazo:
")
or_rechazo <- round(exp(cbind(OR = coef(m_rechazo), confint(m_rechazo))), 3)
print(or_rechazo)

cat("
Resumen completo:
")
print(summary(m_rechazo))


# ── Versión estricta: Rechazo vs Apruebo (excluye nulos) ─────────────────────

cat("
--- M_rechazo_strict: Rechazo vs Apruebo (excluye nulos/blancos) ---
")

m_rechazo_strict <- glm(
  voto_rechazo_strict ~
    indigeneous * cerca_conflicto +
    mujer + edad +
    idx_vio_control + idx_vio_resguardo +
    idx_just_proc + apoyo_movil +
    id_chile + id_causa,
  data   = subset_ola4 |> filter(!is.na(voto_rechazo_strict)),
  family = binomial(link = "logit")
)

cat("
Odds ratios — Rechazo vs Apruebo (estricto):
")
print(round(exp(cbind(OR = coef(m_rechazo_strict), confint(m_rechazo_strict))), 3))


# ── Tabla resumen modelos plebiscito ─────────────────────────────────────────

modelsummary(
  list(
    "Movilización (votó)"          = m_movil,
    "Voto Rechazo (vs resto)"      = m_rechazo,
    "Voto Rechazo (vs Apruebo)"    = m_rechazo_strict
  ),
  exponentiate = TRUE,           # muestra OR directamente
  statistic    = "({std.error})",
  stars        = TRUE,
  fmt          = 3,
  gof_omit     = "AIC|BIC|Log|REML|Num.Obs",
  output       = "output/tabla_plebiscito.html"  # html no requiere pandoc
)

cat("✓ Tabla exportada: output/tabla_plebiscito.html
")


# ── Visualización: probabilidades predichas ───────────────────────────────────

library(marginaleffects)   # instalar si no está: install.packages("marginaleffects")

# Probabilidades predichas de votar Rechazo según identidad × zona
pred_rechazo <- predictions(
  m_rechazo,
  newdata = datagrid(
    indigeneous     = c("no_indi", "indi"),
    cerca_conflicto = c("lejos", "cerca")
  )
) |>
  as_tibble() |>
  mutate(
    grupo = paste0(indigeneous, "
", cerca_conflicto),
    grupo = factor(grupo,
                   levels = c("no_indi
lejos", "no_indi
cerca",
                              "indi
lejos",    "indi
cerca"),
                   labels = c("No indígena
Lejos", "No indígena
Zona excepción",
                              "Indígena
Lejos",    "Indígena
Zona excepción")
    )
  )

p_pred <- ggplot(pred_rechazo,
                 aes(x = grupo, y = estimate,
                     ymin = conf.low, ymax = conf.high,
                     color = indigeneous)) +
  geom_pointrange(size = 0.8, linewidth = 1) +
  scale_color_manual(
    values = c("no_indi" = "#4575B4", "indi" = "#D73027"),
    guide  = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "Probabilidad predicha de voto Rechazo",
    subtitle = "Plebiscito 4 de septiembre 2022 — ELRI ola 4",
    x = NULL, y = "P(Rechazo)",
    caption  = "IC 95% · Resto de variables en sus medias"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_pred)
ggsave("output/fig_prob_rechazo.png", p_pred,
       width = 7, height = 5, dpi = 300)

cat("✓ Figura guardada: output/fig_prob_rechazo.png
")


# ── FIN ───────────────────────────────────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("✓ Script ejecutado correctamente.\n")
cat("  Archivos en output/:\n")
cat("    · fig_trayectorias.png\n")
cat("    · fig_coeficientes.png\n")
cat("    · tabla_modelos.html\n")
cat(strrep("=", 60), "\n")

