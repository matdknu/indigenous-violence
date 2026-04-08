# ============================================================
# 0. Paquetes y preparación
# ============================================================
rm(list = ls())
gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, haven, LMest, here, readr, sjmisc
)

options(stringsAsFactors = FALSE)

dir.create(here("outputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "longitudinal"), recursive = TRUE, showWarnings = FALSE)

# Flujo sugerido:
# 1) Este script estima el modelo longitudinal.
# 2) Guarda el modelo final en `outputs/modelo_4c.rds`.
# 3) Luego `script/03_predictors.R` usa ese objeto para el post-analisis.

# ============================================================
# 1. Cargar base ELRI LONG
# ============================================================
load(here("data/BBDD_ELRI_LONG.RData"))

# ============================================================
# 2. Construir variable indi (autoidentificación indígena)
# ============================================================

# 2.1 Limpiar a1 y mapear códigos especiales a NA
a1_num <- as.numeric(haven::zap_labels(BBDD_ELRI_LONG$a1))
na_codes <- c(88, 99, 8888, 9999)
a1_num[a1_num %in% na_codes] <- NA

# 2.2 Construir indi con etiquetas claras (factor)
BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::mutate(
    a1_num = a1_num,
    indi = dplyr::case_when(
      a1_num %in% 1:11 ~ "indi",
      a1_num == 12     ~ "no_indi",
      TRUE             ~ NA_character_
    )
  )

# 2.3 Hacer indi invariante por folio (moda dentro del sujeto)
mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Moda global por si algún folio queda todo NA
indi_global_mode <- mode_chr(BBDD_ELRI_LONG$indi)

BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::group_by(folio) %>%
  dplyr::mutate(indi = ifelse(is.na(indi), mode_chr(indi), indi)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    indi = ifelse(is.na(indi), indi_global_mode, indi),
    indi = factor(indi, levels = c("no_indi", "indi"))
  )

# ============================================================
# 3. Nuevas variables: identificación, acción colectiva, contacto negativo
# ============================================================

# 3.1 Identificación y compromiso (d6_1 y d6_2)
BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::rename(
    causa      = d6_1,  # Identificación con las causas
    compromiso = d6_2   # Compromiso con causas indígenas
  ) %>%
  dplyr::mutate(
    causa_compromiso = (causa + compromiso) / 2
  )
attr(BBDD_ELRI_LONG$causa_compromiso, "label") <- "Identificación causa"

# 3.2 Acción colectiva (c32_1 y c32_2)
BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::rename(
    firmar_cartas       = c32_1,  # Firmar cartas de apoyo
    protestar_indigenas = c32_2   # Protestar por causas indígenas
  ) %>%
  dplyr::mutate(
    colective_action = (firmar_cartas + protestar_indigenas) / 2
  )
attr(BBDD_ELRI_LONG$colective_action, "label") <- "Collective action"

# 3.3 Contacto negativo extendido (c11)
BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  dplyr::rename(negative_contact_ex = c11)
attr(BBDD_ELRI_LONG$negative_contact_ex, "label") <- "Negative contact extended"

# ============================================================
# 4. Crear subset_data base para LMest
# ============================================================

subset_data <- BBDD_ELRI_LONG %>%
  dplyr::mutate(
    mujer = dplyr::case_when(
      g2 == 1 ~ "0",   # hombre
      g2 == 2 ~ "1",   # mujer
      TRUE    ~ NA_character_
    ),
    edad  = dplyr::case_when(
      g18 %in% 18:24  ~ "18_24",
      g18 %in% 25:34  ~ "25_34",
      g18 %in% 35:44  ~ "35_44",
      g18 %in% 45:54  ~ "45_54",
      g18 %in% 55:64  ~ "55_64",
      g18 %in% 65:89  ~ "65+",
      TRUE            ~ NA_character_
    )
  ) %>%
  dplyr::select(
    folio, ola,
    d3_1, d3_2,        # ítems violencia 1
    d4_2, d4_3,        # ítems violencia 2
    d1_1,              # percepción de conflicto
    c5,                # confianza en pueblos originarios      
    d5_1,              # justicia procedimental indígenas
    colective_action,  # acción colectiva
    causa_compromiso,  # identificación causa indígena
    c13,               # frecuencia de contacto
    #negative_contact_ex,
    urbano_rural,
    mujer,
    indi,
    edad
  )

# ============================================================
# 5. haven_labelled -> numérico + limpieza de códigos especiales
# ============================================================

vars_labelled_to_numeric <- c(
  "d3_1", "d3_2", "d4_2", "d4_3",
  "d1_1", "c5", "d5_1",
  "c13", "urbano_rural"
)

subset_data <- subset_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(vars_labelled_to_numeric),
      ~ as.numeric(haven::zap_labels(.))
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ dplyr::if_else(. %in% c(88, 99, 8888, 9999, 66), NA_real_, .)
    )
  ) %>%
  dplyr::mutate(
    id   = as.integer(factor(folio)),
    ola  = as.integer(ola),
    mujer = factor(
      mujer,
      levels = c("0", "1"),
      labels = c("Hombre", "Mujer")
    ),
    edad = factor(
      edad,
      levels  = c("18_24", "25_34", "35_44", "45_54", "55_64", "65+"),
      ordered = TRUE
    )
  ) %>%
  dplyr::relocate(id, .before = folio)

# ============================================================
# 6. Recodificar ítems de violencia a 2 categorías (1/2)
# ============================================================

reclasificar <- function(x) {
  dplyr::case_when(
    x <= 2 ~ "1", # No justifica
    x == 3 ~ "2", # punto medio (lo sumas al grupo "sí")
    x >= 4 ~ "2", # Sí justifica
    TRUE  ~ NA_character_
  )
}

subset_data <- subset_data %>%
  dplyr::mutate(
    dplyr::across(
      c(d3_1, d3_2, d4_2, d4_3),
      ~ reclasificar(.),
      .names = "{.col}_red"
    ),
    dplyr::across(
      c(d3_1_red, d3_2_red, d4_2_red, d4_3_red),
      ~ as.integer(.)
    )
  )

# (Chequeo rápido opcional)
subset_data %>%
 dplyr::select(dplyr::ends_with("_red")) %>%
 dplyr::summarise(dplyr::across(dplyr::everything(), ~ table(.)))

# ============================================================
# 7. Construir panel 4-olas (1,2,3,4) y limpiar NAs por SUJETO
# ============================================================

covars_latent <- c(
  "urbano_rural", "mujer", "indi", "edad",
  "d1_1", "c5", "d5_1",
  "colective_action", "causa_compromiso", "c13"
)

responses  <- c("d3_1_red", "d3_2_red", "d4_2_red", "d4_3_red")
index_vars <- c("folio", "ola")
vars_all   <- c(index_vars, covars_latent, responses)

# 7.1 Panel con olas 1,2,3,4 y sujetos que tienen las 4 olas
subset_lmest <- subset_data %>%
  dplyr::filter(ola %in% 1:4) %>%
  dplyr::group_by(folio) %>%
  dplyr::filter(dplyr::n_distinct(ola) == 4) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(folio, ola)

# 7.2 (Opcional) imputar d5_1 por mediana ANTES del filtrado completo
subset_lmest <- subset_lmest %>%
  dplyr::mutate(
    d5_1 = dplyr::if_else(
      is.na(d5_1),
      stats::median(d5_1, na.rm = TRUE),
      d5_1
    )
  )

# 7.3 Marcar filas completas y filtrar sujetos completos en TODAS las olas
cc_flags <- stats::complete.cases(subset_lmest[, vars_all])
subset_lmest$cc_all <- cc_flags

id_completos <- subset_lmest %>%
  dplyr::group_by(folio) %>%
  dplyr::summarise(
    all_cc = all(cc_all),
    .groups = "drop"
  ) %>%
  dplyr::filter(all_cc) %>%
  dplyr::pull(folio)

subset_lmest_cc <- subset_lmest %>%
  dplyr::filter(folio %in% id_completos) %>%
  dplyr::select(-cc_all)

# 7.4 Reindexar tiempo (time4 = 1,2,3,4) y asegurar tipos simples
subset_lmest_cc <- subset_lmest_cc %>%
  dplyr::mutate(
    time4 = as.integer(factor(ola, levels = sort(unique(ola))))
  ) %>%
  dplyr::arrange(folio, time4) %>%
  dplyr::mutate(
    folio = as.integer(folio)
  ) %>%
  as.data.frame()

# --- Chequeos de sanidad ------------------------------------
cat("NAs en covariables latentes:\n")
print(sapply(subset_lmest_cc[covars_latent], function(x) sum(is.na(x))))

cat("\nNAs en respuestas:\n")
print(sapply(subset_lmest_cc[responses],     function(x) sum(is.na(x))))

cat("\nConteo por tiempo (time4):\n")
print(table(subset_lmest_cc$time4))

cat("\nMin time y n_time por folio:\n")
min_time_tab <- subset_lmest_cc %>%
  dplyr::group_by(folio) %>%
  dplyr::summarise(
    min_time = min(time4),
    n_time   = dplyr::n_distinct(time4),
    .groups  = "drop"
  )

print(table(min_time_tab$min_time))
print(table(min_time_tab$n_time))

# ============================================================
# 8. Modelo LMest con covariables (k = 1:6 clases)
# ============================================================

set.seed(1234)

subset_lmest_cc |> group_by(ola) |> frq(indi)

modelos <- lmest(
  responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
  latentFormula    = ~ urbano_rural + 
    mujer +
    indi +
    edad + 
    d1_1 +            # percepción conflicto Estado–pueblos originarios
    c5 +              # confianza en pueblos originarios 
    d5_1 +            # justicia procedimental hacia indígenas
    colective_action +# acción colectiva proindígena
    causa_compromiso + 
    c13,         # frecuencia de contacto con pueblos originarios
  index       = c("folio", "time4"),   # id = folio, tiempo = time4 (1–4)
  data        = subset_lmest_cc,
  k           = 1:6,
  start       = 0,
  modBasic    = 3,
  modManifest = "FM",
  paramLatent = "multilogit",
  output      = TRUE,
  out_se      = TRUE
)

# (Opcional)

plot(modelos, what = "modSel")
plot(modelos, what = "CondProb")
plot(modelos, what = "marginal")
summary(modelos)

saveRDS(modelos, here("outputs", "modelos_k_1_6_lmest.rds"))

BBDD_ELRI_LONG |> frq(d3_1)
BBDD_ELRI_LONG |> frq(d3_1)

# Si se selecciona K = 4 como solucion final:
modelo_4c <- lmest(
  responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
  latentFormula    = ~ urbano_rural +
    mujer +
    indi +
    edad +
    d1_1 +
    c5 +
    d5_1 +
    colective_action +
    causa_compromiso +
    c13,
  index       = c("folio", "time4"),
  data        = subset_lmest_cc,
  k           = 4,
  start       = 0,
  modBasic    = 3,
  modManifest = "FM",
  paramLatent = "multilogit",
  output      = TRUE,
  out_se      = TRUE
)

saveRDS(modelo_4c, here("outputs", "modelo_4c.rds"))



modelo_lm4 <- modelo_4c


plot(modelos, what = "modSel")
plot(modelos, what = "CondProb")
plot(modelos, what = "marginal")
summary(modelos)


##----


# Construir matriz de transición desde Ga
# (Para cada logit, calcula probabilidades)

# Transiciones desde Clase 1
logit_1_to_others <- modelo_lm4$Ga[, , 1]  # logit 1
logit_2_to_others <- modelo_lm4$Ga[, , 2]  # logit 2
logit_3_to_others <- modelo_lm4$Ga[, , 3]  # logit 3
logit_4_to_others <- modelo_lm4$Ga[, , 4]  # logit 4

# Convertir logits a probabilidades
# P(clase j | clase i) = exp(logit_j) / sum(exp(logit_k))

# Simplificado: usa la matriz Psi o extrae de predicciones


# ============================================================
# A. TABLA 1: Características de cada clase
# ============================================================

# Probabilidades condicionales (Psi)
psi_df <- data.frame(
  Clase = rep(1:4, 4),
  Item = rep(c("Fuerza policial", "Armas agr.", "Toma terrenos", "Corte carreteras"), each=4),
  Prob_Sí = c(
    modelo_lm4$Psi[2, 1, 1], modelo_lm4$Psi[2, 1, 2], 
    modelo_lm4$Psi[2, 1, 3], modelo_lm4$Psi[2, 1, 4],
    modelo_lm4$Psi[2, 2, 1], modelo_lm4$Psi[2, 2, 2],
    modelo_lm4$Psi[2, 2, 3], modelo_lm4$Psi[2, 2, 4],
    modelo_lm4$Psi[2, 3, 1], modelo_lm4$Psi[2, 3, 2],
    modelo_lm4$Psi[2, 3, 3], modelo_lm4$Psi[2, 3, 4],
    modelo_lm4$Psi[2, 4, 1], modelo_lm4$Psi[2, 4, 2],
    modelo_lm4$Psi[2, 4, 3], modelo_lm4$Psi[2, 4, 4]
  )
)

psi_pivot <- psi_df %>%
  pivot_wider(
    names_from = Clase,
    values_from = Prob_Sí,
    names_prefix = "Clase "
  )

knitr::kable(psi_pivot, digits = 3,
             caption = "Probabilidades de justificar violencia por clase latente")

# ============================================================
# B. TABLA 2: Coeficientes Be (Predictores de clase inicial)
# ============================================================

be_tidy <- as.data.frame(modelo_lm4$Be) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(-Variable, names_to = "Clase", values_to = "Coef") %>%
  mutate(Clase = gsub("V", "", Clase)) %>%
  arrange(Variable, Clase)

# Standard errors
se_be <- as.data.frame(se(modelo_lm4)$seBe) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(-Variable, names_to = "Clase", values_to = "SE") %>%
  mutate(Clase = gsub("V", "", Clase))

be_final <- left_join(be_tidy, se_be) %>%
  mutate(
    z = Coef / SE,
    p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
    OR = exp(Coef),
    significativo = ifelse(p_value < 0.05, "***", "")
  )

knitr::kable(be_final[, c("Variable", "Clase", "Coef", "SE", "p_value", "OR", "significativo")],
             digits = 3,
             caption = "Predictores de pertenencia inicial a clase latente (referencia: Clase 1)")

# ============================================================
# C. TABLA 3: Estabilidad y movilidad entre clases
# ============================================================

# Esto requiere calcular desde Ga
# Por ahora, describe patrones cualitativos



# Índices de bondad
cat("Log-likelihood (k=4):", modelo_lm4$lk, "\n")
cat("AIC:", modelo_lm4$aic, "\n")
cat("BIC:", modelo_lm4$bic, "\n")
cat("Entropía relativa:", modelo_lm4$entropy, "\n")

# Comparar k=3 vs k=4 vs k=5
# ¿Cuál tiene mejor BIC? (menor = mejor)


# ============================================================
# ANÁLISIS DE CAMBIO POR PERÍODO HISTÓRICO
# ============================================================

# Tu matriz de transición tiene 4 PERÍODOS:
# T1→T2 (2016→2018): Pre-estallido
# T2→T3 (2018→2021): Estallido + represión
# T3→T4 (2021→2023): Post-represión

# Extrae transiciones POR período
transiciones_por_periodo <- list(
  "2016-2018" = PI_array[, , , 1],  # T1→T2
  "2018-2021" = PI_array[, , , 2],  # T2→T3
  "2021-2023" = PI_array[, , , 3]   # T3→T4
)

# Propón esto en tu TABLA 3:
# "Tasa de cambio de clase por período"


# ============================================================
# FIGURA: Prevalencia de clases en el tiempo
# ============================================================

# Calcular proporción en cada clase por ola
# (desde modelo_lm4$Piv)

class_trajectory <- data.frame(
  Año = c(2016, 2018, 2021, 2023),
  Rechazadores = c(0.40, 0.39, 0.38, 0.37),  # hipotético
  ProControl = c(0.27, 0.29, 0.24, 0.22),    # baja post-represión?
  ProIndígena = c(0.21, 0.20, 0.28, 0.31),   # SUBE post-represión
  Violentista = c(0.12, 0.12, 0.10, 0.10)    # baja post-represión?
)

class_trajectory_long <- class_trajectory %>%
  pivot_longer(-Año, names_to = "Clase", values_to = "Proporción")

ggplot(class_trajectory_long, aes(x = Año, y = Proporción, color = Clase, group = Clase)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 2019, y = 0.45, label = "Estallido\n+ Represión", 
           size = 3, color = "red") +
  scale_x_continuous(breaks = c(2016, 2018, 2021, 2023)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Volatilidad de Actitudes hacia Violencia en Conflictos Indígenas",
    subtitle = "Proporción en cada clase latente, 2016-2023",
    x = "Año", y = "Proporción de población",
    color = "Clase Latente"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ggsave("output/figura_trayectorias_clases.png", width = 10, height = 6, dpi = 300)

