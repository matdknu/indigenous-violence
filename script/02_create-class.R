# --- Paquetes (usando pacman) -------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, lavaan, psych, semPlot, car, stringr, sjlabelled, sjmisc, here,
  lcmm, haven, LMest, panelr, tidyverse, viridis, xtable, reshape2, ggplot2,
  gridExtra
)

# limpiar espacio
cat("\014")
rm(list = ls())
gc()

# Load data
load(here("data/BBDD_ELRI_LONG.RData"))

# 1) Limpiar a1 y mapear códigos especiales a NA
a1_num <- as.numeric(haven::zap_labels(BBDD_ELRI_LONG$a1))
na_codes <- c(88, 99, 8888, 9999)
a1_num[ a1_num %in% na_codes ] <- NA

# 2) Construir indi con etiquetas claras (factor) y sin NAs "fáciles"
BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  mutate(
    a1_num = a1_num,
    indi = case_when(
      a1_num %in% 1:11 ~ "indi",
      a1_num == 12     ~ "no_indi",
      TRUE             ~ NA_character_
    )
  )

# 3) Hacer indi invariante por folio: imputar por MODA del sujeto y, si sigue NA, por moda global
mode_chr <- function(x){
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# moda global (por si algún folio quedó todo NA)
indi_global_mode <- mode_chr(BBDD_ELRI_LONG$indi)

BBDD_ELRI_LONG <- BBDD_ELRI_LONG %>%
  group_by(folio) %>%
  mutate(indi = ifelse(is.na(indi), mode_chr(indi), indi)) %>%  # moda dentro del folio
  ungroup() %>%
  mutate(indi = ifelse(is.na(indi), indi_global_mode, indi),
         # opción A (recomendada para LMest): factor con baseline explícito
         indi = factor(indi, levels = c("no_indi", "indi"))
         # opción B (si prefieres binario duro):
         # indi_bin = as.integer(indi == "indi")  # 0/1 integer
  )

subset_data <- BBDD_ELRI_LONG %>% 
  dplyr::mutate (mujer = case_when(g2 == 1 ~ "0",
                                   g2 == 2 ~ "1")) %>% 
  dplyr::mutate (edad = case_when(g18 %in% 18:24 ~ "18_24",
                                  g18 %in% 25:34 ~ "25_34",
                                  g18 %in% 35:44 ~ "35_44",
                                  g18 %in% 45:54 ~ "45_54",
                                  g18 %in% 55:64 ~ "55_64", 
                                  g18 %in% 65:89 ~ "65+")) %>% 
  select("folio", "ola", 
         "d3_1", "d3_2",
         "d4_2", "d4_3",
          
         "d1_1",  #percepción de conflicto
         "c5", #Confianza en pueblos originarios      
         "d5_1", #Justicia procidimental indígenas
         "d6_1",  #Siento identificación con la causa indígena
        # "c27_1", #Justicia procidimental indígenas
         "c13",    #Frecuencia de contacto
         #"c26_1", #Normas pone MUCHO NA
         "urbano_rural",
         "mujer",
         "indi",
         "edad")


# --- Confianza --- #
#c2 En general, ¿cuánto confía en los chilenos no indígenas?
#c5 Confianza en los pueblos originarios
#----- Intergrupal ----- #
#c17_4 Me importa mucho entender el punto de vista de los chilenos no indígenas
#c19  ¿Cuán diferentes son los chilenos no indígenas y los (% PUEBLO ORIGINARIO) entre sí?
#c26_1 Mi familia valora que yo tenga amigos (% PUEBLO ORIGINARIO %)
# --- Respeto---- #
#c27_1  En general, las personas respetan a los (% PUEBLO ORIGINARIO)
#c27_3  En general, las personas respetan a los chilenos no indígenas
# --- Percepción de conflicto ---- #
#d1_1   Conflicto diría usted que existe actualmente entre...El Estado chileno y los pueblos originarios?
#d1_2   Conflicto diría usted que existe actualmente entre...Indígenas y no-indígenas?
#---- Justicia procidimental ---- #
#d5_1   En general, los Carabineros tratan a las personas indígenas con respeto.
#d5_2   En general, los Carabineros tratan a las personas NO indígenas con respeto.
#---- Teoría identidad social  -----#
#d6_1   Me siento identificado con la causa de los pueblos indígenas
#---- Teoría del contacto -----#
#c7_2   Con qué frecuencia conversa o interactúa con personas chilenas no-indígenas?
#c13    ¿Con qué frecuencia conversa o interactúa con personas (% PUEBLO ORIGINARIO)?


# Asumiendo que subset_data ya está cargado
# Convertir 'folio' a factor para usarlo como identificador de sujeto
subset_data$folio <- as.factor(subset_data$folio)

# Convertir 'ola' a numérico si no lo es ya
subset_data$ola <- as.numeric(subset_data$ola)
subset_data$folio <- as.numeric(as.character(subset_data$folio))

subset_data <- subset_data %>%
  # Primero, convertimos las columnas haven_labelled a numérico
  mutate(across(c(d3_1, d3_2, d4_2, d4_3), ~as.numeric(zap_labels(.)))) %>%
  # Luego, aplicamos las transformaciones para manejar los valores especiales
  mutate(
    across(where(is.numeric), 
           ~if_else(. %in% c(8888, 9999, 88, 99), NA_real_, .)),
    across(where(is.character), 
           ~if_else(. %in% c("8888", "9999", "88", "99"), NA_character_, .))
  )

glimpse(subset_data)

# recod. 
reclasificar <- function(x) {
  case_when(
    x <= 2 ~ "1", # No justifica
    x == 3 ~ "2", # J
    x >= 4 ~ "2", # Sí justifica
    TRUE ~ NA_character_
  )
}

# Aplicar la reclasificación a las variables de interés
subset_data <- subset_data %>%
  mutate(across(c(d3_1, d3_2, d4_2, d4_3), 
                ~reclasificar(.), 
                .names = "{.col}_red"))

# Verificar la nueva clasificación
subset_data %>%
  select(ends_with("_red")) %>%
  summarise(across(everything(), ~table(.)))



# modelos lmest ----------------------------------------------------------------
subset_data <- subset_data %>% 
  panel_data(subset_data, id = folio, wave = ola) %>% 
  complete_data(min.waves = 4) %>%  
  group_by(folio) %>%
  filter(n_distinct(ola) == 4 & all(ola %in% 1:4)) %>%
  ungroup() %>% 
  as.data.frame()

subset_data %>% group_by (ola) |> frq(indi)


print(colSums(is.na(subset_data)))
table(subset_data$folio, subset_data$ola)
subset_data$d5_1[is.na(subset_data$d5_1)] <- median(subset_data$d5_1, na.rm = TRUE) # imputar un valor NA
subset_data_clean <- subset_data[complete.cases(subset_data), ]
subset_data_clean <- na.omit(subset_data)



# Modelo -----------------------------------------------------------------------
## Modelo 1
modelos <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                  latentFormula =~ urbano_rural + 
                    mujer +
                    indi +
                    edad + 
                    d1_1 +  #percepción conlficto estado
                    c5 +    # confianza pueblos originarios 
                    d5_1 +  # justicia procedimental en personas indígenas
                    d6_1 +  #identificación con la causa indígenas
                   # c27_1 + #respeto a PP.OO
                    c13,   #frecuencia de contacto con PP.OO
                  index = c("folio","ola"),
                  output = TRUE,
                  out_se = TRUE,
                  paramLatent = "multilogit",
                  data = subset_data,
                  k = 1:6,
                  start = 0,
                  modBasic = 3,
                  modManifest="FM",
                  seed = 1234)



#`modBasic` model on the transition probabilities: default 0 for time-heterogeneous transition
#matrices, 1 for time-homogeneous transition matrices, 2 for partial time homogeneity based 
#on two transition matrices one from 2 to (TT-1) and the other for TT.

#modManifest model for manifest distribution 
#("LM" = Latent Markov with stationary transition, "FM" = finite mixture model) 
#where a mixture of AR(1) processes is estimated with common variance and specific correlation coefficients.

modelos <- readRDS("outputs/modelos_lmest.rds")

plot(modelos,what="modSel")
plot(modelos, what = "CondProb")
plot(modelos, what="marginal")

write_rds(modelos, "outputs/modelos_lmest.rds")


## Modelo 4c
modelo_4c <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                    latentFormula =~ urbano_rural + 
                      mujer +
                      indi +
                      edad + 
                      d1_1 +  #percepción conlficto estado
                      c5 +    # confianza pueblos originarios 
                      d5_1 +  # justicia procedimental en personas indígenas
                      d6_1 +  #identificación con la causa indígenas
                      # c27_1 + #respeto a PP.OO
                      c13,   #frecuencia de contacto con PP.OO
                    index = c("folio","ola"),
                    output = TRUE,
                    out_se = TRUE,
                    paramLatent = "multilogit",
                    data = subset_data,
                    k = 4,
                    start = 0,
                    modBasic = 3, #en vez de 1
                    modManifest="LM",
                    seed = 1234)


 ##Acá

modelo_4c <- readRDS("outputs/modelo_4c.rds")

write_rds(modelo_4c, "outputs/modelo_4c.rds")

plot(modelo_4c, what = "CondProb")
plot(modelo_4c, what="marginal")
plot(modelo_4c, what = "transitions")


# Probabilidades de transición
# Probabilidades individuales de transición. 
trans_ind <- modelo_4c$V   

trans_ind

tras_ind1 <- trans_ind[, , 1]
colnames(tras_ind1) <- c("t1_t2_clase1", "t1_t2_clase2", "t1_t2_clase3" , "t1_t2_clase4")
tras_ind1 <- as_tibble(tras_ind1)

tras_ind1

tras_ind2 <- trans_ind[, , 2]
colnames(tras_ind2) <- c("t1_t3_clase1", "t1_t3_clase2", "t1_t3_clase3", "t1_t3_clase4")
tras_ind2 <- as_tibble(tras_ind2)

tras_ind2

tras_ind3 <- trans_ind[, , 3]
colnames(tras_ind3) <- c("t2_t3_clase1", "t2_t3_clase2", "t2_t3_clase3", "t2_t3_clase4")
tras_ind3 <- as_tibble(tras_ind3)

tras_ind3

# Abrir un dispositivo gráfico para guardar el primer gráfico
png("output/marginal_modelo_4c.png", width = 800, height = 600)
plot(modelo_4c, what="marginal")
dev.off()  # Cerrar el dispositivo gráfico


# Abrir un dispositivo gráfico para guardar el segundo gráfico
png("output/transition_modelo_4c.png", width = 800, height = 600)
plot(modelo_4c, what = "transitions")
dev.off()  # Cerrar el dispositivo gráfico


# plots 
LMmodelo <- reshape2::melt(modelo_4c$Psi, level=1)
glimpse(LMmodelo)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))


# Creamos un vector con las descripciones de los ítems
item_descriptions <- c(
  "1" = "El uso de la fuerza por\nparte de Carabineros para\ndisolver protestas de\ngrupos indígenas",
  "2" = "Que agricultores usen\narmas para enfrentar a\ngrupos de personas\nindígenas",
  "3" = "Que grupos de personas\nindígenas se tomen\nterrenos que se\nconsideran propios",
  "4" = "El bloqueo o corte de\ncarreteras por parte de\ngrupos de personas\nindígenas"
)

ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
  geom_col(position = "stack") +
  facet_wrap(~ state, ncol = 1, labeller = labeller(state = function(x) paste("Clase", x))) +
  scale_fill_viridis(discrete = TRUE, 
                     alpha = 1,
                     option = "D",
                     direction = -1,
                     name = "¿Se justifica?",
                     labels = c("No se justifica", "Se justifica")) +
  scale_x_discrete(labels = item_descriptions) +
  labs(x = NULL, y = "P(y)", 
       title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Guardamos 
ggsave("outputs/image/probabilidad_respuesta1.jpg", width = 12, height = 10, dpi = 300)



modelos_lt <-  lmest(responsesFormula = d3_1_red + d3_2_red + d4_2_red + d4_3_red ~ NULL,
                     latentFormula =~ urbano_rural + 
                       mujer +
                       indi +
                       edad + 
                       d1_1 +  #percepción conlficto estado
                       c5 +    # confianza pueblos originarios 
                       d5_1 +  # justicia procedimental en personas indígenas
                       d6_1 +  #identificación con la causa indígenas
                       # c27_1 + #respeto a PP.OO
                       c13,   #frecuencia de contacto con PP.OO
                     index = c("folio","ola"),
                     output = TRUE,
                     out_se = TRUE,
                     paramLatent = "multilogit",
                     data = subset_data,
                     k = 4,
                     start = 0,
                     modBasic = 3,
                     modManifest="FM",
                     seed = 1234)
# Guardamos 

summary(modelos_lt, start = TRUE)


# Extraer coeficientes y p-valores
coefs <- modelos_lt$Ga
se <- modelos_lt$seGa

coefs<-as.data.frame(coefs)
se<-as.data.frame(se)


library(tibble)

coefs <- coefs[, 1:3] |> 
  rownames_to_column(var = "Variable")

names(se) <- paste0(names(se[]), "_se")
se <- se[, 1:3] |> 
  rownames_to_column(var = "Variable")


coefs<-merge(coefs, se, by="Variable")

for (col in c("2.1", "3.1", "4.1")) {
  se_col <- paste0(col, "_se")
  z_col <- paste0(col, "_z")
  p_col <- paste0(col, "_p")
  
  coefs[[z_col]] <- coefs[[col]] / coefs[[se_col]]
  coefs[[p_col]] <- 2 * (1 - pnorm(abs(coefs[[z_col]])))
}

coefs<-coefs[c(1:4, 9, 11, 13)]

coefs[, 2:4] <- exp(coefs[, 2:4])
names(coefs)[2:4] <- c("odds1_2", "odds1_3", "odds1_4")
names(coefs)[5:7] <- c("pvalue1_2", "pvalue1_3", "pvalue1_4")

labels_vars <- c(
  "(Intercept)" = "Intercepto",
  "urbano_rural" = "Zona urbana (1 = urbano, 0 = rural)",
  "mujer1" = "Mujer (1 = mujer, 0 = hombre)",
  "indiindi" = "Identidad indígena (1 = sí, 0 = no)",
  "edad25_34" = "Edad 25–34 años",
  "edad35_44" = "Edad 35–44 años",
  "edad45_54" = "Edad 45–54 años",
  "edad55_64" = "Edad 55–64 años",
  "edad65+" = "Edad 65 o más",
  "d1_1" = "Percepción conflicto Estado–Pueblos Originarios",
  "c5" = "Confianza en pueblos originarios",
  "d5_1" = "Justicia procedimental hacia pueblos indígenas",
  "d6_1" = "Identificación con la causa indígena",
  "c13" = "Frecuencia de contacto con pueblos originarios"
)

coefs$Variable <- ifelse(coefs$Variable %in% names(labels_vars),
                         labels_vars[coefs$Variable],
                         coefs$Variable)

new_order <- c(
  "Variable",
  "odds1_2", "pvalue1_2",
  "odds1_3", "pvalue1_3",
  "odds1_4", "pvalue1_4"
)

# Seleccionamos solo esas columnas en el orden deseado
coefs <- coefs[, new_order]

library(writexl)

# Guardar el data.frame en Excel
write_xlsx(coefs, path = "outputs/coef_transiciones_odds.xlsx")


coefs


library(dplyr)
library(tidyr)
library(ggplot2)

# Se asume que ya tienes el data frame `coefs` en el ambiente,
# con las columnas tal como las imprimiste:
# Variable, odds1_2, pvalue1_2, odds1_3, pvalue1_3, odds1_4, pvalue1_4

# (Opcional) definir niveles del eje Y en el orden en que aparecen
var_levels <- rev(coefs$Variable)

coefs_long <- coefs %>% 
  # si no quieres mostrar el intercept en el gráfico:
  filter(Variable != "intercept") %>% 
  mutate(Variable = factor(Variable, levels = var_levels)) %>% 
  # pasar odds y pvalues a formato largo
  pivot_longer(
    cols = matches("^(odds1_|pvalue1_)"),
    names_to = c("stat", "class"),
    names_pattern = "(odds1|pvalue1)_([234])",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>% 
  mutate(
    comparison = factor(
      class,
      levels = c("2", "3", "4"),
      labels = c("Clase 2 vs Clase 1",
                 "Clase 3 vs Clase 1",
                 "Clase 4 vs Clase 1")
    ),
    signif = pvalue1 < 0.05
  )

g2 <- ggplot(coefs_long,
             aes(x = odds1,
                 y = Variable,
                 colour = signif)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_colour_manual(
    values = c(`TRUE` = "black", `FALSE` = "grey60"),
    labels = c("FALSE" = "No significativo", "TRUE" = "p < 0.05"),
    name = ""
  ) +
  facet_wrap(~ comparison) +
  labs(
    x = "Odds ratio (escala log10)",
    y = NULL,
    title = "Probabilidad relativa de pasar de la Clase 1 a otras clases",
    subtitle = "Modelo multinomial: OR > 1 indica mayor probabilidad de pertenecer a la clase indicada vs Clase 1"
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

g2


ggsave("outputs/image/probabilidad_respuesta1.jpg", width = 16, height = 8, dpi = 300)

