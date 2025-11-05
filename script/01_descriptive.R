# Cargar paquetes con pacman::p_load
pacman::p_load(
  dplyr, tidyverse, stringr, here, haven,       # Manipulación y carga de datos
  lavaan, LMest, lcmm, panelr,                 # Modelos estructurales y longitudinales
  psych, semPlot, car,                         # Psicometría y visualización SEM
  sjlabelled, sjmisc,                          # Etiquetas y gestión de variables
  viridis, xtable                              # Visualización y reporte
)



# limpiar espacio
cat("\014")
rm(list = ls())
gc()

# Load data
load("data/BBDD_ELRI_LONG.RData")
data <- BBDD_ELRI_LONG

# Filtrar personas que aparecen en las 4 olas
data <- data %>%
  group_by(folio) %>%
  filter(n_distinct(ola) == 4) %>%
  ungroup()



subset_data <- data %>% 
  dplyr::mutate(indi = case_when(a1 %in% 1:11 ~ "indi",
                                 a1 == 12 ~ "no_indi")) %>% 
  dplyr::mutate(cat_indi = case_when(a1 == 1 ~ "mapuche", 
                                     a1 %in% c(2, 4, 5, 6, 7, 10) ~ "andino",
                                     a1 == 12 ~ "chileno_noindig",
                                     TRUE ~ "otro")) %>% 
  dplyr::mutate (mujer = case_when(g2 == 1 ~ "0",
                                   g2 == 2 ~ "1")) %>% 
  dplyr::mutate (edad = case_when(g18 %in% 18:24 ~ "18_24",
                                  g18 %in% 25:34 ~ "25_34",
                                  g18 %in% 35:44 ~ "35_44",
                                  g18 %in% 45:54 ~ "45_54",
                                  g18 %in% 55:64 ~ "55_64", 
                                  g18 %in% 65:89 ~ "65+")) %>% 
  select("folio", "ola", indi, 
         "a6", #Identificación con Chile
         "a4", #Identificación con indígenas
         "d3_1", #vio_control_carb
         "d3_2", #vio_control_latif
         "d4_2", # vio_change_terr
         "d4_3", # vio_change_protest
         "d5_1", #proc_just_ind
         "d5_2", #proc_just_noind
         "d5_4", #proc_just_decisiones
         "d6_1", #Identificación causa
         #"c7_2", #Frecuencia de contacto con no indígenas
         "c7_3", #Frecuencia de contacto amistoso con no indígenas
         # "c12", #frecuencia de contacto con indígenas
         "c14", #contacto amistoso con indígenas
         "urbano_rural", "mujer", "edad") %>% 
  rename(id_chile = a6, 
         id_indi = a4, 
         vio_control_carb = d3_1,
         vio_control_latif = d3_2,
         vio_change_terr = d4_2,
         vio_change_protest = d4_3,
         proc_just_ind = d5_1,
         proc_just_noind = d5_2,
         proc_just_decisiones = d5_4,
         id_causa = d6_1,
         #freq_contact_noind = c7_2,
         contact_friendly_noind = c7_3,
         #freq_contact_ind = c12,
         contact_friendly_ind = c14,
         urbano_rural = urbano_rural,
         mujer = mujer,
         edad = edad,
         indigeneous = indi)


# Crear categorías
base_filtro <- subset_data %>% 
  mutate(
    cat_friendly_ind = case_when(
      contact_friendly_ind %in% 1:2 ~ "Contacto no amistoso",
      contact_friendly_ind == 3 ~ "Medio contacto",
      contact_friendly_ind %in% 4:5 ~ "Contacto amistoso",
      TRUE ~ NA_character_
    ),
    cat_vio_control_carb = case_when(
      vio_control_carb %in% 1:2 ~ "Nunca se justifica",
      vio_control_carb %in% 3:5 ~ "Se justifica",
      TRUE ~ NA_character_
      #  ) , 
      #  cat_vio_control_carb = case_when(
      #    vio_control_latif %in% 1:2 ~ "Nunca se justifica",
      #    vio_control_latif %in% 3:5 ~ "Se justifica",
      #    TRUE ~ NA_character_
      #  ), 
      #  cat_vio_control_carb = case_when(
      #    vio_control_carb %in% 1:2 ~ "Nunca se justifica",
      #    vio_control_carb %in% 3:5 ~ "Se justifica",
      #    TRUE ~ NA_character_
      #  ), 
      #  cat_vio_control_carb = case_when(
      #    vio_change_protest %in% 1:2 ~ "Nunca se justifica",
      #    vio_change_protest %in% 3:5 ~ "Se justifica",
      #    TRUE ~ NA_character_
      #  )
    )) %>% 
  select(ola, indigeneous, cat_friendly_ind, cat_vio_control_carb)

# Tabular proporciones dentro de cada grupo de contacto por ola
tabla_resumen <- base_filtro %>% 
  filter(indigeneous  == "indi" |
           indigeneous == "no_indi") %>% 
  drop_na() %>%
  group_by(ola, cat_friendly_ind, cat_vio_control_carb) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(ola, cat_friendly_ind) %>% 
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>% 
  ungroup()



tabla2 <- tabla_resumen %>%
  filter(
    cat_vio_control_carb == "Se justifica",
    cat_friendly_ind != "Medio contacto"
  ) %>%
  mutate(
    ola = case_when(
      ola == 1 ~ "2016",
      ola == 2 ~ "2018",
      ola == 3 ~ "2021",
      ola == 4 ~ "2023"
    ),
    ola = factor(ola, levels = c("2016", "2018", "2021", "2023"))
  )


library(ggplot2)

e.violencia <- ggplot(tabla2, aes(
  x = ola,
  y = porcentaje,
  color = cat_friendly_ind,
  group = cat_friendly_ind,
  label = paste0(round(porcentaje, 1), "%"))
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(
    vjust = -0.8,
    position = position_dodge(width = 0.9),
    size = 2.75
  ) +
  scale_y_continuous(
    limits = c(25, 70),
    labels = function(x) paste0(x, "%")
  ) +
  scale_color_manual(
    values = c(
      "Contacto amistoso" = "black",
      "Contacto no amistoso" = "grey20"
    ),
    labels = c(
      "Contacto amistoso" = "Friendly contact",
      "Contacto no amistoso" = "Unfriendly contact"
    )
  ) +
  facet_wrap(~ cat_friendly_ind, 
             labeller = as_labeller(c(
               "Contacto amistoso" = "Friendly contact",
               "Contacto no amistoso" = "Unfriendly contact"
             ))) +
  xlab(NULL) +
  ylab(NULL) +
  #theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
    # panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Justification of police violence",
    subtitle = "By type of contact with Indigenous people"
  )

e.violencia




getwd()
# Guardar la figura en alta resolución con dimensiones recomendadas
ggsave("code/latent_violence/image/justification_police_violence.png", 
       plot = e.violencia, 
       width = 8, height = 6, dpi = 300, 
       bg = "white")




graficar_violencia <- function(data, var_violencia, titulo_sub) {
  
  cat_var <- paste0("cat_", var_violencia)
  
  base <- data %>%
    mutate(
      cat_friendly_ind = case_when(
        contact_friendly_ind %in% 1:2 ~ "Contacto no amistoso",
        contact_friendly_ind == 3 ~ "Medio contacto",
        contact_friendly_ind %in% 4:5 ~ "Contacto amistoso",
        TRUE ~ NA_character_
      ),
      !!sym(cat_var) := case_when(
        !!sym(var_violencia) %in% 1:2 ~ "Nunca se justifica",
        !!sym(var_violencia) %in% 3:5 ~ "Se justifica",
        TRUE ~ NA_character_
      )
    ) %>%
    select(ola, indigeneous, cat_friendly_ind, !!sym(cat_var))
  
  tabla <- base %>%
    filter(indigeneous %in% c("indi", "no_indi")) %>%
    drop_na() %>%
    group_by(ola, cat_friendly_ind, !!sym(cat_var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(ola, cat_friendly_ind) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
    ungroup() %>%
    filter(
      !!sym(cat_var) == "Se justifica",
      cat_friendly_ind != "Medio contacto"
    ) %>%
    mutate(
      ola = case_when(
        ola == 1 ~ "2016",
        ola == 2 ~ "2018",
        ola == 3 ~ "2021",
        ola == 4 ~ "2023"
      ),
      ola = factor(ola, levels = c("2016", "2018", "2021", "2023"))
    )
  
  ggplot(tabla, aes(
    x = ola,
    y = porcentaje,
    color = cat_friendly_ind,
    group = cat_friendly_ind,
    label = paste0(porcentaje, "%"))
  ) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(
      vjust = -0.8,
      position = position_dodge(width = 0.9),
      size = 2.75
    ) +
    scale_y_continuous(
      limits = c(10, 70),
      labels = function(x) paste0(x, "%")
    ) +
    scale_color_manual(
      values = c(
        "Contacto amistoso" = "black",
        "Contacto no amistoso" = "grey20"
      ),
      labels = c(
        "Contacto amistoso" = "Friendly contact",
        "Contacto no amistoso" = "Unfriendly contact"
      )
    ) +
    facet_wrap(~ cat_friendly_ind,
               labeller = as_labeller(c(
                 "Contacto amistoso" = "Friendly contact",
                 "Contacto no amistoso" = "Unfriendly contact"
               ))) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "bold", size = 12)
    ) +
    labs(
      #title = "",
      subtitle = titulo_sub
    )
}


g1 <- graficar_violencia(subset_data, "vio_control_carb", "Control by police ")
g2 <- graficar_violencia(subset_data, "vio_control_latif", "Control by landowners")
g3 <- graficar_violencia(subset_data, "vio_change_protest", "Protest with disturbance")
g4 <- graficar_violencia(subset_data, "vio_change_terr", "Claiming of territory")

g1
g2
g3
g4
library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol = 2)

ggsave("image/justification_police_violence_indigenous.png", 
       plot = grid.arrange(g1, g2, g3, g4, ncol = 2), 
       width = 8, height = 6, dpi = 300, 
       bg = "white")


graficar_violencia_indi <- function(data, var_violencia, titulo_sub) {
  
  cat_var <- paste0("cat_", var_violencia)
  
  base <- data %>%
    mutate(
      !!sym(cat_var) := case_when(
        !!sym(var_violencia) %in% 1:2 ~ "Nunca se justifica",
        !!sym(var_violencia) %in% 3:5 ~ "Se justifica",
        TRUE ~ NA_character_
      )
    ) %>%
    select(ola, indigeneous, !!sym(cat_var)) %>%
    drop_na()
  
  tabla <- base %>%
    filter(indigeneous %in% c("indi", "no_indi")) %>%
    group_by(ola, indigeneous, !!sym(cat_var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(ola, indigeneous) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
    ungroup() %>%
    filter(!!sym(cat_var) == "Se justifica") %>%
    mutate(
      ola = case_when(
        ola == 1 ~ "2016",
        ola == 2 ~ "2018",
        ola == 3 ~ "2021",
        ola == 4 ~ "2023",
        TRUE ~ as.character(ola)
      ),
      ola = factor(ola, levels = c("2016", "2018", "2021", "2023"))
    )
  
  ggplot(tabla, aes(
    x = ola,
    y = porcentaje,
    color = indigeneous,
    group = indigeneous,
    label = paste0(porcentaje, "%"))
  ) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(
      vjust = -0.8,
      position = position_dodge(width = 0.9),
      size = 2.75
    ) +
    scale_y_continuous(
      limits = c(10, 70),
      labels = function(x) paste0(x, "%")
    ) +
    scale_color_manual(
      values = c(
        "indi" = "black",
        "no_indi" = "grey40"
      ),
      labels = c("indi" = "Indigenous", "no_indi" = "Non-Indigenous")
    ) +
    facet_wrap(~ indigeneous,
               labeller = as_labeller(c(
                 "indi" = "Indigenous",
                 "no_indi" = "Non-Indigenous"
               ))) +
    xlab(NULL) +
    ylab(NULL) +
    labs(
      title = "",
      subtitle = titulo_sub
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "bold", size = 12)
    )
}


g1 <- graficar_violencia_indi(subset_data, "vio_control_carb", "Control by police")
g2 <- graficar_violencia_indi(subset_data, "vio_control_latif", "Control by landowners")
g3 <- graficar_violencia_indi(subset_data, "vio_change_protest", "Protest with disturbance")
g4 <- graficar_violencia_indi(subset_data, "vio_change_terr", "Claiming of territory")

g1
g2
g3
g4

library(gridExtra)

grid.arrange(g1, g2, g3, g4, ncol = 2)

ggsave("image/justification_police_violence_indigenous.png", 
       plot = grid.arrange(g1, g2, g3, g4, ncol = 2), 
       width = 8, height = 6, dpi = 300, 
       bg = "white")

