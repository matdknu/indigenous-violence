# LIBRERÍAS -------------------------------------------------------------------

pacman::p_load(ggplot2, 
               ggraph, 
               igraph, 
               tidygraph, 
               reshape2,
               gridExtra,
               grid,
               ggpubr,
               cowplot,
               knitr,
               kableExtra)


modelo_lm4 <- readRDS("outputs/modelo_4c.rds")

# Visualizar matriz de transición
plot(modelo_lm4, what = "transitions")

# Examinar probabilidades de transición específicas
print(round(apply(modelo_lm4$Pi, c(1,2), mean), 3))

# Probabilidades de respuesta por estado
print(round(modelo_lm4$Psi, 3))

# Visualización de probabilidades condicionales
plot(modelo_lm4, what = "CondProb")

# Distribución marginal de estados
plot(modelo_lm4, what = "marginal")

# Guardar gráfico en un archivo PNG
png("outputs/image/marginal_distribution.png", width = 800, height = 600)
plot(modelo_lm4, what = "marginal")
dev.off()

## plot ------------------------------------------------------------------------ 

# plots 
LMmodelo <- reshape2::melt(modelo_lm4$Psi, level=1)
glimpse(LMmodelo)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))

LMmodelo$value

# Calcular proporcixones usando Piv
proporciones <- colMeans(modelo_lm4$Piv)
proporciones <- round(proporciones * 100, 1)

# Verificar las proporciones
print("Proporción de la muestra en cada clase:")
print(proporciones)

# Crear etiquetas para las clases con sus proporciones
class_labels <- paste("Clase", 1:4, "\n(", proporciones, "%)", sep = "")

# Preparar los datos
LMmodelo <- reshape2::melt(modelo_lm4$Psi, level=1)
LMmodelo <- LMmodelo %>% mutate(value = round(value * 100))

modelo_lm4

# Vector con las descripciones de los ítems
item_descriptions <- c(
  "1" = "The use of force by\nCarabineros to break up\nprotests by Indigenous\ngroups",
  "2" = "Farmers using weapons\nto confront groups of\nIndigenous people",
  "3" = "Indigenous groups\noccupying land they\nconsider their own",
  "4" = "Road blockades or\nclosures by Indigenous\ngroups"
)

LMmodelo


class_labels <- c(
  "1" = "Universal Rejecters of Violence\n(40.1%)",
  "2" = "Pro Control Social (25.9%)",
  "3" = "Pro-Indigenous Force Sympathizers\n(22.2%)", 
  "4" = "Violentista \n(11.8%)"
)



# Crear el gráfico mejorado
p2 <- ggplot(LMmodelo, aes(x = factor(item), y = value, fill = factor(category))) +
  geom_col(position = "stack") +
  facet_wrap(~ state, ncol = 1, 
             labeller = labeller(state = function(x) class_labels[as.numeric(x)])) +
  scale_fill_manual(values = c("#1f2041", "#ffc857"),  # Colores personalizados
                    name = "Justification of Violence",
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = item_descriptions) +
  labs(x = NULL, y = "P(y)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 0, color = "black", hjust = 0.5, vjust = 1, lineheight = 0.8, size = 11),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.spacing = unit(1, "lines")
  )
p2


getwd()

ggsave("outputs/image/p2.png", plot = p2, width = 11.5, height = 9)


# También podemos ver cómo cambian las proporciones en el tiempo usando PI
print("Matriz de transición promedio:")
trans_matrix <- apply(modelo_lm4$PI, c(1,2), mean, na.rm = TRUE)
print(round(trans_matrix, 3))


modelo_lm4

# Resultados de regresión multinomial ------------------------------------------  
# Calculamos errores estándar
# --- Standard errors
errores_estandar <- se(modelo_lm4)

# K inferred from Be (which has K-1 columns vs the reference class)
K <- ncol(modelo_lm4$Be) + 1L
ref_class <- 1L
nonref    <- setdiff(seq_len(K), ref_class)

# Coefs and SEs as data frames, with clear column names
be_df <- as.data.frame(modelo_lm4$Be)
colnames(be_df) <- paste0("Class ", nonref, " vs Class ", ref_class)
be_df$Variable  <- rownames(modelo_lm4$Be)

se_df <- as.data.frame(errores_estandar$seBe)
colnames(se_df) <- paste0("Class ", nonref, " vs Class ", ref_class)
se_df$Variable  <- rownames(modelo_lm4$Be)

# Long format
be_long <- tidyr::pivot_longer(
  be_df,
  cols = starts_with("Class "),
  names_to = "Comparacion",
  values_to = "Coeficiente"
)

se_long <- tidyr::pivot_longer(
  se_df,
  cols = starts_with("Class "),
  names_to = "Comparacion",
  values_to = "SE"
)

be_long <- dplyr::left_join(be_long, se_long, by = c("Variable","Comparacion")) %>%
  mutate(
    IC_lower = Coeficiente - 1.96 * SE,
    IC_upper = Coeficiente + 1.96 * SE,
    OR          = exp(Coeficiente),
    OR_IC_lower = exp(IC_lower),
    OR_IC_upper = exp(IC_upper),
    significativo = (IC_lower * IC_upper > 0)
  ) %>%
  filter(!is.na(Variable), !is.na(Coeficiente), !is.na(IC_lower), !is.na(IC_upper)) %>%
  drop_na()

# Colors per comparison (works for K=3 or K=4; add more if needed)
colores_manuales <- c(
  "Class 2 vs Class 1" = "#4b3f72",
  "Class 3 vs Class 1" = "#ffc857",
  "Class 4 vs Class 1" = "#21f041"
)
# keep only those present
colores_manuales <- colores_manuales[names(colores_manuales) %in% unique(be_long$Comparacion)]

p3 <- ggplot(be_long, aes(x = Variable, y = Coeficiente, color = Comparacion)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = IC_lower, ymax = IC_upper),
                position = position_dodge(width = 0.5), width = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_manual(values = colores_manuales) +
  labs(
    title    = "",
    subtitle = "Coefficients relative to Class 1 (reference)",
    x = "",
    y = "Coefficient (log-odds)"
  ) +
  theme(
    axis.text.x   = element_text(size = 13),
    axis.text.y   = element_text(size = 13),
    legend.position = "bottom",
    legend.text   = element_text(size = 13),
    legend.title  = element_text(size = 14),
    plot.title    = element_text(size = 16),
    strip.text    = element_text(size = 12, face = "bold"),
    axis.title.x  = element_blank(),
    panel.spacing = unit(1, "lines")
  )

p3


#ggsave("code/latent_violence/image/p3.png", plot = p3, width = 11.5, height = 9)

be_long

# Tabla de resultados completa
tabla_resultados <- be_long %>%
  select(Variable, Comparacion, Coeficiente, SE, IC_lower, IC_upper, OR, OR_IC_lower, OR_IC_upper, significativo) %>%
  arrange(Variable, Comparacion)

print("Resultados completos (coeficientes, OR e intervalos de confianza):")
print(knitr::kable(tabla_resultados, digits = 3))

tabla_resultados %>%
  mutate(significativo = ifelse(significativo, "Sí", "No")) %>%
  kable(format = "html", digits = 3, booktabs = TRUE,
        col.names = c("Variable", "Comparación", "Coef.", "SE",
                      "IC Inf.", "IC Sup.", "OR", "OR IC Inf.", "OR IC Sup.", "Signif."),
        caption = "Resultados (log-odds y OR) relativos a Clase 1 (referencia)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  save_kable("outputs/tables/resultados_modelo.html")


# Análisis de patrones de respuesta por estado
print("Probabilidades de respuesta condicional por estado:")
print(round(modelo_lm4$Psi, 3))

# Matriz de transición promedio
trans_matrix <- apply(modelo_lm4$PI, c(1,2), mean, na.rm = TRUE)
print("Matriz de transición promedio:")
print(round(trans_matrix, 3))

trans_matrix


# Crear tibble de transiciones
transitions <- as_tibble(trans_matrix) %>%
  mutate(from = row_number()) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "probability"
  ) %>%
  mutate(to = as.integer(gsub("V", "", to))) 

# Paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales)

# ---- 1) Datos de ejemplo (usa los tuyos si ya existen) -----------------------
transitions <- tribble(
  ~from, ~to, ~probability,
  1, 1, 0.322, 1, 2, 0.179, 1, 3, 0.154, 1, 4, 0.0947,
  2, 1, 0.203, 2, 2, 0.282, 2, 3, 0.172, 2, 4, 0.0935,
  3, 1, 0.214, 3, 2, 0.152, 3, 3, 0.293, 3, 4, 0.0908,
  4, 1, 0.180, 4, 2, 0.194, 4, 3, 0.195, 4, 4, 0.182
)

# ---- 2) Heatmap en B/N con etiquetas en % -----------------------------------
p_heat <- transitions %>%
  mutate(
    from = factor(from, levels = sort(unique(from))),
    to   = factor(to,   levels = sort(unique(to))),
    lbl  = percent(probability, accuracy = 0.1)
  ) %>%
  ggplot(aes(x = to, y = from, fill = probability)) +
  geom_tile(color = "grey70", linewidth = 0.3) +
  # resalta la diagonal (auto-transición)
  geom_tile(data = ~ dplyr::filter(.x, from == to),
            fill = NA, color = "black", linewidth = 0.7) +
  geom_text(aes(label = lbl), size = 3) +
  scale_fill_gradient(
    name = "Probability",
    limits = c(0, 1),
    low = "white", high = "black",
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(x = "To (t+1)", y = "From (t)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.title.x = element_text(margin = margin(b = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )

p_heat


#


# Paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, networkD3, scales)

# ---- Ejemplo de datos (usa los tuyos si ya existen) --------------------------
# transitions <- tribble(
#   ~from, ~to, ~probability,
#   1,1,0.322, 1,2,0.179, 1,3,0.154, 1,4,0.0947,
#   2,1,0.203, 2,2,0.282, 2,3,0.172, 2,4,0.0935,
#   3,1,0.214, 3,2,0.152, 3,3,0.293, 3,4,0.0908,
#   4,1,0.180, 4,2,0.194, 4,3,0.195, 4,4,0.182
# )

# ---- Función general: nodo único "Stay" + cambios a otras clases -------------
plot_transitions_with_stay <- function(transitions, node_stay_label = "Stay (Permanecer)") {
  
  # Asegurar tipos
  df <- transitions %>%
    mutate(from = as.integer(from), to = as.integer(to),
           probability = as.numeric(probability))
  
  classes <- sort(unique(c(df$from, df$to)))
  K <- length(classes)
  
  # Nodos:
  #  - Clases en t
  #  - Nodo Stay (único)
  #  - Clases en t+1 (solo para cambios)
  nodes <- tibble(
    name = c(
      paste0("Class ", classes, " (t)"),
      node_stay_label,
      paste0("Class ", classes, " (t+1)")
    )
  )
  
  # Índices auxiliares
  idx_from_t   <- function(i) which(nodes$name == paste0("Class ", i, " (t)")) - 1L
  idx_to_t1    <- function(j) which(nodes$name == paste0("Class ", j, " (t+1)")) - 1L
  idx_stay     <- which(nodes$name == node_stay_label) - 1L
  
  # Enlaces:
  #  1) Permanencia: Class i (t) --> Stay   (con prob(i,i))
  links_stay <- df %>%
    filter(from == to) %>%
    transmute(
      source = map_int(from, idx_from_t),
      target = idx_stay,
      value  = probability,
      type   = "stay",
      tooltip = paste0("Class ", from, " → permanecer: ", percent(probability, 0.1))
    )
  
  #  2) Cambios: Class i (t) --> Class j (t+1), j != i
  links_change <- df %>%
    filter(from != to) %>%
    transmute(
      source = map_int(from, idx_from_t),
      target = map_int(to,   idx_to_t1),
      value  = probability,
      type   = "change",
      tooltip = paste0("Class ", from, " → Class ", to, ": ", percent(probability, 0.1))
    )
  
  links <- bind_rows(links_stay, links_change)
  
  # Escala de colores en B/N: stay = gris oscuro, change = gris claro
  # (networkD3 usa d3.rgb; definimos por tipo de enlace)
  # Nota: el color de links se mapea por 'group' si existe esa columna
  links <- links %>% mutate(group = type)
  
  colourJS <- 'd3.scaleOrdinal()
                 .domain(["stay","change"])
                 .range(["#4D4D4D","#BDBDBD"])'
  
  sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name",
    fontSize = 12, nodeWidth = 20, sinksRight = FALSE,
    LinkGroup = "group",
    colourScale = colourJS
  )
}

# ---- Llama a la función con tus datos ----------------------------------------
plot_transitions_with_stay(transitions)


## Crear nodos con nombres sustantivos
#nodes <- tibble(
#  name = 1:4,
#  label = c("Ambivalent", "Hardliners", "Pro-Indigenous", "Rejecters"),
#  size = c(0.285, 0.165, 0.37, 0.297),  # Probabilidades de permanencia
#  prop = c(27, 7, 15, 51)        # Proporción de la muestra
#)
#
## Crear el objeto de red
#graph <- tbl_graph(nodes = nodes, edges = transitions, directed = TRUE)
#
## Crear el gráfico
#p4 <- ggraph(graph, layout = 'circle') +
#  geom_edge_arc(
#    aes(label = sprintf("%.2f", probability)),
#    arrow = arrow(length = unit(3, 'mm'), type = "closed"),
#    angle_calc = 'along',
#    label_dodge = unit(4, 'mm'),
#    start_cap = circle(15, 'mm'),
#    end_cap = circle(18, 'mm'),
#    edge_width = 0.5,
#    strength = 0.15,
#    show.legend = FALSE
#  ) +
#  geom_node_point(
#    aes(size = size * 100),
#    color = "white",
#    fill = "#ffc857",
#    shape = 21,
#    stroke = 1
#  ) +
#  geom_node_text(
#    aes(label = sprintf("%s\n(%.1f%%)\np=%.2f", 
#                        label, prop, size)),
#    size = 3.8,
#    color = "#4b3f72"
#  ) +
#  scale_size_continuous(range = c(30, 45)) +  
#  theme_void() +
#  labs(title = "Latent Class Transition Graph") +
#  theme(
#    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#    plot.margin = margin(20, 20, 20, 20),
#    legend.position = "none"
#  ) +
#  coord_fixed(ratio = 1, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))
#
## Mostrar gráfico
#print(p4)
## Guardar gráfico
#ggsave("code/latent_violence/image/p4.png", plot = p4, width = 11.5, height = 9)
#

