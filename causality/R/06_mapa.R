# =============================================================================
# mapa_estado_excepcion.R
# Visualizar comunas en zona de estado de excepción constitucional (2021)
# vs. comunas fuera de la zona — La Araucanía y Biobío
# =============================================================================

pacman::p_load(
  dplyr, ggplot2, sf, stringr, janitor,
  chilemapas, rvest, scales, patchwork
)

# ── 1. Comunas del estado de excepción ───────────────────────────────────────

# Decreto D.S. N°418/2021 — Estado de emergencia Macrozona Sur (12 oct. 2021)
# Cubre TODAS las comunas de 4 provincias:
#   La Araucanía: Cautín (091) + Malleco (092)
#   Biobío:       Arauco (082) + Biobío-provincia (083)
# Fuente: Wikipedia / decreto presidencial

comunas_excepcion <- c(
  # ── Provincia de Cautín — La Araucanía (21 comunas) ──────────────────────
  "09101", # Temuco
  "09102", # Carahue
  "09103", # Cunco
  "09104", # Curarrehue
  "09105", # Freire
  "09106", # Galvarino
  "09107", # Gorbea
  "09108", # Lautaro
  "09109", # Loncoche
  "09110", # Melipeuco
  "09111", # Nueva Imperial
  "09112", # Padre Las Casas
  "09113", # Perquenco
  "09114", # Pitrufquén
  "09115", # Pucón
  "09116", # Saavedra
  "09117", # Teodoro Schmidt
  "09118", # Toltén
  "09119", # Vilcún
  "09120", # Villarrica
  "09121", # Cholchol
  # ── Provincia de Malleco — La Araucanía (11 comunas) ─────────────────────
  "09201", # Angol
  "09202", # Collipulli
  "09203", # Curacautín
  "09204", # Ercilla
  "09205", # Lonquimay
  "09206", # Los Sauces
  "09207", # Lumaco
  "09208", # Purén
  "09209", # Renaico
  "09210", # Traiguén
  "09211", # Victoria
  # ── Provincia de Arauco — Biobío (7 comunas) ──────────────────────────────
  "08201", # Lebu
  "08202", # Arauco
  "08203", # Cañete
  "08204", # Contulmo
  "08205", # Curanilahue
  "08206", # Los Álamos
  "08207", # Tirúa
  # ── Provincia de Biobío — Biobío (14 comunas) ─────────────────────────────
  "08301", # Los Ángeles
  "08302", # Antuco
  "08303", # Cabrero
  "08304", # Laja
  "08305", # Mulchén
  "08306", # Nacimiento
  "08307", # Negrete
  "08308", # Quilaco
  "08309", # Quilleco
  "08310", # San Rosendo
  "08311", # Santa Bárbara
  "08312", # Tucapel
  "08313", # Yumbel
  "08314"  # Alto Biobío
)
# Total: 53 comunas

# ── 2. Mapa base de comunas (chilemapas) ─────────────────────────────────────

# Cargar y forzar clase sf ANTES de cualquier operación dplyr
mapa <- chilemapas::mapa_comunas |>
  st_as_sf() |>                                      # ← CRÍTICO: fijar clase sf primero
  mutate(
    codigo_comuna = str_pad(as.character(codigo_comuna), 5, pad = "0"),
    estado_excepcion = case_when(
      codigo_comuna %in% comunas_excepcion          ~ "Zona de excepción",
      codigo_provincia %in% c("081","082","083",
                              "091","092","093")   ~ "Resto Araucanía / Biobío",
      TRUE                                          ~ "Resto del país"
    ),
    estado_excepcion = factor(
      estado_excepcion,
      levels = c("Zona de excepción",
                 "Resto Araucanía / Biobío",
                 "Resto del país")
    )
  )

# Nombres de comunas del decreto para etiquetas
# Usar codigos_territoriales de chilemapas en lugar de tibble manual
# Esto garantiza que los nombres estén completos y correctos para las 53 comunas
nombres_excepcion <- chilemapas::codigos_territoriales |>
  filter(codigo_comuna %in% comunas_excepcion) |>
  select(codigo_comuna, nombre_corto = nombre_comuna) |>
  # Abreviar nombres largos para que quepan en el mapa
  mutate(nombre_corto = case_when(
    nombre_corto == "Nueva Imperial"  ~ "Nva. Imperial",
    nombre_corto == "Padre las Casas" ~ "P. Las Casas",
    nombre_corto == "Teodoro Schmidt" ~ "T. Schmidt",
    nombre_corto == "San Rosendo"     ~ "S. Rosendo",
    nombre_corto == "Santa Barbara"   ~ "Sta. Bárbara",
    nombre_corto == "Los Angeles"     ~ "Los Ángeles",
    nombre_corto == "Los Alamos"      ~ "Los Álamos",
    nombre_corto == "Los Sauces"      ~ "Los Sauces",
    TRUE ~ nombre_corto
  ))

# Calcular centroides para etiquetas
mapa_excepcion <- mapa |>
  filter(codigo_comuna %in% comunas_excepcion) |>
  left_join(nombres_excepcion, by = "codigo_comuna") |>
  st_as_sf() |>
  mutate(
    centroide = st_centroid(geometry),
    lon = st_coordinates(centroide)[, 1],
    lat = st_coordinates(centroide)[, 2]
  )

# ── 3. MAPA 1 — Chile completo con zoom en zona de conflicto ─────────────────

# Mapa regional para contexto
mapa_regiones <- chilemapas::mapa_comunas |>
  st_as_sf() |>
  group_by(codigo_region) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

# Regiones de interés (08, 09, 10, 14)
regiones_conflicto <- c("08", "09")  # solo Biobío y La Araucanía

p_chile <- ggplot() +
  # Fondo: todas las comunas coloreadas
  geom_sf(
    data = mapa,
    aes(fill = estado_excepcion),
    color = "white", linewidth = 0.05
  ) +
  # Bordes regionales más gruesos
  geom_sf(
    data = mapa_regiones,
    fill = NA, color = "grey40", linewidth = 0.25
  ) +
  # Resaltar comunas de excepción con borde
  geom_sf(
    data = mapa |> filter(estado_excepcion == "Zona de excepción") |> st_as_sf(),
    fill = NA, color = "#B22222", linewidth = 0.6
  ) +
  scale_fill_manual(
    values = c(
      "Zona de excepción"        = "#D73027",
      "Resto Araucanía / Biobío" = "#FC8D59",
      "Resto del país"           = "#E8E8E8"
    ),
    name = NULL
  ) +
  coord_sf(xlim = c(-76, -65), ylim = c(-56, -17)) +
  labs(
    title    = "Estado de excepción constitucional\nde emergencia — Chile, 2021",
    subtitle = "22 comunas decretadas en octubre 2021",
    caption  = "Fuente: elaboración propia con datos ELRI y decreto D.S. N°418/2021"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(size = 7, color = "grey50", hjust = 0.5),
    legend.position = "bottom",
    legend.text   = element_text(size = 8),
    plot.margin   = margin(10, 10, 10, 10)
  )
p_chile

# ── 4. MAPA 2 — Zoom en La Araucanía y Biobío ────────────────────────────────

mapa_zoom <- mapa |>
  filter(codigo_region %in% regiones_conflicto) |>
  st_as_sf()

mapa_regiones_zoom <- mapa_regiones |>
  filter(codigo_region %in% regiones_conflicto) |>
  st_as_sf()

p_zoom <- ggplot() +
  geom_sf(
    data = mapa_zoom,
    aes(fill = estado_excepcion),
    color = "white", linewidth = 0.15
  ) +
  geom_sf(
    data = mapa_regiones_zoom,
    fill = NA, color = "grey30", linewidth = 0.4
  ) +
  geom_sf(
    data = mapa_zoom |> filter(estado_excepcion == "Zona de excepción") |> st_as_sf(),
    fill = NA, color = "#B22222", linewidth = 0.8
  ) +
  # Etiquetas de comunas del decreto
  ggrepel::geom_label_repel(
    data = mapa_excepcion,
    aes(x = lon, y = lat, label = nombre_corto),
    size = 2.2, fill = "white", color = "#B22222",
    label.padding = unit(0.15, "lines"),
    label.size = 0.2,
    max.overlaps = 30,
    segment.color = "#B22222",
    segment.size = 0.3,
    box.padding = 0.3,
    seed = 42
  ) +
  scale_fill_manual(
    values = c(
      "Zona de excepción"        = "#D73027",
      "Resto Araucanía / Biobío" = "#FEC89A",
      "Resto del país"           = "#E8E8E8"
    ),
    name = NULL,
    guide = "none"
  ) +
  coord_sf(
    xlim = c(-74.5, -70.0),
    ylim = c(-41.0, -36.5)
  ) +
  labs(
    title    = "Zoom — La Araucanía y Biobío",
    subtitle = "Comunas con estado de excepción (rojo) vs. resto de la zona"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    plot.margin   = margin(10, 10, 10, 10)
  )

# ── 5. MAPA 3 — Distribución ELRI: N respondentes por comuna ─────────────────
# Requiere subset_data con columna 'comuna'

if (file.exists("data/subset_data.rds")) {
  subset_data <- readRDS("data/subset_data.rds")
  
  # Contar respondentes únicos por comuna (ola 2 = baseline)
  n_por_comuna <- subset_data |>
    filter(ola == 2) |>
    mutate(
      # ELRI tiene comunas de 4 dígitos sin el 0 inicial
      codigo_comuna = str_pad(as.character(comuna), 5, pad = "0")
    ) |>
    count(codigo_comuna, name = "n_respondentes")
  
  mapa_elri <- mapa |>
    left_join(n_por_comuna, by = "codigo_comuna") |>
    mutate(
      tiene_datos    = !is.na(n_respondentes),
      n_respondentes = replace_na(n_respondentes, 0)
    ) |>
    st_as_sf()  # restaurar clase sf tras left_join
  
  mapa_elri_zoom  <- mapa_elri |> filter(codigo_region %in% regiones_conflicto)
  mapa_elri_excep <- mapa_elri |> filter(codigo_region %in% regiones_conflicto,
                                         estado_excepcion == "Zona de excepción")
  
  p_elri <- ggplot() +
    geom_sf(
      data = mapa_elri_zoom,
      aes(fill = n_respondentes),
      color = "white", linewidth = 0.15
    ) +
    geom_sf(
      data = mapa_regiones_zoom,
      fill = NA, color = "grey30", linewidth = 0.4
    ) +
    # Marcar comunas de excepción con contorno rojo
    geom_sf(
      data = mapa_elri_excep,
      fill = NA, color = "#B22222", linewidth = 0.8
    ) +
    scale_fill_distiller(
      palette = "YlOrRd", direction = 1,
      name = "N respondentes\n(baseline ola 2)",
      na.value = "grey90",
      labels = label_number()
    ) +
    coord_sf(xlim = c(-74.5, -70.5), ylim = c(-40.5, -36.5)) +
    labs(
      title    = "Distribución ELRI — Respondentes en zona de conflicto",
      subtitle = "Contorno rojo = zona de excepción · Ola 2 (baseline)"
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.title      = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "grey40"),
      legend.position = "right",
      legend.text     = element_text(size = 8),
      plot.margin     = margin(10, 10, 10, 10)
    )
  
  # Panel combinado: zoom geográfico + distribución ELRI
  p_panel <- p_zoom + p_elri +
    plot_annotation(
      title   = "Zona de estado de excepción constitucional (2021) y muestra ELRI",
      caption = "Contorno rojo = comunas incluidas en D.S. N°418/2021",
      theme   = theme(
        plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.caption = element_text(size = 7, color = "grey50", hjust = 0.5)
      )
    )
  
  ggsave("output/figuras/fig_mapa_elri_excepcion.png",
         p_panel, width = 12, height = 6, dpi = 300)
  cat("✓ Panel ELRI guardado: output/figuras/fig_mapa_elri_excepcion.png\n")
}

# ── 6. Figura principal — mapa completo + zoom (patchwork) ───────────────────

p_final <- p_chile + p_zoom +
  plot_layout(widths = c(1, 1.4)) +
  plot_annotation(
    title   = "Estado de excepción constitucional de emergencia — Chile, 2021",
    subtitle = paste0(
      "22 comunas de La Araucanía, Biobío y Los Lagos bajo decreto D.S. N°418/2021\n",
      "Contorno rojo = zona tratada en análisis cuasi-experimental (ELRI ola 3)"
    ),
    caption = "Fuente: elaboración propia · cartografía via {chilemapas}",
    theme   = theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
      plot.caption  = element_text(size = 7, color = "grey50", hjust = 0.5)
    )
  )

if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

ggsave("output/figuras/fig_mapa_estado_excepcion.png",
       p_final, width = 12, height = 8, dpi = 300)

cat("✓ Mapa principal guardado: output/figuras/fig_mapa_estado_excepcion.png\n")
cat("✓ Comunas bajo decreto:", length(comunas_excepcion), "(53 esperadas)\n")

# ── 7. Tabla resumen de comunas del decreto ───────────────────────────────────

tabla_comunas <- chilemapas::codigos_territoriales |>
  filter(codigo_comuna %in% comunas_excepcion) |>
  select(codigo_comuna, nombre_comuna, nombre_provincia, nombre_region) |>
  arrange(nombre_region, nombre_provincia, nombre_comuna)

cat("\n--- Comunas en zona de excepción ---\n")
print(tabla_comunas, n = 30)

