# =============================================================================
# mapa_estado_excepcion.R
# Visualizar comunas en zona de estado de excepción constitucional (2021)
# vs. comunas fuera de la zona — La Araucanía y Biobío
# =============================================================================

pacman::p_load(
  dplyr, tidyr, ggplot2, sf, stringr, janitor,
  chilemapas, rvest, scales, patchwork, haven
)

ISLAND_COMUNAS <- c("05104", "05201", "12202")

clip_mainland <- function(sf_obj) {
  crs <- st_crs(sf_obj)
  box <- st_as_sfc(
    st_bbox(c(xmin = -76.0, ymin = -56.2, xmax = -66.3, ymax = -17.5), crs = crs)
  )
  out <- suppressWarnings(st_intersection(st_make_valid(sf_obj), box))
  out <- st_collection_extract(out, "POLYGON", warn = FALSE)
  st_as_sf(out)
}

# Norte a la izquierda, sur a la derecha: (lon, lat) → (-lat, lon)
coords_horizontal <- function(sf_obj) {
  m <- st_coordinates(sf_obj)
  data.frame(
    lon = m[, "X"],
    lat = m[, "Y"],
    poly_id = m[, "L1"],
    ring = if ("L2" %in% colnames(m)) m[, "L2"] else 1L,
    part = if ("L3" %in% colnames(m)) m[, "L3"] else 1L,
    hx = -m[, "Y"],
    hy = m[, "X"],
    stringsAsFactors = FALSE
  )
}

resolve_ump_path <- function() {
  candidates <- c(
    "data/UMP.dta",
    Sys.getenv("ELRI_UMP_PATH", unset = ""),
    "../../../social-data-science/ai_simultation/ELRI/data/UMP.dta",
    "../../../CIIR/ELRI/BBDD/UMP.dta"
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) return(normalizePath(hit[[1]]))
  stop(
    "No se encontró UMP.dta. Coloque el archivo en data/UMP.dta ",
    "o defina ELRI_UMP_PATH."
  )
}

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

# ── 8. Mapa horizontal UMP + muestra ELRI (estilo ELRI/05) ───────────────────
# Réplica de mapa_ump_puntos_horizontal: Chile rotado 90° (norte ← sur →),
# polígonos comunales + puntos en centroide (tamaño = N UMP, color = composición).

if (file.exists("data/subset_data.rds")) {
  subset_data <- readRDS("data/subset_data.rds")

  ump_path <- resolve_ump_path()
  cat("✓ UMP:", ump_path, "\n")

  ump_lookup <- read_dta(ump_path) |>
    transmute(
      folio_pad = as.character(folio),
      ump = as.integer(ump),
      manzana = as.integer(substr(as.character(folio), 5, 7))
    ) |>
    distinct(folio_pad, .keep_all = TRUE)

  resumen_folio <- subset_data |>
    filter(ola == 2) |>
    distinct(folio, .keep_all = TRUE) |>
    mutate(
      folio_pad = sprintf("%010d", as.integer(folio)),
      comuna_pad = str_pad(as.character(comuna_cod), 5, pad = "0"),
      es_indigena = as.integer(indigeneous == "indi")
    ) |>
    left_join(ump_lookup, by = "folio_pad")

  pct_ump <- round(100 * mean(!is.na(resumen_folio$ump)), 1)
  if (pct_ump < 95) {
    warning("Solo ", pct_ump, "% de folios con UMP pegado.")
  }

  comuna_stats_ump <- resumen_folio |>
    group_by(comuna_pad) |>
    summarise(
      n_folios = n(),
      n_indigena = sum(es_indigena),
      pct_indigena = round(100 * mean(es_indigena), 1),
      n_ump = n_distinct(ump, na.rm = TRUE),
      n_manzanas = n_distinct(manzana, na.rm = TRUE),
      en_zona_excepcion = any(comuna_pad %in% comunas_excepcion),
      .groups = "drop"
    )

  mapa_ump_base <- chilemapas::mapa_comunas |>
    st_as_sf() |>
    mutate(codigo_comuna = str_pad(as.character(codigo_comuna), 5, pad = "0")) |>
    filter(!codigo_comuna %in% ISLAND_COMUNAS) |>
    mutate(cx = st_coordinates(st_centroid(geometry))[, 1]) |>
    filter(cx > -76.5) |>
    left_join(comuna_stats_ump, by = c("codigo_comuna" = "comuna_pad")) |>
    mutate(
      en_muestra = !is.na(n_folios) & n_folios > 0,
      n_folios = replace_na(n_folios, 0L),
      en_zona_excepcion = replace_na(en_zona_excepcion, FALSE),
      fill_zona = if_else(
        en_zona_excepcion,
        "Zona decreto D.S. 418",
        if_else(en_muestra, "Muestra ELRI", "Sin muestra")
      ),
      fill_zona = factor(
        fill_zona,
        levels = c("Zona decreto D.S. 418", "Muestra ELRI", "Sin muestra")
      )
    )

  mapa_ump_main <- clip_mainland(mapa_ump_base)

  centroides_ump <- mapa_ump_main |>
    filter(en_muestra) |>
    mutate(
      grupo_dom = case_when(
        pct_indigena >= 60 ~ "Mayoría indígena",
        pct_indigena <= 40 ~ "Mayoría no indígena",
        TRUE ~ "Mixta"
      ),
      pt = st_point_on_surface(geometry)
    ) |>
    mutate(
      lon = st_coordinates(pt)[, 1],
      lat = st_coordinates(pt)[, 2],
      hx = -lat,
      hy = lon
    )

  mapa_poly_h <- coords_horizontal(mapa_ump_main) |>
    left_join(
      st_drop_geometry(mapa_ump_main) |> mutate(poly_id = seq_len(n())),
      by = "poly_id"
    )

  mapa_poly_h <- mapa_poly_h |>
    mutate(
      fill_zona = factor(
        fill_zona,
        levels = c("Zona decreto D.S. 418", "Muestra ELRI", "Sin muestra")
      )
    )

  map_theme_h <- theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      legend.position = "right",
      axis.text.x = element_text(size = 9, color = "grey50")
    )

  p_ump_h <- ggplot() +
    geom_polygon(
      data = mapa_poly_h,
      aes(
        x = hx, y = hy,
        group = interaction(poly_id, ring, part),
        fill = fill_zona
      ),
      color = "grey85", linewidth = 0.1
    ) +
    geom_point(
      data = centroides_ump,
      aes(x = hx, y = hy, size = n_ump, color = grupo_dom),
      alpha = 0.78
    ) +
    scale_fill_manual(
      values = c(
        "Zona decreto D.S. 418" = "#fcbba1",
        "Muestra ELRI"          = "#fafafa",
        "Sin muestra"           = "#f0f0f0"
      ),
      name = "Territorio"
    ) +
    scale_size_continuous(range = c(2, 10), name = "N UMP") +
    scale_color_manual(
      values = c(
        "Mayoría indígena"     = "#2a9d8f",
        "Mayoría no indígena"  = "#e76f51",
        "Mixta"                = "#9b59b6"
      ),
      name = "Composición\nmuestra"
    ) +
    coord_fixed(ratio = 1, expand = FALSE) +
    labs(
      title = "Muestra ELRI y clusters UMP — Macrozona Sur",
      subtitle = paste0(
        "Orientación horizontal · Norte (izq.) → Sur (der.) · ",
        nrow(resumen_folio), " folios · ", n_distinct(resumen_folio$ump), " UMP"
      ),
      caption = paste0(
        "Puntos en centroide comunal. Tamaño = N UMP. ",
        "Sombreado = 53 comunas D.S. N°418/2021. Baseline ola 2 (2018)."
      ),
      x = "← Norte · Sur →",
      y = NULL
    ) +
    map_theme_h

  if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

  ggsave(
    "output/figuras/fig_mapa_ump_puntos_horizontal.png",
    p_ump_h, width = 14, height = 6, dpi = 300
  )
  ggsave(
    "output/figuras/fig_mapa_ump_puntos_horizontal.svg",
    p_ump_h, width = 14, height = 6
  )

  cat("✓ Mapa UMP horizontal → output/figuras/fig_mapa_ump_puntos_horizontal.png\n")
}

