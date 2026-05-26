# =============================================================================
# 09_diagramas.R — Diagramas conceptuales del mecanismo de mediación
#
# Propósito: generar (1) el modelo conceptual teórico y (2) el esquema de
#            mediación con coeficientes estimados. NO es un SEM: es mediación
#            causal por diferencia de coeficientes sobre diseño DiD.
# Input:     data/mecanismo.rds (coeficientes del esquema de mediación)
# Output:    output/figuras/fig_modelo_conceptual.png
#            output/figuras/fig_esquema_mediacion.png
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(DiagrammeR, dplyr, broom.mixed)

if (!dir.exists("output/figuras")) dir.create("output/figuras", recursive = TRUE)

# ── Helper: exportar grViz a PNG ─────────────────────────────────────────────
guardar_diagrama <- function(grviz_obj, path, width = 2400) {
  ok <- FALSE
  if (requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
      requireNamespace("rsvg", quietly = TRUE)) {
    tryCatch({
      svg <- DiagrammeRsvg::export_svg(grviz_obj)
      rsvg::rsvg_png(charToRaw(svg), file = path, width = width)
      ok <- TRUE
    }, error = function(e) {
      cat("⚠ export_svg/rsvg:", conditionMessage(e), "\n")
    })
  }
  if (!ok && requireNamespace("webshot2", quietly = TRUE)) {
    tmp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(grviz_obj, tmp_html, selfcontained = TRUE)
    webshot2::webshot(tmp_html, path, vwidth = width, vheight = round(width * 0.55))
    unlink(tmp_html)
    ok <- TRUE
  }
  if (!ok) {
    stop("No se pudo exportar el diagrama. Instale DiagrammeRsvg + rsvg o webshot2.")
  }
  cat("✓ Diagrama guardado:", path, "\n")
}

stars_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

# Estilo monocromático (publicación)
GRVIZ_NODE <- "fontname = Helvetica, shape = box, style = rounded,
         fillcolor = white, color = black, fontcolor = black,
         fontsize = 11, penwidth = 1.1"
GRVIZ_EDGE <- "fontname = Helvetica, fontsize = 10, color = black,
         fontcolor = black, penwidth = 1.0"
GRVIZ_EDGE_DASH <- "fontname = Helvetica, fontsize = 10, color = black,
         fontcolor = black, penwidth = 0.9, style = dashed"

# ── Diagrama 1: modelo conceptual (sin números) ────────────────────────────────

modelo_conceptual <- DiagrammeR::grViz(sprintf("
digraph modelo_conceptual {
  graph [layout = dot, rankdir = LR, fontname = Helvetica,
         bgcolor = white, nodesep = 0.65, ranksep = 1.15, splines = true]
  node  [%s]

  T   [label = 'Estado de excepción\\n(decreto · ola 4)', width = 1.9]
  M   [label = 'Justicia procedimental\\ningroup percibida', width = 2.0]
  Y1  [label = 'Justificación represión\\nestatal (control social)', width = 2.1]
  Y2  [label = 'Justificación resistencia\\n(cambio social)', width = 2.1]

  edge [%s]

  T -> M  [label = '  regularización (+)  ']
  M -> Y1 [label = '  legitimación\\n  procedimental (+)  ']
  M -> Y2 [label = '  supresión (−)  ', style = dashed]
  T -> Y1 [label = '  efecto directo  ', %s]
  T -> Y2 [label = '  efecto directo  ', %s]
}
", GRVIZ_NODE, GRVIZ_EDGE, GRVIZ_EDGE_DASH, GRVIZ_EDGE_DASH))

guardar_diagrama(modelo_conceptual, "output/figuras/fig_modelo_conceptual.png")

# ── Diagrama 2: esquema de mediación con coeficientes ────────────────────────

if (!file.exists("data/mecanismo.rds")) {
  stop("Ejecute R/05_mecanismo.R antes de 09_diagramas.R (falta data/mecanismo.rds).")
}

mec <- readRDS("data/mecanismo.rds")

TERM_DID_DEC <- "periododecreto:indigeneousindi:zona_decretodecreto"

b_T_M <- broom.mixed::tidy(mec$m1_ingroup, effects = "fixed") |>
  dplyr::filter(.data$term == TERM_DID_DEC)

if (nrow(b_T_M) == 0) {
  cand <- broom.mixed::tidy(mec$m1_ingroup, effects = "fixed") |>
    dplyr::filter(
      grepl("periododecreto", .data$term),
      grepl("indigeneousindi", .data$term),
      grepl("zona_decreto|cerca_conflicto", .data$term)
    )
  if (nrow(cand) == 1) b_T_M <- cand
}

comp <- mec$comparacion_atenuacion
aten_ctrl <- comp |> dplyr::filter(.data$vd == "Vio. control", .data$mediador == "Ingroup lag")
aten_resg <- comp |> dplyr::filter(.data$vd == "Vio. resguardo", .data$mediador == "Ingroup lag")

lab_T_M <- if (nrow(b_T_M) > 0) {
  sprintf("β = %.2f%s", b_T_M$estimate[1], stars_p(b_T_M$p.value[1]))
} else {
  "β = —"
}

lab_ctrl <- if (nrow(aten_ctrl) > 0) {
  sprintf("mediación %.0f%%", abs(aten_ctrl$atenuacion[1]))
} else {
  "mediación —"
}

lab_resg <- if (nrow(aten_resg) > 0) {
  sprintf("supresión %.0f%%", abs(aten_resg$atenuacion[1]))
} else {
  "supresión —"
}

esquema_mediacion <- DiagrammeR::grViz(sprintf("
digraph esquema_mediacion {
  graph [layout = dot, rankdir = LR, fontname = Helvetica, bgcolor = white,
         nodesep = 0.65, ranksep = 1.15, splines = true]
  node  [%s]

  T   [label = 'Estado de excepción\\n(decreto · ola 4)', width = 1.9]
  M   [label = 'Justicia procedimental\\ningroup (lag)', width = 2.0]
  Y1  [label = 'Represión estatal\\n(control social)', width = 2.1]
  Y2  [label = 'Resistencia\\n(cambio social)', width = 2.1]

  edge [%s]

  T -> M  [label = '  %s  ']
  M -> Y1 [label = '  %s  ']
  M -> Y2 [label = '  %s  ', style = dashed]
}
", GRVIZ_NODE, GRVIZ_EDGE, lab_T_M, lab_ctrl, lab_resg))

guardar_diagrama(esquema_mediacion, "output/figuras/fig_esquema_mediacion.png")

cat("\n✓ 09_diagramas.R ejecutado correctamente.\n")
