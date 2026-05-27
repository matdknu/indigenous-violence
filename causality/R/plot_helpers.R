# Helpers compartidos para figuras de trayectorias (escala Likert 1–5).

GRUPO_COLORES <- c(
  "No indígena / lejos"          = "#4575B4",
  "No indígena / zona excepción" = "#74ADD1",
  "Indígena / lejos"              = "#D73027",
  "Indígena / zona excepción"     = "#F46D43"
)

GRUPO_LINETYPES <- c(
  "No indígena / lejos"          = "dashed",
  "No indígena / zona excepción" = "solid",
  "Indígena / lejos"              = "dashed",
  "Indígena / zona excepción"     = "solid"
)

LIKERT_Y_MIN <- 1
LIKERT_Y_MAX <- 5

scale_y_likert_shared <- function() {
  scale_y_continuous(
    limits = c(LIKERT_Y_MIN, LIKERT_Y_MAX),
    breaks = LIKERT_Y_MIN:LIKERT_Y_MAX,
    expand = expansion(mult = c(0.02, 0.02))
  )
}

add_scale_grupo_trajectory <- function(p) {
  p +
    scale_color_manual(
      values = GRUPO_COLORES,
      name = NULL,
      guide = guide_legend(
        nrow = 2,
        ncol = 2,
        byrow = TRUE,
        override.aes = list(
          linetype = unname(GRUPO_LINETYPES),
          linewidth = 0.9,
          size = 0,
          fill = NA,
          shape = NA
        )
      )
    ) +
    scale_linetype_manual(values = GRUPO_LINETYPES, guide = "none") +
    scale_fill_manual(values = GRUPO_COLORES, guide = "none")
}

guides_grupo_unico <- function() {
  guides(
    color = guide_legend(
      nrow = 2,
      ncol = 2,
      byrow = TRUE,
      override.aes = list(
        linetype = unname(GRUPO_LINETYPES),
        linewidth = 0.9,
        size = 0,
        fill = NA,
        shape = NA
      )
    ),
    linetype = "none",
    fill = "none",
    shape = "none"
  )
}

theme_trajectory <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      strip.text       = element_text(face = "bold", size = base_size - 1),
      legend.position  = "bottom",
      legend.text      = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold")
    )
}
