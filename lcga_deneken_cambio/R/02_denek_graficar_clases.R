# =============================================================================
# 02_denek_graficar_clases.R — Trayectorias observadas por clase latente
#
# Input:  data/data_proc/denek_lcga_resultados.rds
# Output: output/figuras/denek_trayectorias_vio_camb_lcga.png
#         output/tablas/denek_medias_por_clase.csv
# =============================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, readr, scales)

if (!file.exists("data/data_proc/denek_lcga_resultados.rds")) {
  stop("Ejecute 01_denek_estimar_lcga.R primero.")
}

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

denek_res <- readRDS("data/data_proc/denek_lcga_resultados.rds")
df_denek_lcga <- denek_res$denek_panel_con_clase
denek_modelo  <- denek_res$denek_modelo_final
denek_meta      <- denek_res$denek_metadata

# ── Etiquetas descriptivas por trayectoria observada ─────────────────────────
denek_pct_clase <- df_denek_lcga |>
  dplyr::count(denek_clase, name = "denek_n_clase") |>
  dplyr::mutate(
    denek_pct = round(100 * denek_n_clase / sum(denek_n_clase), 1)
  )

denek_perfil_clases <- df_denek_lcga |>
  group_by(denek_clase, denek_tiempo, denek_anio) |>
  summarise(media = mean(denek_vio_camb_soc), .groups = "drop") |>
  group_by(denek_clase) |>
  summarise(
    media_ini = media[denek_tiempo == 0],
    media_fin = media[denek_tiempo == max(denek_tiempo)],
    delta     = media_fin - media_ini,
    media_avg = mean(media),
    .groups   = "drop"
  ) |>
  left_join(denek_pct_clase, by = "denek_clase") |>
  mutate(
    denek_etiqueta_clase = dplyr::case_when(
      delta >= 0.35 & media_avg >= 3 ~ paste0(
        "Apertura normativa ascendente (", denek_pct, "%)"
      ),
      delta <= -0.35 ~ paste0(
        "Rechazo creciente / declive (", denek_pct, "%)"
      ),
      abs(delta) < 0.15 & media_avg >= 3.2 ~ paste0(
        "Justificación alta y estable (", denek_pct, "%)"
      ),
      abs(delta) < 0.15 & media_avg < 2.5 ~ paste0(
        "Rechazo persistente (", denek_pct, "%)"
      ),
      delta > 0 ~ paste0(
        "Incremento moderado (", denek_pct, "%)"
      ),
      TRUE ~ paste0(
        "Trayectoria intermedia (", denek_pct, "%)"
      )
    )
  )

denek_mapa_etiquetas <- setNames(
  denek_perfil_clases$denek_etiqueta_clase,
  denek_perfil_clases$denek_clase
)

df_denek_lcga <- df_denek_lcga |>
  mutate(
    denek_clase_etq = factor(
      denek_mapa_etiquetas[as.character(denek_clase)],
      levels = denek_perfil_clases$denek_etiqueta_clase[
        order(denek_perfil_clases$media_avg, decreasing = TRUE)
      ]
    )
  )

# ── Medias observadas con IC 95% ─────────────────────────────────────────────
df_denek_plot <- df_denek_lcga |>
  group_by(denek_tiempo, denek_anio, denek_clase, denek_clase_etq) |>
  summarise(
    denek_media = mean(denek_vio_camb_soc, na.rm = TRUE),
    denek_sd    = sd(denek_vio_camb_soc, na.rm = TRUE),
    denek_n     = n(),
    denek_se    = denek_sd / sqrt(denek_n),
    denek_ci_lo = denek_media - 1.96 * denek_se,
    denek_ci_hi = denek_media + 1.96 * denek_se,
    .groups = "drop"
  )

write_csv(df_denek_plot, "output/tablas/denek_medias_por_clase.csv")
write_csv(denek_perfil_clases, "output/tablas/denek_perfil_clases.csv")

n_denek <- denek_meta$n_personas
ng_denek <- denek_modelo$ng
denek_nom_modelo <- denek_res$denek_mejor_nombre

p_denek_trayectorias <- ggplot(
  df_denek_plot,
  aes(
    x = denek_anio,
    y = denek_media,
    colour = denek_clase_etq,
    group = denek_clase_etq
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = denek_ci_lo, ymax = denek_ci_hi),
    width = 0.08,
    linewidth = 0.5
  ) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Justificación de violencia por cambio social — trayectorias LCGA (ELRI)",
    subtitle = paste0(
      "Panel balanceado: 2016, 2018, 2020–2021, 2023 (N = ",
      format(n_denek, big.mark = " ", decimal.mark = ","),
      ") | Modelo: ", denek_nom_modelo, " (", ng_denek, " clases)"
    ),
    x = "Ola",
    y = "Media observada (1 = nada justificado, 5 = totalmente justificado)",
    colour = "Clase latente"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  "output/figuras/denek_trayectorias_vio_camb_lcga.png",
  p_denek_trayectorias,
  width = 10,
  height = 6.5,
  dpi = 300
)

# Actualizar RDS con etiquetas
denek_res$denek_panel_con_clase <- df_denek_lcga
denek_res$denek_perfil_clases   <- denek_perfil_clases
denek_res$denek_mapa_etiquetas  <- denek_mapa_etiquetas
saveRDS(denek_res, "data/data_proc/denek_lcga_resultados.rds")
saveRDS(df_denek_lcga, "data/data_proc/denek_panel_lcga_clases.rds")

cat("✓ Figura: output/figuras/denek_trayectorias_vio_camb_lcga.png\n")
cat("✓ Tablas: output/tablas/denek_medias_por_clase.csv\n")
cat("✓ 02_denek_graficar_clases.R completado.\n")
