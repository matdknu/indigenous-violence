# =============================================================================
# 00_denek_preparar_panel.R — Panel LCGA: violencia por cambio social (ELRI)
#
# Outcome: denek_vio_camb_soc — promedio d4_2 + d4_3 (idx_vio_resguardo)
# Escala: 1 = nada justificado ... 5 = totalmente justificado
#
# Olas ELRI (panel balanceado 4 olas):
#   Ola 1 (2016) | Ola 2 (2018) | Ola 3 (2020–2021) | Ola 4 (2023)
#
# Input:  ../causality/data/panel_completo.rds
# Output: data/data_proc/denek_panel_lcga.rds
#         data/data_proc/denek_lcga_metadata.rds
# =============================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, readr)

root_denek <- normalizePath("..", winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(root_denek, "causality", "data", "panel_completo.rds"))) {
  root_denek <- normalizePath(".", winslash = "/", mustWork = FALSE)
}
src_panel <- file.path(root_denek, "causality", "data", "panel_completo.rds")
if (!file.exists(src_panel)) {
  stop(
    "No se encuentra panel_completo.rds. Ejecute causality/R/01_limpieza.R primero.\n",
    "Ruta esperada: ", src_panel
  )
}

dir.create("data/data_proc", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

panel_elri <- readRDS(src_panel)

denek_olas_lcga <- c(1, 2, 3, 4)
denek_etiqueta_ola <- c("2016", "2018", "2020–2021", "2023")

df_denek_lcga <- panel_elri |>
  transmute(
    denek_id       = folio,
    denek_ola      = as.integer(ola),
    denek_vio_camb_soc = as.numeric(idx_vio_resguardo),
    denek_indigena = as.character(indigeneous),
    denek_zona_exc = as.character(cerca_conflicto)
  ) |>
  filter(denek_ola %in% denek_olas_lcga)

cat("--- denek_vio_camb_soc: N válido por ola (antes de balancear) ---\n")
print(
  df_denek_lcga |>
    group_by(denek_ola) |>
    summarise(
      n_valido = sum(!is.na(denek_vio_camb_soc)),
      n_total  = n(),
      .groups  = "drop"
    ) |>
    mutate(denek_anio = denek_etiqueta_ola[match(denek_ola, denek_olas_lcga)])
)

df_denek_lcga <- df_denek_lcga |> na.omit()

denek_ids_completos <- df_denek_lcga |>
  group_by(denek_id) |>
  filter(n() == length(denek_olas_lcga)) |>
  pull(denek_id) |>
  unique()

df_denek_lcga <- df_denek_lcga |>
  filter(denek_id %in% denek_ids_completos) |>
  mutate(
    denek_tiempo = match(denek_ola, denek_olas_lcga) - 1L,
    denek_anio   = factor(
      denek_etiqueta_ola[match(denek_ola, denek_olas_lcga)],
      levels = denek_etiqueta_ola
    )
  ) |>
  arrange(denek_id, denek_tiempo)

n_denek_personas <- length(unique(df_denek_lcga$denek_id))
n_denek_obs      <- nrow(df_denek_lcga)

cat("\n--- Panel LCGA denek (balanceado) ---\n")
cat("Personas:", n_denek_personas, "\n")
cat("Observaciones:", n_denek_obs, "(", n_denek_personas, "×", length(denek_olas_lcga), ")\n")
cat("Media denek_vio_camb_soc por ola:\n")
print(
  df_denek_lcga |>
    group_by(denek_ola, denek_anio) |>
    summarise(
      media = round(mean(denek_vio_camb_soc), 3),
      sd    = round(sd(denek_vio_camb_soc), 3),
      .groups = "drop"
    )
)

denek_lcga_metadata <- list(
  outcome        = "denek_vio_camb_soc",
  outcome_label  = "Justificación violencia cambio social (d4_2 + d4_3)",
  scale          = "1 = nada justificado, 5 = totalmente justificado",
  olas           = denek_olas_lcga,
  ola_labels     = denek_etiqueta_ola,
  n_personas     = n_denek_personas,
  n_obs          = n_denek_obs,
  source_panel   = src_panel,
  created        = Sys.time()
)

saveRDS(df_denek_lcga, "data/data_proc/denek_panel_lcga.rds")
saveRDS(denek_lcga_metadata, "data/data_proc/denek_lcga_metadata.rds")

write_csv(
  df_denek_lcga |>
    group_by(denek_ola, denek_anio) |>
    summarise(
      n = n(),
      media = mean(denek_vio_camb_soc),
      sd = sd(denek_vio_camb_soc),
      min = min(denek_vio_camb_soc),
      max = max(denek_vio_camb_soc),
      .groups = "drop"
    ),
  "output/tablas/denek_descriptivos_vio_camb.csv"
)

cat("\n✓ 00_denek_preparar_panel.R — guardado en data/data_proc/denek_panel_lcga.rds\n")
