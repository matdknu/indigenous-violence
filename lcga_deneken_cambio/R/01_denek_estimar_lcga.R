# =============================================================================
# 01_denek_estimar_lcga.R — LCGA violencia cambio social (ELRI)
#
# Especificación (intercepto + pendiente fija por clase):
#   denek_vio_camb_soc ~ denek_tiempo + mixture(~ denek_tiempo)
#
# Variante cuadrática:
#   denek_vio_camb_soc ~ denek_tiempo + I(denek_tiempo^2) + mixture(...)
#
# Output: data/data_proc/denek_lcga_resultados.rds
# =============================================================================

rm(list = ls())

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(lcmm, dplyr, readr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x

if (!file.exists("data/data_proc/denek_panel_lcga.rds")) {
  stop("Ejecute 00_denek_preparar_panel.R primero.")
}

df_denek_lcga <- readRDS("data/data_proc/denek_panel_lcga.rds")
denek_lcga_metadata <- readRDS("data/data_proc/denek_lcga_metadata.rds")

cat("═══════════════════════════════════════════════════════════\n")
cat("  LCGA denek — denek_vio_camb_soc (violencia cambio social)\n")
cat("  N =", denek_lcga_metadata$n_personas, "personas ×",
    length(denek_lcga_metadata$olas), "olas =",
    denek_lcga_metadata$n_obs, "obs\n")
cat("═══════════════════════════════════════════════════════════\n\n")

denek_checkpoint <- "data/data_proc/denek_lcga_modelos_crudos.rds"

if (file.exists(denek_checkpoint)) {
  cat(">>> Cargando modelos desde checkpoint (omitir re-estimación)...\n")
  denek_ck <- readRDS(denek_checkpoint)
  denek_modelos_candidatos <- denek_ck$denek_modelos
  denek_criterios          <- denek_ck$denek_criterios
  denek_mejor_nombre       <- denek_ck$denek_mejor_nombre
  denek_tab_lineal         <- denek_ck$denek_tab_lineal
  denek_tab_completa       <- denek_ck$denek_tab_completa
} else {

# ── Modelos lineales (1–4 clases) ────────────────────────────────────────────
cat(">>> Modelos lineales (puede demorar varios minutos)...\n")

denek_lcga1 <- hlme(
  denek_vio_camb_soc ~ denek_tiempo,
  subject = "denek_id",
  ng = 1,
  data = df_denek_lcga
)

denek_lcga2 <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo,
    subject = "denek_id",
    ng = 2,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo
  )
)

denek_lcga3 <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo,
    subject = "denek_id",
    ng = 3,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo
  )
)

denek_lcga4 <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo,
    subject = "denek_id",
    ng = 4,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo
  )
)

cat("\n--- Criterios de ajuste: modelos lineales ---\n")
denek_tab_lineal <- summarytable(
  denek_lcga1, denek_lcga2, denek_lcga3, denek_lcga4,
  which = c("AIC", "BIC", "entropy", "conv", "loglik", "npm", "%class")
)
print(denek_tab_lineal)

# ── Modelos cuadráticos (1–4 clases) ─────────────────────────────────────────
cat("\n>>> Modelos cuadráticos...\n")

denek_lcga1_sq <- hlme(
  denek_vio_camb_soc ~ denek_tiempo + I(denek_tiempo^2),
  subject = "denek_id",
  ng = 1,
  data = df_denek_lcga
)

denek_lcga2_sq <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1_sq,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo + I(denek_tiempo^2),
    subject = "denek_id",
    ng = 2,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo + I(denek_tiempo^2)
  )
)

denek_lcga3_sq <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1_sq,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo + I(denek_tiempo^2),
    subject = "denek_id",
    ng = 3,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo + I(denek_tiempo^2)
  )
)

denek_lcga4_sq <- gridsearch(
  rep = 100, maxiter = 10, minit = denek_lcga1_sq,
  m = hlme(
    denek_vio_camb_soc ~ denek_tiempo + I(denek_tiempo^2),
    subject = "denek_id",
    ng = 4,
    data = df_denek_lcga,
    mixture = ~ denek_tiempo + I(denek_tiempo^2)
  )
)

cat("\n--- Criterios de ajuste: todos los modelos ---\n")
denek_tab_completa <- summarytable(
  denek_lcga1, denek_lcga2, denek_lcga3, denek_lcga4,
  denek_lcga1_sq, denek_lcga2_sq, denek_lcga3_sq, denek_lcga4_sq,
  which = c("AIC", "BIC", "entropy", "conv", "loglik", "npm", "%class")
)
print(denek_tab_completa)

# ── Selección automática por menor BIC (entre convergentes) ───────────────────
denek_modelos_candidatos <- list(
  denek_lcga1      = denek_lcga1,
  denek_lcga2      = denek_lcga2,
  denek_lcga3      = denek_lcga3,
  denek_lcga4      = denek_lcga4,
  denek_lcga1_sq   = denek_lcga1_sq,
  denek_lcga2_sq   = denek_lcga2_sq,
  denek_lcga3_sq   = denek_lcga3_sq,
  denek_lcga4_sq   = denek_lcga4_sq
)

denek_criterios <- tibble::tibble(
  modelo = names(denek_modelos_candidatos),
  AIC    = vapply(denek_modelos_candidatos, function(m) m$AIC, numeric(1)),
  BIC    = vapply(denek_modelos_candidatos, function(m) m$BIC, numeric(1)),
  entropy = vapply(denek_modelos_candidatos, function(m) {
    if (!is.null(m$entropy)) m$entropy else NA_real_
  }, numeric(1)),
  conv   = vapply(denek_modelos_candidatos, function(m) m$conv, numeric(1)),
  ng     = vapply(denek_modelos_candidatos, function(m) m$ng, numeric(1))
) |>
  arrange(BIC)

write_csv(denek_criterios, "output/tablas/denek_lcga_criterios_fit.csv")

denek_mejor_nombre <- denek_criterios |>
  dplyr::filter(conv == 1 | conv == 2) |>
  dplyr::slice_min(BIC, n = 1) |>
  dplyr::pull(modelo)

if (length(denek_mejor_nombre) == 0) {
  denek_mejor_nombre <- denek_criterios |>
    dplyr::slice_min(BIC, n = 1) |>
    dplyr::pull(modelo)
  warning("Ningún modelo con conv=1/2; se usa el de menor BIC de todos modos.")
}

} # fin else checkpoint

denek_mejor_nombre <- denek_mejor_nombre %||% (
  denek_criterios |>
    dplyr::filter(conv == 1 | conv == 2) |>
    dplyr::slice_min(BIC, n = 1) |>
    dplyr::pull(modelo)
)

denek_modelo_final <- denek_modelos_candidatos[[denek_mejor_nombre]]

cat("\n>>> Modelo seleccionado (menor BIC entre convergentes):", denek_mejor_nombre, "\n")
cat("    BIC =", denek_modelo_final$BIC, "| ng =", denek_modelo_final$ng, "\n")

saveRDS(
  list(
    denek_modelos        = denek_modelos_candidatos,
    denek_criterios      = denek_criterios,
    denek_mejor_nombre   = denek_mejor_nombre,
    denek_tab_lineal     = denek_tab_lineal,
    denek_tab_completa   = denek_tab_completa
  ),
  "data/data_proc/denek_lcga_modelos_crudos.rds"
)

# Asignación modal de clase (pprob usa nombre del subject = denek_id)
denek_pprob <- as.data.frame(denek_modelo_final$pprob)
denek_prob_cols <- grep("^prob", names(denek_pprob), value = TRUE)
denek_pprob$denek_prob_clase <- do.call(
  pmax,
  denek_pprob[denek_prob_cols]
)

denek_clases <- denek_pprob |>
  dplyr::rename(denek_clase = class) |>
  dplyr::select(denek_id, denek_clase, denek_prob_clase)

df_denek_lcga <- df_denek_lcga |>
  left_join(denek_clases, by = "denek_id")

saveRDS(
  list(
    denek_modelo_final   = denek_modelo_final,
    denek_mejor_nombre   = denek_mejor_nombre,
    denek_modelos        = denek_modelos_candidatos,
    denek_criterios      = denek_criterios,
    denek_tab_lineal     = denek_tab_lineal,
    denek_tab_completa   = denek_tab_completa,
    denek_clases         = denek_clases,
    denek_panel_con_clase = df_denek_lcga,
    denek_metadata       = denek_lcga_metadata
  ),
  "data/data_proc/denek_lcga_resultados.rds"
)

cat("\n✓ 01_denek_estimar_lcga.R — guardado en data/data_proc/denek_lcga_resultados.rds\n")
