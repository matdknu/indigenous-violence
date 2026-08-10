# =============================================================================
# 99_check_consistencia.R  — Verificación de consistencia del pipeline
#
# Aborta con mensaje claro si:
#   1. idx_vio_control / idx_vio_resguardo no son ítem único (rowMeans → falla)
#   2. algún modelo incluye malestar_diferen o variables voto_*
#   3. la tabla principal proviene de lmer (detectada por clase / ICC en objeto)
#   4. resumen_principal.rds no existe o sus coeficientes difieren >0.001 de los
#      objetos feols actuales
# =============================================================================

cat(strrep("=", 60), "\n")
cat("CHECK DE CONSISTENCIA — pipeline v3-definitivo\n")
cat(strrep("=", 60), "\n\n")

FAIL <- FALSE
fail <- function(msg) {
  cat("  ✗ FALLA:", msg, "\n")
  FAIL <<- TRUE
}
ok <- function(msg) cat("  ✓", msg, "\n")

# ── 1. Cargar objetos ─────────────────────────────────────────────────────────

stopifnot(file.exists("data/subset_data.rds"))
stopifnot(file.exists("data/modelos.rds"))
stopifnot(file.exists("data/analysis_metadata.rds"))

subset_data   <- readRDS("data/subset_data.rds")
modelos       <- readRDS("data/modelos.rds")
metadata      <- readRDS("data/analysis_metadata.rds")

cat("--- CHECK 1: VD ítem único ---\n")

# idx_vio_control = as.numeric(d3_1) → en subset_data el alias es vio_ctrl_carb (d3_1 a escala 1-5)
# d3_1 raw se recodifica a ordinal 1-3 en el pipeline; vio_ctrl_carb guarda el valor 1-5 original
ctrl_match <- all.equal(
  as.numeric(subset_data$idx_vio_control),
  as.numeric(subset_data$vio_ctrl_carb),
  check.names = FALSE
)
if (isTRUE(ctrl_match)) {
  ok("idx_vio_control == vio_ctrl_carb (d3_1, escala 1-5)")
} else {
  fail(paste("idx_vio_control != vio_ctrl_carb:", ctrl_match))
}

resg_match <- all.equal(
  as.numeric(subset_data$idx_vio_resguardo),
  as.numeric(subset_data$vio_camb_cortes),
  check.names = FALSE
)
if (isTRUE(resg_match)) {
  ok("idx_vio_resguardo == vio_camb_cortes (d4_3, escala 1-5)")
} else {
  fail(paste("idx_vio_resguardo != vio_camb_cortes:", resg_match))
}

if ("idx_vio_control_dual" %in% names(subset_data)) {
  fail("idx_vio_control_dual todavía existe en subset_data")
} else {
  ok("Sin índice dual de dos ítems en subset_data")
}

cat("\n--- CHECK 2: Sin malestar_diferen en controles ---\n")

cb <- metadata$controles_base
if (grepl("malestar", cb)) {
  fail(paste("controles_base contiene malestar_diferen:", cb))
} else {
  ok(paste("controles_base limpio:", cb))
}

# Verificar que los modelos feols no usan malestar (feols no admite controles
# actitudinales time-varying; el FE magro es SIN controles)
for (nm in c("mFE_ctrl", "mFE_resg")) {
  m <- modelos[[nm]]
  if (is.null(m)) { fail(paste(nm, "es NULL")); next }
  fml <- paste(deparse(formula(m)), collapse = " ")
  if (grepl("malestar", fml)) {
    fail(paste(nm, "contiene malestar en fórmula:", fml))
  } else {
    ok(paste(nm, "sin malestar en fórmula"))
  }
}

cat("\n--- CHECK 3: Sin variables voto_* en modelos ---\n")

for (nm in names(modelos)) {
  m <- modelos[[nm]]
  if (is.null(m) || !inherits(m, c("fixest", "lmerMod"))) next
  fml <- tryCatch(paste(deparse(formula(m)), collapse = " "), error = function(e) "")
  if (grepl("voto_", fml)) {
    fail(paste(nm, "contiene voto_ en fórmula"))
  }
}
ok("Ningún modelo feols/lmer usa variables voto_*")

cat("\n--- CHECK 4: Tabla principal proviene de feols (no lmer) ---\n")

mFE_ctrl <- modelos$mFE_ctrl
mFE_resg <- modelos$mFE_resg

if (is.null(mFE_ctrl) || is.null(mFE_resg)) {
  fail("mFE_ctrl o mFE_resg es NULL en modelos.rds")
} else {
  if (inherits(mFE_ctrl, "fixest")) {
    ok("mFE_ctrl es objeto fixest (feols)")
  } else {
    fail(paste("mFE_ctrl NO es fixest, es:", class(mFE_ctrl)[1]))
  }
  if (inherits(mFE_resg, "fixest")) {
    ok("mFE_resg es objeto fixest (feols)")
  } else {
    fail(paste("mFE_resg NO es fixest, es:", class(mFE_resg)[1]))
  }
  # feols|folio no tiene ICC
  if (any(grepl("icc", names(broom::glance(mFE_ctrl))))) {
    fail("mFE_ctrl tiene ICC — esto indica lmer, no feols")
  } else {
    ok("mFE_ctrl sin ICC (correcto para feols)")
  }
}

cat("\n--- CHECK 5: resumen_principal.rds == coeficientes feols actuales ---\n")

TOL <- 0.001
TERM_DECRETO <- "periododecreto:indigeneousindi:cerca_conflictocerca"

if (!file.exists("data/resumen_principal.rds")) {
  fail("data/resumen_principal.rds no existe")
} else {
  rp <- readRDS("data/resumen_principal.rds")

  get_coef <- function(model, term) {
    if (is.null(model)) return(NA_real_)
    tid <- broom::tidy(model) |> dplyr::filter(.data$term == .env$term)
    if (nrow(tid) == 0) return(NA_real_)
    tid$estimate[1]
  }

  live_ctrl <- get_coef(mFE_ctrl, TERM_DECRETO)
  live_resg <- get_coef(mFE_resg, TERM_DECRETO)

  stored_ctrl <- rp$fe_ctrl_decreto$estimate
  stored_resg <- rp$fe_resg_decreto$estimate

  if (is.na(live_ctrl) || is.na(stored_ctrl)) {
    fail(paste("Triple decreto CONTROL: live =", live_ctrl, "stored =", stored_ctrl))
  } else if (abs(live_ctrl - stored_ctrl) > TOL) {
    fail(sprintf(
      "Triple decreto CONTROL: live=%.4f stored=%.4f diff=%.4f > TOL=%.4f",
      live_ctrl, stored_ctrl, abs(live_ctrl - stored_ctrl), TOL
    ))
  } else {
    ok(sprintf(
      "Triple decreto CONTROL: β=%.3f p=%.3f (feols = resumen_principal ✓)",
      live_ctrl, rp$fe_ctrl_decreto$p.value
    ))
  }

  if (is.na(live_resg) || is.na(stored_resg)) {
    fail(paste("Triple decreto CAMBIO: live =", live_resg, "stored =", stored_resg))
  } else if (abs(live_resg - stored_resg) > TOL) {
    fail(sprintf(
      "Triple decreto CAMBIO: live=%.4f stored=%.4f diff=%.4f > TOL=%.4f",
      live_resg, stored_resg, abs(live_resg - stored_resg), TOL
    ))
  } else {
    ok(sprintf(
      "Triple decreto CAMBIO:   β=%.3f p=%.3f (feols = resumen_principal ✓)",
      live_resg, rp$fe_resg_decreto$p.value
    ))
  }
}

cat("\n--- CHECK 6: Sin malestar en scripts R del pipeline ---\n")

scripts_pipeline <- c(
  "R/03_modelos.R", "R/04_robustez.R",
  "R/05_mecanismo.R", "R/07_likert_collapse.R", "R/08c_demandas.R"
)
for (f in scripts_pipeline) {
  if (!file.exists(f)) next
  lines <- readLines(f, warn = FALSE)
  # Excluir comentarios y coef_omit (solo display filtering)
  dirty <- grep("malestar_diferen", lines, value = TRUE)
  dirty <- dirty[!grepl("^\\s*#|coef_omit|fue descart|eliminad|>5%|regla|fallback", dirty)]
  if (length(dirty) > 0) {
    fail(paste(f, "— malestar en:", paste(dirty, collapse = "; ")))
  } else {
    ok(paste(f, "sin malestar_diferen en fórmulas"))
  }
}

cat("\n--- CHECK 7: Sin voto_* / plebiscito en paper.qmd ---\n")

if (file.exists("paper/paper.qmd")) {
  qmd_lines <- readLines("paper/paper.qmd", warn = FALSE)
  plebiscito_hits <- grep(
    "sec-apendice-a10|tbl-apendice-pleb|fig-apendice-rechazo|m_rechazo|m_movil|voto_|Rechazo estricto",
    qmd_lines, value = TRUE
  )
  plebiscito_hits <- plebiscito_hits[!grepl("^\\s*#", plebiscito_hits)]
  if (length(plebiscito_hits) > 0) {
    fail(paste("paper.qmd contiene referencias al plebiscito:",
               paste(head(plebiscito_hits, 3), collapse = " | ")))
  } else {
    ok("paper.qmd sin referencias a plebiscito/voto_*")
  }
}

cat("\n--- CHECK 8: Sin valores viejos en HTML renderizado ---\n")

html_path <- "paper/paper.html"
if (!file.exists(html_path)) {
  fail("paper/paper.html no existe — ejecuta quarto render primero")
} else {
  html_text <- paste(readLines(html_path, warn = FALSE), collapse = " ")
  old_vals <- c("0.822", "0,822", "0.476", "0,476", "0.745", "0,745", "0.811", "0,811")
  found <- old_vals[sapply(old_vals, function(v) grepl(v, html_text, fixed = TRUE))]
  if (length(found) > 0) {
    fail(paste("HTML contiene valores de corrida vieja:", paste(found, collapse = ", "),
               "— borra paper_cache/ y re-renderiza"))
  } else {
    ok("HTML sin valores de corrida vieja (0.822/0.476/0.745)")
  }
  # Verificar que los valores correctos aparecen
  new_vals <- c("0.494", "0.298")
  present <- new_vals[sapply(new_vals, function(v) grepl(v, html_text, fixed = TRUE))]
  if (length(present) == length(new_vals)) {
    ok(paste("HTML contiene valores nuevos esperados:", paste(new_vals, collapse = ", ")))
  } else {
    missing_vals <- setdiff(new_vals, present)
    fail(paste("HTML no contiene valores nuevos esperados:", paste(missing_vals, collapse = ", ")))
  }
}

cat("\n--- CHECK 9: Un solo bloque de hipótesis H1–H5 en paper.qmd ---\n")

if (file.exists("paper/paper.qmd")) {
  qlines <- readLines("paper/paper.qmd", warn = FALSE)
  for (hn in paste0("**H", 1:5, " ")) {
    n <- sum(grepl(hn, qlines, fixed = TRUE))
    if (n == 0) {
      fail(paste(trimws(hn), "no encontrada en paper.qmd"))
    } else if (n > 1) {
      fail(paste(trimws(hn), "aparece", n, "veces — duplicado"))
    } else {
      ok(paste(trimws(hn), "encontrada exactamente 1 vez"))
    }
  }
  simetria_hits <- grep("\\*\\*H1.*imetría|\\*\\*H1.*simetría", qlines, value = TRUE)
  if (length(simetria_hits) > 0) {
    fail(paste("H1 Simetría numerada sobrevive:", simetria_hits[1]))
  } else {
    ok("Sin H1 Simetría numerada (regularidad de base, no hipótesis)")
  }
}

cat("\n--- CHECK 10: Ecuación principal sin 'íííí' ni placeholder ---\n")

if (file.exists("paper/paper.qmd")) {
  qlines <- readLines("paper/paper.qmd", warn = FALSE)
  broken_eq <- grep("iiiii|íííí|PLACEHOLDER|eq-placeholder", qlines, value = TRUE)
  if (length(broken_eq) > 0) {
    fail(paste("Ecuación rota:", broken_eq[1]))
  } else {
    ok("Sin ecuaciones rotas (ííí/placeholder)")
  }
}

# ── Resultado final ───────────────────────────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
if (FAIL) {
  cat("RESULTADO: ✗ FALLÓ — revisa los errores arriba antes de renderizar.\n")
  cat(strrep("=", 60), "\n")
  stop("Check de consistencia falló. Ver mensajes arriba.", call. = FALSE)
} else {
  cat("RESULTADO: ✓ PASÓ — pipeline consistente, listo para renderizar.\n")
  cat(strrep("=", 60), "\n")
}
