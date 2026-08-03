# =============================================================================
# verificar_paper_consistencia.R
#
# Audita la consistencia entre el pipeline (data/*.rds) y los números usados
# en paper.qmd (vía paper_results.rds). Genera un reporte Markdown con
# código reproducible y outputs.
#
# Uso:
#   Rscript R/verificar_paper_consistencia.R
#   source("R/verificar_paper_consistencia.R"); verificar_paper_consistencia()
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, broom.mixed, tibble, purrr)

source("R/paper_results.R")
`%||%` <- function(x, y) if (is.null(x)) y else x

TOL <- 0.015  # tolerancia numérica para comparar coeficientes

fmt_num <- function(x, d = 3) {
  if (is.na(x)) return("—")
  gsub(".", ",", format(round(x, d), nsmall = d, trim = TRUE), fixed = TRUE)
}

fmt_p <- function(p) {
  if (is.na(p)) return("—")
  if (p < 0.001) return("< .001")
  gsub(".", ",", sub("^0", "", format(round(p, 3), nsmall = 3)), fixed = TRUE)
}

check_num <- function(actual, expected, label, tol = TOL) {
  ok <- !is.na(actual) && !is.na(expected) && abs(actual - expected) <= tol
  tibble::tibble(
    chequeo = label,
    valor_pipeline = actual,
    valor_paper = expected,
    ok = ok,
    nota = if (ok) "OK" else paste0("Δ = ", round(actual - expected, 4))
  )
}

extraer_coef <- function(model, term) {
  if (is.null(model)) return(c(NA, NA, NA))
  r <- broom.mixed::tidy(model, effects = "fixed") |>
    dplyr::filter(.data$term == .env$term)
  if (nrow(r) == 0) return(c(NA, NA, NA))
  c(r$estimate[1], r$std.error[1], r$p.value[1])
}

verificar_paper_consistencia <- function(root_dir = ".") {
  cat("=== Verificación de consistencia paper ↔ pipeline ===\n\n")

  modelos   <- readRDS(file.path(root_dir, "data/modelos.rds"))
  mecanismo <- readRDS(file.path(root_dir, "data/mecanismo.rds"))
  robustez  <- readRDS(file.path(root_dir, "data/robustez.rds"))
  subset_data <- readRDS(file.path(root_dir, "data/subset_data.rds"))

  paper_path <- file.path(root_dir, "data/paper_results.rds")
  if (!file.exists(paper_path)) {
    refresh_paper_results(root_dir)
  }
  paper <- readRDS(paper_path)

  term_tau4 <- "periododecreto:indigeneousindi:cerca_conflictocerca"
  term_tau3 <- "periodoestallido:indigeneousindi:cerca_conflictocerca"
  term_med  <- "periododecreto:indigeneousindi:zona_decretodecreto"

  mC_ctrl <- modelos$mC_ctrl
  mC_resg <- modelos$mC_resg

  tau4_c <- extraer_coef(mC_ctrl, term_tau4)
  tau4_r <- extraer_coef(mC_resg, term_tau4)
  tau3_c <- extraer_coef(mC_ctrl, term_tau3)
  tau3_r <- extraer_coef(mC_resg, term_tau3)
  ola3_r <- extraer_coef(mC_resg, "periodoestallido")
  ola4_c <- extraer_coef(mC_ctrl, "periododecreto")
  ola4_r <- extraer_coef(mC_resg, "periododecreto")

  med_ing <- if (!is.null(mecanismo$m1_ingroup)) {
    extraer_coef(mecanismo$m1_ingroup, term_med)
  } else c(NA, NA, NA)

  comp <- mecanismo$comparacion_atenuacion
  med_pct <- comp |>
    dplyr::filter(.data$vd == "Vio. control", .data$mediador == "Ingroup lag") |>
    dplyr::pull(.data$atenuacion)
  sup_pct <- comp |>
    dplyr::filter(.data$vd == "Vio. resguardo", .data$mediador == "Ingroup lag") |>
    dplyr::pull(.data$atenuacion)

  # ── Descriptivos baseline (4 grupos) ───────────────────────────────────────
  desc4 <- subset_data |>
    dplyr::filter(.data$ola == 2) |>
    dplyr::group_by(.data$indigeneous, .data$cerca_conflicto) |>
    dplyr::summarise(
      n = dplyr::n(),
      control = mean(.data$idx_vio_control, na.rm = TRUE),
      cambio = mean(.data$idx_vio_resguardo, na.rm = TRUE),
      ingroup = mean(.data$just_proc_ingroup, na.rm = TRUE),
      .groups = "drop"
    )

  # ── Chequeos numéricos ──────────────────────────────────────────────────────
  checks <- dplyr::bind_rows(
    check_num(tau4_c[1], paper$tau4_ctrl$estimate, "τ₄ control (Modelo C)"),
    check_num(tau4_r[1], paper$tau4_resg$estimate, "τ₄ cambio (Modelo C)"),
    check_num(tau3_c[1], paper$tau3_ctrl$estimate, "τ₃ control (estallido DiD)"),
    check_num(tau3_r[1], paper$tau3_resg$estimate, "τ₃ cambio (estallido DiD)"),
    check_num(ola3_r[1], paper$b_ola3_resg, "Efecto período ola 3 — cambio"),
    check_num(ola4_c[1], paper$b_decreto_ctrl, "Efecto período ola 4 — control"),
    check_num(med_ing[1], paper$b_T_M_ingroup, "Mediación paso 1 — ingroup"),
    check_num(med_pct, paper$med_ctrl_pct, "Mediación control (%)", tol = 1),
    check_num(abs(sup_pct), paper$sup_resg_pct, "Supresión cambio (%)", tol = 1),
    check_num(
      paper$desc_indi_cerca_cambio,
      desc4 |> dplyr::filter(.data$indigeneous == "indi", .data$cerca_conflicto == "cerca") |> dplyr::pull(.data$cambio),
      "Baseline indi zona — cambio social"
    ),
    check_num(
      paper$desc_indi_lejos_cambio,
      desc4 |> dplyr::filter(.data$indigeneous == "indi", .data$cerca_conflicto == "lejos") |> dplyr::pull(.data$cambio),
      "Baseline indi fuera — cambio social"
    ),
    check_num(nrow(mC_ctrl@frame), paper$n_modelo_C, "N Modelo C", tol = 0)
  )

  n_folios <- dplyr::n_distinct(subset_data$folio)
  n_dup <- subset_data |>
    dplyr::count(.data$folio, .data$ola) |>
    dplyr::filter(.data$n > 1) |>
    nrow()

  issues <- list()

  # Chequeos post Fase B/C
  if (!is.null(mecanismo$vd_escala) && mecanismo$vd_escala != "continua_1_5") {
    issues <- c(issues, "Mecanismo no usa VD continua 1–5.")
  }
  if (!is.null(robustez$ipw_estimator) &&
      !grepl("feols|cluster", robustez$ipw_estimator)) {
    issues <- c(issues, "IPW aún no usa feols+cluster.")
  }
  if (isTRUE(robustez$ps_includes_outcomes)) {
    issues <- c(issues, "PS todavía incluye outcomes baseline.")
  }

  if (n_dup > 0) {
    issues <- c(issues, paste0(
      "Duplicados folio×ola en subset_data: ", n_dup,
      " pares (revisar 01_limpieza.R distinct folio, ola)."
    ))
  }
  if (paper$n_individuos != n_folios) {
    issues <- c(issues, paste0(
      "N individuos paper (", paper$n_individuos,
      ") ≠ folios únicos (", n_folios, ")."
    ))
  }
  if (paper$n_indi_ola2 + paper$n_noindi_ola2 != n_folios) {
    issues <- c(issues, paste0(
      "Suma baseline étnica (", paper$n_indi_ola2 + paper$n_noindi_ola2,
      ") ≠ folios únicos (", n_folios, ")."
    ))
  }
  notes <- character()
  if (tau4_c[3] >= 0.05) {
    notes <- c(notes, paste0(
      "τ₄ control n.s. (p = ", fmt_p(tau4_c[3]),
      "): narrativa debe enfatizar regularización (Paso 1), no efecto directo."
    ))
  }
  if (tau4_r[3] < 0.05) {
    notes <- c(notes, paste0(
      "τ₄ cambio significativo (p = ", fmt_p(tau4_r[3]), "): hallazgo central confirmado."
    ))
  }
  has_fe <- any(grepl("^FE folio", robustez$resumen_robustez$modelo %||% character()))
  has_dr <- any(grepl("^DRDID", robustez$resumen_robustez$modelo %||% character()))
  if (!has_fe) notes <- c(notes, "FE folio+cluster no aparece en resumen_robustez.")
  if (!has_dr) notes <- c(notes, "DRDID no aparece en resumen_robustez (paquete ausente?).")
  if (!is.null(mecanismo$vd_escala) && mecanismo$vd_escala == "continua_1_5") {
    notes <- c(notes, "Mecanismo usa VD continua 1–5 (alineado con Modelo C).")
  }
  if (!is.null(robustez$ipw_estimator)) {
    notes <- c(notes, paste0("IPW estimator: ", robustez$ipw_estimator))
  }

  rob <- robustez$resumen_robustez |>
    dplyr::filter(.data$modelo == "C — DiD decreto")
  rob_ok <- all(
    abs(rob$estimate[rob$variable_dependiente == "idx_vio_control"] - tau4_c[1]) < TOL,
    abs(rob$estimate[rob$variable_dependiente == "idx_vio_resguardo"] - tau4_r[1]) < TOL
  )
  if (!rob_ok) {
    issues <- c(issues, "tabla_resumen_robustez.csv no coincide con modelos$mC_*.")
  }
  if (any(!checks$ok, na.rm = TRUE)) {
    issues <- c(issues, paste0(
      "Desalineación paper_results ↔ modelos en: ",
      paste(checks$chequeo[!checks$ok], collapse = "; ")
    ))
  }

  status <- if (length(issues) == 0) "✅ CONSISTENTE" else "⚠️ REVISAR"

  # ── Reporte Markdown ────────────────────────────────────────────────────────
  out_md <- file.path(root_dir, "output/VERIFICACION_CONSISTENCIA_PAPER.md")
  dir.create(dirname(out_md), recursive = TRUE, showWarnings = FALSE)

  md <- c(
    "# Verificación de consistencia — paper.qmd vs pipeline",
    "",
    paste0("**Generado:** ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("**Estado global:** ", status),
    "",
    "## 1. Especificación analítica",
    "",
    "| Elemento | Valor |",
    "|----------|-------|",
    "| idx_vio_control | d3_1 (ítem único) |",
    "| idx_vio_resguardo | promedio d4_2 + d4_3 |",
    "| indigeneous | a1 ∈ 1–11 vs 12 |",
    "| perc_injusticia | excluida de modelos principales |",
    "",
    "## 2. Muestra",
    "",
    "| Métrica | Valor |",
    "|---------|-------|",
    paste0("| Folios únicos | ", n_folios, " |"),
    paste0("| Persona-olas (olas 2–4) | ", nrow(subset_data), " |"),
    paste0("| Indígenas baseline (ola 2) | ", paper$n_indi_ola2, " |"),
    paste0("| No indígenas baseline (ola 2) | ", paper$n_noindi_ola2, " |"),
    paste0("| N Modelo C (por VD) | ", paper$n_modelo_C, " |"),
    paste0("| Duplicados folio×ola | ", n_dup, " |"),
    "",
    "## 3. Modelo C — coeficientes principales",
    "",
    "| Término | VD | β | SE | p | En paper_results | ✓ |",
    "|---------|----|---|----|---|------------------|---|",
    sprintf(
      "| DiD decreto (τ₄) | Control | %s | %s | %s | %s | %s |",
      fmt_num(tau4_c[1]), fmt_num(tau4_c[2]), fmt_p(tau4_c[3]),
      fmt_num(paper$tau4_ctrl$estimate),
      if (checks$ok[checks$chequeo == "τ₄ control (Modelo C)"]) "✓" else "✗"
    ),
    sprintf(
      "| DiD decreto (τ₄) | Cambio | %s | %s | %s | %s | %s |",
      fmt_num(tau4_r[1]), fmt_num(tau4_r[2]), fmt_p(tau4_r[3]),
      fmt_num(paper$tau4_resg$estimate),
      if (checks$ok[checks$chequeo == "τ₄ cambio (Modelo C)"]) "✓" else "✗"
    ),
    sprintf(
      "| DiD estallido (τ₃) | Cambio | %s | %s | %s | — | — |",
      fmt_num(tau3_r[1]), fmt_num(tau3_r[2]), fmt_p(tau3_r[3])
    ),
    sprintf(
      "| periodoestallido | Cambio | %s | %s | %s | %s | ✓ |",
      fmt_num(ola3_r[1]), fmt_num(ola3_r[2]), fmt_p(ola3_r[3]), fmt_num(paper$b_ola3_resg)
    ),
    sprintf(
      "| periododecreto | Control | %s | %s | %s | %s | ✓ |",
      fmt_num(ola4_c[1]), fmt_num(ola4_c[2]), fmt_p(ola4_c[3]), fmt_num(paper$b_decreto_ctrl)
    ),
    "",
    "## 4. Mediación",
    "",
    "| Paso | β | p | % mediación control | % supresión cambio |",
    "|------|---|----|---------------------|--------------------|",
    sprintf(
      "| Decreto → ingroup | %s | %s | ~%s%% | ~%s%% |",
      fmt_num(med_ing[1]), fmt_p(med_ing[3]),
      round(paper$med_ctrl_pct, 0), round(paper$sup_resg_pct, 0)
    ),
    "",
    "## 5. Descriptivos baseline (ola 2)",
    "",
    "| Grupo | N | Control | Cambio | Just. ingroup |",
    "|-------|---|---------|--------|---------------|"
  )

  for (i in seq_len(nrow(desc4))) {
    g <- desc4[i, ]
    md <- c(md, sprintf(
      "| %s, %s | %d | %s | %s | %s |",
      g$indigeneous, g$cerca_conflicto, g$n,
      fmt_num(g$control, 2), fmt_num(g$cambio, 2), fmt_num(g$ingroup, 2)
    ))
  }

  md <- c(
    md, "",
    paste0(
      "**Patrón tendencias paralelas (indi):** cambio social zona = ",
      fmt_num(paper$desc_indi_cerca_cambio, 2),
      " vs fuera = ", fmt_num(paper$desc_indi_lejos_cambio, 2),
      " (dirección opuesta al τ₄ post-decreto)."
    ),
    "",
    "## 6. Robustez (τ₄ decreto)",
    "",
    "| Especificación | Control | Cambio |",
    "|----------------|---------|--------|"
  )

  rob_tbl <- robustez$resumen_robustez |>
    dplyr::filter(.data$modelo %in% c(
      "C — DiD decreto", "B — Decreto (3→4)", "PSM",
      "IPW original", "IPW trim 5–95%", "Placebo real (ola1→2)"
    )) |>
    dplyr::select(.data$modelo, .data$variable_dependiente, .data$estimate, .data$signif)

  for (mod in unique(rob_tbl$modelo)) {
    sub <- rob_tbl |> dplyr::filter(.data$modelo == .env$mod)
    ctrl <- sub |> dplyr::filter(.data$variable_dependiente == "idx_vio_control")
    resg <- sub |> dplyr::filter(.data$variable_dependiente == "idx_vio_resguardo")
    md <- c(md, sprintf(
      "| %s | %s %s | %s %s |",
      mod,
      fmt_num(ctrl$estimate[1]), ctrl$signif[1],
      fmt_num(resg$estimate[1]), resg$signif[1]
    ))
  }

  md <- c(
    md, "",
    "## 7. Chequeos automáticos paper_results ↔ modelos",
    "",
    "| Chequeo | Pipeline | paper_results | OK |",
    "|---------|----------|---------------|-----|"
  )
  for (i in seq_len(nrow(checks))) {
    md <- c(md, sprintf(
      "| %s | %s | %s | %s |",
      checks$chequeo[i],
      fmt_num(checks$valor_pipeline[i], 3),
      fmt_num(checks$valor_paper[i], 3),
      if (checks$ok[i]) "✓" else "✗"
    ))
  }

  if (length(issues) > 0) {
    md <- c(md, "", "## 8. Inconsistencias detectadas", "")
    md <- c(md, paste0("- ", issues))
  } else {
    md <- c(md, "", "## 8. Inconsistencias detectadas", "", "_Ninguna._")
  }

  if (length(notes) > 0) {
    md <- c(md, "", "## 9. Notas narrativas (recordatorio)", "")
    md <- c(md, paste0("- ", notes))
  }

  md <- c(
    md, "",
    "## 10. Cómo reproducir",
    "",
    "```bash",
    "cd causality/",
    "bash run_all.sh                    # pipeline completo 01→09",
    "Rscript R/verificar_paper_consistencia.R",
    "Rscript -e 'source(\"R/paper_results.R\"); refresh_paper_results()'",
    "cd paper && quarto render paper.qmd --cache-refresh",
    "```",
    "",
    "Scripts clave: `R/01_limpieza.R`, `R/03_modelos.R`, `R/04_robustez.R`,",
    "`R/05_mecanismo.R`, `R/paper_results.R`.",
    ""
  )

  writeLines(md, out_md, useBytes = TRUE)
  cat("✓ Reporte guardado:", out_md, "\n")
  cat("Estado:", status, "\n")
  if (length(issues)) {
    cat("\nAdvertencias:\n")
    for (i in issues) cat(" -", i, "\n")
  }

  invisible(list(checks = checks, issues = issues, desc4 = desc4, status = status))
}

if (sys.nframe() == 0) {
  verificar_paper_consistencia()
}


