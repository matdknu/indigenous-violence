# Helpers to sync paper/paper.qmd with pipeline outputs (modelos, mecanismo, robustez).

fmt_es <- function(x, digits = 3) {
  vapply(
    x,
    function(val) {
      if (is.na(val)) return("—")
      txt <- format(round(val, digits), nsmall = digits, trim = TRUE)
      gsub(".", ",", txt, fixed = TRUE)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

fmt_p_inline <- function(p) {
  vapply(
    p,
    function(val) {
      if (is.na(val)) return("—")
      if (val < 0.001) return("< .001")
      txt <- format(round(val, 3), nsmall = 3, trim = TRUE)
      txt <- sub("^0", "", txt)
      gsub(".", ",", txt, fixed = TRUE)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

fmt_beta_p <- function(estimate, p.value, digits = 3) {
  paste0("β = ", fmt_es(estimate, digits), ", *p* = ", fmt_p_inline(p.value))
}

fmt_beta_stars <- function(estimate, p.value, digits = 3) {
  stars <- dplyr::case_when(
    is.na(p.value) ~ "",
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "+",
    TRUE ~ ""
  )
  paste0(fmt_es(estimate, digits), if (nzchar(stars)) paste0(" ", stars) else "")
}

coef_row <- function(df, modelo, vd = "idx_vio_control") {
  row <- df |>
    dplyr::filter(.data$modelo == .env$modelo, .data$variable_dependiente == .env$vd)
  if (nrow(row) == 0) {
    return(list(
      estimate = NA_real_, std.error = NA_real_, p.value = NA_real_,
      signif = "", beta_stars = "—", beta_p = "—"
    ))
  }
  row <- row[1, ]
  list(
    estimate = row$estimate,
    std.error = row$std.error,
    p.value = row$p.value,
    signif = row$signif,
    beta_stars = fmt_beta_stars(row$estimate, row$p.value),
    beta_p = fmt_beta_p(row$estimate, row$p.value)
  )
}

tidy_term <- function(model, term) {
  if (is.null(model)) {
    return(tibble::tibble(
      estimate = NA_real_, std.error = NA_real_, p.value = NA_real_
    ))
  }
  broom.mixed::tidy(model, effects = "fixed") |>
    dplyr::filter(.data$term == .env$term)
}

build_tbl_robustez_paper <- function(resumen_robustez) {
  map <- c(
    "C — DiD decreto"        = "Modelo principal M2",
    "IPW original"           = "IPW original",
    "IPW trim 1–99%"         = "IPW trim 1–99 %",
    "IPW trim 5–95%"         = "IPW trim 5–95 %",
    "PSM"                    = "PSM (nearest neighbor)",
    "Placebo real (ola1→2)"  = "Placebo ola 1–2"
  )
  purrr::imap_dfr(map, function(label, modelo) {
    ctrl <- coef_row(resumen_robustez, modelo, "idx_vio_control")
    resg <- coef_row(resumen_robustez, modelo, "idx_vio_resguardo")
    tibble::tibble(
      Especificación = label,
      `Control social` = ctrl$beta_stars,
      `Cambio social` = resg$beta_stars
    )
  })
}

build_paper_results <- function(
    modelos,
    mecanismo = NULL,
    robustez = NULL,
    subset_data = NULL,
    hetero_identidad = NULL,
    term_did_decreto = "periododecreto:indigeneousindi:cerca_conflictocerca",
    term_did_estallido = "periodoestallido:indigeneousindi:cerca_conflictocerca"
) {
  resumen <- robustez$resumen_robustez
  if (is.null(resumen)) {
    stop("robustez$resumen_robustez no encontrado. Ejecute R/04_robustez.R.")
  }

  main_ctrl <- coef_row(resumen, "C — DiD decreto", "idx_vio_control")
  main_resg <- coef_row(resumen, "C — DiD decreto", "idx_vio_resguardo")
  est_ctrl  <- coef_row(resumen, "C — DiD estallido", "idx_vio_control")
  est_resg  <- coef_row(resumen, "C — DiD estallido", "idx_vio_resguardo")
  psm_ctrl  <- coef_row(resumen, "PSM", "idx_vio_control")
  psm_resg  <- coef_row(resumen, "PSM", "idx_vio_resguardo")
  ipw_o_ctrl <- coef_row(resumen, "IPW original", "idx_vio_control")
  ipw_o_resg <- coef_row(resumen, "IPW original", "idx_vio_resguardo")
  ipw_199_ctrl <- coef_row(resumen, "IPW trim 1–99%", "idx_vio_control")
  ipw_199_resg <- coef_row(resumen, "IPW trim 1–99%", "idx_vio_resguardo")
  ipw_595_ctrl <- coef_row(resumen, "IPW trim 5–95%", "idx_vio_control")
  ipw_595_resg <- coef_row(resumen, "IPW trim 5–95%", "idx_vio_resguardo")
  plcb_ctrl <- coef_row(resumen, "Placebo real (ola1→2)", "idx_vio_control")
  plcb_resg <- coef_row(resumen, "Placebo real (ola1→2)", "idx_vio_resguardo")

  mC_ctrl <- modelos$mC_ctrl %||% modelos$m2_ctrl
  mC_resg <- modelos$mC_resg %||% modelos$m2_resg

  tau3_ctrl <- tidy_term(mC_ctrl, term_did_estallido)
  tau3_resg <- tidy_term(mC_resg, term_did_estallido)
  tau3_sig <- any(c(tau3_ctrl$p.value, tau3_resg$p.value) < 0.05, na.rm = TRUE)

  # ── Mediación: extraer desde comparacion_atenuacion o modelos directos ──────
  ate_ctrl <- NA_real_
  ate_resg <- NA_real_
  
  if (!is.null(mecanismo$comparacion_atenuacion)) {
    comp <- mecanismo$comparacion_atenuacion
    # Usar Ingroup lag como mediador principal
    ate_ctrl_row <- comp |> 
      dplyr::filter(.data$vd == "Vio. control", .data$mediador == "Ingroup lag")
    ate_resg_row <- comp |> 
      dplyr::filter(.data$vd == "Vio. resguardo", .data$mediador == "Ingroup lag")
    
    if (nrow(ate_ctrl_row)) ate_ctrl <- ate_ctrl_row$atenuacion[1]
    if (nrow(ate_resg_row)) ate_resg <- ate_resg_row$atenuacion[1]
  }

  med_ctrl_sin <- med_ctrl_med <- med_resg_sin <- med_resg_med <- list(
    estimate = NA_real_, p.value = NA_real_
  )
  
  # Modelos SIN mediador
  # Detectar qué término DiD usar (cerca_conflicto o zona_decreto)
  term_did_mecanismo <- term_did_decreto
  if (!is.null(mecanismo$m_ctrl_sin)) {
    terms_available <- broom.mixed::tidy(mecanismo$m_ctrl_sin, effects = "fixed")$term
    if (any(grepl("zona_decreto", terms_available))) {
      term_did_mecanismo <- gsub("cerca_conflictocerca", "zona_decretodecreto", term_did_decreto)
    }
  }
  
  if (!is.null(mecanismo$m_ctrl_sin)) {
    r <- tidy_term(mecanismo$m_ctrl_sin, term_did_mecanismo)
    if (nrow(r)) med_ctrl_sin <- list(estimate = r$estimate, p.value = r$p.value)
  }
  if (!is.null(mecanismo$m_resg_sin)) {
    r <- tidy_term(mecanismo$m_resg_sin, term_did_mecanismo)
    if (nrow(r)) med_resg_sin <- list(estimate = r$estimate, p.value = r$p.value)
  }
  
  # Modelos CON mediador (ingroup_lag como principal)
  if (!is.null(mecanismo$m_ctrl_ingroup)) {
    r <- tidy_term(mecanismo$m_ctrl_ingroup, term_did_mecanismo)
    if (nrow(r)) med_ctrl_med <- list(estimate = r$estimate, p.value = r$p.value)
  }
  if (!is.null(mecanismo$m_resg_ingroup)) {
    r <- tidy_term(mecanismo$m_resg_ingroup, term_did_mecanismo)
    if (nrow(r)) med_resg_med <- list(estimate = r$estimate, p.value = r$p.value)
  }

  med_ctrl_attenua <- !is.na(med_ctrl_sin$estimate) && 
                      !is.na(med_ctrl_med$estimate) &&
                      med_ctrl_sin$estimate > med_ctrl_med$estimate
  med_resg_attenua <- !is.na(med_resg_sin$estimate) && 
                      !is.na(med_resg_med$estimate) &&
                      med_resg_sin$estimate > med_resg_med$estimate

  n_panel <- if (!is.null(subset_data)) {
    length(unique(subset_data$folio))
  } else {
    NA_integer_
  }

  n_psm <- NA_integer_
  if (!is.null(robustez$m_psm)) {
    md <- tryCatch(match.data(robustez$m_psm), error = function(e) NULL)
    if (!is.null(md) && "folio" %in% names(md)) {
      n_psm <- length(unique(md$folio))
    }
  }

  diag_orig <- robustez$diag_pesos |>
    dplyr::filter(.data$especificacion == "IPW original")
  diag_trim <- robustez$diag_pesos |>
    dplyr::filter(.data$especificacion == "IPW trim 5–95%")

  r_just_ctrl <- r_just_resg <- NA_real_
  if (!is.null(subset_data)) {
    # Usar ingroup_lag si está disponible, sino just_proc_lag
    lag_var <- if ("ingroup_lag" %in% names(subset_data)) {
      "ingroup_lag"
    } else if ("just_proc_lag" %in% names(subset_data)) {
      "just_proc_lag"
    } else {
      NULL
    }
    
    if (!is.null(lag_var)) {
      d4 <- subset_data |> dplyr::filter(.data$ola == 4)
      r_just_ctrl <- stats::cor(
        d4[[lag_var]], d4$idx_vio_control, use = "pairwise.complete.obs"
      )
      r_just_resg <- stats::cor(
        d4[[lag_var]], d4$idx_vio_resguardo, use = "pairwise.complete.obs"
      )
    }
  }

  placebo_ns <- all(c(plcb_ctrl$p.value, plcb_resg$p.value) >= 0.05, na.rm = TRUE)

  # ── Efectos de período (Modelo C) y transición (Modelo B) ───────────────────
  period_estallido_resg <- tidy_term(mC_resg, "periodoestallido")
  period_decreto_ctrl   <- tidy_term(mC_ctrl, "periododecreto")
  period_decreto_resg   <- tidy_term(mC_resg, "periododecreto")
  zona_ola3_resg        <- tidy_term(mC_resg, "periodoestallido:cerca_conflictocerca")
  baseline_zona_resg    <- tidy_term(mC_resg, "cerca_conflictocerca")

  mB_ctrl <- modelos$mB_ctrl
  mB_resg <- modelos$mB_resg
  t2_resg <- tidy_term(mB_resg, "T2_decreto")
  t2_ctrl <- tidy_term(mB_ctrl, "T2_decreto")

  # ── Baseline justicia procedimental e identidad (ola 2) ─────────────────────
  brecha_baseline_indi <- brecha_baseline_noindi <- NA_real_
  predom_brecha_indi <- NA_real_
  if (!is.null(subset_data)) {
    bl <- subset_data |> dplyr::filter(.data$ola == 2)
    brecha_baseline_indi <- bl |>
      dplyr::filter(.data$indigeneous == "indi") |>
      dplyr::summarise(m = mean(.data$brecha_just_proc, na.rm = TRUE)) |>
      dplyr::pull(m)
    brecha_baseline_noindi <- bl |>
      dplyr::filter(.data$indigeneous == "no_indi") |>
      dplyr::summarise(m = mean(.data$brecha_just_proc, na.rm = TRUE)) |>
      dplyr::pull(m)
  }
  if (!is.null(hetero_identidad$predom_baseline)) {
    predom_brecha_indi <- hetero_identidad$predom_baseline |>
      dplyr::filter(.data$indigeneous == "indi") |>
      dplyr::pull(brecha_id)
  }

  # ── Mecanismo paso 1 (DiD sobre mediadores ingroup/outgroup/brecha) ─────────
  term_did_mec <- "periododecreto:indigeneousindi:zona_decretodecreto"
  med_ingroup_did <- med_outgroup_did <- med_brecha_did <- list(
    estimate = NA_real_, p.value = NA_real_, beta_p = "—"
  )
  if (!is.null(mecanismo$m1_ingroup)) {
    r <- tidy_term(mecanismo$m1_ingroup, term_did_mec)
    if (nrow(r)) {
      med_ingroup_did <- list(
        estimate = r$estimate, p.value = r$p.value,
        beta_p = fmt_beta_p(r$estimate, r$p.value)
      )
    }
  }
  if (!is.null(mecanismo$m1_outgroup)) {
    r <- tidy_term(mecanismo$m1_outgroup, term_did_mec)
    if (nrow(r)) {
      med_outgroup_did <- list(
        estimate = r$estimate, p.value = r$p.value,
        beta_p = fmt_beta_p(r$estimate, r$p.value)
      )
    }
  }
  if (!is.null(mecanismo$m1_brecha)) {
    r <- tidy_term(mecanismo$m1_brecha, term_did_mec)
    if (nrow(r)) {
      med_brecha_did <- list(
        estimate = r$estimate, p.value = r$p.value,
        beta_p = fmt_beta_p(r$estimate, r$p.value)
      )
    }
  }

  # ── Heterogeneidad identitaria (terciles) ───────────────────────────────────
  hetero_terciles <- if (!is.null(hetero_identidad$resultados)) {
    hetero_identidad$resultados |>
      dplyr::transmute(
        vd = dplyr::if_else(.data$vd == "idx_vio_control",
                            "Control social", "Cambio social"),
        tercil = .data$tercil,
        estimate = .data$estimate,
        std.error = .data$std.error,
        p.value = .data$p.value,
        beta_p = fmt_beta_p(.data$estimate, .data$p.value)
      )
  } else {
    NULL
  }

  n_indi_ola2 <- n_noindi_ola2 <- n_persona_olas <- NA_integer_
  if (!is.null(subset_data)) {
    n_indi_ola2 <- sum(
      subset_data$indigeneous == "indi" & subset_data$ola == 2,
      na.rm = TRUE
    )
    n_noindi_ola2 <- sum(
      subset_data$indigeneous == "no_indi" & subset_data$ola == 2,
      na.rm = TRUE
    )
    n_persona_olas <- nrow(subset_data)
  }

  or_rechazo_zona <- p_or_zona <- NA_real_
  if (!is.null(modelos$m_rechazo)) {
    cf <- coef(modelos$m_rechazo)
    if ("cerca_conflictocerca" %in% names(cf)) {
      or_rechazo_zona <- as.numeric(exp(cf["cerca_conflictocerca"]))
      p_or_zona <- summary(modelos$m_rechazo)$coefficients[
        "cerca_conflictocerca", "Pr(>|z|)"
      ]
    }
  }

  med_ctrl_pct <- if (!is.na(ate_ctrl)) ate_ctrl else NA_real_
  sup_resg_pct <- if (!is.na(ate_resg)) abs(ate_resg) else NA_real_

  sens_mapuche_tbl <- if (!is.null(robustez$sens_mapuche_compare)) {
    robustez$sens_mapuche_compare |>
      dplyr::mutate(
        beta_stars = purrr::map2_chr(
          .data$estimate, .data$p.value, fmt_beta_stars
        ),
        beta_p = purrr::map2_chr(
          .data$estimate, .data$p.value, fmt_beta_p
        )
      ) |>
      dplyr::transmute(
        muestra = .data$modelo,
        vd = .data$vd_label,
        beta_stars = .data$beta_stars,
        beta_p = .data$beta_p
      )
  } else {
    NULL
  }

  list(
    resumen_robustez = resumen,
    tbl_robustez = build_tbl_robustez_paper(resumen),
    n_panel = n_panel,
    n_psm = n_psm,
    tau3_sig = tau3_sig,
    tau4_ctrl = main_ctrl,
    tau4_resg = main_resg,
    tau3_ctrl = as.list(tau3_ctrl),
    tau3_resg = as.list(tau3_resg),
    psm_ctrl = psm_ctrl,
    psm_resg = psm_resg,
    ipw_o_ctrl = ipw_o_ctrl,
    ipw_o_resg = ipw_o_resg,
    ipw_199_ctrl = ipw_199_ctrl,
    ipw_199_resg = ipw_199_resg,
    ipw_595_ctrl = ipw_595_ctrl,
    ipw_595_resg = ipw_595_resg,
    plcb_ctrl = plcb_ctrl,
    plcb_resg = plcb_resg,
    ate_ctrl = ate_ctrl,
    ate_resg = ate_resg,
    med_ctrl_sin = med_ctrl_sin,
    med_ctrl_med = med_ctrl_med,
    med_resg_sin = med_resg_sin,
    med_resg_med = med_resg_med,
    med_ctrl_attenua = med_ctrl_attenua,
    med_resg_attenua = med_resg_attenua,
    r_just_ctrl = r_just_ctrl,
    r_just_resg = r_just_resg,
    placebo_ns = placebo_ns,
    diag_ipw_orig = if (nrow(diag_orig)) as.list(diag_orig[1, ]) else NULL,
    diag_ipw_trim595 = if (nrow(diag_trim)) as.list(diag_trim[1, ]) else NULL,
    period_estallido_resg = as.list(period_estallido_resg),
    period_decreto_ctrl = as.list(period_decreto_ctrl),
    period_decreto_resg = as.list(period_decreto_resg),
    zona_ola3_resg = as.list(zona_ola3_resg),
    baseline_zona_resg = as.list(baseline_zona_resg),
    t2_resg = as.list(t2_resg),
    t2_ctrl = as.list(t2_ctrl),
    brecha_baseline_indi = brecha_baseline_indi,
    brecha_baseline_noindi = brecha_baseline_noindi,
    predom_brecha_indi = predom_brecha_indi,
    med_ingroup_did = med_ingroup_did,
    med_outgroup_did = med_outgroup_did,
    med_brecha_did = med_brecha_did,
    hetero_terciles = hetero_terciles,

    sens_mapuche_tbl = sens_mapuche_tbl,
    n_individuos = n_panel,
    n_persona_olas = n_persona_olas,
    n_indi_ola2 = n_indi_ola2,
    n_noindi_ola2 = n_noindi_ola2,
    b_ola3_resg = period_estallido_resg$estimate,
    p_ola3_resg = period_estallido_resg$p.value,
    b_decreto_resg = t2_resg$estimate,
    p_decreto_resg = t2_resg$p.value,
    b_T_M_ingroup = med_ingroup_did$estimate,
    p_T_M_ingroup = med_ingroup_did$p.value,
    med_ctrl_pct = med_ctrl_pct,
    sup_resg_pct = sup_resg_pct,
    or_rechazo_zona = or_rechazo_zona,
    p_or_zona = p_or_zona,
    tau4_resg_ipw = ipw_o_resg$estimate,
    p_tau4_resg_ipw = ipw_o_resg$p.value,
    tau4_ctrl_ipw = ipw_o_ctrl$estimate,
    p_tau4_ctrl_ipw = ipw_o_ctrl$p.value
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Regenera data/paper_results.rds desde los .rds del pipeline.
refresh_paper_results <- function(root_dir = ".") {
  modelos <- readRDS(file.path(root_dir, "data/modelos.rds"))
  mecanismo <- if (file.exists(file.path(root_dir, "data/mecanismo.rds"))) {
    readRDS(file.path(root_dir, "data/mecanismo.rds"))
  } else {
    NULL
  }
  robustez <- readRDS(file.path(root_dir, "data/robustez.rds"))
  subset_data <- readRDS(file.path(root_dir, "data/subset_data.rds"))
  hetero_identidad <- if (file.exists(file.path(root_dir, "data/hetero_identidad.rds"))) {
    readRDS(file.path(root_dir, "data/hetero_identidad.rds"))
  } else {
    NULL
  }
  paper <- build_paper_results(
    modelos = modelos,
    mecanismo = mecanismo,
    robustez = robustez,
    subset_data = subset_data,
    hetero_identidad = hetero_identidad
  )
  saveRDS(paper, file.path(root_dir, "data/paper_results.rds"))
  cat("✓ paper_results.rds actualizado\n")
  cat("  τ₄ cambio social: β =", round(paper$tau4_resg$estimate, 3),
      "p =", round(paper$tau4_resg$p.value, 3), "\n")
  cat("  τ₄ control social: β =", round(paper$tau4_ctrl$estimate, 3),
      "p =", round(paper$tau4_ctrl$p.value, 3), "\n")
  cat("  Mediación control: ~", round(paper$med_ctrl_pct, 0), "%\n")
  cat("  Supresión cambio: ~", round(paper$sup_resg_pct, 0), "%\n")
  cat("  N indígena ola 2:", paper$n_indi_ola2, "\n")
  cat("  N no indígena ola 2:", paper$n_noindi_ola2, "\n")
  cat("  N individuos panel:", paper$n_individuos, "\n")
  cat("  N persona-olas:", paper$n_persona_olas, "\n")
  invisible(paper)
}
