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

  ate_ctrl <- mecanismo$ate_ctrl %||% NA_real_
  ate_resg <- mecanismo$ate_resg %||% NA_real_

  med_ctrl_sin <- med_ctrl_med <- med_resg_sin <- med_resg_med <- list(
    estimate = NA_real_, p.value = NA_real_
  )
  if (!is.null(mecanismo$m2_ctrl_sin)) {
    r <- tidy_term(mecanismo$m2_ctrl_sin, term_did_decreto)
    if (nrow(r)) med_ctrl_sin <- list(estimate = r$estimate, p.value = r$p.value)
  }
  if (!is.null(mecanismo$m2_ctrl_med)) {
    r <- tidy_term(mecanismo$m2_ctrl_med, term_did_decreto)
    if (nrow(r)) med_ctrl_med <- list(estimate = r$estimate, p.value = r$p.value)
  }
  if (!is.null(mecanismo$m2_resg_sin)) {
    r <- tidy_term(mecanismo$m2_resg_sin, term_did_decreto)
    if (nrow(r)) med_resg_sin <- list(estimate = r$estimate, p.value = r$p.value)
  }
  if (!is.null(mecanismo$m2_resg_med)) {
    r <- tidy_term(mecanismo$m2_resg_med, term_did_decreto)
    if (nrow(r)) med_resg_med <- list(estimate = r$estimate, p.value = r$p.value)
  }

  med_ctrl_attenua <- med_ctrl_sin$estimate > med_ctrl_med$estimate
  med_resg_attenua <- med_resg_sin$estimate > med_resg_med$estimate

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
  if (!is.null(subset_data) && "just_proc_lag" %in% names(subset_data)) {
    d4 <- subset_data |> dplyr::filter(.data$ola == 4)
    r_just_ctrl <- stats::cor(
      d4$just_proc_lag, d4$idx_vio_control, use = "pairwise.complete.obs"
    )
    r_just_resg <- stats::cor(
      d4$just_proc_lag, d4$idx_vio_resguardo, use = "pairwise.complete.obs"
    )
  }

  placebo_ns <- all(c(plcb_ctrl$p.value, plcb_resg$p.value) >= 0.05, na.rm = TRUE)

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
    diag_ipw_trim595 = if (nrow(diag_trim)) as.list(diag_trim[1, ]) else NULL
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
