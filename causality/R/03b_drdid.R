# =============================================================================
# 03b_drdid.R — DR-DiD doblemente robusto (Sant'Anna & Zhao 2020)
#
# Propósito: estimar el ATT por grupo étnico (indígena / no indígena) usando
#            el estimador DR-DiD para panel de 2 períodos. El "efecto triple"
#            del decreto (grupo × zona × tiempo) se aproxima como diferencia
#            de ATTs: ATT_indi − ATT_no_indi.
#
# Transiciones:
#   (1) Decreto        ola 2 → ola 4  (tratamiento principal)
#   (2) Proceso pol.   ola 2 → ola 3  (τ₃; coyuntura 2019–2021)
#   (3) Placebo real   ola 1 → ola 2  (sin shocks → ATT ≈ 0)
#
# Tratamiento: cerca_conflicto == "cerca" (zona decreto D.S. 418, 12 oct 2021)
# Covariables: SOLO baseline (ola 2 / ola 1 según transición) de la lista
#              sobreviviente a la regla del 5% NA:
#              mujer, edad, urbano_rural, id_chile, id_causa,
#              perc_desigualdad, apoyo_movil
#
# Input:   data/subset_data.rds, data/panel_completo.rds
# Output:  data/drdid.rds, output/tablas/tabla_drdid.html
# =============================================================================

set.seed(2024)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, gt, stringr)

# Instalar DRDID si no está disponible
if (!requireNamespace("DRDID", quietly = TRUE)) {
  tryCatch(
    install.packages("DRDID", repos = "https://cloud.r-project.org"),
    error = function(e) NULL
  )
}
has_drdid <- requireNamespace("DRDID", quietly = TRUE)

if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

subset_data    <- readRDS("data/subset_data.rds")
panel_completo <- readRDS("data/panel_completo.rds")

# ── Covariables baseline (solo pre-tratamiento) ───────────────────────────────
# Tomadas en la ola PRE de cada transición (ola 2, 2 o 1 respectivamente).
# Estas son estrictamente pre-tratamiento → su inclusión en el PS y en el
# outcome model del DR-DiD es válida (a diferencia de outcomes baseline).

COVARS_BASE <- c(
  "mujer", "edad", "urbano_rural",
  "id_chile", "id_causa", "perc_desigualdad", "apoyo_movil"
)

# ── Funciones auxiliares ──────────────────────────────────────────────────────

signif_stars <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ "+",
    TRUE      ~ ""
  )
}

# Construye el data.frame de panel 2 períodos para DRDID
# datos   : data.frame longitudinal
# ola_pre : ola de referencia (pre)
# ola_post: ola post-tratamiento
# grupo   : valor de 'indigeneous' a filtrar (NULL = todos)
prep_drdid_panel <- function(datos, ola_pre, ola_post, grupo = NULL) {
  d <- datos

  if (!is.null(grupo)) {
    d <- d |> dplyr::filter(.data$indigeneous == grupo)
  }

  d <- d |>
    dplyr::filter(.data$ola %in% c(ola_pre, ola_post)) |>
    dplyr::mutate(
      id    = as.integer(factor(.data$folio)),
      year  = as.integer(.data$ola),
      treat = as.integer(.data$cerca_conflicto == "cerca")
    )

  # Convertir covariables a numérico
  covar_cols_raw <- COVARS_BASE[COVARS_BASE %in% names(d)]
  for (cv in covar_cols_raw) {
    d[[paste0(cv, "_n")]] <- as.numeric(d[[cv]])
  }
  covar_n <- paste0(covar_cols_raw, "_n")

  # CLAVE: DRDID panel=TRUE requiere que las covariables sean INVARIANTES en el
  # tiempo. Tomamos los valores del período BASE (ola_pre) y los usamos para
  # AMBAS filas de cada individuo → covariables estrictamente pre-tratamiento.
  baseline_covs <- d |>
    dplyr::filter(.data$year == ola_pre) |>
    dplyr::select(dplyr::all_of(c("id", covar_n)))

  # Eliminar versiones variables y reemplazar con baseline
  d <- d |>
    dplyr::select(-dplyr::all_of(covar_n)) |>
    dplyr::left_join(baseline_covs, by = "id")

  # Eliminar NA en outcome, tratamiento y covariables
  d <- d |>
    dplyr::filter(
      !is.na(.data$treat),
      dplyr::if_all(dplyr::all_of(covar_n), ~ !is.na(.x))
    ) |>
    dplyr::distinct(.data$id, .data$year, .keep_all = TRUE)

  # Solo IDs con ambas olas (requisito panel balanceado DRDID)
  ids_ok <- d |>
    dplyr::count(.data$id) |>
    dplyr::filter(.data$n == 2L) |>
    dplyr::pull(.data$id)

  d |> dplyr::filter(.data$id %in% ids_ok)
}

# Corre DRDID para una VD y un subpanel
run_drdid_one <- function(panel_df, vd) {
  if (nrow(panel_df) < 40) {
    warning("Muestra muy pequeña (", nrow(panel_df), " obs); DRDID omitido.")
    return(NULL)
  }
  panel_df$y <- as.numeric(panel_df[[vd]])
  panel_df <- panel_df[!is.na(panel_df$y), ]

  covar_n <- paste0(COVARS_BASE[COVARS_BASE %in% names(panel_df)], "_n")
  # Verificar que covariables existan y tengan varianza
  covar_n <- covar_n[vapply(covar_n, function(v) {
    v %in% names(panel_df) && var(panel_df[[v]], na.rm = TRUE) > 0
  }, logical(1))]

  xf <- if (length(covar_n) > 0) {
    as.formula(paste("~", paste(covar_n, collapse = " + ")))
  } else {
    ~ 1
  }

  result <- tryCatch(
    DRDID::drdid(
      yname  = "y",
      tname  = "year",
      idname = "id",
      dname  = "treat",
      xformla = xf,
      data   = as.data.frame(panel_df),
      panel  = TRUE,
      boot   = FALSE
    ),
    error = function(e) {
      cat("  ⚠ DRDID con covars:", conditionMessage(e), "→ reintento ~1\n")
      tryCatch(
        DRDID::drdid(
          yname  = "y",
          tname  = "year",
          idname = "id",
          dname  = "treat",
          xformla = ~ 1,
          data   = as.data.frame(panel_df),
          panel  = TRUE,
          boot   = FALSE
        ),
        error = function(e2) {
          cat("  ⚠ DRDID ~1:", conditionMessage(e2), "\n")
          NULL
        }
      )
    }
  )
  result
}

# Extrae ATT y SE de un objeto DRDID
extract_att <- function(obj) {
  if (is.null(obj)) return(list(att = NA_real_, se = NA_real_))
  list(att = as.numeric(obj$ATT), se = as.numeric(obj$se))
}

# Bootstrap IC para la diferencia de ATTs (ATT_indi - ATT_no_indi)
# Remuestrea por ID (cluster bootstrap). Construcción vectorizada (rápida).
boot_att_diff <- function(datos, vd, ola_pre, ola_post, n_boot = 200L, seed = 2024) {
  set.seed(seed)

  resample_panel <- function(panel_df, samp_ids) {
    # panel_df tiene 2 filas por id; empajar filas de IDs remuestreados
    # y reasignar id 1..n para evitar duplicados en DRDID
    idx <- unlist(lapply(samp_ids, function(i) which(panel_df$id == i)), use.names = FALSE)
    out <- panel_df[idx, , drop = FALSE]
    # Nuevos IDs: cada bloque de 2 filas = un individuo
    n_pers <- length(samp_ids)
    out$id <- rep(seq_len(n_pers), each = 2L)
    rownames(out) <- NULL
    out
  }

  calc_diff <- function(d_indi, d_noni) {
    r_i <- run_drdid_one(d_indi, vd)
    r_n <- run_drdid_one(d_noni, vd)
    att_i <- extract_att(r_i)$att
    att_n <- extract_att(r_n)$att
    if (is.na(att_i) || is.na(att_n)) return(NA_real_)
    att_i - att_n
  }

  p_indi <- prep_drdid_panel(datos, ola_pre, ola_post, grupo = "indi")
  p_noni <- prep_drdid_panel(datos, ola_pre, ola_post, grupo = "no_indi")

  if (nrow(p_indi) < 40 || nrow(p_noni) < 40) {
    return(list(diff = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_))
  }

  # Ordenar por id, year para que each=2 sea estable
  p_indi <- p_indi[order(p_indi$id, p_indi$year), ]
  p_noni <- p_noni[order(p_noni$id, p_noni$year), ]

  ids_indi <- unique(p_indi$id)
  ids_noni <- unique(p_noni$id)

  point <- calc_diff(p_indi, p_noni)

  boot_diffs <- vapply(seq_len(n_boot), function(b) {
    samp_i <- sample(ids_indi, length(ids_indi), replace = TRUE)
    samp_n <- sample(ids_noni, length(ids_noni), replace = TRUE)
    di <- resample_panel(p_indi, samp_i)
    dn <- resample_panel(p_noni, samp_n)
    suppressWarnings(calc_diff(di, dn))
  }, numeric(1))

  boot_diffs <- boot_diffs[is.finite(boot_diffs)]
  list(
    diff  = point,
    ci_lo = if (length(boot_diffs)) unname(quantile(boot_diffs, 0.025, na.rm = TRUE)) else NA_real_,
    ci_hi = if (length(boot_diffs)) unname(quantile(boot_diffs, 0.975, na.rm = TRUE)) else NA_real_
  )
}

# ── Panel completo (incluye ola 1 para el placebo) ────────────────────────────

# Para placebo (ola 1 → ola 2) necesitamos panel_completo
panel_all <- panel_completo |>
  dplyr::filter(!is.na(.data$indigeneous))

# ── Ejecutar DRDID para las tres transiciones ─────────────────────────────────

if (!has_drdid) {
  cat("⚠ Paquete DRDID no disponible. Instala con install.packages('DRDID').\n")
  drdid_resultados <- list()
} else {

  cat("\n", strrep("=", 60), "\n")
  cat("DR-DiD DOBLEMENTE ROBUSTO — SANT'ANNA & ZHAO (2020)\n")
  cat(strrep("=", 60), "\n\n")

  transiciones <- list(
    list(label = "Decreto (ola 2→4)",           pre = 2L, post = 4L, datos = subset_data),
    list(label = "Proceso politización (ola 2→3)", pre = 2L, post = 3L, datos = subset_data),
    list(label = "Placebo real (ola 1→2)",       pre = 1L, post = 2L, datos = panel_all)
  )

  vds <- c("idx_vio_control", "idx_vio_resguardo")
  vd_labels <- c(
    idx_vio_control   = "Control social (status quo)",
    idx_vio_resguardo = "Cambio social (resguardo territorial)"
  )

  drdid_resultados <- list()

  for (trans in transiciones) {
    for (vd in vds) {
      cat(strrep("-", 50), "\n")
      cat("Transición:", trans$label, "| VD:", vd_labels[vd], "\n\n")

      p_indi <- prep_drdid_panel(trans$datos, trans$pre, trans$post, grupo = "indi")
      p_noni <- prep_drdid_panel(trans$datos, trans$pre, trans$post, grupo = "no_indi")

      cat("  N submuestra indígena:",     nrow(p_indi), "obs (",
          n_distinct(p_indi$id[p_indi$year == trans$pre]), "individuos)\n")
      cat("  N submuestra no indígena:",  nrow(p_noni), "obs (",
          n_distinct(p_noni$id[p_noni$year == trans$pre]), "individuos)\n")

      # Estimar ATT por subgrupo
      set.seed(2024)
      obj_indi <- run_drdid_one(p_indi, vd)
      set.seed(2024)
      obj_noni <- run_drdid_one(p_noni, vd)

      att_indi <- extract_att(obj_indi)
      att_noni <- extract_att(obj_noni)

      cat("  ATT indígena:     β =", round(att_indi$att, 4),
          "  SE =", round(att_indi$se, 4), "\n")
      cat("  ATT no indígena:  β =", round(att_noni$att, 4),
          "  SE =", round(att_noni$se, 4), "\n")

      # Bootstrap IC para la diferencia
      cat("  Bootstrapping diferencia (n_boot = 200)…\n")
      set.seed(2024)
      boot_res <- boot_att_diff(trans$datos, vd, trans$pre, trans$post,
                                n_boot = 200L, seed = 2024)

      diff_att <- if (!is.na(att_indi$att) && !is.na(att_noni$att)) {
        att_indi$att - att_noni$att
      } else NA_real_

      # p-valor asintótico para la diferencia (delta method: SE² = SE_i² + SE_n²)
      se_diff <- sqrt((att_indi$se^2 + att_noni$se^2))
      z_diff  <- if (is.finite(se_diff) && se_diff > 0) diff_att / se_diff else NA_real_
      p_diff  <- if (is.finite(z_diff)) 2 * stats::pnorm(-abs(z_diff)) else NA_real_

      cat("  Δ ATT (indi − no indi): β =", round(diff_att, 4),
          " SE(Δ) =", round(se_diff, 4),
          " p =", format.pval(p_diff, digits = 3), signif_stars(p_diff), "\n")
      cat("  IC 95% bootstrap: [", round(boot_res$ci_lo, 4), ",",
          round(boot_res$ci_hi, 4), "]\n\n")

      key <- paste0(gsub("[^a-z0-9]", "_", tolower(trans$label)), "__", vd)
      drdid_resultados[[key]] <- list(
        transicion  = trans$label,
        ola_pre     = trans$pre,
        ola_post    = trans$post,
        vd          = vd,
        vd_label    = unname(vd_labels[vd]),
        att_indi    = att_indi$att,
        se_indi     = att_indi$se,
        att_noni    = att_noni$att,
        se_noni     = att_noni$se,
        diff_att    = diff_att,
        se_diff     = se_diff,
        p_diff      = p_diff,
        signif      = signif_stars(p_diff),
        ci_boot_lo  = boot_res$ci_lo,
        ci_boot_hi  = boot_res$ci_hi,
        obj_indi    = obj_indi,
        obj_noni    = obj_noni
      )
    }
  }

  # ── Tabla resumen ─────────────────────────────────────────────────────────────

  tabla_df <- drdid_resultados |>
    purrr::map_dfr(function(x) {
      tibble::tibble(
        Transición        = x$transicion,
        VD                = x$vd_label,
        ATT_indígena      = round(x$att_indi, 3),
        SE_indi           = round(x$se_indi, 3),
        ATT_no_indígena   = round(x$att_noni, 3),
        SE_noni           = round(x$se_noni, 3),
        `Δ ATT (i−ni)`    = round(x$diff_att, 3),
        `SE(Δ)`           = round(x$se_diff, 3),
        `p(Δ)`            = round(x$p_diff, 4),
        Sig               = x$signif,
        `IC 95% boot`     = paste0("[", round(x$ci_boot_lo, 3), ", ",
                                    round(x$ci_boot_hi, 3), "]")
      )
    })

  cat(strrep("=", 60), "\n")
  cat("RESUMEN DR-DiD\n")
  cat(strrep("=", 60), "\n\n")
  print(as.data.frame(tabla_df))
  cat("\n")

  tabla_df |>
    gt(groupname_col = "Transición") |>
    tab_header(
      title    = "DR-DiD doblemente robusto por grupo étnico",
      subtitle = paste0(
        "ATT(zona vs. lejos) por submuestra · Δ ATT = indígena − no indígena · ",
        "IC 95% por bootstrap de IDs (n=200) · Covariables: solo baseline ola 2"
      )
    ) |>
    cols_label(
      VD              = "Variable dependiente",
      ATT_indígena    = "ATT (indi)",
      SE_indi         = "SE",
      ATT_no_indígena = "ATT (no indi)",
      SE_noni         = "SE",
      `Δ ATT (i−ni)` = "Δ ATT",
      `SE(Δ)`        = "SE(Δ)",
      `p(Δ)`         = "p",
      Sig             = "",
      `IC 95% boot`  = "IC 95% boot."
    ) |>
    tab_footnote(
      footnote = paste0(
        "Transición decreto = ola 2 (2018) → ola 4 (2023). ",
        "Tratamiento: zona decreto D.S. 418 (53 comunas). ",
        "Proceso de politización = ola 2→3. Placebo = ola 1→2. ",
        "Estimando: efecto territorial/contextual de vivir en zona militarizada. ",
        "+ p<.1, * p<.05, ** p<.01, *** p<.001"
      )
    ) |>
    tab_options(
      table.border.top.style    = "solid",
      table.border.bottom.style = "solid",
      table.font.size           = px(11)
    ) |>
    gtsave("output/tablas/tabla_drdid.html")
  cat("✓ Tabla DR-DiD guardada: output/tablas/tabla_drdid.html\n")

  saveRDS(
    list(
      resultados  = drdid_resultados,
      tabla_df    = tabla_df,
      covars_base = COVARS_BASE
    ),
    "data/drdid.rds"
  )
  cat("✓ Resultados DRDID guardados: data/drdid.rds\n")

  cat("\n✓ 03b_drdid.R ejecutado correctamente.\n")
}
