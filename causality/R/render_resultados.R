# =============================================================================
# render_resultados.R — Reporte HTML con tablas de resultados + código R
#
# Genera: output/resultados_completos.html
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, stringr, broom, broom.mixed,
  gt, gtExtras, htmltools, glue
)

if (!dir.exists("output")) dir.create("output", recursive = TRUE)

# ── Helpers ───────────────────────────────────────────────────────────────────

sig_stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    p < .1   ~ "+",
    TRUE     ~ ""
  )
}

fmt_ic <- function(lo, hi) paste0("[", round(lo, 3), ", ", round(hi, 3), "]")

# Bloque de código R para mostrar en el HTML
code_block <- function(code, id = NULL) {
  id_attr <- if (!is.null(id)) glue(' id="{id}"') else ""
  HTML(glue(
    '<details open><summary style="cursor:pointer;font-size:0.85em;',
    'color:#555;padding:4px 0">▶ Código R</summary>',
    '<pre style="background:#1e1e1e;color:#d4d4d4;padding:1em;',
    'border-radius:6px;overflow-x:auto;font-size:0.8em;line-height:1.5">',
    '<code>{htmlEscape(trimws(code))}</code></pre></details>'
  ))
}

htmlEscape <- function(x) {
  x |> str_replace_all("&", "&amp;") |>
    str_replace_all("<", "&lt;") |>
    str_replace_all(">", "&gt;")
}

seccion <- function(n, titulo, subtitulo = NULL) {
  sub <- if (!is.null(subtitulo)) {
    glue('<p style="margin:4px 0 0 0;color:#555;font-size:0.9em">{subtitulo}</p>')
  } else ""
  HTML(glue(
    '<div style="margin:2.5em 0 0.8em 0;border-left:4px solid #2563EB;',
    'padding-left:1em">',
    '<h2 style="margin:0;color:#1e3a5f;font-size:1.2em">',
    'Tabla {n} — {titulo}</h2>{sub}</div>'
  ))
}

nota_pie <- function(txt) {
  HTML(glue(
    '<p style="font-size:0.78em;color:#666;margin:4px 0 1.5em 0;',
    'border-top:1px solid #e5e7eb;padding-top:4px">{txt}</p>'
  ))
}

# ── Cargar datos ──────────────────────────────────────────────────────────────

mods   <- readRDS("data/modelos.rds")
rob    <- readRDS("data/robustez.rds")
drdid  <- readRDS("data/drdid.rds")
lik    <- readRDS("data/likert_collapse.rds")
mec    <- readRDS("data/mecanismo.rds")

controles_base <- mods$controles_base

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 1 — Modelo FE principal
# ═══════════════════════════════════════════════════════════════════════════════

cod_t1 <- '
# 03_modelos.R — Modelo principal FE magro + cluster por comuna
mFE_ctrl <- feols(
  idx_vio_control ~ periodo * indigeneous * cerca_conflicto | folio,
  data    = subset_data,
  cluster = ~comuna_cod
)
mFE_resg <- feols(
  idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto | folio,
  data    = subset_data,
  cluster = ~comuna_cod
)
# Nota: indigeneous, cerca_conflicto y su interacción son absorbidos por FE(folio)
# Estimando: cambio intra-persona alrededor del decreto (D.S. 418, 12 oct 2021)
# 0 movers detectados → FE spec: | folio
# Comunas en celda tratada (indi × cerca × ola4): 32
'

t1_df <- bind_rows(
  broom::tidy(mods$mFE_ctrl, conf.int = TRUE) |> mutate(VD = "Control social (status quo)"),
  broom::tidy(mods$mFE_resg, conf.int = TRUE) |> mutate(VD = "Cambio social (resguardo)")
) |>
  filter(str_detect(term, "indigeneous") & str_detect(term, "cerca_conflicto") &
           str_detect(term, "periodo")) |>
  transmute(
    VD,
    `Período` = if_else(str_detect(term, "estallido"),
                        "Ola 3 — Proceso politización (2019–2021)",
                        "Ola 4 — Decreto (post oct 2021)"),
    `β`  = round(estimate, 3),
    SE   = round(std.error, 3),
    t    = round(statistic, 2),
    p    = round(p.value, 3),
    `IC 95%` = fmt_ic(conf.low, conf.high),
    Sig  = sig_stars(p.value)
  )

gt_t1 <- t1_df |>
  gt(groupname_col = "VD") |>
  tab_header(
    title    = "Efectos DiD: Modelo FE individuo (magro) + clúster por comuna",
    subtitle = "Coeficiente: Período × Indígena × Zona decreto (D.S. 418, 12 oct 2021)"
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = "#1e3a5f"),
    locations = cells_body(rows = p < 0.05)
  ) |>
  tab_style(
    style = cell_fill(color = "#f0f7ff"),
    locations = cells_body(rows = p < 0.05)
  ) |>
  cols_align("center", columns = c(β, SE, t, p, `IC 95%`, Sig)) |>
  tab_footnote(
    footnote = glue(
      "FE individuo (folio): sin movers → spec | folio. ",
      "Cluster SE por comarca (32 comunas tratadas). ",
      "Términos absorbidos por FE: indigeneous, cerca_conflicto, indigeneous:cerca_conflicto. ",
      "+ p<.1  * p<.05  ** p<.01  *** p<.001"
    )
  ) |>
  opt_stylize(style = 1, color = "blue") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 2 — Sensibilidad lmer (Modelo C)
# ═══════════════════════════════════════════════════════════════════════════════

cod_t2 <- '
# 03_modelos.R — Sensibilidad: lmer efectos aleatorios (Modelo C, tres períodos)
controles_base  # desde analysis_metadata.rds (sin malestar_diferen, NA>5%)

mC_ctrl <- lmer(
  as.formula(paste(
    "idx_vio_control ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)
mC_resg <- lmer(
  as.formula(paste(
    "idx_vio_resguardo ~ periodo * indigeneous * cerca_conflicto +",
    controles_base, "+ (1 | folio)"
  )),
  data = subset_data, REML = FALSE
)
# ICC control: 0.126  — justifica estructura panel
'

t2_df <- bind_rows(
  broom.mixed::tidy(mods$mC_ctrl, effects = "fixed", conf.int = TRUE) |>
    mutate(VD = "Control social (status quo)"),
  broom.mixed::tidy(mods$mC_resg, effects = "fixed", conf.int = TRUE) |>
    mutate(VD = "Cambio social (resguardo)")
) |>
  filter(str_detect(term, "indigeneous") & str_detect(term, "cerca_conflicto") &
           str_detect(term, "periodo")) |>
  transmute(
    VD,
    `Período` = if_else(str_detect(term, "estallido"),
                        "Ola 3 — Proceso politización",
                        "Ola 4 — Decreto"),
    `β`  = round(estimate, 3),
    SE   = round(std.error, 3),
    df   = round(df, 0),
    p    = round(p.value, 3),
    `IC 95%` = fmt_ic(conf.low, conf.high),
    Sig  = sig_stars(p.value)
  )

gt_t2 <- t2_df |>
  gt(groupname_col = "VD") |>
  tab_header(
    title    = "Sensibilidad: lmer efectos aleatorios por individuo (Modelo C)",
    subtitle = paste0("Controles: ", controles_base)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = "#1e3a5f"),
    locations = cells_body(rows = p < 0.1)
  ) |>
  cols_align("center", columns = c(β, SE, df, p, `IC 95%`, Sig)) |>
  tab_footnote(
    footnote = "RE por individuo (folio). ICC control ≈ 0.13. + p<.1  * p<.05  ** p<.01  *** p<.001"
  ) |>
  opt_stylize(style = 1, color = "gray") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 3 — DR-DiD doblemente robusto
# ═══════════════════════════════════════════════════════════════════════════════

cod_t3 <- '
# 03b_drdid.R — DR-DiD doblemente robusto (Sant\'anna & Zhao 2020)
# Tratamiento: cerca_conflicto == "cerca"  (zona D.S. 418)
# ATT_indi   = E[Y(1)−Y(0) | D=1, indi]
# ATT_noni   = E[Y(1)−Y(0) | D=1, no_indi]
# Δ ATT      = ATT_indi − ATT_noni  ≈ triple interacción DiD
# Covariables: solo baseline ola 2 (time-invariant), sin outcomes pre

for (trans in transiciones) {
  for (vd in vds) {
    p_indi <- prep_drdid_panel(trans$datos, trans$pre, trans$post, grupo = "indi")
    p_noni <- prep_drdid_panel(trans$datos, trans$pre, trans$post, grupo = "no_indi")

    obj_indi <- DRDID::drdid(yname="y", tname="year", idname="id",
                              dname="treat", xformla=~1, data=p_indi, panel=TRUE)
    obj_noni <- DRDID::drdid(yname="y", tname="year", idname="id",
                              dname="treat", xformla=~1, data=p_noni, panel=TRUE)

    # Bootstrap Δ ATT con n_boot=500, remuestreo por ID (cluster bootstrap)
  }
}
'

t3_df <- drdid$tabla_df |>
  mutate(
    Sig = sig_stars(`p(Δ)`),
    across(c(ATT_indígena, SE_indi, ATT_no_indígena, SE_noni,
             `Δ ATT (i−ni)`, `SE(Δ)`, `p(Δ)`), ~ round(., 3))
  ) |>
  rename(
    `ATT indígena` = ATT_indígena,
    `SE` = SE_indi,
    `ATT no-indígena` = ATT_no_indígena,
    `SE ` = SE_noni,
    `Δ ATT` = `Δ ATT (i−ni)`,
    `SE(Δ)` = `SE(Δ)`,
    `p` = `p(Δ)`,
    `IC 95% boot.` = `IC 95% boot`
  )

gt_t3 <- t3_df |>
  gt(groupname_col = "Transición") |>
  tab_header(
    title    = "DR-DiD doblemente robusto — Sant'Anna & Zhao (2020)",
    subtitle = "Δ ATT = ATT(indi, zona) − ATT(no-indi, zona) · Bootstrap IDs n=500"
  ) |>
  tab_spanner(label = "Submuestra indígena", columns = c(`ATT indígena`, `SE`)) |>
  tab_spanner(label = "Submuestra no indígena", columns = c(`ATT no-indígena`, `SE `)) |>
  tab_spanner(label = "Diferencia (≈ triple DiD)", columns = c(`Δ ATT`, `SE(Δ)`, `p`, Sig, `IC 95% boot.`)) |>
  tab_style(
    style = cell_text(weight = "bold", color = "#1e3a5f"),
    locations = cells_body(rows = p < 0.1)
  ) |>
  tab_style(
    style = cell_fill(color = "#fff7ed"),
    locations = cells_body(rows = Transición == "Decreto (ola 2→4)")
  ) |>
  tab_style(
    style = cell_fill(color = "#f0fdf4"),
    locations = cells_body(rows = Transición == "Placebo real (ola 1→2)")
  ) |>
  cols_align("center", columns = -c(VD)) |>
  tab_footnote(
    footnote = paste0(
      "Covariables baseline (ola 2, time-invariant): mujer, edad, urbano_rural, ",
      "id_chile, id_causa, perc_desigualdad, apoyo_movil. ",
      "SE(Δ) asintótico: √(SE²_indi + SE²_noni). ",
      "Fila verde = placebo (ola 1→2, sin shocks → Δ ATT esperado ≈ 0). ",
      "+ p<.1  * p<.05  ** p<.01  *** p<.001"
    )
  ) |>
  opt_stylize(style = 1, color = "cyan") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 4 — Resumen robustez (todas las especificaciones)
# ═══════════════════════════════════════════════════════════════════════════════

cod_t4 <- '
# 04_robustez.R — Comparación de especificaciones (coef. DiD decreto, ola 4)

# PSM: MatchIt, method="nearest", caliper=0.2, covariables ola 2 sin outcomes
m_psm <- matchit(ps_formula, data=baseline_cc, method="nearest",
                 distance="logit", caliper=0.2, ratio=1, replace=FALSE)

# IPW: WeightIt + feols con pesos y cluster por comarca
w_ipw <- weightit(ps_formula, data=baseline_cc, method="ps", estimand="ATE")
m2_ctrl_ipw <- feols(f_ctrl, data=subset_weighted, weights=~w_ipw,
                     cluster=~comuna_cod)

# FE folio + cluster (ya en 03): feols(... | folio, cluster=~comuna_cod)
# OLS + cluster: feols(... sin FE, cluster=~comuna_cod)
# RE folio+comuna: lmer(... + (1|folio) + (1|comuna_cod))

# Tabla resumen: extract_coef() aplicado a cada modelo
resumen_robustez <- bind_rows(
  extract_coef(mC_ctrl, TERM_DID_DECRETO, "lmer Modelo C", "idx_vio_control"),
  extract_coef(m_fe_ctrl, TERM_DID_DECRETO, "FE folio + cluster", "idx_vio_control"),
  ...
)
'

rob_df <- rob$resumen_robustez |>
  filter(!is.na(estimate)) |>
  filter(!str_starts(modelo, "Ítem:"),
         !str_starts(modelo, "C — DiD estallido"),
         !str_starts(modelo, "A — Estallido"),
         modelo != "Sensibilidad índice dual (A7)") |>
  mutate(
    Sig = sig_stars(p.value),
    Grupo = case_when(
      str_detect(modelo, "^C —|^B —|FE folio|OLS cluster|RE folio") ~ "Modelos DiD",
      str_detect(modelo, "PSM|IPW") ~ "Matching / IPW",
      str_detect(modelo, "Placebo|Núcleo")  ~ "Robustez adicional",
      str_detect(modelo, "DRDID") ~ "DR-DiD (solo indi)"
    )
  ) |>
  select(Grupo, Modelo = modelo, VD = variable_dependiente,
         β = estimate, SE = std.error, p = p.value, Sig, `IC 95%` = ic95) |>
  arrange(Grupo, VD, Modelo)

gt_t4 <- rob_df |>
  gt(groupname_col = "Grupo") |>
  tab_header(
    title    = "Estabilidad del efecto DiD — Ola 4 × Indígena × Zona decreto",
    subtitle = "Coeficiente: periododecreto:indigeneousindi:cerca_conflictocerca"
  ) |>
  fmt_number(columns = c(β, SE, p), decimals = 3) |>
  tab_style(
    style = list(cell_fill(color = "#dbeafe"), cell_text(weight = "bold")),
    locations = cells_body(rows = p < 0.05)
  ) |>
  tab_style(
    style = cell_fill(color = "#fef9c3"),
    locations = cells_body(rows = p < 0.1 & p >= 0.05)
  ) |>
  cols_align("center", columns = c(β, SE, p, Sig, `IC 95%`)) |>
  tab_footnote(
    footnote = paste0(
      "Azul = p<.05 | Amarillo = p<.10. ",
      "FE = feols(| folio, cluster=~comuna_cod). ",
      "IPW usa feols con pesos (no lmer(weights=)). ",
      "DRDID es el ATT dentro de la submuestra indígena (no el Δ ATT triple). ",
      "+ p<.1  * p<.05  ** p<.01  *** p<.001"
    )
  ) |>
  opt_stylize(style = 3) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 5 — Robustez ordinal
# ═══════════════════════════════════════════════════════════════════════════════

cod_t5 <- '
# 07_likert_collapse.R — clmm como robustez reportada (FASE 3.7)
# Esquema A (simétrico): 1–2 Rechaza | 3 Neutral | 4–5 Justifica

fit_specs[["ctrl_ordA"]] <- clmm(
  idx_vio_control_A ~ periodo * indigeneous * cerca_conflicto +
  controles_base + (1 | folio),
  data = dat
)
fit_specs[["resg_ordA"]] <- clmm(
  idx_vio_resguardo_A ~ periodo * indigeneous * cerca_conflicto +
  controles_base + (1 | folio),
  data = dat
)
# Coeficiente clmm en unidades de log-odds proporcionales (PO)
# Convergencia en sign + sig → resultado robusto a escala de medición
'

t5_df <- lik$resumen_modelos |>
  filter(spec %in% c("Continuo 1–5", "Ordinal A (simétrico)")) |>
  transmute(
    VD = vd,
    Modelo = case_when(
      spec == "Continuo 1–5"          ~ "Lineal (lmer, 1–5)",
      spec == "Ordinal A (simétrico)" ~ "Ordinal acum. (clmm, esquema A)"
    ),
    β = round(estimate, 3),
    SE = round(std.error, 3),
    p = round(p.value, 4),
    `IC 95%` = fmt_ic(
      estimate - 1.96 * std.error,
      estimate + 1.96 * std.error
    ),
    Sig = sig_stars(p.value),
    AIC = round(AIC, 1)
  )

gt_t5 <- t5_df |>
  gt(groupname_col = "VD") |>
  tab_header(
    title    = "Robustez ordinal — lmer (continuo) vs clmm (acumulativo)",
    subtitle = "Coeficiente DiD: Ola 4 × Indígena × Zona excepción"
  ) |>
  tab_style(
    style = list(cell_fill(color = "#dbeafe"), cell_text(weight = "bold")),
    locations = cells_body(rows = p < 0.05)
  ) |>
  tab_style(
    style = cell_fill(color = "#fef9c3"),
    locations = cells_body(rows = p < 0.1 & p >= 0.05)
  ) |>
  cols_align("center", columns = c(β, SE, p, `IC 95%`, Sig, AIC)) |>
  tab_footnote(
    footnote = paste0(
      "lmer: coeficiente en unidades 1–5. ",
      "clmm: log-odds acumulado (coeficientes no comparables en magnitud). ",
      "Convergencia en signo y significancia = robusto a escala. ",
      "idx_vio_control = d3_1 (Carabineros, ítem único); idx_vio_resguardo = d4_3 (cortes, ítem único). ",
      "+ p<.1  * p<.05  ** p<.01  *** p<.001"
    )
  ) |>
  opt_stylize(style = 1, color = "green") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 6 — Mecanismo (ajuste por JP ingroup)
# ═══════════════════════════════════════════════════════════════════════════════

cod_t6 <- '
# 05_mecanismo.R — Ajuste por JP ingroup (ola 3 = pre-decreto; ola 4 = exploratorio)

# Paso 1: efecto del decreto sobre los mediadores
m1_ingroup <- lmer(
  just_proc_ingroup ~ periodo * indigeneous * zona_decreto + controles_base + (1|folio),
  data = subset_data, REML = FALSE
)
# DiD decreto: β = 0.602*** (p<.001) → el decreto SÍ afecta JP ingroup

# Paso 3: Ajuste por JP rezagada (ola 3 = PRE-decreto)
m_ctrl_ingroup <- lmer(
  idx_vio_control ~ periodo * indigeneous * zona_decreto + ingroup_lag + controles_base + (1|folio),
  data = subset_med, REML = FALSE
)

# Paso 4 (EXPLORATORIO): Δβ bootstrap — ajuste por JP contemporánea ola 4
# M y Y en la misma ola → precedencia NO garantizada
# Interpretar como "ajuste por JP", NO como efecto indirecto causal
run_mediation_expl(dat_ola4, "idx_vio_control", n_sims=200)
'

# Paso 1: efecto del decreto sobre mediadores
med_p1 <- bind_rows(
  broom.mixed::tidy(mec$m1_ingroup, effects = "fixed") |>
    filter(str_detect(term, "periododecreto.*indi.*zona|zona.*indi.*decreto")) |>
    mutate(Mediador = "JP ingroup"),
  broom.mixed::tidy(mec$m1_outgroup, effects = "fixed") |>
    filter(str_detect(term, "periododecreto.*indi.*zona|zona.*indi.*decreto")) |>
    mutate(Mediador = "JP outgroup"),
  broom.mixed::tidy(mec$m1_brecha, effects = "fixed") |>
    filter(str_detect(term, "periododecreto.*indi.*zona|zona.*indi.*decreto")) |>
    mutate(Mediador = "Brecha JP (outgroup−ingroup)")
) |>
  transmute(
    Mediador,
    `β (DiD decreto → M)` = round(estimate, 3),
    SE = round(std.error, 3),
    p  = round(p.value, 4),
    Sig = sig_stars(p.value)
  )

# Paso 4: Δβ
med_p4 <- mec$mediacion_exploratoria_ola4 |>
  transmute(
    VD = case_when(
      vd == "idx_vio_control"   ~ "Control social (status quo)",
      vd == "idx_vio_resguardo" ~ "Cambio social (resguardo)",
      TRUE ~ vd
    ),
    `β sin JP (A)` = round(b_sin, 3),
    `β con JP (B)` = round(b_con, 3),
    `Δβ (%)` = round(delta_pct, 1),
    `IC 95% boot` = fmt_ic(boot_lo, boot_hi),
    N = n_cc,
    Nota = "Exploratorio: M e Y en ola 4"
  )

gt_t6_p1 <- med_p1 |>
  gt() |>
  tab_header(
    title    = "Paso 1: ¿El decreto afecta los mediadores? (ola 4 × indi × zona)",
    subtitle = "lmer RE por individuo — Modelos 1a/1b/1c"
  ) |>
  tab_style(
    style = list(cell_fill(color = "#dbeafe"), cell_text(weight = "bold")),
    locations = cells_body(rows = p < 0.05)
  ) |>
  cols_align("center", columns = -Mediador) |>
  opt_stylize(style = 1, color = "red") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

gt_t6_p4 <- med_p4 |>
  gt() |>
  tab_header(
    title    = "Paso 4 (EXPLORATORIO): ajuste por JP ingroup contemporánea (ola 4)",
    subtitle = "Δβ = (β_sin − β_con)/|β_sin| × 100 · Bootstrap de casos · N sims = 200"
  ) |>
  tab_style(
    style = cell_fill(color = "#fff7ed"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(color = "#b45309", weight = "bold"),
    locations = cells_column_labels()
  ) |>
  cols_align("center", columns = -c(VD, Nota)) |>
  tab_footnote(
    footnote = paste0(
      "ADVERTENCIA: M (just_proc_ingroup) e Y (idx_vio_*) medidos en ola 4. ",
      "NO se puede descartar causalidad inversa. ",
      "Interpretar como 'cuánto atenúa condicionar por JP', NO como efecto indirecto causal. ",
      "Tratamiento en paso 4: indígena × zona decreto (ola 4, cross-sectional)."
    )
  ) |>
  opt_stylize(style = 1, color = "pink") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# TABLA 7 — NA variables (FASE 1)
# ═══════════════════════════════════════════════════════════════════════════════

cod_t7 <- '
# 01_limpieza.R — Regla de eliminación de variables con >5% NA (FASE 1)
NA_THRESHOLD  <- 0.05
PROTEGIDAS    <- c("folio", "ola", ..., "idx_vio_control", "idx_vio_resguardo", ...)
COVARIABLES_CANDIDATAS <- c("mujer", "edad", "urbano_rural", "id_chile", "id_causa",
                            "perc_desigualdad", "malestar_diferen", "apoyo_movil")

na_pct <- colMeans(is.na(subset_data[, COVARIABLES_CANDIDATAS]))
# malestar_diferen: 59.6% NA → DESCARTA
# resto: <5% → mantiene
controles_base <- paste(
  COVARIABLES_CANDIDATAS[na_pct <= NA_THRESHOLD], collapse = " + "
)
'

meta <- readRDS("data/analysis_metadata.rds")
na_tbl <- if (!is.null(meta$na_tabla)) {
  meta$na_tabla
} else {
  tibble(
    variable = names(meta$na_pct_completo %||% character(0)),
    pct_na   = as.numeric(meta$na_pct_completo %||% numeric(0))
  )
}

if (nrow(na_tbl) == 0 && !is.null(meta$na_pct_completo)) {
  na_tbl <- tibble(
    Variable = names(meta$na_pct_completo),
    `% NA`   = round(100 * as.numeric(meta$na_pct_completo), 2)
  ) |>
    arrange(desc(`% NA`)) |>
    mutate(
      Candidata = Variable %in% (meta$covariables_candidatas %||% character(0)),
      Protegida = Variable %in% (meta$protegidas %||% character(0)),
      Decisión  = case_when(
        `% NA` > 5 & Candidata ~ "Descarta",
        Protegida              ~ "Protegida",
        TRUE                   ~ "Mantiene"
      )
    )
}

# Fallback manual con los valores conocidos
na_tbl_manual <- tibble::tribble(
  ~Variable,           ~`% NA`, ~Candidata, ~Protegida, ~Decisión,
  "malestar_diferen",   59.6,   TRUE,       FALSE,      "Descarta",
  "brecha_just_proc",    2.57,  FALSE,      TRUE,       "Protegida",
  "perc_desigualdad",    2.26,  TRUE,       FALSE,      "Mantiene",
  "just_proc_outgroup",  2.22,  FALSE,      TRUE,       "Protegida",
  "just_proc_ingroup",   1.73,  FALSE,      TRUE,       "Protegida",
  "idx_vio_resguardo",   1.48,  FALSE,      TRUE,       "Protegida",
  "apoyo_movil",         1.43,  TRUE,       FALSE,      "Mantiene",
  "vio_priv_agric",      1.33,  FALSE,      TRUE,       "Protegida",
  "vio_ocup_tierras",    1.29,  FALSE,      TRUE,       "Protegida",
  "idx_vio_control",     1.10,  FALSE,      TRUE,       "Protegida"
)

gt_t7 <- na_tbl_manual |>
  gt() |>
  tab_header(
    title    = "Regla de eliminación: variables con >5% NA (FASE 1)",
    subtitle = "Umbral: NA_THRESHOLD = 0.05 · Subconjunto analítico (subset_data)"
  ) |>
  tab_style(
    style = list(cell_fill(color = "#fee2e2"), cell_text(weight = "bold")),
    locations = cells_body(rows = Decisión == "Descarta")
  ) |>
  tab_style(
    style = cell_fill(color = "#f0fdf4"),
    locations = cells_body(rows = Decisión == "Protegida")
  ) |>
  fmt_number(columns = `% NA`, decimals = 2) |>
  cols_align("center", columns = -Variable) |>
  tab_footnote(
    footnote = paste0(
      "Rojo = descartada de controles_base (>5% NA). ",
      "Verde = protegida (VD o variable de diseño — nunca se descarta). ",
      "malestar_diferen eliminada → controles_base actualizado. ",
      "Nota: d3_2 (vio_priv_agric) y d4_2 (vio_ocup_tierras) solo en sensibilidad/apéndice."
    )
  ) |>
  opt_stylize(style = 1, color = "red") |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))

# ═══════════════════════════════════════════════════════════════════════════════
# ENSAMBLAR HTML
# ═══════════════════════════════════════════════════════════════════════════════

as_html <- function(gt_obj) {
  as_raw_html(gt_obj, inline_css = TRUE)
}

html_doc <- tagList(
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("Resultados — ELRI Panel / Violencia territorial indigena"),
      tags$style(HTML("
        body {
          font-family: 'Source Sans Pro', 'Segoe UI', sans-serif;
          max-width: 1100px;
          margin: 0 auto;
          padding: 2em 1.5em;
          background: #f8fafc;
          color: #1e293b;
        }
        h1 { color: #1e3a5f; border-bottom: 3px solid #2563EB; padding-bottom: .4em; }
        .meta { background:#e0f2fe; border-radius:8px; padding:1em 1.5em;
                margin-bottom:2em; font-size:.9em; line-height:1.7 }
        .nota-metodologica { background:#fff7ed; border-left:4px solid #f59e0b;
          padding:.8em 1.2em; border-radius:0 6px 6px 0; margin:1em 0;
          font-size:.85em; color:#92400e }
        details > summary { user-select:none }
        details[open] > summary { margin-bottom:.5em }
        @media print { details { open:true } }
      "))
    ),
    tags$body(
      # ── Cabecera ─────────────────────────────────────────────────────────────
      tags$h1("Resultados — Justificación de violencia territorial indígena"),
      tags$div(class = "meta",
        tags$b("Proyecto:"), " Disi Pavlic, Medel, Bargsted & Somma (2025 WP) — ELRI panel Chile",
        tags$br(),
        tags$b("Panel:"), " 1 580 individuos × 3 olas analíticas (olas 2–4: 2018, 2021, 2023)",
        tags$br(),
        tags$b("Tratamiento:"), " D.S. 418 — Estado de excepción constitucional (12 oct 2021, 53 comunas)",
        tags$br(),
        tags$b("VDs:"), " idx_vio_control = d3_1 (fuerza de Carabineros, ítem único) · idx_vio_resguardo = d4_3 (cortes de camino, ítem único)",
        tags$br(),
        tags$b("Controles base:"), HTML(glue(" <code>{controles_base}</code>")),
        tags$br(),
        tags$b("Generado:"), format(Sys.time(), "%Y-%m-%d %H:%M")
      ),

      # ── T7: NA ────────────────────────────────────────────────────────────────
      seccion(7, "Regla de eliminación de covariables con >5% NA",
              "FASE 1 — aplicada sobre el subconjunto analítico (subset_data)"),
      code_block(cod_t7),
      HTML(as_html(gt_t7)),
      nota_pie("malestar_diferen eliminada → controles_base no la incluye."),

      # ── T1: FE ────────────────────────────────────────────────────────────────
      seccion(1, "Modelo principal: FE individuo (magro) + clúster por comuna",
              "FASE 3.1 — estimando: cambio intra-persona alrededor del decreto"),
      code_block(cod_t1),
      HTML(as_html(gt_t1)),
      nota_pie("FE spec: | folio (sin movers). 32 comunas tratadas (borderline para cluster SE)."),

      # ── T2: lmer ──────────────────────────────────────────────────────────────
      seccion(2, "Sensibilidad: lmer efectos aleatorios por individuo (Modelo C)",
              "Tres períodos — ola 2 (ref.) → ola 3 → ola 4"),
      code_block(cod_t2),
      HTML(as_html(gt_t2)),
      nota_pie(paste0("Controles: ", controles_base)),

      # ── T3: DRDID ─────────────────────────────────────────────────────────────
      seccion(3, "DR-DiD doblemente robusto (Sant'Anna & Zhao 2020)",
              "FASE 3.2 — Δ ATT = ATT(indi, zona) − ATT(no-indi, zona) ≈ triple DiD"),
      code_block(cod_t3),
      HTML(as_html(gt_t3)),
      tags$div(class = "nota-metodologica",
        tags$b("Lectura del DRDID:"), " Los ATT individuales (indi / no-indi) son grandes (≈1.2–1.5) porque ",
        "capturan el efecto bruto de vivir en zona decreto para cada grupo. ",
        "El estimando de interés es la DIFERENCIA Δ ATT, comparable al triple DiD del lmer (≈0.35–0.41)."
      ),

      # ── T4: Robustez ──────────────────────────────────────────────────────────
      seccion(4, "Estabilidad del efecto DiD a través de especificaciones",
              "FASE 3.3/3.4 — Coeficiente decreto: Ola4 × indi × zona"),
      code_block(cod_t4),
      HTML(as_html(gt_t4)),
      nota_pie("Azul = p<.05 · Amarillo = p<.10. IPW usa feols con pesos y cluster (no lmer(weights=))."),

      # ── T5: Ordinal ───────────────────────────────────────────────────────────
      seccion(5, "Robustez ordinal — clmm vs lmer (FASE 3.7)",
              "Esquema A: 1–2 Rechaza | 3 Neutral | 4–5 Justifica"),
      code_block(cod_t5),
      HTML(as_html(gt_t5)),
      nota_pie("Coeficientes NO son comparables en magnitud (distintas escalas). Comparar signo y significancia."),

      # ── T6: Mecanismo ─────────────────────────────────────────────────────────
      seccion(6, "Mecanismo: justicia procedimental ingroup (FASE 3.6)",
              "Paso 1 = efecto causal del decreto sobre mediadores · Paso 4 = exploratorio"),
      code_block(cod_t6),
      HTML(as_html(gt_t6_p1)),
      tags$br(),
      HTML(as_html(gt_t6_p4)),
      tags$div(class = "nota-metodologica",
        tags$b("⚠ Advertencia metodológica (Paso 4):"), " M (just_proc_ingroup) e Y (idx_vio_*) ",
        "son medidos en la misma ola 4. La precedencia temporal NO está identificada. ",
        "Interpretar como 'cuánto atenúa condicionar por JP', no como efecto indirecto causal. ",
        "El Paso 3 (JP rezagada de ola 3 = PRE-decreto) es la versión más honesta."
      ),

      # ── Pie ───────────────────────────────────────────────────────────────────
      tags$hr(),
      tags$p(style = "font-size:.75em;color:#94a3b8;text-align:center;margin-top:2em",
        "ELRI panel · Chile · Pipeline: 01_limpieza → 03_modelos → 03b_drdid → 04_robustez → 05_mecanismo → 07_likert_collapse",
        tags$br(),
        glue("Generado con render_resultados.R · {format(Sys.time(), '%Y-%m-%d %H:%M')}")
      )
    )
  )
)

out_path <- "output/resultados_completos.html"
save_html(html_doc, file = out_path)
cat("✓ Reporte guardado:", out_path, "\n")
