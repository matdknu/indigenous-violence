# Cargar paquetes con pacman::p_load
pacman::p_load(
  dplyr, tidyverse, stringr, here, haven,       # Manipulación y carga de datos
  lavaan, LMest, lcmm, panelr,                 # Modelos estructurales y longitudinales
  psych, semPlot, car,                         # Psicometría y visualización SEM
  sjlabelled, sjmisc,                          # Etiquetas y gestión de variables
  viridis, xtable                              # Visualización y reporte
)

library(lcmm)
library(dplyr)
library(misty)


# limpiar espacio
cat("\014")
rm(list = ls())
gc()

# Load data
load("data/BBDD_ELRI_LONG.RData")
data <- BBDD_ELRI_LONG

data |> select(ola, folio, c2)

df_lcga <- data %>%
  transmute(
    folio = as.integer(factor(folio)),        # <- requerido numérico
   ola = match(                                       # w01,w02,w03,... -> 1,2,3...
      readr::parse_number(as.character(ola)),
      sort(unique(readr::parse_number(as.character(ola))))
    ),
    c2 = as.numeric(haven::zap_labels(c2))
  ) %>%
  arrange(folio,ola) %>%
  # Mantener observaciones completas y sujetos con >= 2 olas
  tidyr::drop_na(c2,ola) %>%
  group_by(folio) %>%
  filter(n() >= 4) %>%
  ungroup()

df_lcga <- as.na(df_lcga, na = c(88, 99, 8888, 9999))
df_lcga<-df_lcga %>% na.omit()


# (Opcional) Estandarizar outcome si ayuda a convergencia
# df_lcga <- df_lcga %>% mutate(c2 = scale(c2)[,1])

# ---- 3) Modelos ----
# 1-clase: punto de partida
m1 <- hlme(
  fixed   = c2 ~ola,
  random  = ~ 1,                  # intercepto aleatorio
  subject = "folio",
  data    = df_lcga,
  na.action = 1
)

# 2 a 4 clases con gridsearch (inicios aleatorios)
m2 <- gridsearch(
  rep = 100, maxiter = 20, minit = m1,
  m = hlme(
    fixed   = c2 ~ola,
    mixture = ~ola,             # pendiente por clase
    random  = ~ 1,
    subject = "folio",
    ng      = 2,
    data    = df_lcga,
    nwg     = TRUE,               # var. residual por clase
    na.action = 1
  )
)

m3 <- gridsearch(
  rep = 100, maxiter = 20, minit = m1,
  m = hlme(
    fixed   = c2 ~ola,
    mixture = ~ola,
    random  = ~ 1,
    subject = "folio",
    ng      = 3,
    data    = df_lcga,
    nwg     = TRUE,
    na.action = 1
  )
)

m4 <- gridsearch(
  rep = 100, maxiter = 20, minit = m1,
  m = hlme(
    fixed   = c2 ~ola,
    mixture = ~ola,
    random  = ~ 1,
    subject = "folio",
    ng      = 4,
    data    = df_lcga,
    nwg     = TRUE,
    na.action = 1
  )
)

m1
m2
m3
m4

write_rds(m1, "output/modelos/m1_confianza.rds")
write_rds(m2, "output/modelos/m2_confianza.rds")
write_rds(m3, "output/modelos/m3_confianza.rds")
write_rds(m4, "output/modelos/m4_confianza.rds")


# ---- 4) Comparación rápida ----
fit_tab <- tibble::tibble(
  ng     = c(1, 2, 3, 4),
  loglik = c(m1$loglik, m2$loglik, m3$loglik, m4$loglik),
  AIC    = c(m1$AIC,    m2$AIC,    m3$AIC,    m4$AIC),
  BIC    = c(m1$BIC,    m2$BIC,    m3$BIC,    m4$BIC),
  conv  = c(m1$conv,   m2$conv,   m3$conv,   m4$conv)
)

print(fit_tab)

summarytable(m1, m2, m3, m4, which = c("AIC","BIC", "entropy", "conv", "loglik", "npm", "%class"))


# Estimamos modelos con término cuadrático
# =========================
# 3bis) Modelos cuadráticos (usando c2, ola, folio)
# =========================

# Asegurar términos centrados (mejor convergencia)
if (!all(c("ola_c","ola2_c") %in% names(df_lcga))) {
  df_lcga <- df_lcga |>
    dplyr::mutate(
      ola_c  = ola - mean(ola, na.rm = TRUE),
      ola2_c = ola_c^2
    )
}

# 1 clase (base)
m1_sq <- hlme(
  fixed     = c2 ~ ola_c + ola2_c,
  random    = ~ 1,
  subject   = "folio",  # <- numérico
  data      = df_lcga,
  na.action = 1
)

# 2-4 clases (pendiente y curvatura por clase)
m2_sq <- gridsearch(
  rep = 100, maxiter = 20, minit = m1_sq,
  m = hlme(
    fixed     = c2 ~ ola_c + ola2_c,
    mixture   = ~ ola_c + ola2_c,
    random    = ~ 1,
    subject   = "folio",
    ng        = 2,
    data      = df_lcga,
    nwg       = TRUE,
    na.action = 1
  )
)

m3_sq <- gridsearch(
  rep = 100, maxiter = 20, minit = m1_sq,
  m = hlme(
    fixed     = c2 ~ ola_c + ola2_c,
    mixture   = ~ ola_c + ola2_c,
    random    = ~ 1,
    subject   = "folio",
    ng        = 3,
    data      = df_lcga,
    nwg       = TRUE,
    na.action = 1
  )
)

m4_sq <- gridsearch(
  rep = 100, maxiter = 20, minit = m1_sq,
  m = hlme(
    fixed     = c2 ~ ola_c + ola2_c,
    mixture   = ~ ola_c + ola2_c,
    random    = ~ 1,
    subject   = "folio",
    ng        = 4,
    data      = df_lcga,
    nwg       = TRUE,
    na.action = 1
  )
)

m4_sq

# Comparación de cuadráticos
fit_tab_sq <- tibble::tibble(
  ng     = c(1, 2, 3, 4),
  loglik = c(m1_sq$loglik, m2_sq$loglik, m3_sq$loglik, m4_sq$loglik),
  AIC    = c(m1_sq$AIC,    m2_sq$AIC,    m3_sq$AIC,    m4_sq$AIC),
  BIC    = c(m1_sq$BIC,    m2_sq$BIC,    m3_sq$BIC,    m4_sq$BIC),
  conv   = c(m1_sq$conv,   m2_sq$conv,   m3_sq$conv,   m4_sq$conv)
)
print(fit_tab_sq)

# Tabla conjunta lineal vs cuadrático
summarytable(
  m1, m2, m3, m4,
  m1_sq, m2_sq, m3_sq, m4_sq,
  which = c("AIC","BIC","entropy","conv","loglik","npm","%class")
)

write_rds(m1_sq, "output/modelos/m1_sq_confianza.rds")
write_rds(m2_sq, "output/modelos/m2_sq_confianza.rds")
write_rds(m3_sq, "output/modelos/m3_sq_confianza.rds")
write_rds(m4_sq, "output/modelos/m4_sq_confianza.rds")



# ---- 5) Posteriores y asignación de clase (elige el mejor, p.ej. m3/m4) ----
best <- m4_sq  # <— cambia aquí según BIC/AIC
post <- postprob(best)
head(post)

# === Extraer clases por sujeto de forma simple (sin postprob) ===
pp <- best$pprob                       # matriz con id y prob_k
names(pp) <- tolower(names(pp))        # homogeneizar nombres


# Detectar columna id (hlme fue ajustado con subject = "id_num")
id_col <- dplyr::case_when(
  "subject" %in% names(pp) ~ "subject",
  "id_num" %in% names(pp)  ~ "id_num",
  "idnum"  %in% names(pp)  ~ "idnum",
  "id"     %in% names(pp)  ~ "id",
  TRUE ~ names(pp)[1] # fallback: primera columna
)

# Si no viene la columna 'class', la construimos con el argmax de las prob_k
if (!"class" %in% names(pp)) {
  prob_cols <- grep("^prob", names(pp), value = TRUE)
  pp$class <- max.col(as.matrix(pp[, prob_cols, drop = FALSE]), ties.method = "first")
}




# === 1) Calidad de asignación y tabla resumen por clase (fix pipe base) ===
pp <- best$pprob
names(pp) <- tolower(names(pp))
prob_cols <- grep("^prob", names(pp), value = TRUE)

# matriz de probabilidades fuera de mutate (evita usar '.')
prob_mat <- as.matrix(dplyr::select(pp, dplyr::all_of(prob_cols)))

pp <- pp |>
  dplyr::mutate(
    prob_max   = do.call(pmax, c(across(dplyr::all_of(prob_cols)), list(na.rm = TRUE))),
    hard_class = max.col(prob_mat, ties.method = "first")
  )

# Medias de prob posterior por clase
summary_pp <- pp |>
  dplyr::summarise(dplyr::across(dplyr::all_of(prob_cols), mean)) |>
  round(3)
summary_pp

# Porcentaje con prob_max > .7/.8/.9  (ya no dará NaN)
cutoffs <- c(.7, .8, .9)
data.frame(
  cutoff = cutoffs,
  pct    = sapply(cutoffs, function(c) mean(pp$prob_max > c) * 100)
)

# === 2) Asignación dura y unión a los datos ===
# (si vienes del bloque anterior, pp/prob_cols/prob_mat ya existen; si no, se recrean)
if (!exists("pp")) {
  pp <- best$pprob
  names(pp) <- tolower(names(pp))
}
if (!exists("prob_cols")) prob_cols <- grep("^prob", names(pp), value = TRUE)
if (!exists("prob_mat"))  prob_mat  <- as.matrix(dplyr::select(pp, dplyr::all_of(prob_cols)))

pp <- pp |>
  dplyr::mutate(
    prob_max   = do.call(pmax, c(across(dplyr::all_of(prob_cols)), list(na.rm = TRUE))),
    hard_class = max.col(prob_mat, ties.method = "first")
  )

# Detectar nombre de columna id que trae lcmm en pprob
id_col <- dplyr::case_when(
  "subject" %in% names(pp) ~ "subject",
  "id_num"  %in% names(pp) ~ "id_num",
  "id"      %in% names(pp) ~ "id",
  TRUE ~ names(pp)[1]
)

# Tabla de tamaños por clase dura
tab_clases <- pp |>
  dplyr::count(hard_class, name = "n")
tab_clases

# Unir clase a df_lcga
df_classes <- df_lcga |>
  dplyr::left_join(pp |>
                     dplyr::select(!!rlang::sym(id_col), hard_class, prob_max),
                   by = c("folio" = id_col))

dplyr::count(df_classes, hard_class)

# === 3) Calidad de clasificación (resumen) ===
# Medias de prob posterior por clase (como en el print de lcmm)
summary_pp <- pp |>
  dplyr::summarise(dplyr::across(dplyr::all_of(prob_cols), ~mean(.x, na.rm = TRUE))) |>
  round(3)
summary_pp  # columnas prob1, prob2, prob3

# Porcentaje con prob_max > .7/.8/.9
cutoffs <- c(.7, .8, .9)
pct_por_umbral <- data.frame(
  cutoff = cutoffs,
  pct    = sapply(cutoffs, function(c) mean(pp$prob_max > c, na.rm = TRUE) * 100)
)
pct_por_umbral

# Entropía normalizada y R^2 de clasificación (1 - entropía media)
entropy_row <- -rowSums(prob_mat * log(pmax(prob_mat, 1e-12)), na.rm = TRUE) / log(ncol(prob_mat))
classif_R2  <- 1 - mean(entropy_row, na.rm = TRUE)
classif_R2


# === 4) Trayectorias promedio por clase (líneas suaves) ===
suppressPackageStartupMessages(library(ggplot2))
traj_mean <- df_classes |>
  dplyr::group_by(hard_class, ola) |>
  dplyr::summarise(
    n    = dplyr::n(),
    mean = mean(c2, na.rm = TRUE),
    se   = sd(c2, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# Etiquetas prolijas
labs_clase <- paste0("Clase ", traj_mean$hard_class |> unique())
names(labs_clase) <- sort(unique(traj_mean$hard_class))

ggplot(traj_mean, aes(x = ola, y = mean, group = hard_class)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), alpha = 0.15) +
  scale_x_continuous(breaks = sort(unique(traj_mean$ola))) +
  scale_y_continuous(limits = c(1, 5)) +
  facet_wrap(~ hard_class, labeller = labeller(hard_class = labs_clase)) +
  labs(x = "Ola (orden num.)", y = "Apoyo al uso de armas", 
       title = "Trayectorias promedio por clase (IC95%)") +
  theme_minimal(base_size = 12)
