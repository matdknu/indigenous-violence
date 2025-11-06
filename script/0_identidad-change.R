# =============================================================================
# Identity dynamics (indi vs no_indi) across 4 waves
# - Loads ELRI LONG
# - Builds indi/no_indi variable
# - Keeps only folios observed in waves 1:4
# - Creates sequence patterns per folio
# - Classifies patterns into 5 categories
# - Summaries + Sankey + "nodes & lines" plot
# =============================================================================

# --- Packages ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, stringr, purrr, here, haven,
  ggplot2, scales, networkD3, htmlwidgets, knitr
)

# --- I/O ---------------------------------------------------------------------
IN_FILE  <- here::here("data", "BBDD_ELRI_LONG.RData")
OUT_DIR  <- here::here("output", "images")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# --- Load data ---------------------------------------------------------------
load(IN_FILE)
stopifnot(exists("BBDD_ELRI_LONG"))
data <- BBDD_ELRI_LONG

# --- Helper: zap haven labels to numeric safely ------------------------------
zap_num <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

# --- Build indi / no_indi from a1 (handle special codes) ---------------------
a1_num <- zap_num(data$a1)
a1_num[a1_num %in% c(88, 99, 8888, 9999)] <- NA

data <- data %>%
  mutate(
    indi_raw = dplyr::case_when(
      a1_num %in% 1:11 ~ "indi",
      a1_num == 12     ~ "no_indi",
      TRUE             ~ NA_character_
    )
  )

# Make indi invariant within folio (mode within folio, fallback to global mode)
mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}
indi_global_mode <- mode_chr(data$indi_raw)

data <- data %>%
  group_by(folio) %>%
  mutate(indi = ifelse(is.na(indi_raw), mode_chr(indi_raw), indi_raw)) %>%
  ungroup() %>%
  mutate(indi = ifelse(is.na(indi), indi_global_mode, indi),
         indi = factor(indi, levels = c("no_indi","indi")))

# --- Keep only folios with waves 1..4 present --------------------------------
data <- data %>%
  mutate(ola = zap_num(ola)) %>%
  filter(!is.na(folio), !is.na(ola)) %>%
  mutate(ola = as.integer(ola))

complete_ids <- data %>%
  filter(ola %in% 1:4) %>%
  group_by(folio) %>%
  summarise(ok = identical(sort(unique(ola)), 1:4), .groups = "drop") %>%
  filter(ok) %>% pull(folio)

df_seq <- data %>%
  filter(folio %in% complete_ids, ola %in% 1:4) %>%
  transmute(
    folio,
    ola  = as.integer(ola),
    indi = as.character(indi)
  ) %>%
  distinct(folio, ola, indi) %>%
  arrange(folio, ola)

# --- Build per-folio pattern: "indi-indi-no_indi-indi" -----------------------
patterns <- df_seq %>%
  group_by(folio) %>%
  summarise(
    pattern   = paste(indi[order(ola)], collapse = "-"),
    first_ind = first(indi[order(ola)]),
    last_ind  = last(indi[order(ola)]),
    n_states  = n_distinct(indi),
    .groups = "drop"
  )

# --- Top patterns (like your table) ------------------------------------------
pat_freq <- patterns %>%
  count(pattern, sort = TRUE, name = "n") %>%
  mutate(pct = n / sum(n)) %>%
  slice_head(n = 15)

# --- Classifier into 5 categories -------------------------------------------
clasificar_patron <- function(pat_str) {
  tokens <- str_split(pat_str, "-", simplify = TRUE)
  tokens <- as.character(tokens[1, ])
  first  <- tokens[1]
  last   <- tokens[length(tokens)]
  runs   <- length(rle(tokens)$values)     # number of runs (segments)
  uniq   <- length(unique(tokens))
  
  case_when(
    uniq == 1 & first == "indi"                        ~ "identidad indigena estable",
    uniq == 1 & first == "no_indi"                     ~ "identidad no indigena estable",
    first == "indi"    & last == "no_indi" & runs == 2 ~ "identidad indigena hacia no indígena",
    first == "no_indi" & last == "indi"    & runs == 2 ~ "identidad no indigena hacia indígena",
    TRUE                                               ~ "identidad inestable"
  )
}

df_cat <- pat_freq %>%
  mutate(categoria = map_chr(pattern, clasificar_patron))

resumen_cat <- patterns %>%
  mutate(categoria = map_chr(pattern, clasificar_patron)) %>%
  count(categoria, name = "N", sort = TRUE) %>%
  mutate(Pct = N / sum(N))

# --- Print summaries in console ----------------------------------------------
cat("\n# === Top 15 patterns ===\n")
print(knitr::kable(df_cat, digits = 3))

cat("\n# === Category summary (5 types) ===\n")
print(knitr::kable(resumen_cat, digits = 3))



