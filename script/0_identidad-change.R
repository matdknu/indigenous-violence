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

# --- Sankey data (W1→W2→W3→W4) ----------------------------------------------
mk_links <- function(df, w_from, w_to) {
  df %>%
    filter(ola %in% c(w_from, w_to)) %>%
    distinct(folio, ola, indi) %>%                # avoid duplicates
    tidyr::pivot_wider(names_from = ola, values_from = indi) %>%
    count(`{w_from}` := .data[[as.character(w_from)]],
          `{w_to}`   := .data[[as.character(w_to)]],
          name = "value") %>%
    rename(source = `:=`, target = `:=`) %>%      # patch names
    rename(source = !!as.name(as.character(w_from)),
           target = !!as.name(as.character(w_to))) %>%
    mutate(
      source = paste0("W", w_from, ":", source),
      target = paste0("W", w_to,   ":", target)
    )
}
links12 <- mk_links(df_seq, 1, 2)
links23 <- mk_links(df_seq, 2, 3)
links34 <- mk_links(df_seq, 3, 4)
links_all <- bind_rows(links12, links23, links34)

nodes <- data.frame(name = sort(unique(c(links_all$source, links_all$target))))
links_mapped <- links_all %>%
  mutate(
    source_id = match(source, nodes$name) - 1,
    target_id = match(target, nodes$name) - 1
  ) %>%
  select(source_id, target_id, value)

# --- Save Sankey as HTML ------------------------------------------------------
sank <- sankeyNetwork(
  Links = as.data.frame(links_mapped), Nodes = nodes,
  Source = "source_id", Target = "target_id",
  Value = "value", NodeID = "name",
  fontSize = 12, nodeWidth = 30, sinksRight = FALSE
)
htmlwidgets::saveWidget(sank, file = here::here("output", "sankey_identity.html"), selfcontained = TRUE)
cat("\nSaved: output/sankey_identity.html\n")

# --- “Nodes of permanence + lines of change” plot -----------------------------
# Node totals per wave/state
nodes_counts <- df_seq %>%
  count(ola, indi, name = "N") %>%
  mutate(
    x = ola,
    y = ifelse(indi == "indi", 1, 0)
  )

# Transitions counts with change flag
count_trans <- function(df, w_from, w_to) {
  df %>%
    filter(ola %in% c(w_from, w_to)) %>%
    distinct(folio, ola, indi) %>%
    tidyr::pivot_wider(names_from = ola, values_from = indi) %>%
    count(`{w_from}` := .data[[as.character(w_from)]],
          `{w_to}`   := .data[[as.character(w_to)]],
          name = "n") %>%
    rename(from = `:=`, to = `:=`) %>%
    rename(from = !!as.name(as.character(w_from)),
           to   = !!as.name(as.character(w_to))) %>%
    mutate(
      wave_from = w_from,
      wave_to   = w_to,
      change    = ifelse(from == to, "permanencia", "cambio")
    )
}
L12 <- count_trans(df_seq, 1, 2)
L23 <- count_trans(df_seq, 2, 3)
L34 <- count_trans(df_seq, 3, 4)
edges <- bind_rows(L12, L23, L34) %>%
  mutate(
    x    = wave_from,
    xend = wave_to,
    y    = ifelse(from == "indi", 1, 0),
    yend = ifelse(to   == "indi", 1, 0)
  )


p_change <- ggplot() +
  geom_curve(
    data = edges,
    aes(x = x, y = y, xend = xend, yend = yend, size = n, linetype = change, color = change),
    curvature = 0.15, alpha = 0.6
  ) +
  geom_point(
    data = nodes_counts,
    aes(x = x, y = y, size = N),
    shape = 21, fill = "white", stroke = 1.1
  ) +
  geom_text(
    data = nodes_counts,
    aes(x = x, y = y, label = paste0(indi, "\nN=", N)),
    vjust = -1.1, fontface = "bold"
  ) +
  scale_x_continuous(breaks = 1:4, labels = paste0("W", 1:4), expand = expansion(add = 0.2)) +
  scale_y_continuous(breaks = c(0,1), labels = c("no_indi", "indi")) +
  scale_size_continuous(range = c(0.5, 3.5), guide = "none") +
  scale_color_manual(values = c("permanencia" = "#1f2041", "cambio" = "#ffc857")) +
  scale_linetype_manual(values = c("permanencia" = "solid", "cambio" = "11")) +
  labs(
    x = NULL, y = NULL,
    title = "Transiciones de identidad indígena (W1→W4)",
    subtitle = "Nodos = recuentos por estado y ola; Curvas = flujos entre olas; color/estilo = cambio vs permanencia",
    caption = "Fuente: ELRI LONG. Nodos escalados por N (estado×ola); curvas escaladas por conteos de transición."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

outfile_png <- file.path(OUT_DIR, "transiciones_identidad_nodes_lines.png")
ggsave(outfile_png, p_change, width = 11, height = 6.5, dpi = 300)
cat("Saved:", outfile_png, "\n")

# --- Optional: Category totals table (CSV) -----------------------------------
readr::write_csv(resumen_cat, file.path(OUT_DIR, "resumen_categorias_identidad.csv"))
cat("Saved:", file.path(OUT_DIR, "resumen_categorias_identidad.csv"), "\n")

# --- Done --------------------------------------------------------------------
cat("\nAll done ✅\n")


