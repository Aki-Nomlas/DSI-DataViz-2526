# transit_costs_beeswarm.R
# An annotated beeswarm of urban-rail construction costs.
# Data: NYU Marron Transit Costs Project, 2026 update.
#       https://ultraviolet.library.nyu.edu/records/9wnjp-kez15
#
# Run from the same directory as tcp_merged.csv, or change `data_path` below.

# ---- 0. PACKAGES -----------------------------------------------------------
# install.packages(c("ggbeeswarm", "ggrepel", "scales","countrycode"))

library(tidyverse)
library(ggbeeswarm)   # geom_quasirandom — better than geom_jitter for this
library(ggrepel)      # collision-free label placement
library(scales)       # log breaks, dollar formatting
library(countrycode)  # ISO2 -> full country names

data_path <- "tcp_merged.csv"
out_dir   <- "."

# ---- 1. LOAD & RENAME ------------------------------------------------------
tcp_raw <- read_csv(data_path, show_col_types = FALSE)

tcp <- tcp_raw %>%
  rename(
    cost_km    = Cost_km_2023_dollars,  # main y variable, PPP+inflation adjusted
    length_km  = Length,
    iso2       = Country,
    start_year = Start_year,
    end_year   = End_year,
    pct_tunnel = TunnelPer,
    stations   = Stations
  ) %>%
  mutate(
    # countrycode handles most ISO-2 codes; patch a few that the dataset
    # encodes differently (UK vs GB, etc.)
    country_name = countrycode(iso2, "iso2c", "country.name", warn = FALSE),
    country_name = case_when(
      iso2 == "UK" ~ "United Kingdom",
      iso2 == "TW" ~ "Taiwan",
      iso2 == "HK" ~ "Hong Kong",
      iso2 == "DR" ~ "Dominican Republic",
      TRUE         ~ country_name
    )
  )

# ---- 2. LEGAL-TRADITION JOIN -----------------------------------------------
# Source: JuriGlobe research group, University of Ottawa Faculty of Law.
# https://www.juriglobe.ca/eng/syst-onu/index-alpha.php
#
# JuriGlobe's five top-level categories, used here as-is:
#   Civil law / Common law / Mixed / Muslim law / Customary law
#
# The lookup CSV also carries `mixed_components` so the breakdown of any
# Mixed jurisdiction (e.g. India = Common+Muslim+Customary) is auditable.

legal_lookup <- read_csv("juriglobe_lookup.csv", show_col_types = FALSE)

tcp <- tcp %>%
  left_join(legal_lookup %>% select(iso2, juriglobe_class, mixed_components),
            by = "iso2") %>%
  rename(legal_tradition = juriglobe_class) %>%
  mutate(
    legal_tradition = replace_na(legal_tradition, "Unclassified"),
    legal_tradition = factor(
      legal_tradition,
      levels = c("Common law", "Civil law", "Mixed",
                 "Muslim law", "Customary law", "Unclassified")
    )
  )

# ---- 3. FILTER & ORDER -----------------------------------------------------
MIN_PROJECTS <- 3  # only show countries with at least this many phases.
# Set to 3 (not 5) so the UK (3 London projects) and
# Australia (4) make it in — both crucial to the
# common-law narrative.

plot_data <- tcp %>%
  filter(!is.na(cost_km), !is.na(country_name)) %>%
  group_by(country_name) %>%
  filter(n() >= MIN_PROJECTS) %>%
  mutate(country_label = paste0(country_name, "  (n=", n(), ")")) %>%
  ungroup() %>%
  mutate(
    country_label = fct_reorder(country_label, cost_km,
                                .fun = median, .desc = FALSE)
  )

cat("Rows in plot:", nrow(plot_data), "\n")
cat("Countries:   ", n_distinct(plot_data$country_name), "\n\n")
print(plot_data %>% count(country_name, legal_tradition, sort = TRUE))

# ---- 4. PICK OUTLIERS TO ANNOTATE ------------------------------------------
# Hand-picked projects that anchor the story: a few "wild expensive" markers,
# plus a couple of "look how cheap" counter-examples to make the contrast.
to_label <- plot_data %>%
  mutate(
    label_text = case_when(
      Line == "East Side Access"          ~ "East Side Access\n(LIRR → Grand Central)",
      str_detect(Phase, "Second Avenue Phase 1") ~ "Second Ave Subway, Phase 1",
      str_detect(Phase, "Second Avenue Phase 2") ~ "Second Ave Subway, Phase 2",
      Line == "Line 7" & City == "New York"      ~ "7 Line extension",
      str_detect(Line, "Tung Chung")             ~ "Tung Chung Line ext.\n(Hong Kong)",
      str_detect(Line, "Sha Tin")                ~ "Sha Tin–Central Link",
      str_detect(Line, "Crossrail|Elizabeth")    ~ "Crossrail / Elizabeth Line",
      City == "Madrid" & str_detect(Line, "L9|Line 9|MetroSur") ~ "Madrid Line 9 / MetroSur",
      City == "Seoul"  & str_detect(Line, "^9$|Line 9")         ~ "Seoul Line 9",
      str_detect(Line, "Marmaray")               ~ "Istanbul Marmaray",
      City == "Vancouver" & str_detect(Line, "Broadway|Millennium") ~ "Vancouver Broadway",
      City == "Cairo"  & cost_km > 800           ~ paste0("Cairo ", Line),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label_text)) %>%
  distinct(City, Line, Phase, .keep_all = TRUE)

# ---- 5. PLOT ---------------------------------------------------------------
palette <- c(
  "Common law"    = "#d62828",  # red
  "Civil law"     = "#1f77b4",  # blue
  "Mixed"         = "#9467bd",  # purple
  "Muslim law"    = "#2ca02c",  # green
  "Customary law" = "#8c564b",  # brown (no countries in our data, kept for completeness)
  "Unclassified"  = "#bbbbbb"
)

p <- ggplot(plot_data, aes(x = country_label, y = cost_km)) +
  # faint horizontal reference at the global median, for orientation
  geom_hline(yintercept = median(plot_data$cost_km),
             linetype = "dashed", color = "grey70", linewidth = 0.3) +
  geom_quasirandom(
    aes(color = legal_tradition, size = length_km),
    width  = 0.38,
    alpha  = 0.65,
    method = "quasirandom"
  ) +
  geom_text_repel(
    data          = to_label,
    aes(label = label_text),
    size          = 3,
    fontface      = "italic",
    lineheight    = 0.9,
    box.padding   = 0.7,
    point.padding = 0.3,
    min.segment.length = 0,
    segment.color = "grey50",
    max.overlaps  = Inf
  ) +
  scale_y_log10(
    labels = label_dollar(suffix = "M", accuracy = 1),
    breaks = c(20, 50, 100, 200, 500, 1000, 2000, 5000)
  ) +
  scale_size(range = c(0.4, 4.5), guide = "none") +
  scale_color_manual(values = palette, drop = FALSE) +  # keep all 5 in legend
  coord_flip() +  # countries on y-axis reads better with long names
  labs(
    title    = "How much does a kilometer of subway cost?",
    subtitle = "Each dot is one urban-rail project phase. Horizontal position is cost per km (log scale, 2023 USD).\nColor encodes legal tradition; dot size is project length.",
    x        = NULL,
    y        = "Cost per kilometer (2023 USD)",
    color    = "Legal tradition",
    caption  = paste(
      "Data: Levy, Goldwyn, Ensari & Chitti (2025), NYU Marron Transit Costs Project (DOI: 10.58153/9wnjp-kez15).",
      "Legal tradition per JuriGlobe, University of Ottawa (juriglobe.ca). Countries with ≥3 projects shown.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "grey30", margin = margin(b = 12)),
    plot.caption       = element_text(color = "grey50"),
    legend.position    = "top",
    axis.text.y        = element_text(size = 10)
  )

p

# ---- 6. SAVE ---------------------------------------------------------------
ggsave(file.path(out_dir, "beeswarm_v1.pdf"),
       p, width = 13, height = 14, device = cairo_pdf)
ggsave(file.path(out_dir, "beeswarm_v1.png"),
       p, width = 13, height = 14, dpi = 220, bg = "white")

cat("\nWrote beeswarm_v1.pdf and beeswarm_v1.png to", out_dir, "\n")