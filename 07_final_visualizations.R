# ============================================================
# Transit Equity in Queens -- Final Visualizations
# ============================================================
# Self-contained script that produces the four headline visuals:
#
#   1. Walk-time to nearest subway station (block-group choropleth)
#   2. Trifecta scatter (income, zero-vehicle %, walk-time)
#   3. Transit-quality tiers (subway / SBS / local bus / none)
#   4. High-need block groups served only by SBS or local bus
#
# Setup (one-time):
#   install.packages(c("tidycensus", "sf", "tidyverse", "tidytransit"))
#   file.edit("~/.Renviron")  # add: CENSUS_API_KEY=your_key
#                              # then restart R
#
# ============================================================

library(tidycensus)
library(sf)
library(tidyverse)
library(tidytransit)


stopifnot(nzchar(Sys.getenv("CENSUS_API_KEY")))

# --- Configuration ---------------------------------------------------
ACS_YEAR             <- 2023
TARGET_CRS           <- 2263       # NY State Plane Long Island, US ft
WALK_SPEED_MPH       <- 3
TIER_WALK_MIN        <- 10         # 10-min walk = ~0.5 mi
RESIDENTIAL_MIN_HH   <- 50         # drop non-residential block groups
HIGH_NEED_PERCENTILE <- 0.75       # top-quartile dependency cutoff
out_dir              <- "."        # save plots here

# ============================================================
# DATA
# ============================================================

# --- 1. ACS: income + vehicle availability --------------------------
queens_data <- get_acs(
  geography = "block group",
  variables = c(
    median_income     = "B19013_001",
    total_hh          = "B25044_001",
    owner_no_vehicle  = "B25044_003",
    renter_no_vehicle = "B25044_010"
  ),
  state    = "NY",
  county   = "Queens",
  year     = ACS_YEAR,
  geometry = TRUE,
  output   = "wide"
) |>
  mutate(
    pct_zero_vehicle = if_else(
      total_hhE > 0,
      100 * (owner_no_vehicleE + renter_no_vehicleE) / total_hhE,
      NA_real_
    ),
    median_income_clean = if_else(median_incomeE > 0,
                                  median_incomeE, NA_real_)
  ) |>
  st_transform(TARGET_CRS)

# --- 2. Subway lines (data.ny.gov) + stations (MTA GTFS) ------------
subway <- st_read(
  "https://data.ny.gov/api/geospatial/s692-irgq?method=export&format=GeoJSON",
  quiet = TRUE
) |> st_transform(TARGET_CRS)

# Clip transit lines to the actual Queens outline (not just a bounding
# box) so that subway and SBS segments running through Brooklyn,
# Manhattan, or the Bronx don't appear in the frame.
queens_outline <- st_union(queens_data) |> st_make_valid()
subway_queens  <- st_intersection(subway, queens_outline)

cat("Loading subway GTFS...\n")
subway_gtfs <- read_gtfs(
  "http://web.mta.info/developers/data/nyct/subway/google_transit.zip"
)
stations <- subway_gtfs$stops |>
  filter(!is.na(stop_lat), !is.na(stop_lon),
         stop_lat != 0, stop_lon != 0) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(TARGET_CRS)

# --- 3. Bus GTFS feeds and SBS extraction ---------------------------
cat("Loading NYCT Queens bus GTFS...\n")
gtfs_q <- read_gtfs("http://web.mta.info/developers/data/nyct/bus/google_transit_queens.zip")
cat("Loading MTA Bus Company GTFS...\n")
gtfs_b <- read_gtfs("http://web.mta.info/developers/data/busco/google_transit.zip")

gtfs_q <- gtfs_as_sf(gtfs_q)
gtfs_b <- gtfs_as_sf(gtfs_b)

# Identify SBS routes -- naming varies across feeds (Q44+SBS / Q44+ /
# Q44-SBS), so we use a flexible matcher and restrict to "Q" routes.
clean_sbs_name <- function(x) {
  x <- str_replace(x, regex("[-+]?SBS$", ignore_case = TRUE), "")
  gsub("+", "", x, fixed = TRUE)
}

find_sbs_routes <- function(gtfs_obj, route_regex = "^Q") {
  gtfs_obj$routes |>
    mutate(
      .route_clean = clean_sbs_name(route_short_name),
      .sbs_text    = paste(route_id, route_short_name,
                           route_long_name, route_desc)
    ) |>
    filter(
      endsWith(route_id, "+") |
        grepl("SBS|Select Bus Service", .sbs_text, ignore.case = TRUE)
    ) |>
    filter(str_detect(.route_clean, route_regex)) |>
    select(-.route_clean, -.sbs_text)
}

sbs_routes_q <- find_sbs_routes(gtfs_q)
sbs_routes_b <- find_sbs_routes(gtfs_b)

# Build SBS shapes sf
build_sbs_sf <- function(gtfs_obj, sbs_routes) {
  if (nrow(sbs_routes) == 0) return(NULL)
  shape_route_map <- gtfs_obj$trips |>
    filter(route_id %in% sbs_routes$route_id) |>
    distinct(shape_id, route_id) |>
    left_join(sbs_routes |> select(route_id, route_short_name,
                                   route_long_name),
              by = "route_id")
  gtfs_obj$shapes |>
    inner_join(shape_route_map, by = "shape_id")
}

sbs_q_sf <- build_sbs_sf(gtfs_q, sbs_routes_q)
sbs_b_sf <- build_sbs_sf(gtfs_b, sbs_routes_b)

sbs_parts <- Filter(function(x) !is.null(x) && nrow(x) > 0,
                    list(sbs_q_sf, sbs_b_sf))
sbs_all <- do.call(rbind, sbs_parts) |>
  st_transform(TARGET_CRS) |>
  mutate(
    route_clean  = clean_sbs_name(route_short_name),
    is_woodhaven = route_clean %in% c("Q52", "Q53")
  )

# Pick the longest shape per route for plotting (each route has
# multiple direction-variants; one shape per route reads cleaner).
sbs_drawn <- sbs_all |>
  mutate(len = as.numeric(st_length(geometry))) |>
  group_by(route_clean) |>
  slice_max(len, n = 1, with_ties = FALSE) |>
  ungroup()

# Clip SBS to Queens outline (same reason as subway above)
sbs_drawn <- st_intersection(sbs_drawn, queens_outline)

# --- 4. Bus stops sf (defensive helper) -----------------------------
stops_to_sf <- function(stops_tbl, crs = TARGET_CRS) {
  if (is.null(stops_tbl) || nrow(stops_tbl) == 0) {
    return(st_sf(stop_id = character(), geometry = st_sfc(crs = crs)))
  }
  if (inherits(stops_tbl, "sf")) {
    valid <- stops_tbl[!st_is_empty(st_geometry(stops_tbl)), ]
    if (is.na(st_crs(valid))) st_crs(valid) <- 4326
    return(st_transform(valid, crs))
  }
  stops_tbl |>
    filter(!is.na(stop_lat), !is.na(stop_lon),
           stop_lat != 0, stop_lon != 0) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    st_transform(crs)
}

get_sbs_stops <- function(gtfs_obj, sbs_routes) {
  if (nrow(sbs_routes) == 0) return(stops_to_sf(NULL))
  trip_ids <- gtfs_obj$trips |>
    filter(route_id %in% sbs_routes$route_id) |>
    pull(trip_id)
  stop_ids <- gtfs_obj$stop_times |>
    filter(trip_id %in% trip_ids) |>
    distinct(stop_id) |>
    pull(stop_id)
  gtfs_obj$stops |>
    filter(stop_id %in% stop_ids) |>
    stops_to_sf()
}

sbs_stops <- bind_rows(
  get_sbs_stops(gtfs_q, sbs_routes_q),
  get_sbs_stops(gtfs_b, sbs_routes_b)
)
all_bus_stops <- bind_rows(
  stops_to_sf(gtfs_q$stops),
  stops_to_sf(gtfs_b$stops)
)

# ============================================================
# DERIVED METRICS
# ============================================================

# --- 5. Walk-minutes from each block group to each mode -------------
# point_on_surface > centroid for irregular polygons (Rockaways etc.)
block_group_points <- st_point_on_surface(queens_data)

walk_minutes_to <- function(origins, destinations,
                            speed_mph = WALK_SPEED_MPH) {
  if (nrow(destinations) == 0) return(rep(Inf, nrow(origins)))
  idx  <- st_nearest_feature(origins, destinations)
  d_ft <- as.numeric(st_distance(origins, destinations[idx, ],
                                 by_element = TRUE))
  d_ft / 5280 / speed_mph * 60
}

queens_data <- queens_data |>
  mutate(
    walk_subway   = walk_minutes_to(block_group_points, stations),
    walk_sbs_stop = walk_minutes_to(block_group_points, sbs_stops),
    walk_any_bus  = walk_minutes_to(block_group_points, all_bus_stops),
    walk_bin = cut(walk_subway,
                   breaks = c(0, 5, 10, 15, 25, 45, Inf),
                   labels = c("<5 min", "5-10", "10-15",
                              "15-25", "25-45", "45+ min"),
                   include.lowest = TRUE)
  )

# --- 6. Transit dependency index + tier classification --------------
score_high <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(NA_real_, length(x)))
  scales::rescale(x, to = c(0, 100), from = rng)
}
score_low <- function(x) 100 - score_high(x)

queens_data <- queens_data |>
  mutate(
    is_residential = !is.na(total_hhE) & total_hhE >= RESIDENTIAL_MIN_HH,
    zero_vehicle_score = score_high(pct_zero_vehicle),
    low_income_score   = score_low(median_income_clean),
    transit_dependency_index = rowMeans(
      cbind(zero_vehicle_score, low_income_score), na.rm = TRUE
    ),
    transit_dependency_index = if_else(is.nan(transit_dependency_index),
                                       NA_real_,
                                       transit_dependency_index),
    transit_tier = case_when(
      !is_residential                ~ NA_character_,
      walk_subway   <= TIER_WALK_MIN ~ "Tier 1 -- Subway",
      walk_sbs_stop <= TIER_WALK_MIN ~ "Tier 2 -- SBS only",
      walk_any_bus  <= TIER_WALK_MIN ~ "Tier 3 -- Local bus only",
      TRUE                           ~ "Tier 4 -- None"
    ),
    transit_tier = factor(
      transit_tier,
      levels = c("Tier 1 -- Subway",
                 "Tier 2 -- SBS only",
                 "Tier 3 -- Local bus only",
                 "Tier 4 -- None")
    )
  )

high_need_cut <- quantile(queens_data$transit_dependency_index,
                          HIGH_NEED_PERCENTILE, na.rm = TRUE)

# ============================================================
# THE FOUR VISUALIZATIONS
# ============================================================

# --- Visual 1: Walk-time-to-subway choropleth ------------------------
walk_palette <- c("<5 min"  = "#fff5e6",
                  "5-10"    = "#fdd0a2",
                  "10-15"   = "#fdae6b",
                  "15-25"   = "#e6550d",
                  "25-45"   = "#a63603",
                  "45+ min" = "#3f0a01")

p1_walk <- ggplot() +
  geom_sf(data = queens_data,
          aes(fill = walk_bin),
          color = "white", linewidth = 0.03) +
  geom_sf(data = subway_queens, color = "black", linewidth = 0.45) +
  scale_fill_manual(
    values   = walk_palette,
    name     = "Walk minutes\nto nearest station",
    na.value = "grey90"
  ) +
  labs(
    title    = "Most of Queens is more than 15 minutes' walk from a subway",
    subtitle = "Straight-line walking time at 3 mph from each block group to the nearest MTA subway station",
    caption  = "Sources: U.S. Census Bureau ACS 2019-2023, MTA static GTFS"
  ) +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(size = 10, color = "grey30"),
        plot.caption    = element_text(size = 8,  color = "grey60"),
        legend.position = c(0.10, 0.30))

ggsave(file.path(out_dir, "01_walk_time_to_subway.png"),
       p1_walk, width = 10, height = 8, dpi = 300, bg = "white")

# --- Visual 2: Trifecta scatter --------------------------------------
queens_df <- queens_data |>
  st_drop_geometry() |>
  filter(!is.na(median_income_clean),
         !is.na(pct_zero_vehicle),
         !is.na(walk_subway))

p2_trifecta <- ggplot(queens_df,
                      aes(x = median_income_clean,
                          y = pct_zero_vehicle,
                          color = walk_subway)) +
  geom_point(alpha = 0.55, size = 1.3) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3,
                                                    suffix = "k")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_color_viridis_c(
    option = "rocket", direction = -1,
    name   = "Walk min to\nnearest subway",
    limits = c(0, 60), oob = scales::squish,
    breaks = c(0, 15, 30, 45, 60),
    labels = c("0", "15", "30", "45", "60+")
  ) +
  labs(
    title    = "Income, transit dependency, and access -- the three-way picture",
    subtitle = "Bottom-left = forced car ownership: low income, low zero-vehicle %, long walk to subway",
    x        = "Median household income",
    y        = "% zero-vehicle households",
    caption  = "Each point = one Queens block group. ACS 2019-2023, MTA"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(color = "grey40"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "02_trifecta_scatter.png"),
       p2_trifecta, width = 9, height = 6.5, dpi = 300, bg = "white")

# --- Visual 3: Transit-quality tier map ------------------------------
# Palette: warm-progression for Tiers 1-3 with a distinct hue for
# Tier 4 (muted violet). Avoids the previous teal-vs-navy collision.
tier_palette <- c(
  "Tier 1 -- Subway"         = "#2a9d8f",  # teal-green
  "Tier 2 -- SBS only"       = "#e9c46a",  # mustard
  "Tier 3 -- Local bus only" = "#e76f51",  # terracotta
  "Tier 4 -- None"           = "#5e548e"   # muted violet (distinct hue)
)

p3_tier <- ggplot() +
  geom_sf(data = queens_data,
          aes(fill = transit_tier),
          color = "white", linewidth = 0.03) +
  scale_fill_manual(
    values   = tier_palette,
    name     = "Best transit\nwithin 10-min walk",
    na.value = "grey92",
    drop     = FALSE
  ) +
  # Subway in black solid; SBS in magenta solid for high contrast
  # against every tier color (and the grey NA fill).
  geom_sf(data = subway_queens, color = "black",   linewidth = 0.5) +
  geom_sf(data = sbs_drawn,     color = "#d6336c", linewidth = 0.8,
          alpha = 0.95) +
  labs(
    title    = "Transit-quality tiers: what's the best mode you can walk to?",
    subtitle = "Subway lines in black, SBS routes in magenta",
    caption  = "Sources: U.S. Census Bureau ACS, MTA static GTFS"
  ) +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(size = 10, color = "grey30"),
        plot.caption    = element_text(size = 8,  color = "grey60"),
        legend.position = "bottom")

ggsave(file.path(out_dir, "03_transit_quality_tiers.png"),
       p3_tier, width = 11, height = 8.5, dpi = 300, bg = "white")

# --- Visual 4: High-need block groups without subway access ----------
no_subway_high_need <- queens_data |>
  filter(transit_tier %in% c("Tier 2 -- SBS only",
                             "Tier 3 -- Local bus only"),
         transit_dependency_index >= high_need_cut)

p4_focus <- ggplot() +
  geom_sf(data = queens_data,
          fill = "grey92", color = "white", linewidth = 0.03) +
  geom_sf(data = subway_queens, color = "black",   linewidth = 0.5) +
  # SBS in a muted blue here so it doesn't fight the bright red
  # high-need polygons (which carry the visual attention).
  geom_sf(data = sbs_drawn,     color = "#1f78b4", linewidth = 0.7,
          alpha = 0.85) +
  geom_sf(data = no_subway_high_need,
          fill = "#d7301f", color = "white", linewidth = 0.05,
          alpha = 0.95) +
  labs(
    title    = "High-need block groups served only by SBS or local bus",
    subtitle = paste0("Top-quartile transit dependency, no subway within ",
                      TIER_WALK_MIN, "-min walk -- ",
                      nrow(no_subway_high_need), " block groups"),
    caption  = "Sources: U.S. Census Bureau ACS, MTA static GTFS"
  ) +
  theme_void() +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "grey30"),
        plot.caption  = element_text(size = 8,  color = "grey60"))

ggsave(file.path(out_dir, "04_high_need_no_subway.png"),
       p4_focus, width = 10, height = 8.5, dpi = 300, bg = "white")

# --- Display + summary ----------------------------------------------
print(p1_walk)
print(p2_trifecta)
print(p3_tier)
print(p4_focus)

cat("\n========================================\n")
cat("HEADLINE NUMBERS FOR THE WRITE-UP\n")
cat("========================================\n")
cat("Block groups analyzed (residential): ",
    sum(queens_data$is_residential, na.rm = TRUE), "\n")
cat("Median walk to nearest subway:       ",
    round(median(queens_data$walk_subway, na.rm = TRUE), 1), " min\n")
cat("Block groups > 15 min from subway:   ",
    sum(queens_data$walk_subway > 15, na.rm = TRUE), " of ",
    nrow(queens_data), "\n")
cat("Top-quartile dependency cutoff:      ",
    round(high_need_cut, 1), " (on 0-100 index)\n")
cat("High-need + no-subway block groups:  ",
    nrow(no_subway_high_need), "\n\n")

cat("Tier distribution:\n")
print(table(queens_data$transit_tier, useNA = "ifany"))

cat("\nPlots saved to: ", normalizePath(out_dir), "\n")
cat("  1. 01_walk_time_to_subway.png\n")
cat("  2. 02_trifecta_scatter.png\n")
cat("  3. 03_transit_quality_tiers.png\n")
cat("  4. 04_high_need_no_subway.png\n")
