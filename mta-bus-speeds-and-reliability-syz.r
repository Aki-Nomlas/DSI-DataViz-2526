library(tidyverse)
library(ggplot2)
#read.csv('MTA_Bus_Route_Segment_Speeds__2023_-_2024_20251117.csv')     #VERY large dataset!!
bus <- read.csv('Q52-Q53.csv')

# initial plot ------------------------------------------------------------

segments <- bus %>%
  mutate(segment = paste(Timepoint.Stop.Name, "→", Next.Timepoint.Stop.Name)) %>%
  group_by(segment, Direction) %>%
  summarise(
    med_speed = median(Average.Road.Speed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(med_speed) %>%
  slice_head(n = 15) %>%   # 15 slowest segments
  mutate(segment = reorder(segment, med_speed))

segments %>% 
  ggplot(aes(x = med_speed, y = segment)) +
  geom_col() +
  labs(
    title = "Slowest Q52/Q53 Segments (Median Speed)",
    x = "Median speed (mph)",
    y = NULL
  ) +
  facet_wrap(
    vars(Direction)
    ,ncol  = 1
    ,scales = "free_y"
  )


# initial peeks -----------------------------------------------------------

bus <- tibble(bus)

#install.packages("sf")

bus %>% glimpse()

# average speed -- using a weighted average
stats::weighted.mean(bus$Average.Road.Speed, bus$Road.Distance)
# itd be interesting to compare to relevant points of context -- this bus vs others

# the two colons are to use a function in a package, without necessarily loading the whole package


# how to clean segments? --------------------------------------------------

bus_segments <- bus %>%
  mutate(segment = paste(Timepoint.Stop.Name, "→", Next.Timepoint.Stop.Name))


# tidycensus::get_acs()
# tidycensus::get_flows()

# lehdr::grab_lodes()


# let's remove the less relevant areas  -----------------------------------

unwanted_stops <- c(
  "BEACH CHANNEL DR/BEACH 54 ST",
  "BEACH 116 ST/ROCKAWAY BEACH BL",
  "ROOSEVELT AV/61 ST"
)

segments_selected <- bus %>%
  filter(
    !Timepoint.Stop.Name %in% unwanted_stops,
  ) %>%
  mutate(segment = paste(Timepoint.Stop.Name, "→", Next.Timepoint.Stop.Name))

# spatializing ------------------------------------------------------------

library(sf)
#install.packages("mapview")

busf <- st_as_sf(
  segments_selected,
  wkt = "Timepoint.Stop.Georeference"
  ,crs = 4326
) %>% 
  st_transform(4326)


mapview::mapview(busf, zcol = "Average.Road.Speed")
busf  %>% 
  ggplot(
    aes(color = Average.Road.Speed)
  ) +
  geom_sf()


# beginning visualizing speed throughout the day, weekday vs weekend --------

library(lubridate)

# parse timestamp + create weekday/weekend flag
bus_t <- segments_selected %>%
  mutate(
    ts = parse_date_time(Timestamp, orders = c("Y b d I:M:S p")),
    day_type = if_else(Day.of.Week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

# (A) Overall route-level profile: median speed by hour, weekday vs weekend
hour_profile <- bus_t %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    med_speed = median(Average.Road.Speed, na.rm = TRUE),
    p25 = quantile(Average.Road.Speed, .25, na.rm = TRUE),
    p75 = quantile(Average.Road.Speed, .75, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(hour_profile, aes(x = Hour.of.Day, y = med_speed, group = day_type, linetype = day_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 speed profile by hour: Weekday vs Weekend",
    x = "Hour of day",
    y = "Median road speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()


# Great. Now let's compare that with the rest of the system ---------------

#pulling data directly from MTA
library(readr)

bus_overall <- read_csv("https://data.ny.gov/api/views/6ksi-7cxr/rows.csv?accessType=DOWNLOAD")

# Systemwide (weighted) average speed by month: weekday vs weekend
bus2 <- bus_overall %>%
  mutate(
    day_type_label = case_when(
      day_type %in% c(0, 1) ~ if_else(day_type == 0, "Weekday", "Weekend"),
      day_type %in% c(1, 2) ~ if_else(day_type == 1, "Weekday", "Weekend"),
      TRUE ~ as.character(day_type)
    )
  )

sys_month <- bus2 %>%
  group_by(month, day_type_label) %>%
  summarise(
    total_mileage = sum(total_mileage, na.rm = TRUE),
    total_time    = sum(total_operating_time, na.rm = TRUE),
    avg_speed_sys = total_mileage / total_time,
    .groups = "drop"
  )

ggplot(sys_month, aes(x = month, y = avg_speed_sys, linetype = day_type_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(
    title = "NYC bus speeds systemwide (weighted): Weekday vs Weekend",
    x = NULL,
    y = "Average speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()

#OK, the bus speed is VERY fast in 2020, probably because of the pandemic. let's just do the average of 2023-2024

bus2 <- bus_overall %>%
  mutate(
    year = year(month),
    day_type_label = case_when(
      day_type %in% c(0, 1) ~ if_else(day_type == 0, "Weekday", "Weekend"),
      day_type %in% c(1, 2) ~ if_else(day_type == 1, "Weekday", "Weekend"),
      TRUE ~ paste0("DayType_", day_type)
    ),
    period = factor(period, levels = c("Off-Peak", "Peak"))
  ) %>%
  filter(year %in% c(2023, 2024))

# systemwide (weighted) speed by period, weekday vs weekend
sys_period_2324 <- bus2 %>%
  group_by(day_type_label, period) %>%
  summarise(
    total_mileage = sum(total_mileage, na.rm = TRUE),
    total_time    = sum(total_operating_time, na.rm = TRUE),
    avg_speed_sys = total_mileage / total_time,
    .groups = "drop"
  )

ggplot(sys_period_2324,
       aes(x = period, y = avg_speed_sys, group = day_type_label, linetype = day_type_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "NYC bus speeds systemwide (weighted), 2023–2024: Peak vs Off-Peak",
    x = NULL,
    y = "Average speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()

# I just realized that this is a much simpler dataset that doesn't have the same level of detail like the main dataset
# *sigh* let's load the mega dataset


# start breaking down the huge dataset by filtering only SBS routes -------

bus_master <- read.csv('MTA_Bus_Route_Segment_Speeds__2023_-_2024_20251117.csv') #initial load

#Let's just focus on SBS routes for now

bus_master_sbs <- bus_master %>%
  filter(Route.Type == "SBS")

# 60 megs is a lot more managable...

# since manhattan buses are notoriously slow, let's only focus on the outerborough routes for now

bus_no_manhattan_seg <- bus_master_sbs %>%
  filter(Year %in% c(2023, 2024),
         Borough != "Manhattan") %>%
  mutate(day_type = if_else(Day.of.Week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

hour_profile_no_manhattan <- bus_no_manhattan_seg %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    med_speed = median(Average.Road.Speed, na.rm = TRUE),
    p25 = quantile(Average.Road.Speed, .25, na.rm = TRUE),
    p75 = quantile(Average.Road.Speed, .75, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(hour_profile_no_manhattan,
       aes(x = Hour.of.Day, y = med_speed, group = day_type, linetype = day_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "NYC outerborough SBS speed profile by hour, 2023–2024",
    x = "Hour of day",
    y = "Median road speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()

# Interesting. Let's now bring this average to the original plot graph for context
# Q52/Q53: median + IQR by hour, weekday vs weekend (same as before, no Timestamp parsing needed)
q_hour <- segments_selected %>%
  mutate(day_type = if_else(Day.of.Week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    med_speed = median(Average.Road.Speed, na.rm = TRUE),
    p25 = quantile(Average.Road.Speed, .25, na.rm = TRUE),
    p75 = quantile(Average.Road.Speed, .75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(series = paste0("Q52/Q53 — ", day_type))

# Outer-borough SBS context line: weighted average speed by hour (exclude Manhattan segments)
outer_sbs_hour <- bus_master_sbs %>%   # assumes SBS-only already
  filter(Borough != "Manhattan") %>%
  filter(!is.na(Road.Distance), !is.na(Average.Travel.Time), !is.na(Bus.Trip.Count)) %>%
  mutate(
    miles = Road.Distance,
    hours = Average.Travel.Time / 60,  # minutes -> hours
    w = Bus.Trip.Count
  ) %>%
  group_by(Hour.of.Day) %>%
  summarise(
    avg_speed = sum(miles * w, na.rm = TRUE) / sum(hours * w, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(series = "Outer-borough SBS — weighted avg")

# Combined plot
ggplot() +
  geom_ribbon(
    data = q_hour,
    aes(x = Hour.of.Day, ymin = p25, ymax = p75, group = day_type),
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_line(
    data = q_hour,
    aes(x = Hour.of.Day, y = med_speed, linetype = series),
    linewidth = 1
  ) +
  geom_line(
    data = outer_sbs_hour,
    aes(x = Hour.of.Day, y = avg_speed, linetype = series),
    linewidth = 1
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 speed profile by hour + outer-borough SBS context",
    x = "Hour of day",
    y = "Speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()

# this shows that the Woodhaven/cross bay segment of the Q52/53 is actually quite a bit above outerborough average.
# but is speed really a good gauge of reliability?


# --- Q52/Q53 route-hour snapshots (sum of segment travel times) ---
q_snap <- bus_master_sbs %>%
  filter(Route.ID %in% c("Q52+", "Q53+")) %>%
  mutate(
    ts = parse_date_time(Timestamp, orders = "Y b d I:M:S p"),
    date = as.Date(ts),
    day_type = if_else(Day.of.Week %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
    seg_min = Average.Travel.Time
  ) %>%
  group_by(Route.ID, Direction, date, Hour.of.Day, day_type) %>%
  summarise(
    total_min   = sum(seg_min, na.rm = TRUE),
    total_miles = sum(Road.Distance, na.rm = TRUE),
    n_seg = n(),
    .groups = "drop"
  ) %>%
  # optional guardrail: drop weird/incomplete snapshots
  filter(total_min > 0, total_miles > 0)

# --- reliability summary by hour ---
q_reliab_hour <- q_snap %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    p50_min = quantile(total_min, 0.50, na.rm = TRUE),
    p90_min = quantile(total_min, 0.90, na.rm = TRUE),
    buffer_min = p90_min - p50_min,
    pti = p90_min / p50_min,  # planning time index
    .groups = "drop"
  )

# Plot A: buffer minutes (more intuitive “unreliability” story)
ggplot(q_reliab_hour, aes(Hour.of.Day, buffer_min, linetype = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 reliability by hour: extra buffer needed (P90 - median)",
    x = "Hour of day",
    y = "Buffer minutes",
    linetype = NULL
  ) +
  theme_minimal()

# Plot B: planning time index (ratio)
ggplot(q_reliab_hour, aes(Hour.of.Day, pti, linetype = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 reliability by hour: Planning Time Index (P90 / median)",
    x = "Hour of day",
    y = "PTI",
    linetype = NULL
  ) +
  theme_minimal()

outer_sbs_hour <- bus_master_sbs %>%
  filter(Borough != "Manhattan") %>%
  mutate(
    hours = Average.Travel.Time / 60,
    w = Bus.Trip.Count
  ) %>%
  group_by(Hour.of.Day) %>%
  summarise(
    avg_speed = sum(Road.Distance * w, na.rm = TRUE) / sum(hours * w, na.rm = TRUE),
    .groups = "drop"
  )


# ANOTHER ATTEMPT ---------------------------------------------------------

# Speed graph & slowest SBS bus -------------------------------------------


# 1) Q52/Q53 average speed throughout the day (weighted)
q_routes <- c("Q52", "Q52+", "Q53", "Q53+")

q_hour <- bus_master_sbs %>%  # or your full bus_master if you want non-SBS too
  filter(Route.ID %in% q_routes) %>%
  mutate(
    day_type = if_else(Day.of.Week %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
    w = Bus.Trip.Count,
    miles = Road.Distance,
    hours = Average.Travel.Time / 60
  ) %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    avg_speed = sum(miles * w, na.rm = TRUE) / sum(hours * w, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(q_hour, aes(Hour.of.Day, avg_speed, linetype = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 average speed by hour (weighted)",
    x = "Hour of day",
    y = "Average speed (mph)",
    linetype = NULL
  ) +
  theme_minimal()



# 2) “Slowest bus” ranking (by Route.ID), weighted average speed

slowest_routes <- bus_master_sbs %>%
  mutate(
    w = Bus.Trip.Count,
    miles = Road.Distance,
    hours = Average.Travel.Time / 60
  ) %>%
  group_by(Route.ID) %>%
  summarise(
    total_miles = sum(miles * w, na.rm = TRUE),
    total_hours = sum(hours * w, na.rm = TRUE),
    avg_speed = total_miles / total_hours,
    .groups = "drop"
  ) %>%
  filter(total_miles > 1000) %>%           # guardrail: drop tiny-sample routes (tune as needed)
  arrange(avg_speed)

# top 20 slowest
slowest_routes %>%
  slice_head(n = 20) %>%
  mutate(Route.ID = fct_reorder(Route.ID, avg_speed, .desc = TRUE)) %>%  # reverse
  ggplot(aes(avg_speed, Route.ID)) +
  geom_col() +
  labs(
    title = "Slowest SBS routes (weighted average speed)",
    x = "Average speed (mph)",
    y = NULL
  ) +
  theme_minimal()



# Compare by reliability instead (Buffer minutes) -------------------------


# Build route-hour “snapshots” (sum segment travel times across the route)

sbs_snap <- bus_master_sbs %>%
  filter(Year %in% c(2023, 2024)) %>%
  mutate(
    day_type = if_else(Day.of.Week %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
    date = as.Date(substr(Timestamp, 1, 11), format = "%Y %b %d")
  ) %>%
  group_by(Route.ID, Direction, Borough, date, Hour.of.Day, day_type) %>%  # <- keep Borough
  summarise(
    total_min = sum(Average.Travel.Time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_min > 0)

# Build route-hour “snapshots” (sum segment travel times across the route)

# Q52/Q53 reliability by hour
q_reliab <- sbs_snap %>%
  filter(Route.ID %in% c("Q52+","Q53+")) %>%
  group_by(day_type, Hour.of.Day) %>%
  summarise(
    p50 = quantile(total_min, .50, na.rm = TRUE),
    p90 = quantile(total_min, .90, na.rm = TRUE),
    buffer_min = p90 - p50,
    pti = p90 / p50,
    .groups="drop"
  )

# Outer-borough SBS: exclude Manhattan segments (simple baseline)
outer_reliab <- sbs_snap %>%
  filter(Borough != "Manhattan") %>%
  group_by(Hour.of.Day) %>%
  summarise(
    p50 = quantile(total_min, .50, na.rm = TRUE),
    p90 = quantile(total_min, .90, na.rm = TRUE),
    buffer_min = p90 - p50,
    pti = p90 / p50,
    .groups="drop"
  )
# Compare Q52/Q53 vs “outer-borough SBS baseline” (same metric, by hour)

outer_snap <- bus_master_sbs %>%
  filter(Year %in% c(2023, 2024), Borough != "Manhattan",
         !Route.ID %in% c("Q52+","Q53+")) %>%
  mutate(date = as.Date(substr(Timestamp, 1, 11), format = "%Y %b %d")) %>%
  group_by(Route.ID, Direction, date, Hour.of.Day) %>%
  summarise(total_min = sum(Average.Travel.Time, na.rm=TRUE), .groups="drop") %>%
  filter(total_min > 0)

outer_reliab <- outer_snap %>%
  group_by(Hour.of.Day) %>%
  summarise(
    p50 = quantile(total_min, .50, na.rm=TRUE),
    p90 = quantile(total_min, .90, na.rm=TRUE),
    buffer_min = p90 - p50,
    pti = p90 / p50,
    .groups="drop"
  )

# Plot the buffer minutes profile (this is the “unreliable commute” story)

ggplot() +
  geom_line(data = q_reliab,
            aes(Hour.of.Day, buffer_min, linetype = paste0("Q52/Q53 — ", day_type)),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Reliability by hour: extra buffer minutes needed (P90 − median), 2023–2024",
    x = "Hour of day",
    y = "Buffer minutes",
    linetype = NULL
  ) +
  theme_minimal()

# Q52/Q53 isn’t the slowest, but here’s where it ranks on reliability during commute hours.

commute_hours <- 7:10

rank_weekday_am <- sbs_snap %>%
  filter(day_type == "Weekday", Hour.of.Day %in% commute_hours) %>%
  group_by(Route.ID) %>%
  summarise(
    p50 = quantile(total_min, .50, na.rm=TRUE),
    p90 = quantile(total_min, .90, na.rm=TRUE),
    buffer_min = p90 - p50,
    pti = p90 / p50,
    .groups="drop"
  ) %>%
  arrange(desc(buffer_min))

# see where Q52/Q53 land
rank_weekday_am %>% filter(Route.ID %in% c("Q52+","Q53+"))

# plot top 20 worst by buffer
rank_weekday_am %>%
  slice_head(n = 20) %>%
  mutate(Route.ID = reorder(Route.ID, buffer_min)) %>%
  ggplot(aes(buffer_min, Route.ID)) +
  geom_col() +
  labs(title="Worst SBS routes by reliability (weekday AM): buffer minutes (P90 − median)",
       x="Buffer minutes", y=NULL) +
  theme_minimal()



# inspecting B44+ (why is the buffer minutes so long?) --------------------

sbs_snap <- bus_master_sbs %>%
  filter(Year %in% c(2023, 2024)) %>%
  mutate(
    day_type = if_else(Day.of.Week %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
    date = as.Date(substr(Timestamp, 1, 11), format = "%Y %b %d"),
    seg_id = paste(Timepoint.Stop.ID, Next.Timepoint.Stop.ID, sep = "->")
  ) %>%
  group_by(Route.ID, Direction, date, Hour.of.Day, day_type) %>%
  summarise(
    total_min   = sum(Average.Travel.Time, na.rm = TRUE),
    total_miles = sum(Road.Distance, na.rm = TRUE),
    n_seg       = n_distinct(seg_id),
    max_stop    = max(Stop.Order, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_min > 0, total_miles > 0)

commute_hours <- 7:10

b44 <- sbs_snap %>%
  filter(Route.ID == "B44+", day_type == "Weekday", Hour.of.Day %in% commute_hours)

b44 %>%
  summarise(
    n = n(),
    p50 = quantile(total_min, .50, na.rm=TRUE),
    p90 = quantile(total_min, .90, na.rm=TRUE),
    p99 = quantile(total_min, .99, na.rm=TRUE),
    miles_med = median(total_miles, na.rm=TRUE),
    miles_iqr = IQR(total_miles, na.rm=TRUE),
    nseg_rng = paste(range(n_seg, na.rm=TRUE), collapse="–"),
    maxstop_rng = paste(range(max_stop, na.rm=TRUE), collapse="–")
  )

# Look for multiple clusters (pattern mixing signal)
ggplot(b44, aes(total_miles)) + geom_histogram(bins = 60)
ggplot(b44, aes(max_stop)) + geom_histogram(binwidth = 1)
ggplot(b44, aes(total_min)) + geom_histogram(bins = 60)

# Are a few extreme days driving P90/P99?
b44 %>%
  arrange(desc(total_min)) %>%
  select(date, Hour.of.Day, Direction, total_min, total_miles, n_seg, max_stop) %>%
  slice_head(n = 30) %>% 
  print(n = nrow(.))

# the B44+ being unusually long in average buffer minutes reveals that my analysis method is incorrect
# Each row is an average segment travel time/speed for a specific month × day-of-week × hour-of-day bin
# (plus route/direction/segment), along with a trip count.
# From ChatGPT: "..., so you cannot measure trip-to-trip or day-to-day unreliability directly; you can only 
# measure how much the average conditions vary across these bins. The MTA also notes these are coarse estimates
# from GPS and can vary with holidays, etc."
# "reconstructing “end-to-end route time” by summing segments can be fragile because routes can have multiple 
# paths between timepoints and timepoint definitions can change."


# BUFFER INDEX/BUFFER PER 5MI ---------------------------------------------



# PSEUDO Buffer Index for Q52/Q53 using minutes-per-mile (mpm)
# - Uses month × day-of-week × hour bins as the “samples”
# - Computes BI = (P95 - mean) / mean on route-level mpm
# - Also outputs "buffer minutes per 5 miles" = (P95 - mean) * 5

q_samples_w <- bus_master_sbs %>%
  filter(Route.ID %in% c("Q52+","Q53+"),
         Year %in% c(2023, 2024),
         Road.Distance > 0, Bus.Trip.Count > 0) %>%
  mutate(
    month_id = as.Date(sprintf("%04d-%02d-01", Year, Month)),
    day_group = if_else(Day.of.Week %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
    seg_mpm = Average.Travel.Time / Road.Distance,
    seg_w = Road.Distance * Bus.Trip.Count
  ) %>%
  group_by(Route.ID, Direction, month_id, Day.of.Week, day_group, Hour.of.Day) %>%
  summarise(
    route_mpm = weighted.mean(seg_mpm, w = seg_w, na.rm = TRUE),
    w_total   = sum(seg_w, na.rm = TRUE),          # total “miles * trips” represented
    trips     = sum(Bus.Trip.Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(route_mpm), route_mpm > 0)

q_samples_w2 <- q_samples_w %>% filter(w_total >= quantile(w_total, 0.10, na.rm=TRUE))

# without this filtering, there seems to be a weird spike at hour 0

# creating a custom function that computes a weighted percentile of x using weights w

wtd_q <- function(x, w, p) {
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= p)[1]]
}


q_bi_hour_w <- q_samples_w2 %>%
  group_by(day_group, Hour.of.Day) %>%
  summarise(
    mean_mpm = weighted.mean(route_mpm, w_total, na.rm = TRUE),
    p95_mpm  = wtd_q(route_mpm, w_total, 0.95),
    buffer_5mi_min = (p95_mpm - mean_mpm) * 5,
    .groups = "drop"
  )

ggplot(q_bi_hour_w, aes(Hour.of.Day, buffer_5mi_min, linetype = day_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Q52/Q53 extra buffer minutes per 5 miles by hour (weighted P95 − weighted mean)",
    x = "Hour of day",
    y = "Extra minutes per 5 miles",
    linetype = NULL
  ) +
  theme_minimal()



# Outer-borough SBS baseline (no weekday/weekend split)

outer_samples <- bus_master_sbs %>%
  filter(Borough != "Manhattan",
         Road.Distance > 0, Bus.Trip.Count > 0) %>%
  mutate(
    month_id = as.Date(sprintf("%04d-%02d-01", Year, Month)),
    seg_mpm  = Average.Travel.Time / Road.Distance,
    seg_w    = Road.Distance * Bus.Trip.Count
  ) %>%
  group_by(Route.ID, Direction, month_id, Day.of.Week, Hour.of.Day) %>%
  summarise(
    route_mpm = weighted.mean(seg_mpm, w = seg_w, na.rm = TRUE),
    w_total   = sum(seg_w, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(is.finite(route_mpm), route_mpm > 0)

# hour-specific low-coverage filter
outer_samples2 <- outer_samples %>%
  group_by(Hour.of.Day) %>%
  filter(w_total >= quantile(w_total, 0.10, na.rm = TRUE)) %>%
  ungroup()

outer_bi_hour <- outer_samples2 %>%
  group_by(Hour.of.Day) %>%
  summarise(
    mean_mpm = weighted.mean(route_mpm, w_total, na.rm = TRUE),
    p95_mpm  = wtd_q(route_mpm, w_total, 0.95),
    buffer_5mi_min = (p95_mpm - mean_mpm) * 5,
    .groups = "drop"
  ) %>%
  mutate(series = "Outer-borough SBS")

# Q52/Q53 line
q_samples <- bus_master_sbs %>%
  filter(Route.ID %in% c("Q52+","Q53+"),
         Road.Distance > 0, Bus.Trip.Count > 0) %>%
  mutate(
    month_id = as.Date(sprintf("%04d-%02d-01", Year, Month)),
    seg_mpm  = Average.Travel.Time / Road.Distance,
    seg_w    = Road.Distance * Bus.Trip.Count
  ) %>%
  group_by(Route.ID, Direction, month_id, Day.of.Week, Hour.of.Day) %>%
  summarise(
    route_mpm = weighted.mean(seg_mpm, w = seg_w, na.rm = TRUE),
    w_total   = sum(seg_w, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(is.finite(route_mpm), route_mpm > 0)

q_samples2 <- q_samples %>%
  group_by(Hour.of.Day) %>%
  filter(w_total >= quantile(w_total, 0.10, na.rm = TRUE)) %>%
  ungroup()

q_bi_hour <- q_samples2 %>%
  group_by(Hour.of.Day) %>%
  summarise(
    mean_mpm = weighted.mean(route_mpm, w_total, na.rm = TRUE),
    p95_mpm  = wtd_q(route_mpm, w_total, 0.95),
    buffer_5mi_min = (p95_mpm - mean_mpm) * 5,
    .groups = "drop"
  ) %>%
  mutate(series = "Q52/Q53")


plot_df <- bind_rows(q_bi_hour, outer_bi_hour)

ggplot(plot_df, aes(Hour.of.Day, buffer_5mi_min)) +
  geom_line(aes(linetype = series), linewidth = 1) +
  scale_linetype_manual(
    values = c("Q52/Q53" = "solid", "Outer-borough SBS" = "dashed")
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23)) +
  labs(
    title = "Extra buffer minutes per 5 miles by hour (weighted P95 − weighted mean, 2023–2024)",
    x = "Hour of day",
    y = "Extra minutes per 5 miles",
    linetype = NULL
  ) +
  theme_minimal()


# can we compare this with the subway? ----------------------------------------

# library(readr)
# library(janitor)
# 
# # 1) Download
# subway_e2e <- read_csv(
#   "https://data.ny.gov/api/views/sp9g-mzjh/rows.csv?accessType=DOWNLOAD",
#   show_col_types = FALSE
# ) %>% 
#   clean_names()
# 
# # 2) Auto-detect the key columns (print these and sanity-check)
# dist_col <- grep("distance", names(subway_e2e), value = TRUE)[1]
# avg_col  <- grep("actual.*(avg|average)", names(subway_e2e), value = TRUE, ignore.case = TRUE)[1]
# p75_col  <- grep("actual.*75", names(subway_e2e), value = TRUE, ignore.case = TRUE)[1]
# w_col    <- grep("actual.*trains|scheduled.*trains|num.*trains", names(subway_e2e),
#                  value = TRUE, ignore.case = TRUE)[1]
# 
# c(dist_col = dist_col, avg_col = avg_col, p75_col = p75_col, weight_col = w_col)
# 
# # 3) Compute buffer per 5 miles (P75 - mean), 2023–2024
# subway_buf <- subway_e2e %>%
#   filter(month >= as.Date("2023-01-01"), month <= as.Date("2024-12-01")) %>%
#   mutate(
#     day_group = if_else(schedule_day_type %in% c("Saturday","Sunday"), "Weekend", "Weekday"),
#     buffer5mi_p75 = ((.data[[p75_col]] - .data[[avg_col]]) / .data[[dist_col]]) * 5
#   ) %>%
#   filter(is.finite(buffer5mi_p75), buffer5mi_p75 >= 0)
# 
# # 4) “Typical subway” baseline (systemwide, weighted by trains if available)
# subway_baseline <- subway_buf %>%
#   group_by(day_group, time_period) %>%
#   summarise(
#     buffer5mi_p75 = if (!is.na(w_col) && length(w_col) == 1) {
#       weighted.mean(buffer5mi_p75, w = .data[[w_col]], na.rm = TRUE)
#     } else {
#       mean(buffer5mi_p75, na.rm = TRUE)
#     },
#     .groups = "drop"
#   ) %>%
#   mutate(time_period = factor(time_period, levels = c("AM peak","midday","PM peak","evening","overnight")))
# 
# ggplot(subway_baseline, aes(x = time_period, y = buffer5mi_p75, linetype = day_group, group = day_group)) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   labs(
#     title = "Subway reliability proxy: extra buffer minutes per 5 miles (P75 − mean), 2023–2024",
#     x = NULL, y = "Extra minutes per 5 miles", linetype = NULL
#   ) +
#   theme_minimal()




