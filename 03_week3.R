# =============================================================================
# 03_week3.R
# Week 3 extensions
#
# PART A: Industry-level R&D intensity using lagged assets R&D(t) / Assets(t-1)
#   A1: Initial winsorized plot — top 5 industries (no outlier removal yet)
#   A2: Spike investigation — identify which firms drive spikes and in which years
#   A3: Outlier removal (data-driven) + sector-level re-winsorization
#   A4: Final plots with cleaned data
#       - Median winsorized ratio by sector (firm-level)
#       - Economy-wide ratio by sector (aggregate)
#   A5: Deep-dive tables — top outlier firms within each sector
#   A6: NAICS first appearance
#
# PART B: GDP ratio
#   - Aggregate R&D(t) / GDP(t-1)
#
# Input:  adjusted_rd.rds
#         FRED API (series: GDP)
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(fredr)
library(here)

fredr_set_key("834bcb81acd443cc4bd27634b71b84d1")


# -- NAICS lookup -------------------------------------------------------------

naics_labels <- tibble(
  naics2 = c("11","21","22","23","31","32","33","42","44","45",
             "48","49","51","52","53","54","55","56","61","62",
             "71","72","81","92"),
  sector = c(
    "11 - Agriculture","21 - Mining","22 - Utilities",
    "23 - Construction","31 - Manufacturing (Food/Apparel)",
    "32 - Manufacturing (Paper/Chemical)","33 - Manufacturing (Metal/Tech)",
    "42 - Wholesale Trade","44 - Retail Trade (Stores)",
    "45 - Retail Trade (Nonstore)","48 - Transportation",
    "49 - Warehousing/Postal","51 - Information",
    "52 - Finance & Insurance","53 - Real Estate",
    "54 - Professional Services","55 - Management",
    "56 - Administrative Services","61 - Educational Services",
    "62 - Health Care","71 - Arts & Entertainment",
    "72 - Accommodation & Food","81 - Other Services",
    "92 - Public Administration"
  )
)


# -- Load and prepare data ----------------------------------------------------

data <- readRDS(here("adjusted_rd.rds")) |>
  filter(fyear >= 1970, xrd > 0, !is.na(naics), !is.na(at), at > 0) |>
  mutate(naics2 = substr(as.character(naics), 1, 2)) |>
  left_join(naics_labels, by = "naics2") |>
  arrange(gvkey, fyear) |>
  group_by(gvkey) |>
  mutate(at_lag = lag(at)) |>
  ungroup() |>
  mutate(
    ratio = xrd / at_lag,
    ratio = ifelse(is.finite(ratio), ratio, NA)
  )


# =============================================================================
# PART A1: Initial winsorized plot — top 5 industries (before outlier removal)
# Winsorize by sector-year (>= 10 firms required, otherwise leave as-is)
# Run this first, look at the graph, then proceed to A2 to investigate spikes
# =============================================================================

data_wins <- data |>
  group_by(fyear, sector) |>
  mutate(
    n          = sum(!is.na(ratio)),
    p01        = ifelse(n >= 10, quantile(ratio, 0.01, na.rm = TRUE), NA),
    p99        = ifelse(n >= 10, quantile(ratio, 0.99, na.rm = TRUE), NA),
    ratio_wins = case_when(
      is.na(p01) ~ ratio,
      TRUE       ~ pmin(pmax(ratio, p01), p99)
    )
  ) |>
  ungroup() |>
  select(-p01, -p99, -n)

# Top 5 sectors by mean winsorized ratio — require avg >= 20 firms/year
sector_size_initial <- data_wins |>
  filter(!is.na(sector), sector != "Unknown") |>
  group_by(sector, fyear) |>
  summarise(n_firms = n_distinct(gvkey), .groups = "drop") |>
  group_by(sector) |>
  summarise(mean_firms = mean(n_firms), .groups = "drop")

sector_ratio_initial <- data_wins |>
  filter(!is.na(sector), sector != "Unknown") |>
  group_by(sector) |>
  summarise(mean_ratio = mean(ratio_wins, na.rm = TRUE), .groups = "drop")

top5_initial <- sector_size_initial |>
  left_join(sector_ratio_initial, by = "sector") |>
  filter(mean_firms >= 20) |>
  slice_max(mean_ratio, n = 5) |>
  pull(sector)

# Industry summary
rd_ratio_initial <- data_wins |>
  filter(sector %in% top5_initial) |>
  group_by(fyear, sector) |>
  summarise(
    median  = median(ratio_wins, na.rm = TRUE),
    mean    = mean(ratio_wins,   na.rm = TRUE),
    .groups = "drop"
  )

# Plot — inspect this for spikes before proceeding
ggplot(rd_ratio_initial, aes(x = fyear, y = median, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "R&D Intensity — Top 5 Industries (1970–2025)",
    subtitle = "Median winsorized R&D(t)/Assets(t-1) — before outlier removal",
    x        = "Fiscal Year",
    y        = "Median R&D / Total Assets",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))


# =============================================================================
# PART A2: Spike investigation
# Identify which firms are driving spikes and in which years
# =============================================================================

# All firms with suspiciously high ratios
spike_firms <- data_wins |>
  filter(ratio > 0.5) |>
  arrange(desc(ratio)) |>
  select(conm, gvkey, fyear, sector, xrd, at, at_lag, ratio)

View(spike_firms)

# Top 5 worst firms per year
spike_by_year <- data_wins |>
  filter(!is.na(ratio)) |>
  group_by(fyear) |>
  slice_max(ratio, n = 5) |>
  ungroup() |>
  arrange(fyear, desc(ratio)) |>
  select(conm, gvkey, fyear, sector, xrd, at, at_lag, ratio)

View(spike_by_year)

# How many firms per year exceed ratio > 1
data_wins |>
  filter(ratio > 1) |>
  group_by(fyear) |>
  summarise(n_extreme = n(), .groups = "drop") |>
  print(n = 60)


# =============================================================================
# PART A3: Outlier removal — data-driven
# Flag any firm that ever has ratio > 1 (R&D exceeds prior-year total assets)
# Then re-winsorize by sector-year with the same >= 10 firm guard
# =============================================================================

outlier_gvkeys <- data_wins |>
  filter(ratio > 1) |>
  distinct(gvkey) |>
  pull(gvkey)

cat("Number of outlier firms flagged:", length(outlier_gvkeys), "\n")

# Check whether winsorization alone was sufficient
data_wins |>
  filter(gvkey %in% outlier_gvkeys) |>
  summarise(
    n_obs     = n(),
    mean_raw  = mean(ratio,      na.rm = TRUE),
    mean_wins = mean(ratio_wins, na.rm = TRUE),
    max_raw   = max(ratio,       na.rm = TRUE),
    max_wins  = max(ratio_wins,  na.rm = TRUE)
  ) |>
  print()

# Remove outliers and re-winsorize by sector-year
data_clean <- data |>
  filter(!gvkey %in% outlier_gvkeys) |>
  group_by(fyear, sector) |>
  mutate(
    n          = sum(!is.na(ratio)),
    p01        = ifelse(n >= 10, quantile(ratio, 0.01, na.rm = TRUE), NA),
    p99        = ifelse(n >= 10, quantile(ratio, 0.99, na.rm = TRUE), NA),
    ratio_wins = case_when(
      is.na(p01) ~ ratio,
      TRUE       ~ pmin(pmax(ratio, p01), p99)
    )
  ) |>
  ungroup() |>
  select(-p01, -p99, -n)


# =============================================================================
# PART A4: Final plots with cleaned data
# Top 5 sectors require avg >= 20 firms/year to exclude sparse sectors
# Economy-wide plot excludes first-year entries (at_lag is NA) and
# sector-years with fewer than 10 firms
# =============================================================================

# Top 5 sectors after outlier removal
sector_size <- data_clean |>
  filter(!is.na(sector), sector != "Unknown") |>
  group_by(sector, fyear) |>
  summarise(n_firms = n_distinct(gvkey), .groups = "drop") |>
  group_by(sector) |>
  summarise(mean_firms = mean(n_firms), .groups = "drop")

sector_ratio <- data_clean |>
  filter(!is.na(sector), sector != "Unknown") |>
  group_by(sector) |>
  summarise(mean_ratio = mean(ratio_wins, na.rm = TRUE), .groups = "drop")

top5_sectors <- sector_size |>
  left_join(sector_ratio, by = "sector") |>
  filter(mean_firms >= 20) |>
  slice_max(mean_ratio, n = 5) |>
  pull(sector)

cat("Top 5 sectors after outlier removal:\n")
print(top5_sectors)

# Industry summary
rd_ratio_industry <- data_clean |>
  filter(sector %in% top5_sectors) |>
  group_by(fyear, sector) |>
  summarise(
    n_firms = n(),
    median  = median(ratio_wins, na.rm = TRUE),
    mean    = mean(ratio_wins,   na.rm = TRUE),
    p25     = quantile(ratio_wins, 0.25, na.rm = TRUE),
    p75     = quantile(ratio_wins, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Plot A: median winsorized ratio by sector (firm-level)
ggplot(rd_ratio_industry, aes(x = fyear, y = median, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "R&D Intensity — Top 5 Industries (1970–2025)",
    subtitle = "Median winsorized R&D(t)/Assets(t-1) — outlier firms removed",
    x        = "Fiscal Year",
    y        = "Median R&D / Total Assets",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))

# Plot B: economy-wide ratio by sector (aggregate)
# Excludes first-year entries (no at_lag) and sector-years with < 10 firms
rd_ratio_economy <- data_clean |>
  filter(sector %in% top5_sectors, !is.na(at_lag)) |>
  group_by(fyear, sector) |>
  summarise(
    n_firms       = n_distinct(gvkey),
    ratio_economy = sum(xrd, na.rm = TRUE) / sum(at_lag, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_firms >= 10)

ggplot(rd_ratio_economy, aes(x = fyear, y = ratio_economy, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Economy-Wide R&D Intensity — Top 5 Industries (1970–2025)",
    subtitle = "Aggregate R&D(t) / Aggregate Assets(t-1) — sparse sectors and first-year entries excluded",
    x        = "Fiscal Year",
    y        = "Total R&D / Total Assets",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))


# =============================================================================
# PART A5: Deep-dive tables — top outlier firms within each sector
# =============================================================================

for (s in top5_sectors) {
  cat("\n--- Top firms by ratio:", s, "---\n")
  data_clean |>
    filter(sector == s) |>
    arrange(desc(ratio)) |>
    select(conm, gvkey, fyear, xrd, at, at_lag, ratio) |>
    head(10) |>
    print()
}


# =============================================================================
# PART A6: NAICS first appearance
# =============================================================================

data |>
  filter(!is.na(naics)) |>
  group_by(naics2, sector) |>
  summarise(
    first_year = min(fyear),
    n_firms    = n_distinct(gvkey),
    .groups    = "drop"
  ) |>
  arrange(first_year) |>
  print(n = 30)


# =============================================================================
# PART B: Aggregate R&D as share of prior-year GDP
# =============================================================================

gdp <- fredr(
  series_id         = "GDP",
  frequency         = "a",
  observation_start = as.Date("1970-01-01"),
  observation_end   = as.Date("2025-01-01")
) |>
  mutate(fyear = as.integer(format(date, "%Y"))) |>
  select(fyear, gdp_nominal = value) |>
  mutate(gdp_lag = lag(gdp_nominal))

rd_gdp_ratio <- data |>
  group_by(fyear) |>
  summarise(total_xrd = sum(xrd, na.rm = TRUE), .groups = "drop") |>
  left_join(gdp, by = "fyear") |>
  mutate(rd_gdp = total_xrd / (gdp_lag * 1000)) |>
  filter(!is.na(rd_gdp))

ggplot(rd_gdp_ratio, aes(x = fyear, y = rd_gdp)) +
  geom_line(color = "#2196F3", linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Aggregate Corporate R&D as Share of Prior Year GDP (1970–2025)",
    subtitle = "Nominal R&D(t) / Nominal GDP(t-1) — all Compustat firms",
    x        = "Fiscal Year",
    y        = "R&D / GDP(t-1)"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

firms_1995 <- data_clean |>
  filter(sector == "52 - Finance & Insurance", fyear == 1995) |>
  pull(gvkey)

firms_1996 <- data_clean |>
  filter(sector == "52 - Finance & Insurance", fyear == 1996) |>
  pull(gvkey)

data_clean |>
  filter(
    gvkey %in% setdiff(firms_1995, firms_1996),
    fyear == 1995,
    sector == "52 - Finance & Insurance"
  ) |>
  arrange(desc(at)) |>
  select(conm, gvkey, fyear, xrd, at) |>
  print(n = 20)



