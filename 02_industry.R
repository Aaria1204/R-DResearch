# =============================================================================
# 02_industry.R
# Industry-level R&D analysis (Tasks 4–6)
#
# Task 4: Aggregate R&D dollar figures by 2-digit NAICS sector
# Task 5: R&D/Assets ratio by sector — top 5 industries
# Task 6: Firm counts and share reporting positive R&D by sector
#
# Input:  data/processed/adjusted_rd.rds
#         rd_ratio object from 01_national.R (recomputed here for self-containment)
# Output: figures saved to output/figures/
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

data_slim <- readRDS(here("adjusted_rd.rds")) |>
  select(gvkey, fyear, conm, naics, at, xrd,
         at_PPI, at_GDP_DEF,
         xrd_PPI, xrd_GDP_DEF)


# -- NAICS 2-digit lookup table -----------------------------------------------

naics_labels <- tibble(
  naics2 = c("11","21","22","23","31","32","33","42","44","45",
             "48","49","51","52","53","54","55","56","61","62",
             "71","72","81","92"),
  sector = c(
    "11 - Agriculture",
    "21 - Mining",
    "22 - Utilities",
    "23 - Construction",
    "31 - Manufacturing (Food/Apparel)",
    "32 - Manufacturing (Paper/Chemical)",
    "33 - Manufacturing (Metal/Tech)",
    "42 - Wholesale Trade",
    "44 - Retail Trade (Stores)",
    "45 - Retail Trade (Nonstore)",
    "48 - Transportation",
    "49 - Warehousing/Postal",
    "51 - Information",
    "52 - Finance & Insurance",
    "53 - Real Estate",
    "54 - Professional Services",
    "55 - Management",
    "56 - Administrative Services",
    "61 - Educational Services",
    "62 - Health Care",
    "71 - Arts & Entertainment",
    "72 - Accommodation & Food",
    "81 - Other Services",
    "92 - Public Administration"
  )
)

# -- Add sector labels to data ------------------------------------------------

data_slim <- data_slim |>
  mutate(
    naics2 = substr(as.character(naics), 1, 2),
    naics2 = ifelse(is.na(naics) | naics2 == "NA", "Unknown", naics2)
  ) |>
  left_join(naics_labels, by = "naics2") |>
  mutate(sector = ifelse(is.na(sector), "Unknown", sector))

# -- Top 5 sectors by total inflation-adjusted R&D ----------------------------

top5_sectors <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025, sector != "Unknown") |>
  group_by(sector) |>
  summarise(total_xrd = sum(xrd_GDP_DEF, na.rm = TRUE)) |>
  slice_max(total_xrd, n = 5) |>
  pull(sector)


# =============================================================================
# TASK 4: Aggregate R&D by industry
# =============================================================================

rd_ts_industry <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  group_by(fyear, sector) |>
  summarise(
    xrd_nominal = sum(xrd,         na.rm = TRUE),
    xrd_PPI     = sum(xrd_PPI,     na.rm = TRUE),
    xrd_GDP_DEF = sum(xrd_GDP_DEF, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = c(xrd_nominal, xrd_PPI, xrd_GDP_DEF),
    names_to  = "deflator",
    values_to = "rd_total"
  ) |>
  mutate(deflator = recode(deflator,
    "xrd_nominal" = "Nominal",
    "xrd_PPI"     = "PPI",
    "xrd_GDP_DEF" = "GDP Deflator"
  ))

# All sectors faceted
ggplot(rd_ts_industry, aes(x = fyear, y = rd_total / 1000, color = deflator)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ sector, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c(
    "Nominal"      = "gray50",
    "PPI"          = "#E91E63",
    "GDP Deflator" = "#4CAF50"
  )) +
  scale_x_continuous(breaks = c(1960, 1990, 2025)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Aggregate R&D Expenditure by Industry (1960–2025)",
    subtitle = "Total R&D in $B by 2-digit NAICS sector",
    x        = "Fiscal Year",
    y        = "Total R&D ($ Billions)",
    color    = "Deflator"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    strip.text      = element_text(face = "bold", size = 7)
  )

# Top 5 sectors
rd_ts_industry |>
  filter(sector %in% top5_sectors, deflator == "GDP Deflator") |>
  ggplot(aes(x = fyear, y = rd_total / 1000, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Aggregate R&D Expenditure — Top 5 Industries (1960–2025)",
    subtitle = "GDP deflator, 2025 dollars",
    x        = "Fiscal Year",
    y        = "Total R&D ($ Billions)",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )


# =============================================================================
# TASK 5: R&D/Assets ratio by sector
# =============================================================================

rd_ratio <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  mutate(ratio = xrd / at) |>
  mutate(ratio = ifelse(is.finite(ratio), ratio, NA)) |>
  mutate(
    p01        = quantile(ratio, 0.01, na.rm = TRUE),
    p99        = quantile(ratio, 0.99, na.rm = TRUE),
    ratio_wins = pmin(pmax(ratio, p01), p99)
  ) |>
  select(-p01, -p99)

rd_ratio_industry <- rd_ratio |>
  left_join(
    data_slim |> select(gvkey, fyear, sector) |> distinct(),
    by = c("gvkey", "fyear"),
    relationship = "many-to-many"
  ) |>
  group_by(fyear, sector) |>
  summarise(
    mean   = mean(ratio_wins,          na.rm = TRUE),
    median = median(ratio_wins,        na.rm = TRUE),
    p25    = quantile(ratio_wins, 0.25, na.rm = TRUE),
    p75    = quantile(ratio_wins, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

rd_ratio_industry |>
  filter(sector %in% top5_sectors) |>
  ggplot(aes(x = fyear, y = mean, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "R&D Intensity — Top 5 Industries (1960–2025)",
    subtitle = "Winsorized mean R&D/Assets ratio",
    x        = "Fiscal Year",
    y        = "Mean R&D / Total Assets",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))


# =============================================================================
# TASK 6: Share of firms reporting positive R&D by sector
# =============================================================================

rd_firms_industry <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  group_by(fyear, sector) |>
  summarise(
    n_total    = n_distinct(gvkey),
    n_positive = n_distinct(gvkey[xrd > 0]),
    pct        = n_positive / n_total,
    .groups    = "drop"
  )

rd_firms_industry |>
  filter(sector %in% top5_sectors) |>
  ggplot(aes(x = fyear, y = pct, color = sector)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Share of Firms Reporting Positive R&D — Top 5 Industries (1960–2025)",
    subtitle = "% of Compustat firms with xrd > 0 within each sector",
    x        = "Fiscal Year",
    y        = "% of Firms",
    color    = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))

