# =============================================================================
# 01_national.R
# National-level R&D analysis (Tasks 1–3)
#
# Task 1: Aggregate R&D dollar figures by year (nominal, PPI, GDP deflator)
# Task 2: R&D/Assets ratio — distributional parameters + winsorized plot
# Task 3: Firm counts and share reporting positive R&D over time
#
# Input:  data/processed/adjusted_rd.rds
# Output: figures saved to output/figures/
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)

data_slim <- readRDS(here("adjusted_rd.rds")) |>
  select(gvkey, fyear, conm, naics, at, xrd,
         at_PPI, at_GDP_DEF,
         xrd_PPI, xrd_GDP_DEF)

# -- Variable labels ----------------------------------------------------------
attr(data_slim$gvkey,       "label") <- "Firm identifier (Compustat)"
attr(data_slim$fyear,       "label") <- "Fiscal year"
attr(data_slim$naics,       "label") <- "North American Industry Classification Code"
attr(data_slim$at,          "label") <- "Total assets (nominal, $M)"
attr(data_slim$xrd,         "label") <- "R&D expense (nominal, $M)"
attr(data_slim$at_PPI,      "label") <- "Total assets adjusted to 2025 dollars using PPI"
attr(data_slim$at_GDP_DEF,  "label") <- "Total assets adjusted to 2025 dollars using GDP deflator"
attr(data_slim$xrd_PPI,     "label") <- "R&D expense adjusted to 2025 dollars using PPI"
attr(data_slim$xrd_GDP_DEF, "label") <- "R&D expense adjusted to 2025 dollars using GDP deflator"


# =============================================================================
# TASK 1: Aggregate R&D time series
# =============================================================================

rd_timeseries <- data_slim |>
  group_by(fyear) |>
  summarise(
    xrd         = sum(xrd,         na.rm = TRUE),
    xrd_PPI     = sum(xrd_PPI,     na.rm = TRUE),
    xrd_GDP_DEF = sum(xrd_GDP_DEF, na.rm = TRUE)
  ) |>
  filter(fyear >= 1960, fyear <= 2025)

rd_long <- rd_timeseries |>
  pivot_longer(
    cols      = c(xrd, xrd_PPI, xrd_GDP_DEF),
    names_to  = "deflator",
    values_to = "rd_total"
  ) |>
  mutate(deflator = recode(deflator,
    "xrd"         = "Nominal",
    "xrd_PPI"     = "PPI",
    "xrd_GDP_DEF" = "GDP Deflator"
  ))

ggplot(rd_long, aes(x = fyear, y = rd_total / 1000, color = deflator)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    "Nominal"      = "gray50",
    "PPI"          = "#E91E63",
    "GDP Deflator" = "#4CAF50"
  )) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Aggregate U.S. Corporate R&D Expenditure (1960–2025)",
    subtitle = "Total across all Compustat firms, in 2025 dollars",
    x        = "Fiscal Year",
    y        = "Total R&D ($ Billions)",
    color    = "Deflator"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )


# =============================================================================
# TASK 2: R&D/Assets ratio — winsorized distributional parameters
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

rd_ratio_dist <- rd_ratio |>
  group_by(fyear) |>
  summarise(
    p25    = quantile(ratio_wins, 0.25, na.rm = TRUE),
    median = median(ratio_wins,        na.rm = TRUE),
    mean   = mean(ratio_wins,          na.rm = TRUE),
    p75    = quantile(ratio_wins, 0.75, na.rm = TRUE)
  )

ggplot(rd_ratio_dist, aes(x = fyear)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#2196F3", alpha = 0.2) +
  geom_line(aes(y = median, color = "Median"), linewidth = 0.9) +
  geom_line(aes(y = mean,   color = "Mean"),   linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c(
    "Median" = "#2196F3",
    "Mean"   = "#E91E63"
  )) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "R&D Intensity Across U.S. Firms (1960–2025)",
    subtitle = "Winsorized R&D/Assets ratio — shaded band shows 25th–75th percentile",
    x        = "Fiscal Year",
    y        = "R&D / Total Assets",
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
# TASK 3: Firm counts and share reporting positive R&D
# =============================================================================

rd_firms <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  group_by(fyear) |>
  summarise(
    n_total    = n_distinct(gvkey),
    n_positive = n_distinct(gvkey[xrd > 0]),
    pct        = n_positive / n_total
  )

p1 <- ggplot(rd_firms, aes(x = fyear, y = n_positive)) +
  geom_line(color = "#2196F3", linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Number of Firms Reporting Positive R&D",
    x     = "Fiscal Year",
    y     = "Number of Firms"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p2 <- ggplot(rd_firms, aes(x = fyear, y = pct)) +
  geom_line(color = "#E91E63", linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Firms Reporting Positive R&D",
    x     = "Fiscal Year",
    y     = "% of All Compustat Firms"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p1 + p2 +
  plot_annotation(
    title    = "R&D Reporting Across U.S. Firms (1960–2025)",
    subtitle = "Left: raw count — Right: share of all Compustat firms",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  )

