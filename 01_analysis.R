
library(dplyr)

data_adj <- readRDS("adjusted_rd.rds")

# Load the saved adjusted dataset
data_adj <- readRDS("adjusted_rd.rds")

# Keep only relevant columns
data_slim <- data_adj |>
  select(gvkey, fyear, conm, naics, at, xrd,
         at_PPI, at_GDP_DEF,
         xrd_PPI, xrd_GDP_DEF)

# Save slim version
saveRDS(data_slim, "adjusted_rd_slim.rds")

#Descriptions 
attr(data_slim$gvkey,       "label") <- "Firm identifier (Compustat)"
attr(data_slim$fyear,       "label") <- "Fiscal year"
attr(data_slim$naics,       "label") <- "North American Industry Classification Code"
attr(data_slim$at,          "label") <- "Total assets (nominal, $M)"
attr(data_slim$xrd,         "label") <- "R&D expense (nominal, $M)"
attr(data_slim$at_PPI,      "label") <- "Total assets adjusted to 2025 dollars using PPI"
attr(data_slim$at_GDP_DEF,  "label") <- "Total assets adjusted to 2025 dollars using GDP deflator"
attr(data_slim$xrd_PPI,     "label") <- "R&D expense adjusted to 2025 dollars using PPI"
attr(data_slim$xrd_GDP_DEF, "label") <- "R&D expense adjusted to 2025 dollars using GDP deflator"

library(dplyr)
library(ggplot2)
library(tidyr)

# 1a. Aggregate R&D across firms each year
rd_timeseries <- data_slim |>
  group_by(fyear) |>
  summarise(
    xrd = sum(xrd,         na.rm = TRUE),
    xrd_PPI     = sum(xrd_PPI,     na.rm = TRUE),
    xrd_GDP_DEF = sum(xrd_GDP_DEF, na.rm = TRUE)
  ) |>
  filter(fyear >= 1960, fyear <= 2025)

# 1b. Pivot to long format for ggplot
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
# 1c. Plot
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

# 2a. Compute R&D/Assets ratio for each firm-year
rd_ratio <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  mutate(
    ratio_nominal = xrd         / at,
    ratio_PPI     = xrd_PPI     / at_PPI,
    ratio_GDP_DEF = xrd_GDP_DEF / at_GDP_DEF
  ) |>
  # Remove infinite/NaN values (firms with 0 or NA assets)
  mutate(across(starts_with("ratio_"), ~ ifelse(is.finite(.), ., NA)))

# 2b. Distributional parameters across firms each year
rd_ratio_dist <- rd_ratio |>
  group_by(fyear) |>
  summarise(
    across(
      c(ratio_nominal, ratio_PPI, ratio_GDP_DEF),
      list(
        p25    = ~ quantile(., 0.25, na.rm = TRUE),
        median = ~ median(.,        na.rm = TRUE),
        mean   = ~ mean(.,          na.rm = TRUE),
        p75    = ~ quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# 2c. Plot the average ratio for each deflator
rd_ratio_mean <- rd_ratio_dist |>
  select(fyear, ratio_nominal_mean, ratio_PPI_mean, ratio_GDP_DEF_mean) |>
  pivot_longer(
    cols      = -fyear,
    names_to  = "deflator",
    values_to = "mean_ratio"
  ) |>
  mutate(deflator = recode(deflator,
                           "ratio_nominal_mean" = "Nominal",
                           "ratio_PPI_mean"     = "PPI",
                           "ratio_GDP_DEF_mean" = "GDP Deflator"
  ))

ggplot(rd_ratio_mean, aes(x = fyear, y = mean_ratio, color = deflator)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    "Nominal"      = "gray50",
    "PPI"          = "#E91E63",
    "GDP Deflator" = "#4CAF50"
  )) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Average R&D Intensity Across U.S. Firms (1960–2025)",
    subtitle = "Mean R&D/Assets ratio across all Compustat firms by year",
    x        = "Fiscal Year",
    y        = "Mean R&D / Total Assets",
    color    = "Deflator"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

#trying winsorization to handle outliers in the R&D/Assets ratio
rd_ratio <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  mutate(ratio = xrd / at) |>
  mutate(ratio = ifelse(is.finite(ratio), ratio, NA)) |>
  mutate(
    p01 = quantile(ratio, 0.01, na.rm = TRUE),
    p99 = quantile(ratio, 0.99, na.rm = TRUE),
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
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )


# 3a. Count and percentage of firms reporting positive R&D each year
rd_firms <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  group_by(fyear) |>
  summarise(
    n_total    = n_distinct(gvkey),
    n_positive = n_distinct(gvkey[xrd > 0]),
    pct        = n_positive / n_total
  )

# 3b. Plot both side by side
library(patchwork)

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


# NAICS 2-digit sector lookup table
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

# Add 2-digit NAICS and join labels
data_slim <- data_slim |>
  mutate(
    naics2 = substr(as.character(naics), 1, 2),
    naics2 = ifelse(is.na(naics) | naics2 == "NA", "Unknown", naics2)
  ) |>
  left_join(naics_labels, by = "naics2") |>
  mutate(sector = ifelse(is.na(sector), "Unknown", sector))


#Task 4: R&D by industry over time
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


#concise version 
# Find top 10 industries by total R&D
top_sectors <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025) |>
  group_by(sector) |>
  summarise(total_xrd = sum(xrd_GDP_DEF, na.rm = TRUE)) |>
  slice_max(total_xrd, n = 10) |>
  pull(sector)

top5_sectors <- data_slim |>
  filter(fyear >= 1960, fyear <= 2025, sector != "Unknown") |>
  group_by(sector) |>
  summarise(total_xrd = sum(xrd_GDP_DEF, na.rm = TRUE)) |>
  slice_max(total_xrd, n = 5) |>
  pull(sector)

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

rd_ratio_industry <- rd_ratio |>
  left_join(
    data_slim |> select(gvkey, fyear, sector) |> distinct(),
    by = c("gvkey", "fyear"),
    relationship = "many-to-many"
  ) |>
  group_by(fyear, sector) |>
  summarise(
    mean   = mean(ratio,           na.rm = TRUE),
    median = median(ratio,         na.rm = TRUE),
    p25    = quantile(ratio, 0.25, na.rm = TRUE),
    p75    = quantile(ratio, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Task 5
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


#Task 6 
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




