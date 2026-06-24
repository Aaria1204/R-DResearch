library(dplyr)
library(here)

#Data Checks 

dat <- readRDS(here("adjusted_rd.rds"))

# Extract 2- and 3-digit NAICS
dat <- dat |>
  mutate(
    naics2 = substr(as.character(naics), 1, 2),
    naics3 = substr(as.character(naics), 1, 3)
  )

# --- 1. Top 5 sectors (2-digit) by avg winsorized intensity ---
# (quick approximation — no winsorization yet, just to see who comes out on top)
top5_check <- dat |>
  filter(!is.na(xrd), xrd > 0, !is.na(at), at > 0, !is.na(naics2)) |>
  arrange(gvkey, fyear) |>
  group_by(gvkey) |>
  mutate(at_lag = lag(at)) |>
  ungroup() |>
  filter(!is.na(at_lag), at_lag > 0) |>
  mutate(intensity = xrd / at_lag) |>
  filter(intensity <= 1) |>  # same outlier rule as Week 3
  group_by(naics2, fyear) |>
  summarise(n_firms = n(), med_intensity = median(intensity), .groups = "drop") |>
  filter(n_firms >= 10) |>
  group_by(naics2) |>
  summarise(avg_firms = mean(n_firms), avg_intensity = mean(med_intensity), .groups = "drop") |>
  filter(avg_firms >= 20) |>
  arrange(desc(avg_intensity)) |>
  slice_head(n = 8)  # show top 8 so you can see who's just outside top 5

print(top5_check)

# --- 2. 3-digit firm counts within those top sectors ---
top5_naics2 <- top5_check$naics2[1:5]

naics3_coverage <- dat |>
  filter(naics2 %in% top5_naics2, !is.na(xrd), xrd > 0, !is.na(at), at > 0) |>
  arrange(gvkey, fyear) |>
  group_by(gvkey) |>
  mutate(at_lag = lag(at)) |>
  ungroup() |>
  filter(!is.na(at_lag)) |>
  mutate(intensity = xrd / at_lag) |>
  filter(intensity <= 1) |>
  group_by(naics2, naics3) |>
  summarise(
    n_firm_years = n(),
    avg_firms_per_year = n_distinct(gvkey) / n_distinct(fyear),
    avg_intensity = mean(intensity),
    .groups = "drop"
  ) |>
  arrange(naics2, desc(avg_intensity))

print(naics3_coverage, n = 60)

# --- 3. Decade coverage across all sectors ---
decade_coverage <- dat |>
  filter(!is.na(xrd), xrd > 0, !is.na(at), at > 0, !is.na(naics2)) |>
  mutate(decade = paste0(floor(fyear / 10) * 10, "s")) |>
  group_by(naics2, decade) |>
  summarise(n_firms = n_distinct(gvkey), .groups = "drop") |>
  tidyr::pivot_wider(names_from = decade, values_from = n_firms, values_fill = 0)

print(decade_coverage, n = 30)

# =============================================================================
# 04_week4.R
# Week 4: Deeper industry drill-down + decade-level rankings
#
# Builds directly on 03_week3.R — sources it to reuse:
#   naics_labels, data_clean, outlier_gvkeys, top5_sectors
#
# PART A: 3-digit drill-down within top 5 sectors
#   - Top 5 subsectors per sector selected by avg_firms_per_year (data-driven)
#   - Winsorized median intensity over time, one plot per sector
#
# PART B: Decade comparison — bump chart of sector rankings
#   - All sectors ranked by avg winsorized median intensity per decade
#   - Sectors appearing in top 10 in at least one decade shown
#   - Starts 1970s (pre-1970 coverage too sparse)
#
# Input:  adjusted_rd.rds (via 03_week3.R)
# =============================================================================

source(here::here("03_week3.R"))
dev.new()  # force a fresh device so subsequent plots aren't swallowed

# -- Add 3-digit NAICS to data_clean ------------------------------------------

data_clean <- data_clean |>
  mutate(naics3 = substr(as.character(naics), 1, 3))

# -- 3-digit label lookup -----------------------------------------------------
# Extend as needed; unlabelled codes fall back to "NAICS XXX"

naics3_labels <- tibble(
  naics3 = c(
    "325","326","323","322","324","327","321",
    "334","333","339","335","336","332","331","337",
    "511","514","519","513","518","516","512",
    "524","522","523","521","525",
    "541","542","543","551","561"
  ),
  subsector = c(
    "Chemical Mfg","Plastics & Rubber","Printing","Paper",
    "Petroleum","Nonmetallic Mineral","Wood",
    "Computer & Electronics","Industrial Machinery","Misc Mfg",
    "Electrical Equipment","Transportation Equip",
    "Fabricated Metal","Primary Metal","Furniture",
    "Publishing (incl. software)","Data Processing",
    "Other Information","Broadcasting","Telecom",
    "Internet & Publishing","Motion Picture",
    "Insurance","Credit Intermediation","Securities",
    "Monetary Auth.","Funds & Trusts",
    "Professional & Tech Services","R&D Services","Advertising",
    "Mgmt of Companies","Admin & Support"
  )
)

# =============================================================================
# PART A: 3-digit drill-down within top 5 sectors — all subsectors, no guards
# =============================================================================

# All subsector-year medians, no firm count restriction
subsector_year <- data_clean |>
  filter(sector %in% top5_sectors, !is.na(naics3)) |>
  group_by(sector, naics3, fyear) |>
  summarise(
    n_firms   = n(),
    med_ratio = median(ratio, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  left_join(naics3_labels, by = "naics3") |>
  mutate(subsector = coalesce(subsector, paste0("NAICS ", naics3)))

cat("\n=== Part A: Subsectors within top 5 sectors ===\n")
subsector_year |>
  distinct(sector, naics3, subsector) |>
  arrange(sector, naics3) |>
  print(n = 60)

# One plot per sector — all subsectors as colored lines
# Using scale_color_viridis for many categories
for (sec in top5_sectors) {
  
  plot_data <- subsector_year |> filter(sector == sec)
  if (nrow(plot_data) == 0) {
    cat("No data for sector:", sec, "\n")
    next
  }
  
  n_subsectors <- n_distinct(plot_data$subsector)
  
  p <- ggplot(plot_data,
              aes(x = fyear, y = med_ratio, color = subsector)) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_color_viridis_d(option = "turbo") +
    labs(
      title    = paste0("R&D Intensity: ", sec),
      subtitle = "Median R&D(t)/Assets(t-1) by 3-digit NAICS subsector — all subsectors shown",
      x        = "Fiscal Year",
      y        = "Median R&D / Total Assets",
      color    = "Subsector"
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 10, color = "gray40"),
      legend.position = "bottom",
      legend.text     = element_text(size = 7),
      axis.text.x     = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = guide_legend(nrow = ceiling(n_subsectors / 3)))
  
  print(p)
}


 # =============================================================================
# PART B: Decade comparison — bump chart of sector rankings
# =============================================================================

# Sector-year summary (reuse winsorized ratio_wins from data_clean)
sector_year_summary <- data_clean |>
  filter(!is.na(sector), sector != "Unknown") |>
  group_by(sector, fyear) |>
  filter(n() >= 10) |>
  summarise(
    med_ratio = median(ratio_wins, na.rm = TRUE),
    n_firms   = n(),
    .groups   = "drop"
  )

# Decade-level avg of yearly medians, then rank within decade
decade_ranks <- sector_year_summary |>
  filter(fyear >= 1970) |>
  mutate(decade = paste0(floor(fyear / 10) * 10, "s")) |>
  group_by(sector, decade) |>
  summarise(
    decade_intensity = mean(med_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(decade) |>
  mutate(rank = rank(-decade_intensity, ties.method = "min")) |>
  ungroup() |>
  # Keep only sectors that reach top 10 in at least one decade
  group_by(sector) |>
  filter(min(rank) <= 10) |>
  ungroup()

cat("\n=== Part B: Sector Rankings by Decade ===\n")
decade_ranks |>
  arrange(decade, rank) |>
  select(decade, rank, sector, decade_intensity) |>
  print(n = 80)

# Bump chart
p_bump <- ggplot(decade_ranks,
                 aes(x = decade, y = rank,
                     group = sector, color = sector)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  geom_text(
    data = decade_ranks |> filter(decade == min(decade)),
    aes(label = sector), hjust = 1.08, size = 2.6
  ) +
  geom_text(
    data = decade_ranks |> filter(decade == max(decade)),
    aes(label = sector), hjust = -0.08, size = 2.6
  ) +
  scale_y_reverse(breaks = 1:10) +
  scale_color_brewer(palette = "Paired") +
  labs(
    title    = "Sector R&D Intensity Rankings by Decade (1970s–2020s)",
    subtitle = "Rank based on avg winsorized median R&D/Assets; top 10 in any decade shown",
    x        = "Decade",
    y        = "Rank (1 = highest intensity)"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "none",
    plot.margin   = margin(10, 140, 10, 140)
  )

print(p_bump)
