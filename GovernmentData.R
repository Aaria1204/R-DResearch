# =============================================================================
# 04_ncses_pie.R
# NCSES National Patterns Table 2 — Sector share pie charts by decade
# Sectors: All Federal, Total Business, Higher Education, Nonprofit
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. LOAD DATAr
# =============================================================================
# !! Set your file path here !!


# Table 2 has 3 header rows — skip them and assign names manually
# Column structure (constant 2017 $M, which is what we want):
#   Col 1  = Year
#   Col 2  = All performers total (current $)
#   Col 3  = All federal total (current $)
#   ...
# The constant 2017 $ block mirrors the current $ block — check your sheet.
# We'll read raw first so you can inspect the structure.

file_path <- here("NSFGovDataT2.xlsx")

raw <- read_excel(file_path, sheet = 1, skip = 3, col_names = FALSE)

# Inspect: run this to see what you have before proceeding
# View(raw)
# names(raw)

# =============================================================================
# 2. EXTRACT COLUMNS
# =============================================================================
# Once you've inspected, identify the correct column indices for:
#   - Year
#   - All Federal (constant 2017 $M)
#   - Total Business (constant 2017 $M)
#   - Higher Education (constant 2017 $M)
#   - Nonprofit (constant 2017 $M)
#
# Update the column numbers below to match your sheet.

rd_raw <- raw |>
  select(
    year        = 1,   # <-- Year column
    federal     = 3,   # <-- All Federal, constant 2017 $M
    business    = 11,  # <-- Total Business, constant 2017 $M
    higher_ed   = 19,  # <-- Higher Education, constant 2017 $M
    nonprofit   = 24   # <-- Nonprofit, constant 2017 $M
  ) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(year), year >= 1953)

# =============================================================================
# 3. ASSIGN DECADES
# =============================================================================
rd_decade <- rd_raw |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade) |>
  summarise(
    federal   = mean(federal,   na.rm = TRUE),
    business  = mean(business,  na.rm = TRUE),
    higher_ed = mean(higher_ed, na.rm = TRUE),
    nonprofit = mean(nonprofit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = c(federal, business, higher_ed, nonprofit),
    names_to  = "sector",
    values_to = "value"
  ) |>
  mutate(
    sector = recode(sector,
                    federal   = "Federal",
                    business  = "Business",
                    higher_ed = "Higher Ed",
                    nonprofit = "Nonprofit"
    ),
    decade = factor(decade, levels = sort(unique(decade)))
  )

# =============================================================================
# 4. PIE CHART SUBPLOT
# =============================================================================
sector_colors <- c(
  "Federal"   = "#3182bd",
  "Business"  = "#bd3131",
  "Higher Ed" = "#31a854",
  "Nonprofit" = "#8e5bbf"
)

decades  <- levels(rd_decade$decade)
n        <- length(decades)
ncols    <- 4
nrows    <- ceiling(n / ncols)

p2 <- plot_ly()

for (i in seq_along(decades)) {
  d     <- rd_decade |> filter(decade == decades[i])
  col_i <- ((i - 1) %% ncols) + 1
  row_i <- ceiling(i / ncols)
  
  x_gap  <- 0.02
  y_gap  <- 0.10
  cell_w <- (1 - x_gap * (ncols + 1)) / ncols
  cell_h <- (1 - y_gap * (nrows + 1)) / nrows
  
  x0 <- x_gap * col_i + cell_w * (col_i - 1)
  x1 <- x0 + cell_w
  y0 <- 1 - (y_gap * row_i + cell_h * row_i)
  y1 <- y0 + cell_h
  
  p2 <- p2 |> add_trace(
    labels        = d$sector,
    values        = d$value,
    type          = "pie",
    name          = decades[i],
    title         = list(text = paste0("<b>", decades[i], "</b>"),
                         font = list(size = 13)),
    domain        = list(x = c(x0, x1), y = c(y0, y1)),
    marker        = list(colors = unname(sector_colors[d$sector])),
    textinfo      = "percent",
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "$%{value:,.0f}M (2017 $)<br>",
      "%{percent}<extra>", decades[i], "</extra>"
    ),
    legendgroup   = d$sector,   # <-- added here
    showlegend    = (i == 1)
  )
}

p2 <- p2 |>
  layout(
    title = list(
      text = "<b>U.S. R&D Expenditure Share by Sector — By Decade</b><br><sup>Decade averages, Constant 2017 $M — NCSES National Patterns Table 2</sup>",
      x    = 0.05
    ),
    legend = list(
      title       = list(text = "<b>Sector</b>"),
      orientation = "h",
      x           = 0.5,
      xanchor     = "center",
      y           = -0.05
    ),
    margin = list(t = 80, b = 60)
  )

p2


# =============================================================================
# GovernmentData.R
# NCSES National Patterns — Stacked bar: sector × R&D type, all years
#
# Sources:
#   T2  = total R&D by performer (used for sector totals reference)
#   T3  = basic research by performer
#   T4  = applied research by performer
#   T5  = experimental development by performer
#
# Sectors: Federal, Business, Higher Ed, Nonprofit
# All values: constant 2017 $M
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. LOAD DATA — all four files from project root
# =============================================================================
# Run View(raw_t3) etc. after loading to confirm column positions

read_ncses <- function(filename) {
  read_excel(here(filename), sheet = 1, skip = 3, col_names = FALSE)
}

raw_t3 <- read_ncses("NSFGovDataT3.xlsx")  # Basic research
raw_t4 <- read_ncses("NSFGovDataT4.xlsx")  # Applied research
raw_t5 <- read_ncses("NSFGovDataT5.xlsx")  # Experimental development

# =============================================================================
# 2. EXTRACT COLUMNS
# =============================================================================
# !! After running View(raw_t3), confirm these column indices match your sheet.
# The four performer columns (constant 2017 $M) should be the same across T3-T5.
#
# Typical NCSES Table 3/4/5 layout (constant 2017 $ block):
#   Col 1  = Year
#   Col 3  = All Federal
#   Col 11 = Total Business
#   Col 19 = Higher Education
#   Col 24 = Nonprofit
#
# Update below if your columns differ.

extract_cols <- function(raw, rd_type_label) {
  raw |>
    select(
      year      = 1,
      federal   = 3,   # All Federal
      business  = 11,  # Total Business
      higher_ed = 19,  # Higher Education
      nonprofit = 24   # Nonprofit
    ) |>
    mutate(
      across(everything(), as.numeric),
      rd_type = rd_type_label
    ) |>
    filter(!is.na(year), year >= 1953)
}

basic  <- extract_cols(raw_t3, "Basic")
applied <- extract_cols(raw_t4, "Applied")
experimental <- extract_cols(raw_t5, "Experimental")

# =============================================================================
# 3. COMBINE AND RESHAPE
# =============================================================================
rd_all <- bind_rows(basic, applied, experimental) |>
  pivot_longer(
    cols      = c(federal, business, higher_ed, nonprofit),
    names_to  = "sector",
    values_to = "value"
  ) |>
  mutate(
    sector = recode(sector,
                    federal   = "Federal",
                    business  = "Business",
                    higher_ed = "Higher Ed",
                    nonprofit = "Nonprofit"
    ),
    sector  = factor(sector,  levels = c("Federal", "Business", "Higher Ed", "Nonprofit")),
    rd_type = factor(rd_type, levels = c("Basic", "Applied", "Experimental")),
    # Combined label for legend grouping
    group   = paste0(sector, " — ", rd_type)
  )

# =============================================================================
# 4. COLOR PALETTE
# =============================================================================
# Hue = sector, lightness = R&D type (dark → basic, mid → applied, light → experimental)

palette <- c(
  "Federal — Basic"          = "#1a4f7a",
  "Federal — Applied"        = "#3182bd",
  "Federal — Experimental"   = "#6baed6",
  "Business — Basic"         = "#7a1a1a",
  "Business — Applied"       = "#bd3131",
  "Business — Experimental"  = "#d6806b",
  "Higher Ed — Basic"        = "#1a6b2e",
  "Higher Ed — Applied"      = "#31a854",
  "Higher Ed — Experimental" = "#74c87a",
  "Nonprofit — Basic"        = "#3d1a7a",
  "Nonprofit — Applied"      = "#8e5bbf",
  "Nonprofit — Experimental" = "#b99fd6"
)

# =============================================================================
# 5. BUILD STACKED BAR CHART
# =============================================================================
p <- plot_ly()

for (grp in names(palette)) {
  d <- rd_all |> filter(group == grp)
  if (nrow(d) == 0) next
  
  p <- p |> add_trace(
    data          = d,
    x             = ~year,
    y             = ~value,
    type          = "bar",
    name          = grp,
    marker        = list(color = palette[[grp]]),
    legendgroup   = grp,
    hovertemplate = paste0(
      "<b>", grp, "</b><br>",
      "Year: %{x}<br>",
      "$%{y:,.0f}M (2017 $)<extra></extra>"
    )
  )
}

p <- p |>
  layout(
    barmode = "stack",
    title   = list(
      text = "<b>U.S. R&D Expenditures by Sector and R&D Type</b><br><sup>Constant 2017 $M — All Years — NCSES National Patterns</sup>",
      x    = 0.05
    ),
    xaxis = list(
      title    = "Year",
      tickmode = "linear",
      dtick    = 5,
      tickangle = -45
    ),
    yaxis = list(
      title      = "R&D Expenditures (Constant 2017 $M)",
      tickformat = ",.0f"
    ),
    legend = list(
      title       = list(text = "<b>Sector — R&D Type</b>"),
      orientation = "v",
      x           = 1.01,
      y           = 1,
      tracegroupgap = 5
    ),
    margin  = list(r = 220, b = 80),
    bargap  = 0.1
  )

p

# =============================================================================
# GovernmentData.R
# NCSES National Patterns — Stacked bar: sector × R&D type, all years
#
# Sources:
#   T2  = total R&D by performer (used for sector totals reference)
#   T3  = basic research by performer
#   T4  = applied research by performer
#   T5  = experimental development by performer
#
# Sectors: Federal, Business, Higher Ed, Nonprofit
# All values: constant 2017 $M
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. LOAD DATA — all four files from project root
# =============================================================================
# Run View(raw_t3) etc. after loading to confirm column positions

read_ncses <- function(filename) {
  read_excel(here(filename), sheet = 1, skip = 3, col_names = FALSE)
}

raw_t3 <- read_ncses("NSFGovDataT3.xlsx")  # Basic research
raw_t4 <- read_ncses("NSFGovDataT4.xlsx")  # Applied research
raw_t5 <- read_ncses("NSFGovDataT5.xlsx")  # Experimental development

# =============================================================================
# 2. EXTRACT COLUMNS
# =============================================================================
# !! After running View(raw_t3), confirm these column indices match your sheet.
# The four performer columns (constant 2017 $M) should be the same across T3-T5.
#
# Typical NCSES Table 3/4/5 layout (constant 2017 $ block):
#   Col 1  = Year
#   Col 3  = All Federal
#   Col 11 = Total Business
#   Col 19 = Higher Education
#   Col 24 = Nonprofit
#
# Update below if your columns differ.

extract_cols <- function(raw, rd_type_label) {
  raw |>
    select(
      year      = 1,
      federal   = 3,   # All Federal
      business  = 11,  # Total Business
      higher_ed = 19,  # Higher Education
      nonprofit = 24   # Nonprofit
    ) |>
    mutate(
      across(everything(), as.numeric),
      rd_type = rd_type_label
    ) |>
    filter(!is.na(year), year >= 1953)
}

basic  <- extract_cols(raw_t3, "Basic")
applied <- extract_cols(raw_t4, "Applied")
experimental <- extract_cols(raw_t5, "Experimental")

# =============================================================================
# 3. COMBINE AND RESHAPE
# =============================================================================
rd_all <- bind_rows(basic, applied, experimental) |>
  pivot_longer(
    cols      = c(federal, business, higher_ed, nonprofit),
    names_to  = "sector",
    values_to = "value"
  ) |>
  mutate(
    sector = recode(sector,
                    federal   = "Federal",
                    business  = "Business",
                    higher_ed = "Higher Ed",
                    nonprofit = "Nonprofit"
    ),
    sector  = factor(sector,  levels = c("Federal", "Business", "Higher Ed", "Nonprofit")),
    rd_type = factor(rd_type, levels = c("Basic", "Applied", "Experimental")),
    # Combined label for legend grouping
    group   = paste0(sector, " — ", rd_type)
  )

# =============================================================================
# 4. COLOR PALETTE — R&D type only (shared across all 4 sector charts)
# =============================================================================

# =============================================================================
# 5. BUILD ONE BAR CHART PER SECTOR
# =============================================================================
# Colors by R&D type only (consistent across all 4 charts)
type_colors <- c(
  "Basic"        = "#1f77b4",
  "Applied"      = "#ff7f0e",
  "Experimental" = "#2ca02c"
)

make_sector_chart <- function(sector_name) {
  d <- rd_all |> filter(sector == sector_name)
  
  p <- plot_ly()
  
  for (rtype in c("Basic", "Applied", "Experimental")) {
    dt <- d |> filter(rd_type == rtype)
    p <- p |> add_trace(
      data          = dt,
      x             = ~year,
      y             = ~value,
      type          = "bar",
      name          = rtype,
      marker        = list(color = type_colors[[rtype]]),
      hovertemplate = paste0(
        "<b>", rtype, "</b><br>",
        "Year: %{x}<br>",
        "$%{y:,.0f}M (2017 $)<extra></extra>"
      )
    )
  }
  
  p |> layout(
    barmode = "stack",
    title   = list(
      text = paste0("<b>", sector_name, " R&D Expenditures by Type</b><br>",
                    "<sup>Constant 2017 $M — All Years — NCSES National Patterns</sup>"),
      x    = 0.05
    ),
    xaxis = list(
      title     = "Year",
      tickmode  = "linear",
      dtick     = 5,
      tickangle = -45
    ),
    yaxis = list(
      title      = "R&D Expenditures (Constant 2017 $M)",
      tickformat = ",.0f"
    ),
    legend = list(
      title       = list(text = "<b>R&D Type</b>"),
      orientation = "h",
      x           = 0.5,
      xanchor     = "center",
      y           = -0.2
    ),
    margin = list(b = 100),
    bargap = 0.1
  )
}

# Render all four — run each line individually or together
p_federal    <- make_sector_chart("Federal")
p_business   <- make_sector_chart("Business")
p_higher_ed  <- make_sector_chart("Higher Ed")
p_nonprofit  <- make_sector_chart("Nonprofit")

p_federal
p_business
p_higher_ed
p_nonprofit

# =============================================================================
# GovernmentData.R  — Table 7 section
# NCSES Basic Research: source of funds × performer
# Two charts:
#   Chart A: bars by funder, stacked by performer
#   Chart B: bars by performer, stacked by funder
# Decade averages, constant 2017 $M
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. LOAD TABLE 7
# =============================================================================
raw_t7 <- read_excel(here("NSFGovDataT7.xlsx"), sheet = 1, skip = 3, col_names = FALSE)

# View(raw_t7)  # <-- run this first to confirm column positions

# =============================================================================
# 2. EXTRACT COLUMNS (constant 2017 $M)
# =============================================================================
# Table 7 layout: rows = years, columns = performers nested under funders
# Typical structure:
#   Col 1  = Year
#   Funder groups (each with performer sub-columns):
#     Federal funder:
#       Col 3  = Federal intramural performer
#       Col 4  = FFRDC performer
#       Col 5  = Business performer
#       Col 6  = Higher Ed performer
#       Col 7  = Nonprofit performer
#     Business funder:
#       Col 9  = Business performer
#       Col 10 = Higher Ed performer
#     Higher Ed funder:
#       Col 12 = Higher Ed performer
#     Nonprofit funder:
#       Col 14 = Nonprofit performer
#       Col 15 = Higher Ed performer
#
# !! Update column numbers after running View(raw_t7) !!

t7 <- raw_t7 |>
  select(
    year                        = 1,
    # Federal funder → performers
    fed_to_fed                  = 3,
    fed_to_ffrdc                = 4,
    fed_to_business             = 5,
    fed_to_higher_ed            = 6,
    fed_to_nonprofit            = 7,
    # Business funder → performers
    business_to_business        = 9,
    business_to_higher_ed       = 10,
    # Higher Ed funder → performers
    higher_ed_to_higher_ed      = 12,
    # Nonprofit funder → performers
    nonprofit_to_nonprofit      = 14,
    nonprofit_to_higher_ed      = 15
  ) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(year), year >= 1953)

# =============================================================================
# 3. RESHAPE TO LONG: funder × performer × year
# =============================================================================
t7_long <- t7 |>
  pivot_longer(
    cols      = -year,
    names_to  = "flow",
    values_to = "value"
  ) |>
  separate(flow, into = c("funder", "performer"), sep = "_to_") |>
  mutate(
    funder = recode(funder,
                    fed        = "Federal",
                    business   = "Business",
                    higher_ed  = "Higher Ed",
                    nonprofit  = "Nonprofit"
    ),
    performer = recode(performer,
                       fed        = "Federal",
                       ffrdc      = "FFRDC",
                       business   = "Business",
                       higher_ed  = "Higher Ed",
                       nonprofit  = "Nonprofit"
    )
  )

# =============================================================================
# 4. DECADE AVERAGES
# =============================================================================
t7_decade <- t7_long |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade, funder, performer) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(decade = factor(decade, levels = sort(unique(decade))))

# =============================================================================
# 5. COLOR PALETTES
# =============================================================================
performer_colors <- c(
  "Federal"   = "#1a4f7a",
  "FFRDC"     = "#6baed6",
  "Business"  = "#bd3131",
  "Higher Ed" = "#31a854",
  "Nonprofit" = "#8e5bbf"
)

funder_colors <- c(
  "Federal"   = "#3182bd",
  "Business"  = "#e06c6c",
  "Higher Ed" = "#74c87a",
  "Nonprofit" = "#b99fd6"
)

# =============================================================================
# 6. CHART A: bars by funder, stacked by performer
#    "Where does each funder send basic research money?"
# =============================================================================
make_chart_a <- function() {
  p <- plot_ly()
  
  for (perf in names(performer_colors)) {
    d <- t7_decade |> filter(performer == perf)
    if (all(is.na(d$value))) next
    
    p <- p |> add_trace(
      data          = d,
      x             = ~decade,
      y             = ~value,
      type          = "bar",
      name          = perf,
      legendgroup   = perf,
      marker        = list(color = performer_colors[[perf]]),
      facet         = ~funder,
      hovertemplate = paste0(
        "<b>Performer: ", perf, "</b><br>",
        "Funder: %{customdata}<br>",
        "Decade: %{x}<br>",
        "$%{y:,.0f}M (2017 $)<extra></extra>"
      ),
      customdata    = ~funder
    )
  }
  
  p |> layout(
    barmode = "stack",
    title   = list(
      text = "<b>Basic Research: Where Does Each Funder Send Money?</b><br><sup>Bars = Funder, Stack = Performer — Decade Averages, Constant 2017 $M</sup>",
      x    = 0.05
    ),
    xaxis  = list(title = "Decade"),
    yaxis  = list(title = "R&D Expenditures (Constant 2017 $M)", tickformat = ",.0f"),
    legend = list(
      title       = list(text = "<b>Performer</b>"),
      orientation = "h",
      x = 0.5, xanchor = "center", y = -0.15
    ),
    margin = list(b = 100)
  )
}

# =============================================================================
# 7. CHART B: bars by performer, stacked by funder
#    "Where does each performer get its basic research funding from?"
# =============================================================================
make_chart_b <- function() {
  p <- plot_ly()
  
  for (fnd in names(funder_colors)) {
    d <- t7_decade |> filter(funder == fnd)
    if (all(is.na(d$value))) next
    
    p <- p |> add_trace(
      data          = d,
      x             = ~decade,
      y             = ~value,
      type          = "bar",
      name          = fnd,
      legendgroup   = fnd,
      marker        = list(color = funder_colors[[fnd]]),
      hovertemplate = paste0(
        "<b>Funder: ", fnd, "</b><br>",
        "Performer: %{customdata}<br>",
        "Decade: %{x}<br>",
        "$%{y:,.0f}M (2017 $)<extra></extra>"
      ),
      customdata    = ~performer
    )
  }
  
  p |> layout(
    barmode = "stack",
    title   = list(
      text = "<b>Basic Research: Where Does Each Performer Get Funding From?</b><br><sup>Bars = Performer, Stack = Funder — Decade Averages, Constant 2017 $M</sup>",
      x    = 0.05
    ),
    xaxis  = list(title = "Decade"),
    yaxis  = list(title = "R&D Expenditures (Constant 2017 $M)", tickformat = ",.0f"),
    legend = list(
      title       = list(text = "<b>Funder</b>"),
      orientation = "h",
      x = 0.5, xanchor = "center", y = -0.15
    ),
    margin = list(b = 100)
  )
}

# =============================================================================
# 8. RENDER
# =============================================================================
p_funder_to_performer   <- make_chart_a()
p_performer_from_funder <- make_chart_b()

p_funder_to_performer
p_performer_from_funder
