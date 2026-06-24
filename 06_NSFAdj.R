# =============================================================================
# 05_NSF.R
# NCSES National Patterns of R&D Resources
#
# Section 1: Table 2  — Sector share pie charts by decade
# Section 2: Tables 3/4/5 — Stacked bars: sector × R&D type (one chart per sector)
# Section 3: Table 7  — Basic research funder → performer flow (two charts)
#
# All values: constant 2017 $M
#
# Excel structure (confirmed from NSFGovDataT2.xlsx inspection):
#   Row 1:    "Table N"
#   Row 2:    full title
#   Row 3:    "(Millions of current and of constant 2017 dollars)"
#   Row 4:    top-level headers (merged cells appear as value + NAs)
#   Row 5:    sub-headers
#   Row 6:    "Current $millions"
#   Rows 7–N: current $ data (1953–2024)
#   Row N+1:  "Constant 2017 $millions"   <-- block divider
#   Rows N+2+: constant 2017 $ data (1953–2024)  <-- this is what we use
# =============================================================================

library(tidyverse)
library(readxl)
library(here)
library(plotly)

# =============================================================================
# HELPER 1: build_col_names()
#
# Reads the two header rows (rows 4 and 5) from the Excel file, fills merged
# cell NAs forward in row 4, then pastes row4 + row5 together to produce
# meaningful column names like "Businessd_Total" or "Higher educatione_Total".
# Col 1 is always renamed to "Year".
#
# No column positions are hardcoded — names come directly from the file.
# =============================================================================
build_col_names <- function(filename) {
  # Read rows 4, 5, 6 (skip=3 starts at row 4). Use n_max=3 to avoid
  # read_excel silently dropping trailing NA columns (known issue with n_max=1).
  raw_head <- read_excel(here(filename), sheet = 1, skip = 3,
                         col_names = FALSE, n_max = 3)
  
  h1 <- as.character(raw_head[1, ])  # row 4: top-level headers
  h2 <- as.character(raw_head[2, ])  # row 5: sub-headers
  
  # Fill NAs forward in h1 — merged cells only store the value in the
  # leftmost cell; the rest are NA. This restores the intended grouping.
  for (i in seq_along(h1)) {
    if (is.na(h1[i]) && i > 1) h1[i] <- h1[i - 1]
  }
  
  # Paste: "All federal" + "Totalg" → "All federal_Totalg"
  # Where h2 is NA (e.g. col 1 "Year"), just use h1 directly.
  merged    <- paste(h1, h2, sep = "_")
  merged    <- str_replace(merged, "_NA$", "")
  merged[1] <- "Year"
  
  merged
}

# =============================================================================
# HELPER 2: read_ncses()
#
# Loads a full NCSES table, assigns the merged column names from build_col_names,
# then finds the "Constant 2017 $millions" label row and keeps only the data
# below it — discarding the current $ block entirely.
#
# The label is matched with "^Constant 2017" to avoid accidentally matching
# the subtitle row 3 which also contains the word "constant".
# =============================================================================
read_ncses <- function(filename) {
  col_names <- build_col_names(filename)
  n_cols    <- length(col_names)
  
  # Read all data rows (skip=5 skips rows 1–5; row 6 "Current $millions"
  # becomes the first row and gets filtered out below as a non-numeric year)
  raw <- read_excel(here(filename), sheet = 1, skip = 5,
                    col_names = col_names,
                    col_types = rep("text", n_cols))
  
  # Find the constant 2017 $ block divider row
  label_row <- which(str_detect(raw$Year, regex("^Constant 2017", ignore_case = TRUE)))
  
  if (length(label_row) == 0) {
    stop(paste("Could not find 'Constant 2017 $millions' label in", filename,
               "— check that the file structure matches expectations."))
  }
  
  # Keep only rows after the label, convert to numeric, drop non-year rows
  raw |>
    slice((label_row + 1):n()) |>
    mutate(Year = suppressWarnings(as.numeric(Year))) |>
    mutate(across(-Year, suppressWarnings(as.numeric))) |>
    filter(!is.na(Year), Year >= 1953)
}

# =============================================================================
# HELPER 3: find_performer_cols()
#
# Returns the column names for the four performer totals by their known
# structural positions in the header (col 3, 9, 16, 22 — confirmed from
# inspection of NSFGovDataT2.xlsx). This avoids hardcoding the actual name
# strings (which contain footnote letters that may differ across files)
# while still being driven by the file's own headers.
# =============================================================================
find_performer_cols <- function(filename) {
  col_names <- build_col_names(filename)
  # Positions confirmed from file inspection:
  #   col 3  = All federal total
  #   col 9  = Business total
  #   col 16 = Higher education total
  #   col 22 = Nonprofit total
  list(
    federal   = col_names[3],
    business  = col_names[9],
    higher_ed = col_names[16],
    nonprofit = col_names[22]
  )
}

# =============================================================================
# HELPER 4: extract_performers()
#
# Selects year + the four performer total columns from a loaded NCSES table.
# Column names are looked up dynamically via find_performer_cols() so nothing
# is hardcoded. Optionally tags rows with an rd_type label (for T3/T4/T5).
# =============================================================================
extract_performers <- function(df, filename, rd_type_label = NULL) {
  cols <- find_performer_cols(filename)
  
  out <- df |>
    select(
      year      = Year,
      federal   = all_of(cols$federal),
      business  = all_of(cols$business),
      higher_ed = all_of(cols$higher_ed),
      nonprofit = all_of(cols$nonprofit)
    ) |>
    filter(!is.na(year))
  
  if (!is.null(rd_type_label)) out <- mutate(out, rd_type = rd_type_label)
  out
}

# =============================================================================
# SECTION 1: TABLE 2 — Pie charts by decade
#
# Data: constant 2017 $M performer totals, 1953–2024
# Method: group years into decades, take mean of each sector within each decade,
#         display as pie chart grid (4 columns × 2 rows = 8 pies)
# Note: 2020s average covers only 2020–2024 (5 years, not 10)
# =============================================================================
raw_t2 <- read_ncses("NSFGovDataT2.xlsx")
rd_raw <- extract_performers(raw_t2, "NSFGovDataT2.xlsx")

sector_colors <- c(
  "Federal"   = "#3182bd",
  "Business"  = "#bd3131",
  "Higher Ed" = "#31a854",
  "Nonprofit" = "#8e5bbf"
)

rd_decade <- rd_raw |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade) |>
  summarise(across(c(federal, business, higher_ed, nonprofit),
                   \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(cols      = c(federal, business, higher_ed, nonprofit),
               names_to  = "sector",
               values_to = "value") |>
  mutate(
    sector = recode(sector,
                    federal   = "Federal",
                    business  = "Business",
                    higher_ed = "Higher Ed",
                    nonprofit = "Nonprofit"),
    decade = factor(decade, levels = sort(unique(decade)))
  )

decades <- levels(rd_decade$decade)
n       <- length(decades)
ncols   <- 4
nrows   <- ceiling(n / ncols)

p2 <- plot_ly()

for (i in seq_along(decades)) {
  d     <- rd_decade |> filter(decade == decades[i])
  col_i <- ((i - 1) %% ncols) + 1
  row_i <- ceiling(i / ncols)
  
  x_gap  <- 0.02
  y_gap  <- 0.08
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
    showlegend = TRUE
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
      y           = -0.12
    ),
    margin = list(t = 80, b = 120)
  )

p2

# =============================================================================
# SECTION 2: TABLES 3/4/5 — Stacked bar charts by sector
#
# Data: T3 = basic research, T4 = applied research, T5 = experimental dev.
#       All constant 2017 $M, 1953–2024.
# Method: combine all three tables, stack R&D types within each sector,
#         produce one chart per sector (4 charts total).
# =============================================================================
raw_t3 <- read_ncses("NSFGovDataT3.xlsx")
raw_t4 <- read_ncses("NSFGovDataT4.xlsx")
raw_t5 <- read_ncses("NSFGovDataT5.xlsx")

rd_all <- bind_rows(
  extract_performers(raw_t3, "NSFGovDataT3.xlsx", "Basic"),
  extract_performers(raw_t4, "NSFGovDataT4.xlsx", "Applied"),
  extract_performers(raw_t5, "NSFGovDataT5.xlsx", "Experimental")
) |>
  pivot_longer(cols      = c(federal, business, higher_ed, nonprofit),
               names_to  = "sector",
               values_to = "value") |>
  mutate(
    sector = recode(sector,
                    federal   = "Federal",
                    business  = "Business",
                    higher_ed = "Higher Ed",
                    nonprofit = "Nonprofit"),
    sector  = factor(sector,  levels = c("Federal", "Business", "Higher Ed", "Nonprofit")),
    rd_type = factor(rd_type, levels = c("Basic", "Applied", "Experimental"))
  )

type_colors <- c(
  "Basic"        = "#1f77b4",
  "Applied"      = "#ff7f0e",
  "Experimental" = "#2ca02c"
)

make_sector_chart <- function(sector_name) {
  d <- rd_all |> filter(sector == sector_name)
  p <- plot_ly()
  for (rtype in levels(rd_all$rd_type)) {
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
    xaxis  = list(title = "Year", tickmode = "linear", dtick = 5, tickangle = -45),
    yaxis  = list(title = "R&D Expenditures (Constant 2017 $M)", tickformat = ",.0f"),
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

p_federal   <- make_sector_chart("Federal")
p_business  <- make_sector_chart("Business")
p_higher_ed <- make_sector_chart("Higher Ed")
p_nonprofit <- make_sector_chart("Nonprofit")

p_federal
p_business
p_higher_ed
p_nonprofit

t7_decade |> filter(decade == "2020s") |> pull(decade) |> unique()
t7_long |> filter(year >= 2020) |> distinct(year) |> arrange(year)


# =============================================================================
# Section 3: Table 7 — Basic Research Funder → Performer Sankey
# =============================================================================

# Load T7
t7_col_names <- build_col_names("NSFGovDataT7.xlsx")
raw_t7       <- read_ncses("NSFGovDataT7.xlsx")

# Identify flow columns programmatically:
# - exclude Year
# - exclude totals (names ending in "_Total" or containing "All performers")
# - everything remaining is a funder_performer flow column
flow_col_names <- t7_col_names[
  t7_col_names != "Year" &
    !str_ends(t7_col_names, "_Total") &
    !str_detect(t7_col_names, "All performers")
]

# Reshape to long — column names are already "Funder_Performer" format
t7_long <- raw_t7 |>
  select(Year, all_of(flow_col_names)) |>
  mutate(year = suppressWarnings(as.numeric(Year))) |>
  filter(!is.na(year)) |>
  select(-Year) |>
  pivot_longer(cols = -year, names_to = "flow", values_to = "value") |>
  # Split on first underscore to get funder and performer
  # e.g. "Nonfederal government_Higher education" → funder="Nonfederal government", performer="Higher education"
  separate(flow, into = c("funder", "performer"), sep = "_(?=[^_]+$)", extra = "merge") |>
  filter(!is.na(value), value > 0)

# Decade averages per flow
t7_decade <- t7_long |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade, funder, performer) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(decade = factor(decade, levels = sort(unique(decade))))

# Funder totals per decade — for hover % of funder total calculation
t7_funder_totals <- t7_decade |>
  group_by(decade, funder) |>
  summarise(funder_total = sum(value, na.rm = TRUE), .groups = "drop")

t7_decade <- t7_decade |>
  left_join(t7_funder_totals, by = c("decade", "funder")) |>
  mutate(pct_of_funder = value / funder_total * 100)

# Verify funder/performer combinations look right — uncomment to check:
t7_decade |> filter(decade == "2010s") |> select(funder, performer, value) |> print(n = 30)

# =============================================================================
# NODE SETUP
# Funder nodes get " (funder)" suffix internally to prevent plotly from
# collapsing same-name funder/performer pairs into self-loops.
# Display labels strip this suffix.
# =============================================================================
funders    <- t7_decade |> distinct(funder)    |> pull(funder)
performers <- c("Higher education", "Federal intramural", "FFRDC",
                "Business", "Nonprofit", "Nonfederal government")

funder_nodes   <- paste0(funders, " (funder)")
all_nodes      <- c(funder_nodes, performers)
display_labels <- str_remove(all_nodes, " \\(funder\\)")

funder_colors <- c(
  "Federal"               = "#3182bd",
  "Nonfederal government" = "#74c87a",
  "Business"              = "#bd3131",
  "Higher education"      = "#e6a817",
  "Nonprofit"             = "#8e5bbf"
)

node_colors <- c(
  unname(funder_colors[funders]),
  rep("#cccccc", length(performers))
)

# =============================================================================
# BUILD INTERACTIVE SANKEY — default 2010s, dropdown for other decades
# =============================================================================
decades_available <- levels(t7_decade$decade)
default_decade    <- "2010s"

p_sankey <- plot_ly()

for (i in seq_along(decades_available)) {
  dec <- decades_available[i]
  d   <- t7_decade |> filter(decade == dec)
  
  source_idx <- match(paste0(d$funder, " (funder)"), all_nodes) - 1
  target_idx <- match(d$performer, all_nodes) - 1
  
  link_colors <- sapply(d$funder, function(f) {
    col <- funder_colors[[f]]
    r   <- strtoi(substr(col, 2, 3), 16L)
    g   <- strtoi(substr(col, 4, 5), 16L)
    b   <- strtoi(substr(col, 6, 7), 16L)
    sprintf("rgba(%d,%d,%d,0.4)", r, g, b)
  })
  
  hover_labels <- paste0(
    d$funder, " → ", d$performer, "<br>",
    "$", formatC(round(d$value), format = "d", big.mark = ","), "M (2017 $)<br>",
    round(d$pct_of_funder, 1), "% of ", d$funder, " funding"
  )
  
  p_sankey <- p_sankey |> add_trace(
    type        = "sankey",
    orientation = "h",
    arrangement = "snap",
    visible     = (dec == default_decade),
    node = list(
      label     = display_labels,
      color     = node_colors,
      pad       = 20,
      thickness = 25,
      line      = list(color = "white", width = 0.5)
    ),
    link = list(
      source = source_idx,
      target = target_idx,
      value  = d$value,
      color  = link_colors,
      label  = hover_labels
    )
  )
}

# Dropdown buttons
buttons <- lapply(seq_along(decades_available), function(i) {
  dec        <- decades_available[i]
  visibility <- rep(FALSE, length(decades_available))
  visibility[i] <- TRUE
  list(
    method = "update",
    args   = list(
      list(visible = as.list(visibility)),
      list(title = list(
        text = paste0(
          "<b>U.S. Basic Research: Source of Funds → Performer</b><br>",
          "<sup>", dec, " decade average (2020s = 2020–2022) — Constant 2017 $M — NCSES Table 7</sup>"
        ),
        x = 0.05
      ))
    ),
    label = dec
  )
})

p_sankey <- p_sankey |>
  layout(
    title = list(
      text = paste0(
        "<b>U.S. Basic Research: Source of Funds → Performer</b><br>",
        "<sup>", default_decade,
        " decade average (2020s = 2020–2022) — Constant 2017 $M — NCSES Table 7</sup>"
      ),
      x = 0.05
    ),
    updatemenus = list(list(
      type       = "dropdown",
      direction  = "down",
      x          = 0.01,
      xanchor    = "left",
      y          = 1.10,
      yanchor    = "top",
      showactive = TRUE,
      buttons    = buttons
    )),
    font   = list(size = 12),
    height = 600,
    margin = list(t = 80, l = 10, r = 10, b = 20)
  )

p_sankey

# =============================================================================
# Section 3b: Total Basic Research Funder → Performer Sankey
# Sum of all years 1953–2022, constant 2017 $M
# =============================================================================

t7_total <- t7_long |>
  group_by(funder, performer) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

t7_total_funder <- t7_total |>
  group_by(funder) |>
  summarise(funder_total = sum(value), .groups = "drop")

t7_total <- t7_total |>
  left_join(t7_total_funder, by = "funder") |>
  mutate(pct_of_funder = value / funder_total * 100)

source_idx <- match(paste0(t7_total$funder, " (funder)"), all_nodes) - 1
target_idx <- match(t7_total$performer, all_nodes) - 1

link_colors <- sapply(t7_total$funder, function(f) {
  col <- funder_colors[[f]]
  r   <- strtoi(substr(col, 2, 3), 16L)
  g   <- strtoi(substr(col, 4, 5), 16L)
  b   <- strtoi(substr(col, 6, 7), 16L)
  sprintf("rgba(%d,%d,%d,0.4)", r, g, b)
})

hover_labels <- paste0(
  t7_total$funder, " → ", t7_total$performer, "<br>",
  "$", formatC(round(t7_total$value), format = "d", big.mark = ","), "M (2017 $)<br>",
  round(t7_total$pct_of_funder, 1), "% of ", t7_total$funder, " funding"
)

p_sankey_total <- plot_ly(
  type        = "sankey",
  orientation = "h",
  arrangement = "snap",
  node = list(
    label     = display_labels,
    color     = node_colors,
    pad       = 20,
    thickness = 25,
    line      = list(color = "white", width = 0.5)
  ),
  link = list(
    source = source_idx,
    target = target_idx,
    value  = t7_total$value,
    color  = link_colors,
    label  = hover_labels
  )
) |>
  layout(
    title = list(
      text = "<b>U.S. Basic Research: Source of Funds → Performer</b><br><sup>Total 1953–2022 — Constant 2017 $M — NCSES Table 7</sup>",
      x    = 0.05
    ),
    font   = list(size = 12),
    height = 600,
    margin = list(t = 80, l = 10, r = 10, b = 20)
  )

p_sankey_total
