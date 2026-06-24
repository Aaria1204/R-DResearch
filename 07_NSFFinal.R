# =============================================================================
# t6_funder_pie.R
# NCSES National Patterns Table 6 — Funder share pie charts by decade
#
# Funders: Federal, Nonfederal Government, Business, Higher Education, Nonprofit
# Values: constant 2017 $M, decade averages
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. HELPERS — programmatic column detection
# =============================================================================

# Reads two header rows (rows 4–5) and returns a tibble mapping
# column index → (funder_label, performer_label).
# Row 4 holds the top-level funder name (forward-filled across merged cells).
# Row 5 holds the performer sub-label (e.g. "Total", "Business", …).
build_col_map <- function(filepath) {
  raw_hdr <- read_excel(filepath, sheet = 1, skip = 3,
                        n_max = 2, col_names = FALSE)
  
  # Forward-fill merged-cell NAs in the funder row (row 1 of raw_hdr)
  funder_row <- as.character(raw_hdr[1, ])
  for (j in seq_along(funder_row)) {
    if (is.na(funder_row[j]) && j > 1) funder_row[j] <- funder_row[j - 1]
  }
  
  performer_row <- as.character(raw_hdr[2, ])
  
  tibble(
    col_index = seq_along(funder_row),
    funder    = funder_row,
    performer = performer_row
  )
}

# Returns the 1-based column index for the "Total" performer under a given funder.
find_funder_total_col <- function(col_map, funder_name) {
  col_map |>
    filter(
      str_detect(funder,    regex(funder_name, ignore_case = TRUE)),
      str_detect(performer, regex("^total$",   ignore_case = TRUE))
    ) |>
    pull(col_index)
}

# =============================================================================
# 2. LOAD DATA
# =============================================================================
file_path <- here("NSFGovDataT6.xlsx")

# Build column map from the two header rows
col_map <- build_col_map(file_path)

# Identify funder "Total" column positions programmatically
funder_specs <- tribble(
  ~label,                   ~search_term,
  "Federal",                "^Federal$",
  "Nonfederal Government",  "Nonfederal government",
  "Business",               "^Business$",
  "Higher Education",       "Higher education",
  "Nonprofit",              "^Nonprofit$"
)

funder_cols <- funder_specs |>
  rowwise() |>
  mutate(col_index = find_funder_total_col(col_map, search_term)) |>
  ungroup()

# Confirm all funders were found — stop early if any are missing
stopifnot(
  "Could not locate all funder 'Total' columns — inspect col_map" =
    !any(is.na(funder_cols$col_index)) && nrow(funder_cols) == 5
)

# Read raw data (skip 3 header rows; no column names)
raw <- read_excel(file_path, sheet = 1, skip = 3, col_names = FALSE)

# =============================================================================
# 3. LOCATE CONSTANT 2017 $ BLOCK
# =============================================================================
# The sheet stacks Current $ and Constant 2017 $ blocks vertically.
# Find the row index where the constant block begins.

const_row_idx <- which(
  str_detect(as.character(raw[[1]]), regex("^Constant 2017", ignore_case = TRUE))
)
stopifnot("Constant 2017 block not found" = length(const_row_idx) == 1)

# Data rows: everything after the label row through end of sheet
const_data <- raw[(const_row_idx + 1):nrow(raw), ]

# =============================================================================
# 4. EXTRACT YEAR + FUNDER TOTALS
# =============================================================================
# Year is always column 1; funder totals are at the detected col indices.
# Years like "2023a" / "2024b" (footnote suffixes) are stripped to numeric.

col_positions <- c(1, funder_cols$col_index)
col_labels    <- c("year", funder_cols$label)

rd_raw <- const_data |>
  select(all_of(col_positions)) |>
  setNames(col_labels) |>
  mutate(
    # Strip footnote letters (e.g. "2023a" → 2023)
    year = as.numeric(str_extract(as.character(year), "^[0-9]+")),
    across(-year, as.numeric)
  ) |>
  filter(!is.na(year), year >= 1953)

# =============================================================================
# 5. DECADE AVERAGES
# =============================================================================
rd_decade <- rd_raw |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade) |>
  summarise(across(-year, \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(
    cols      = -decade,
    names_to  = "funder",
    values_to = "value"
  ) |>
  mutate(
    funder = factor(funder, levels = funder_specs$label),
    decade = factor(decade, levels = sort(unique(decade)))
  )

# =============================================================================
# 6. COLORS
# =============================================================================
funder_colors <- c(
  "Federal"                = "#3182bd",
  "Nonfederal Government"  = "#6baed6",
  "Business"               = "#bd3131",
  "Higher Education"       = "#31a854",
  "Nonprofit"              = "#8e5bbf"
)

# =============================================================================
# 7. PIE CHART SUBPLOTS — one per decade
# =============================================================================
decades <- levels(rd_decade$decade)
n       <- length(decades)
ncols   <- 4
nrows   <- ceiling(n / ncols)

x_gap  <- 0.02
y_gap  <- 0.10
cell_w <- (1 - x_gap * (ncols + 1)) / ncols
cell_h <- (1 - y_gap * (nrows + 1)) / nrows

p_t6 <- plot_ly()

# Add one invisible scatter trace per funder purely to drive the legend.
# Pie traces in Plotly don't support a shared legend across subplots cleanly,
# so this is the reliable workaround.
for (fnd in names(funder_colors)) {
  p_t6 <- p_t6 |> add_trace(
    x           = list(NA),
    y           = list(NA),
    type        = "scatter",
    mode        = "markers",
    name        = fnd,
    legendgroup = fnd,
    marker      = list(color = funder_colors[[fnd]], size = 10, symbol = "square"),
    showlegend  = TRUE,
    hoverinfo   = "skip"
  )
}

for (i in seq_along(decades)) {
  d     <- rd_decade |> filter(decade == decades[i])
  col_i <- ((i - 1) %% ncols) + 1
  row_i <- ceiling(i / ncols)
  
  x0 <- x_gap * col_i + cell_w * (col_i - 1)
  x1 <- x0 + cell_w
  y0 <- 1 - (y_gap * row_i + cell_h * row_i)
  y1 <- y0 + cell_h
  
  p_t6 <- p_t6 |> add_trace(
    labels        = d$funder,
    values        = d$value,
    type          = "pie",
    name          = decades[i],
    title         = list(
      text = paste0("<b>", decades[i], "</b>"),
      font = list(size = 13)
    ),
    domain        = list(x = c(x0, x1), y = c(y0, y1)),
    marker        = list(
      colors = unname(funder_colors[as.character(d$funder)])
    ),
    textinfo      = "percent",
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "$%{value:,.0f}M (2017 $)<br>",
      "%{percent}<extra>", decades[i], "</extra>"
    ),
    showlegend = FALSE   # legend driven by the scatter traces above
  )
}

p_t6 <- p_t6 |>
  layout(
    title = list(
      text = paste0(
        "<b>U.S. R&D Expenditures by Source of Funds — By Decade</b><br>",
        "<sup>Decade aggregates, Constant 2017 $M — NCSES National Patterns Table 6</sup>"
      ),
      x = 0.05
    ),
    legend = list(
      title       = list(text = "<b>Funder</b>"),
      orientation = "h",
      x           = 0.5,
      xanchor     = "center",
      y           = -0.05
    ),
    margin = list(t = 80, b = 60)
  )

p_t6


# =============================================================================
# t6_sankey.R
# NCSES National Patterns Table 6 — Interactive Sankey: funder → performer
#
# Left nodes  = source of funds  (Federal, Nonfederal Gov, Business, Higher Ed, Nonprofit)
# Right nodes = performers        (Federal intramural, FFRDC, Business, Higher Ed, Nonprofit)
# Flow width  = constant 2017 $M, decade totals
# Dropdown    = select decade
#
# Key design note: funder node labels get an invisible unicode suffix (\u200b)
# so Plotly treats left/right nodes as distinct even when they share a name
# (e.g. "Business" funder vs "Business" performer). Without this, Plotly
# draws self-loops instead of left-to-right flows.
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. COLUMN MAP HELPERS
# =============================================================================
build_col_map <- function(filepath) {
  raw_hdr <- read_excel(filepath, sheet = 1, skip = 3,
                        n_max = 2, col_names = FALSE)
  
  funder_row <- as.character(raw_hdr[1, ])
  for (j in seq_along(funder_row)) {
    if (is.na(funder_row[j]) && j > 1) funder_row[j] <- funder_row[j - 1]
  }
  
  tibble(
    col_index     = seq_along(funder_row),
    funder        = funder_row,
    performer_sub = as.character(raw_hdr[2, ])
  )
}

# =============================================================================
# 2. DEFINE FLOWS PROGRAMMATICALLY
# =============================================================================
build_flow_spec <- function(col_map) {
  col_map |>
    filter(
      col_index > 2,
      !str_detect(performer_sub, regex("^total$",       ignore_case = TRUE)),
      !str_detect(funder,        regex("^Year$",         ignore_case = TRUE)),
      !str_detect(funder,        regex("All funding",    ignore_case = TRUE))
    ) |>
    mutate(
      funder_label = case_when(
        str_detect(funder, regex("^Federal$",             ignore_case = TRUE)) ~ "Federal",
        str_detect(funder, regex("Nonfederal government", ignore_case = TRUE)) ~ "Nonfederal Gov",
        str_detect(funder, regex("^Business$",            ignore_case = TRUE)) ~ "Business",
        str_detect(funder, regex("Higher education",      ignore_case = TRUE)) ~ "Higher Education",
        str_detect(funder, regex("^Nonprofit$",           ignore_case = TRUE)) ~ "Nonprofit",
        TRUE ~ funder
      ),
      performer_label = case_when(
        str_detect(performer_sub, regex("Federal intramural", ignore_case = TRUE)) ~ "Federal intramural",
        str_detect(performer_sub, regex("^FFRDC$",            ignore_case = TRUE)) ~ "FFRDC",
        str_detect(performer_sub, regex("Nonfederal gov",     ignore_case = TRUE)) ~ "Nonfederal Gov",
        str_detect(performer_sub, regex("^Business$",         ignore_case = TRUE)) ~ "Business",
        str_detect(performer_sub, regex("Higher education",   ignore_case = TRUE)) ~ "Higher Education",
        str_detect(performer_sub, regex("^Nonprofit$",        ignore_case = TRUE)) ~ "Nonprofit",
        TRUE ~ performer_sub
      )
    ) |>
    select(col_index, funder_label, performer_label)
}

# =============================================================================
# 3. LOAD & EXTRACT DATA
# =============================================================================
file_path <- here("NSFGovDataT6.xlsx")

col_map   <- build_col_map(file_path)
flow_spec <- build_flow_spec(col_map)

raw <- read_excel(file_path, sheet = 1, skip = 3, col_names = FALSE)

const_row_idx <- which(
  str_detect(as.character(raw[[1]]), regex("^Constant 2017", ignore_case = TRUE))
)
stopifnot("Constant 2017 block not found" = length(const_row_idx) == 1)

const_data <- raw[(const_row_idx + 1):nrow(raw), ]

flow_cols   <- c(1, flow_spec$col_index)
flow_labels <- c("year", paste0("flow_", seq_len(nrow(flow_spec))))

rd_flows <- const_data |>
  select(all_of(flow_cols)) |>
  setNames(flow_labels) |>
  mutate(
    year = as.numeric(str_extract(as.character(year), "^[0-9]+")),
    across(-year, \(x) suppressWarnings(as.numeric(as.character(x))))
  ) |>
  filter(!is.na(year), year >= 1953)

# =============================================================================
# 4. DECADE TOTALS — long format
# =============================================================================
rd_long <- rd_flows |>
  mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
  group_by(decade) |>
  summarise(across(starts_with("flow_"), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(
    cols      = starts_with("flow_"),
    names_to  = "flow_id",
    values_to = "value"
  ) |>
  mutate(flow_num = as.integer(str_extract(flow_id, "[0-9]+"))) |>
  left_join(
    flow_spec |> mutate(flow_num = row_number()),
    by = "flow_num"
  ) |>
  select(decade, funder = funder_label, performer = performer_label, value) |>
  filter(!is.na(value), value > 0) |>
  mutate(decade = factor(decade, levels = sort(unique(decade))))

# =============================================================================
# 5. NODE DEFINITIONS
# =============================================================================
# Funder nodes get an invisible zero-width space suffix (\u200b) so Plotly
# treats "Business\u200b" (left) and "Business" (right) as different nodes,
# drawing a proper left-to-right flow instead of a self-loop.
# The displayed label is identical to the naked eye.

funder_display <- c("Federal", "Nonfederal Gov", "Business",
                    "Higher Education", "Nonprofit")
performer_display <- c("Federal intramural", "FFRDC", "Nonfederal Gov",
                       "Business", "Higher Education", "Nonprofit")

# Internal node names used for indexing
funder_nodes    <- paste0(funder_display,    "\u200b")  # left side
performer_nodes <- performer_display                     # right side

all_nodes    <- c(funder_nodes, performer_nodes)
# Displayed labels strip the suffix
all_labels   <- c(funder_display, performer_display)

funder_colors <- c(
  "#3182bd",   # Federal
  "#6baed6",   # Nonfederal Gov
  "#bd3131",   # Business
  "#31a854",   # Higher Education
  "#8e5bbf"    # Nonprofit
)

performer_colors <- c(
  "#1a4f7a",   # Federal intramural
  "#9ecae1",   # FFRDC
  "#4292c6",   # Nonfederal Gov
  "#e06c6c",   # Business
  "#74c87a",   # Higher Education
  "#b99fd6"    # Nonprofit
)

node_colors <- c(funder_colors, performer_colors)

# Named lookup using the internal node names (0-based for Plotly)
node_idx <- setNames(seq_along(all_nodes) - 1, all_nodes)

# Named color lookup for links (keyed by internal funder node name)
funder_color_map <- setNames(funder_colors, funder_nodes)

# =============================================================================
# 6. HELPER: proportional y positions for one decade
# =============================================================================
compute_node_y <- function(nodes, display_names, decade_label, side,
                           pad_frac = 0.02) {
  if (side == "funder") {
    # Match on display name (strip suffix for lookup)
    clean <- str_remove(nodes, "\u200b")
    totals <- rd_long |>
      filter(decade == decade_label) |>
      group_by(funder) |>
      summarise(total = sum(value), .groups = "drop") |>
      rename(node = funder)
    lookup_nodes <- clean
  } else {
    totals <- rd_long |>
      filter(decade == decade_label) |>
      group_by(performer) |>
      summarise(total = sum(value), .groups = "drop") |>
      rename(node = performer)
    lookup_nodes <- nodes
  }
  
  totals <- tibble(node = lookup_nodes) |>
    left_join(totals, by = "node") |>
    mutate(total = replace_na(total, 0))
  
  n     <- nrow(totals)
  grand <- sum(totals$total)
  if (grand == 0) return(seq(0.05, 0.95, length.out = n))
  
  shares    <- totals$total / grand
  heights   <- shares * (1 - n * pad_frac)
  bottoms   <- cumsum(c(0, heights[-n] + pad_frac))
  midpoints <- bottoms + heights / 2
  
  pmax(0.02, pmin(0.98, midpoints))
}

# =============================================================================
# 7. BUILD SANKEY TRACES — one per decade
# =============================================================================
decades <- levels(rd_long$decade)

make_sankey <- function(decade_label) {
  d <- rd_long |> filter(decade == decade_label)
  
  # Map data funder names to internal node names (with suffix)
  funder_internal_map <- setNames(funder_nodes, funder_display)
  
  y_funders    <- compute_node_y(funder_nodes,    funder_display,    decade_label, "funder")
  y_performers <- compute_node_y(performer_nodes, performer_display, decade_label, "performer")
  
  node_x <- c(rep(0.01, length(funder_nodes)), rep(0.99, length(performer_nodes)))
  node_y <- c(y_funders, y_performers)
  
  # Sources use suffixed funder names; targets use plain performer names
  src_internal <- funder_internal_map[d$funder]
  link_colors  <- sapply(src_internal, function(f) {
    hex <- funder_color_map[[f]]
    r   <- strtoi(substr(hex, 2, 3), 16)
    g   <- strtoi(substr(hex, 4, 5), 16)
    b   <- strtoi(substr(hex, 6, 7), 16)
    sprintf("rgba(%d,%d,%d,0.4)", r, g, b)
  })
  
  list(
    type        = "sankey",
    orientation = "h",
    arrangement = "fixed",
    node = list(
      label     = all_labels,
      color     = node_colors,
      x         = node_x,
      y         = node_y,
      pad       = 8,
      thickness = 20,
      line      = list(color = "white", width = 0.5)
    ),
    link = list(
      source        = unname(node_idx[src_internal]),
      target        = unname(node_idx[d$performer]),
      value         = d$value,
      color         = unname(link_colors),
      customdata    = paste0(
        d$funder, " \u2192 ", d$performer, "<br>",
        "$", formatC(d$value, format = "f", digits = 0, big.mark = ","), "M (2017 $)"
      ),
      hovertemplate = "%{customdata}<extra></extra>"
    )
  )
}

sankey_traces <- lapply(decades, make_sankey)

# =============================================================================
# 8. DROPDOWN BUTTONS
# =============================================================================
buttons <- lapply(seq_along(decades), function(i) {
  visibility <- rep(FALSE, length(decades))
  visibility[i] <- TRUE
  list(
    method  = "update",
    label   = decades[i],
    args    = list(list(visible = visibility))
  )
})

# Default: 2010s (most recent complete decade)
default_decade <- length(decades) - 1
for (i in seq_along(sankey_traces)) {
  sankey_traces[[i]]$visible <- (i == default_decade)
}

# =============================================================================
# 9. ASSEMBLE PLOT
# =============================================================================
p_sankey <- plot_ly()

for (trace in sankey_traces) {
  p_sankey <- p_sankey |> add_trace(
    type        = trace$type,
    orientation = trace$orientation,
    arrangement = trace$arrangement,
    node        = trace$node,
    link        = trace$link,
    visible     = trace$visible
  )
}

p_sankey <- p_sankey |>
  layout(
    title = list(
      text = paste0(
        "<b>U.S. R&D Expenditures: Source of Funds \u2192 Performer</b><br>",
        "<sup>Decade totals, Constant 2017 $M \u2014 NCSES National Patterns Table 6</sup>"
      ),
      x = 0.05
    ),
    updatemenus = list(list(
      type       = "dropdown",
      direction  = "down",
      x          = 0.01,
      xanchor    = "left",
      y          = 1.12,
      yanchor    = "top",
      showactive = TRUE,
      buttons    = buttons
    )),
    annotations = list(
      list(x = 0.01, y = 1.17, text = "<b>Select decade:</b>",
           showarrow = FALSE, xref = "paper", yref = "paper",
           font = list(size = 12)),
      list(x = 0.02, y = -0.03, text = "Source of Funds",
           showarrow = FALSE, xref = "paper", yref = "paper",
           font = list(size = 11, color = "#555555")),
      list(x = 0.98, y = -0.03, text = "Performer",
           showarrow = FALSE, xref = "paper", yref = "paper",
           font = list(size = 11, color = "#555555"))
    ),
    margin = list(t = 120, b = 60, l = 20, r = 20),
    font   = list(family = "Arial", size = 11)
  )

p_sankey
# =============================================================================
# t7t8t9_by_funder.R
# NCSES Tables 7, 8, 9 — R&D type breakdown by funding source over time
#
# T7 = Basic research, T8 = Applied research, T9 = Experimental development
# One chart per funding source (Federal, Nonfederal Gov, Business,
#   Higher Education, Nonprofit)
# Each chart: stacked area, x = year, stack = R&D type, y = constant 2017 $M
# =============================================================================

library(tidyverse)
library(readxl)
library(plotly)
library(here)

# =============================================================================
# 1. HELPERS — same pattern as t6 scripts
# =============================================================================

# Reads two header rows, forward-fills merged-cell NAs, returns column map
build_col_map <- function(filepath) {
  raw_hdr <- read_excel(filepath, sheet = 1, skip = 3,
                        n_max = 2, col_names = FALSE)

  funder_row <- as.character(raw_hdr[1, ])
  for (j in seq_along(funder_row)) {
    if (is.na(funder_row[j]) && j > 1) funder_row[j] <- funder_row[j - 1]
  }

  tibble(
    col_index     = seq_along(funder_row),
    funder        = funder_row,
    performer_sub = as.character(raw_hdr[2, ])
  )
}

# Returns column index of "Total" performer under a given funder
find_funder_total_col <- function(col_map, funder_pattern) {
  col_map |>
    filter(
      str_detect(funder,        regex(funder_pattern, ignore_case = TRUE)),
      str_detect(performer_sub, regex("^total$",      ignore_case = TRUE))
    ) |>
    pull(col_index)
}

# Reads constant 2017 $ block and returns year + funder totals
extract_funder_totals <- function(filepath, rd_type_label) {
  col_map <- build_col_map(filepath)

  # Funder search patterns → display labels (same order throughout)
  funder_specs <- tribble(
    ~label,                  ~pattern,
    "Federal",               "^Federal$",
    "Nonfederal Gov",        "Nonfederal government",
    "Business",              "^Business$",
    "Higher Education",      "Higher education",
    "Nonprofit",             "^Nonprofit$"
  )

  funder_cols <- funder_specs |>
    rowwise() |>
    mutate(col_index = find_funder_total_col(col_map, pattern)) |>
    ungroup()

  stopifnot(
    "Could not locate all funder Total columns — inspect col_map" =
      !any(is.na(funder_cols$col_index)) && nrow(funder_cols) == 5
  )

  raw <- read_excel(filepath, sheet = 1, skip = 3, col_names = FALSE)

  const_row_idx <- which(
    str_detect(as.character(raw[[1]]), regex("^Constant 2017", ignore_case = TRUE))
  )
  stopifnot("Constant 2017 block not found" = length(const_row_idx) == 1)

  const_data <- raw[(const_row_idx + 1):nrow(raw), ]

  col_positions <- c(1, funder_cols$col_index)
  col_labels    <- c("year", funder_cols$label)

  const_data |>
    select(all_of(col_positions)) |>
    setNames(col_labels) |>
    mutate(
      year    = as.numeric(str_extract(as.character(year), "^[0-9]+")),
      across(-year, \(x) suppressWarnings(as.numeric(as.character(x)))),
      rd_type = rd_type_label
    ) |>
    filter(!is.na(year), year >= 1953)
}

# =============================================================================
# 2. LOAD ALL THREE TABLES
# =============================================================================
basic        <- extract_funder_totals(here("NSFGovDataT7.xlsx"), "Basic")
applied      <- extract_funder_totals(here("NSFGovDataT8.xlsx"), "Applied")
experimental <- extract_funder_totals(here("NSFGovDataT9.xlsx"), "Experimental")

# =============================================================================
# 3. COMBINE AND RESHAPE
# =============================================================================
rd_all <- bind_rows(basic, applied, experimental) |>
  pivot_longer(
    cols      = c("Federal", "Nonfederal Gov", "Business",
                  "Higher Education", "Nonprofit"),
    names_to  = "funder",
    values_to = "value"
  ) |>
  mutate(
    rd_type = factor(rd_type, levels = c("Basic", "Applied", "Experimental")),
    funder  = factor(funder,  levels = c("Federal", "Nonfederal Gov", "Business",
                                         "Higher Education", "Nonprofit"))
  )

# =============================================================================
# 4. COLORS — R&D type (consistent across all funder charts)
# =============================================================================
type_colors <- c(
  "Basic"        = "#1f77b4",
  "Applied"      = "#ff7f0e",
  "Experimental" = "#2ca02c"
)

# =============================================================================
# 5. BUILD ONE STACKED AREA CHART PER FUNDER
# =============================================================================
# Stacked area shows both total scale and type composition over time.
# Each R&D type is one trace; fill = "tonexty" stacks them.

make_funder_chart <- function(funder_name) {
  d <- rd_all |>
    filter(funder == funder_name) |>
    arrange(rd_type, year)   # must be ordered for fill stacking

  p <- plot_ly()

  for (rtype in c("Basic", "Applied", "Experimental")) {
    dt <- d |> filter(rd_type == rtype)

    p <- p |> add_trace(
      data          = dt,
      x             = ~year,
      y             = ~value,
      type          = "scatter",
      mode          = "none",       # no markers/lines — area only
      name          = rtype,
      stackgroup    = "one",        # stacks all traces in the same group
      fillcolor      = paste0(
        # hex → rgba at 80% opacity for a clean area look
        local({
          hex <- type_colors[[rtype]]
          r   <- strtoi(substr(hex, 2, 3), 16)
          g   <- strtoi(substr(hex, 4, 5), 16)
          b   <- strtoi(substr(hex, 6, 7), 16)
          sprintf("rgba(%d,%d,%d,0.8)", r, g, b)
        })
      ),
      hovertemplate = paste0(
        "<b>", rtype, "</b><br>",
        "Year: %{x}<br>",
        "$%{y:,.0f}M (2017 $)<extra></extra>"
      )
    )
  }

  p |> layout(
    title = list(
      text = paste0(
        "<b>", funder_name, ": R&D Expenditures by Type</b><br>",
        "<sup>Constant 2017 $M — All Years — NCSES National Patterns Tables 7, 8, 9</sup>"
      ),
      x = 0.05
    ),
    xaxis = list(
      title     = "Year",
      tickmode  = "linear",
      dtick     = 10,
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
    hovermode = "x unified"
  )
}

# =============================================================================
# 6. RENDER
# =============================================================================
p_federal      <- make_funder_chart("Federal")
p_nonfed_gov   <- make_funder_chart("Nonfederal Gov")
p_business     <- make_funder_chart("Business")
p_higher_ed    <- make_funder_chart("Higher Education")
p_nonprofit    <- make_funder_chart("Nonprofit")

p_federal
p_nonfed_gov
p_business
p_higher_ed
p_nonprofit
