# U.S. R&D Investment Patterns: An Empirical Analysis

An empirical analysis of U.S. research and development expenditure from 1953 to present, examining how investment has shifted across sectors, industries, and research types over seven decades. Built in R using Compustat firm-level data and NCSES national aggregate data.

## What This Project Does

- Cleans and inflation-adjusts 70+ years of firm-level R&D data from Compustat (WRDS)
- Analyzes R&D intensity trends across industries using 3-digit NAICS classification
- Visualizes how sector rankings have shifted across decades via bump charts
- Combines public and private R&D data to build a complete picture of the U.S. innovation landscape
- Produces interactive Plotly visualizations of funder-to-performer funding flows using NCSES data

## Data Sources
- **Compustat (WRDS):** Firm-level R&D expenditure (`xrd`) and total assets (`at`) for U.S. public companies. Raw data excluded from repo due to WRDS redistribution restrictions.
- **NCSES National Patterns of R&D Resources (Tables T2–T9):** Public aggregate data from NSF covering federal, business, higher education, and nonprofit sectors. All values in constant 2017 dollars.
- 
## Interactive Charts
- [Sector Share by Decade](charts/sector_shares_pie.html)
- [R&D by Type — Federal](charts/federal_rd_by_type.html)
- [R&D by Type — Business](charts/business_rd_by_type.html)
- [Basic Research: Funder to Performer](charts/basic_funder_to_performer.html)
- 
## Scripts
| Script | Description |
|--------|-------------|
| `00_cleaning.qmd` | Load, clean, and inflation-adjust Compustat data to constant 2025 dollars |
| `01_national.R` | National R&D aggregate trends over time |
| `02_industry.R` | Industry-level R&D intensity (`xrd / lagged total assets`), winsorized at sector level |
| `03_week3.R` | 3-digit NAICS drill-downs within top sectors |
| `04_week4.R` | Bump chart of sector R&D intensity rank changes across decades |
| `05_NSF.R` | NCSES interactive visualizations: sector share pie charts, R&D type breakdowns, and basic research funder-to-performer flow charts |

## Tools & Methods
- **R** — `tidyverse`, `ggplot2`, `plotly`, `readxl`, `here`, `fredr`
- Inflation adjustment via FRED API (PPI / GDP deflator)
- Sector-level winsorization with firm-count guard to reduce outlier distortion
- Interactive visualizations built with Plotly
