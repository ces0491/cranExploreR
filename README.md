# cranExploreR

A Shiny app for exploring CRAN package viability. Search for any R package to view download statistics, maintenance health, dependency information, and version history.

## Features

### Explorer

- **Search** CRAN packages by name or keyword
- **Download stats** — daily, weekly, monthly, and yearly totals with a 12-month trend chart
- **Viability score** (0-100) — composite indicator based on recency, download momentum, volume, ecosystem adoption, and maturity
- **Package metadata** — maintainer, license, publication dates, R version requirements
- **Dependencies** — imports, depends, and suggests with version constraints
- **Version history** — full release timeline from CRAN
- **Reverse dependencies** — breakdown by depends, imports, suggests, and linking

### Browse

Discover packages through three navigation modes:

- **Popular Packages** — top downloaded packages from the last month
- **Categories** — 16 curated topics (Data Wrangling, Visualization, Machine Learning, Time Series, etc.)
- **Alphabetical** — A-Z letter buttons to browse by package name

Click any row in the results table to jump to the Explorer tab with that package loaded.

### Compare

Side-by-side comparison of 2-3 packages with overlaid download trends and a summary table covering viability scores, downloads, reverse dependencies, and release counts.

## Data Sources

| Source | Provides |
| ------ | -------- |
| [crandb](https://crandb.r-pkg.org) | Package metadata and version history |
| [cranlogs](https://cranlogs.r-pkg.org) | Download statistics |
| [R package search](https://search.r-pkg.org) | Full-text package search |

## Requirements

- R 4.0+
- Packages: `shiny`, `bslib`, `plotly`, `DT`, `httr2`, `jsonlite`

## Running Locally

```r
# Install dependencies
install.packages(c("shiny", "bslib", "plotly", "DT", "httr2", "jsonlite"))

# Run the app
shiny::runApp()
```

## Viability Score

The viability score weights five factors:

| Factor | Weight | What it measures |
| ------ | ------ | ---------------- |
| Recency | 30% | How recently the package was updated |
| Download momentum | 25% | Whether downloads are growing or declining |
| Download volume | 20% | Absolute monthly download count |
| Ecosystem adoption | 15% | Number of reverse dependencies |
| Maturity | 10% | Number of releases over time |

## Project Structure

``` text
cranExploreR/
├── app.R              # Entry point
├── ui.R               # UI layout (bslib page_navbar)
├── server.R           # Server logic and reactive outputs
├── R/
│   ├── api_functions.R  # CRAN API calls (crandb, cranlogs, search)
│   └── helpers.R        # Health score calculation, formatting utilities
└── manifest.json      # Deployment manifest
```
