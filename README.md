# cranExploreR

An interactive dashboard for evaluating CRAN packages before adding them to your project. Choosing the right dependencies is one of the most consequential decisions in any R project — a package that loses its maintainer, stops getting updates, or has a shrinking user base can become a liability that is expensive to replace later.

cranExploreR pulls live data from CRAN APIs to give you download statistics, maintenance health, dependency information, version history, and a composite viability score.

## Features

### Explorer

- **Search** CRAN packages by name or keyword with server-side search
- **Download stats** — yesterday, past 7 days, past 30 days, and past 365 days with auto-scaling display
- **Download trend chart** — 12-month weekly downloads with toggleable overlays:
  - Weekly totals (default)
  - Cumulative downloads
  - Mean weekly downloads over the period
  - Version releases, colouring the weekly series by the version current at the time
- **Download statistics** — lifetime downloads, days on CRAN, daily and weekly averages, and peak day and week
- **Viability score** (0-100) — weighted composite of recency, download momentum, volume, ecosystem adoption, and maturity
- **Package links** — direct links to CRAN page, documentation/vignettes, GitHub repo, and issue tracker (when available)
- **Package metadata** — maintainer, license, publication dates, R version requirements
- **Dependencies** — imports, depends, and suggests with version constraints
- **Version history** — full release timeline from CRAN
- **Reverse dependencies** — breakdown by depends, imports, suggests, and linking

### Browse

Discover packages through three navigation modes:

- **Popular Packages** — top downloaded packages from the last month via cranlogs
- **Categories** — 16 curated topics (Data Wrangling, Visualization, Machine Learning, Time Series, Spatial, Bayesian, and more)
- **Alphabetical** — A-Z letter buttons to browse by package name, showing the first 50 matches in name order alongside the full count

Each result row includes direct CRAN and documentation links. Click any row to jump to the Explorer tab with that package loaded.

### Compare

Side-by-side comparison of 2-3 packages:

- **Package search** built into the sidebar — search and click to fill comparison slots
- **Overlaid download trends** — weekly downloads for all selected packages on one chart
- **Summary table** — viability scores, monthly/yearly downloads, reverse dependencies, release counts, and license info

## Data Sources

| Source | Provides |
| ------ | -------- |
| [crandb](https://crandb.r-pkg.org) | Package metadata and version history |
| [CRAN](https://cran.r-project.org) | Current published version, read from the package DESCRIPTION |
| [cranlogs](https://cranlogs.r-pkg.org) | Download statistics and top packages |
| [R package search](https://search.r-pkg.org) | Full-text package search |

crandb rebuilds its index on its own schedule and can sit several days behind
CRAN, so a newly published release will not appear there straight away. The app
reads the DESCRIPTION file from CRAN alongside it and, where the two disagree,
shows the CRAN version and adds the missing release to the version history.
Download figures still come from cranlogs and can lag by a day or two.

Parsed responses are cached in memory for 15 minutes, so revisiting a package or
pulling it into a comparison costs no further requests.

## Requirements

- R 4.1+ (the code uses the native `|>` pipe)
- Packages: `shiny`, `bslib`, `plotly`, `DT`, `httr2`, `jsonlite`
- To run the tests: `testthat`

## Running Locally

```r
# Install dependencies
install.packages(c("shiny", "bslib", "plotly", "DT", "httr2", "jsonlite"))

# Run the app
shiny::runApp()
```

## Tests

The unit tests cover scoring, formatting, dependency parsing, the CRAN
reconciliation, search query construction and the response cache. They stub the
HTTP layer, so they run offline and without touching the CRAN APIs.

```r
install.packages("testthat")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
```

## Deployment

The app is deployed to Posit Connect Cloud from the `main` branch. A GitHub
Actions CI pipeline runs on every push to `main` and on pull requests targeting
it — R file parsing, linting, app structure, function checks, unit tests, and
live API smoke tests — before Connect Cloud picks up the changes.

`manifest.json` pins the deployment's R version and package set. Regenerate it
whenever a dependency changes:

```r
rsconnect::writeManifest(
  appDir = ".",
  appFiles = c(
    "app.R", "ui.R", "server.R",
    "R/api_functions.R", "R/helpers.R",
    ".lintr", "README.md", ".github/workflows/ci.yml"
  )
)
```

## Viability Score

The viability score (0-100) is a weighted composite that summarises five dimensions into a single number. It is a starting point for evaluation, not a definitive judgement — context always matters.

| Factor | Weight | What it measures |
| ------ | ------ | ---------------- |
| Recency | 30% | How recently the package was updated on CRAN |
| Download momentum | 25% | Whether downloads are growing, stable, or declining |
| Download volume | 20% | Absolute number of monthly downloads |
| Ecosystem adoption | 15% | Number of other packages that depend on it |
| Maturity | 10% | Total number of releases over the package's lifetime |

A factor whose data cannot be fetched is dropped from the calculation rather
than scored zero, and the score is renormalised over the remaining weight, so an
upstream failure reads as missing data rather than as a low score. The Explorer
score card reports how much weighting the score was computed over, and the
Compare table marks such a score as partial.

## Project Structure

```text
cranExploreR/
├── app.R                # Entry point
├── ui.R                 # UI layout (bslib page_navbar)
├── server.R             # Server logic and reactive outputs
├── R/
│   ├── api_functions.R  # CRAN API calls, request cache (CRAN, crandb, cranlogs, search)
│   └── helpers.R        # Health score, formatting, category definitions
├── tests/
│   ├── testthat.R       # Test runner
│   └── testthat/        # Offline unit tests, HTTP layer stubbed
├── .lintr               # Linter config (suppresses false positives)
├── .github/
│   └── workflows/
│       └── ci.yml       # CI pipeline (parse, lint, structure, unit tests, smoke tests)
└── manifest.json        # Posit Connect deployment manifest
```
