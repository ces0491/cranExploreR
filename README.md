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
  - Trend, drawing the comparison behind the momentum verdict: the mean daily rate over the earlier period stepping to the mean over the last 30 days, with the figures on hover
  - Version releases, colouring the weekly series by the version current at the end of each week. Judging from the end rather than the start means a release published mid-week still owns that week; two releases inside one week collapse to the later, and the version history table remains the complete list
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
| [CRAN](https://cran.r-project.org) | Current published versions, from the package DESCRIPTION and the repository index |
| [cranlogs](https://cranlogs.r-pkg.org) | Download statistics and top packages |
| [R package search](https://search.r-pkg.org) | Full-text package search |

crandb rebuilds its index on its own schedule and can sit several days behind
CRAN, so a newly published release will not appear there straight away. The app
reads the DESCRIPTION file from CRAN alongside it and, where the two disagree,
shows the CRAN version and adds the missing release to the version history.

The search index behind the sidebar and the Browse tab lags in the same way,
so the version shown against each result is corrected against the CRAN
repository index. That is one request covering every package CRAN currently
publishes, which a per-package DESCRIPTION read could not manage for a 50-row
page. A package that has since been archived keeps the version the search
index reported.

The version shown is the published *source* version, taken from the package
DESCRIPTION and the repository index. CRAN publishes the source first and
builds binaries afterwards, separately for each R branch, so the Windows and
macOS binaries listed on a package's CRAN page can trail it by hours or days.
A reader comparing this app to that page may well see an older number there.
On a platform where `install.packages()` resolves to a binary, that older
version is what gets installed until the build lands.

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

The unit tests cover the scoring bands and their labels, download momentum,
the version-to-week mapping behind the chart, formatting, dependency parsing,
the CRAN reconciliation, search query construction and the response cache.
They stub the HTTP layer, so they run offline and without touching the CRAN
APIs.

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
| Download momentum | 25% | Mean downloads per day over the last 30 days against the mean over the preceding days in the 365-day window |
| Download volume | 20% | Monthly downloads, in seven bands, shown with the count and the package's percentile across CRAN |
| Ecosystem adoption | 15% | Number of other CRAN packages that depend on it, with its percentile |
| Maturity | 10% | Time since first publication on CRAN; the release count is shown alongside |

The band edges are drawn from the repository, not picked by eye. A sample of
600 packages puts the median at under 300 downloads a month, with close to nine
in ten falling between 100 and 1,000. Bands a decade wide would therefore give
most of CRAN the same score, so volume runs in seven bands cut finer through
that range. The edges are absolute counts: a few hundred downloads a month is a
small user base however much of the repository it beats. The percentile sits
beside the count in the label so both readings are available.

About 70% of packages have no reverse dependencies, counted across the whole
repository index. That single value holds too much of the distribution to
subdivide, so the thresholds there are coarse and no dependents is reported
plainly: for a leaf package it describes the kind of package as much as its
health.

Maturity is time since first publication. Release count tracks age poorly —
they correlate at 0.49 across a 140-package sample, in which one package in
eight had been on CRAN eight years or more with four releases or fewer — so
the release count appears in the label as a track record without feeding the
score.

Momentum compares rates per day rather than period totals, because cranlogs
returns data only from a package's first publication. A package four months old
has four months of rows, so dividing its total by twelve months would understate
the baseline threefold and report a decline as growth. The window is the last
365 days: the mean daily rate over the most recent 30 against the mean over the
days before them. Under 60 days of download data leaves no baseline to compare
against, and momentum is reported as unavailable.

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
