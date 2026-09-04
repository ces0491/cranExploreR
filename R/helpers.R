# Helper functions for data transformation and display

# Browse tab category definitions
# Each category maps to search keywords used with the R-hub search API
BROWSE_CATEGORIES <- c(
  "Data Wrangling" = "data manipulation wrangling transform",
  "Visualization" = "plotting visualization graphics chart",
  "Machine Learning" = "machine learning prediction",
  "Statistical Methods" = "statistical test inference",
  "Time Series" = "time series forecast temporal",
  "Spatial & Mapping" = "spatial map geographic geospatial",
  "Text & NLP" = "text mining natural language corpus",
  "Web & APIs" = "web API http scraping REST client",
  "Databases" = "database SQL connection driver DBI",
  "Reporting" = "report markdown document knitr quarto",
  "Finance & Economics" = "finance economic trading portfolio",
  "Bioinformatics" = "bioinformatics genomics biological",
  "Bayesian" = "bayesian MCMC posterior sampling prior",
  "Survival Analysis" = "survival hazard censoring kaplan cox",
  "High Performance" = "parallel computing performance Rcpp",
  "Reproducibility" = "reproducible pipeline workflow"
)

#' Format numbers with thousands separators
#'
#' Vectorised: a vector in gives a vector out, with "N/A" in the
#' positions that were missing.
#'
#' @param x Numeric vector
#' @return Character vector the same length as x, or "N/A" when empty
format_number <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("N/A")
  }

  out <- rep("N/A", length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    out[ok] <- formatC(
      round(x[ok]), format = "f", digits = 0, big.mark = ","
    )
  }
  out
}

#' Build an HTML links string for a package
#' @param pkg_name Character, the package name
#' @return Character, HTML string with icon links
package_links_html <- function(pkg_name) {
  cran <- paste0(
    "https://cran.r-project.org/package=", pkg_name
  )
  docs <- paste0(
    "https://cran.r-project.org/web/packages/",
    pkg_name, "/vignettes/"
  )

  paste0(
    "<a href=\"", cran,
    "\" target=\"_blank\" title=\"CRAN page\">",
    "<i class=\"fa-solid fa-box\"></i></a>",
    "&nbsp;&nbsp;",
    "<a href=\"", docs,
    "\" target=\"_blank\" title=\"Documentation\">",
    "<i class=\"fa-solid fa-book\"></i></a>"
  )
}

#' Compare the recent download rate against the period before it
#'
#' Rates per day, not period totals, because cranlogs returns data only
#' from a package's first publication. A package four months old has four
#' months of rows, and dividing its total by twelve would understate the
#' baseline by three times, reporting a decline as growth.
#'
#' @param daily Data frame from fetch_daily_downloads(), with `date` and
#'   `count`
#' @param window Integer, length in days of the recent period
#' @return List with `ratio`, `recent`, `baseline` and `baseline_days`,
#'   or NULL when there is too little history to compare
download_momentum <- function(daily, window = 30) {
  if (is.null(daily) || !is.data.frame(daily)) return(NULL)
  if (!all(c("date", "count") %in% names(daily))) return(NULL)
  if (nrow(daily) < window * 2) return(NULL)

  daily <- daily[order(daily$date), ]
  cutoff <- max(daily$date) - window

  recent <- daily$count[daily$date > cutoff]
  before <- daily$count[daily$date <= cutoff]
  if (length(recent) == 0 || length(before) == 0) return(NULL)

  recent_rate <- mean(recent, na.rm = TRUE)
  baseline_rate <- mean(before, na.rm = TRUE)
  if (is.na(recent_rate) || is.na(baseline_rate)) return(NULL)
  if (baseline_rate <= 0) return(NULL)

  list(
    ratio = recent_rate / baseline_rate,
    recent = recent_rate,
    baseline = baseline_rate,
    baseline_days = length(before)
  )
}

#' Place a monthly download count within the CRAN distribution
#' @param monthly Numeric, a package's downloads in the last month
#' @param distribution Numeric vector from fetch_download_distribution()
#' @return Integer percentile 0-100, or NULL when it cannot be placed
download_percentile <- function(monthly, distribution) {
  if (is.null(monthly) || length(monthly) == 0 || is.na(monthly)) {
    return(NULL)
  }
  if (is.null(distribution) || length(distribution) == 0) return(NULL)

  round(100 * mean(distribution <= monthly))
}

#' Describe where a package sits in the CRAN download distribution
#' @param pct Integer percentile from download_percentile()
#' @return Character phrase, or NULL when pct is NULL
percentile_phrase <- function(pct) {
  if (is.null(pct)) return(NULL)

  suffix <- "% of CRAN packages"

  if (pct >= 99) return(paste0("in the top 1", suffix))
  if (pct >= 90) return(paste0("in the top ", 100 - pct, suffix))
  if (pct <= 10) {
    return(paste0("in the bottom ", max(pct, 1), suffix))
  }
  paste0("above ", pct, suffix)
}

#' Say what share of CRAN shares a value of zero
#' @param distribution Numeric vector of counts across CRAN
#' @return Character phrase, or NULL when the distribution is missing
share_phrase <- function(distribution) {
  if (is.null(distribution) || length(distribution) == 0) return(NULL)

  paste0(
    "as for ", round(100 * mean(distribution == 0)),
    "% of CRAN packages"
  )
}

#' Join a label to the figures behind it
#' @param label Character, the band name
#' @param value Character, the measured value, or NULL
#' @param unit Character, appended to value, or NULL
#' @param context Character, the comparison phrase, or NULL
#' @return Character
with_context <- function(label, value = NULL, unit = NULL,
                         context = NULL) {
  parts <- c(
    if (!is.null(value)) paste0(value, unit %||% ""),
    context
  )
  if (length(parts) == 0) return(label)
  paste0(label, " \u2014 ", paste(parts, collapse = ", "))
}

#' Format a duration in years for display
#' @param years Numeric
#' @return Character
format_years <- function(years) {
  if (years < 1) {
    months <- max(1, round(years * 12))
    return(paste0(months, if (months == 1) " month" else " months"))
  }
  paste0(round(years, 1), " years")
}

#' Band a monthly download count
#'
#' Seven bands rather than five, cut finer between 50 and 2,000 where
#' most of CRAN sits. Decade-wide bands put close to nine in ten
#' packages into a single one.
#'
#' @param monthly Numeric, downloads in the last month
#' @return List with `points`, `label` and `sentiment`
volume_band <- function(monthly) {
  bands <- list(
    list(min = 100000, points = 20, label = "Very high", sentiment = "good"),
    list(min = 10000, points = 18, label = "High", sentiment = "good"),
    list(min = 2000, points = 15, label = "Substantial", sentiment = "good"),
    list(min = 500, points = 12, label = "Moderate", sentiment = "neutral"),
    list(min = 200, points = 8, label = "Modest", sentiment = "warn"),
    list(min = 50, points = 5, label = "Low", sentiment = "bad"),
    list(min = -Inf, points = 2, label = "Very low", sentiment = "bad")
  )
  for (b in bands) if (monthly >= b$min) return(b)
  bands[[length(bands)]]
}

#' Band a package's age on CRAN
#' @param years Numeric, years since first publication
#' @return List with `points`, `label` and `sentiment`
maturity_band <- function(years) {
  bands <- list(
    list(min = 10, points = 10, label = "Long-established",
         sentiment = "good"),
    list(min = 6, points = 8, label = "Well-established",
         sentiment = "good"),
    list(min = 3, points = 6, label = "Established",
         sentiment = "neutral"),
    list(min = 1, points = 4, label = "Establishing",
         sentiment = "warn"),
    list(min = -Inf, points = 2, label = "New to CRAN",
         sentiment = "warn")
  )
  for (b in bands) if (years >= b$min) return(b)
  bands[[length(bands)]]
}

#' Calculate a maintenance health score (0-100)
#'
#' Each factor contributes to the denominator only when the data behind
#' it arrived. A failed request therefore removes a factor rather than
#' scoring it zero, so a network blip does not read as a judgement on the
#' package. `weight_available` reports how much of the full 100 points of
#' weighting the score was computed over.
#'
#' @param metadata List from fetch_package_metadata()
#' @param versions_data List from fetch_package_versions()
#' @param download_totals List from fetch_download_totals()
#' @param rev_deps List from fetch_reverse_deps(), or NULL if unavailable
#' @param daily_downloads Data frame from fetch_daily_downloads(), used
#'   for the momentum factor; without it momentum is reported unavailable
#'   rather than guessed at from period totals
#' @param benchmarks List from fetch_benchmarks() holding the CRAN-wide
#'   `volume` and `revdeps` distributions; when supplied the volume and
#'   ecosystem labels say where the package sits against the rest of CRAN
#' @return List with `score`, `details` and `weight_available`
calculate_health_score <- function(
  metadata, versions_data, download_totals, rev_deps,
  daily_downloads = NULL, benchmarks = NULL
) {
  score <- 0
  max_score <- 0
  details <- list()

  unavailable <- function(what) {
    list(
      text = paste0(what, " unavailable"),
      sentiment = "unknown"
    )
  }

  # 1. Recency of last update (max 30 points)
  timeline <- versions_data$timeline
  if (!is.null(timeline) && length(timeline) > 0) {
    max_score <- max_score + 30
    dates <- as.Date(substr(unlist(timeline), 1, 10))
    last_update <- max(dates, na.rm = TRUE)
    days_since <- as.numeric(Sys.Date() - last_update)

    if (days_since <= 90) {
      score <- score + 30
      details$recency <- list(
        text = "Updated within last 3 months",
        sentiment = "good"
      )
    } else if (days_since <= 180) {
      score <- score + 25
      details$recency <- list(
        text = "Updated within last 6 months",
        sentiment = "good"
      )
    } else if (days_since <= 365) {
      score <- score + 18
      details$recency <- list(
        text = "Updated within last year",
        sentiment = "neutral"
      )
    } else if (days_since <= 730) {
      score <- score + 10
      details$recency <- list(
        text = "Updated within last 2 years",
        sentiment = "warn"
      )
    } else {
      score <- score + 3
      details$recency <- list(
        text = paste0(
          "Last updated ",
          round(days_since / 365, 1),
          " years ago"
        ),
        sentiment = "bad"
      )
    }
  } else {
    details$recency <- unavailable("Update history")
  }

  monthly <- download_totals$last_month %||% NA

  # 2. Download momentum (max 25 points)
  # The Trend overlay on the chart shows the comparison behind this,
  # so the label stays short.
  mom <- download_momentum(daily_downloads)
  if (!is.null(mom)) {
    max_score <- max_score + 25
    ratio <- mom$ratio

    if (ratio >= 1.1) {
      score <- score + 25
      details$momentum <- list(
        text = "Downloads trending up",
        sentiment = "good"
      )
    } else if (ratio >= 0.9) {
      score <- score + 20
      details$momentum <- list(
        text = "Downloads stable",
        sentiment = "good"
      )
    } else if (ratio >= 0.7) {
      score <- score + 12
      details$momentum <- list(
        text = "Downloads slightly declining",
        sentiment = "warn"
      )
    } else {
      score <- score + 5
      details$momentum <- list(
        text = "Downloads declining",
        sentiment = "bad"
      )
    }
  } else {
    details$momentum <- unavailable("Download trend")
  }

  # 3. Download volume (max 20 points)
  #
  # Sampling the repository puts close to nine in ten packages between
  # 100 and 1,000 downloads a month, so decade-wide bands would give
  # almost every package the same score. These cut finer through that
  # range, and stay absolute counts because a few hundred downloads a
  # month is a small user base however much of CRAN it happens to beat.
  if (!is.na(monthly)) {
    max_score <- max_score + 20

    band <- volume_band(monthly)
    score <- score + band$points
    details$volume <- list(
      text = with_context(
        paste0(band$label, " download volume"),
        format_number(monthly), "/month",
        percentile_phrase(
          download_percentile(monthly, benchmarks$volume)
        )
      ),
      sentiment = band$sentiment
    )
  } else {
    details$volume <- unavailable("Download volume")
  }

  # 4. Reverse dependencies (max 15 points)
  #
  # A zero count is a real signal about the package. NULL means the
  # lookup failed, which says nothing about it either way.
  #
  # The thresholds stay as they are: 70% of CRAN has no reverse
  # dependencies at all, so the mass sits on a single value that no
  # rebanding can subdivide. What was misleading was scoring the modal
  # case in red, since a leaf package having no dependents describes
  # what kind of package it is rather than how healthy it is.
  if (!is.null(rev_deps)) {
    max_score <- max_score + 15
    rev_total <- rev_deps$total %||% 0
    context <- percentile_phrase(
      download_percentile(rev_total, benchmarks$revdeps)
    )

    if (rev_total >= 100) {
      score <- score + 15
      details$ecosystem <- list(
        text = with_context(
          paste0(rev_total, " reverse dependencies"),
          NULL, NULL, "core ecosystem package"
        ),
        sentiment = "good"
      )
    } else if (rev_total >= 20) {
      score <- score + 12
      details$ecosystem <- list(
        text = with_context(
          paste0(rev_total, " reverse dependencies"),
          NULL, NULL, context
        ),
        sentiment = "good"
      )
    } else if (rev_total >= 5) {
      score <- score + 8
      details$ecosystem <- list(
        text = with_context(
          paste0(rev_total, " reverse dependencies"),
          NULL, NULL, context
        ),
        sentiment = "neutral"
      )
    } else if (rev_total >= 1) {
      score <- score + 4
      details$ecosystem <- list(
        text = paste0(
          rev_total,
          if (rev_total == 1) {
            " reverse dependency"
          } else {
            " reverse dependencies"
          }
        ),
        sentiment = "warn"
      )
    } else {
      details$ecosystem <- list(
        text = with_context(
          "No reverse dependencies", NULL, NULL,
          share_phrase(benchmarks$revdeps)
        ),
        sentiment = "neutral"
      )
    }
  } else {
    details$ecosystem <- unavailable("Reverse dependencies")
  }

  # 5. Maturity (max 10 points)
  #
  # Scored on time since first publication, not release count. Counting
  # releases correlates only 0.49 with age, so a package that had sat on
  # CRAN for a decade with three releases was labelled "relatively new".
  # Release count still appears in the text, where it reads as a track
  # record rather than as a claim about age.
  first_release <- if (!is.null(timeline) && length(timeline) > 0) {
    min(as.Date(substr(unlist(timeline), 1, 10)), na.rm = TRUE)
  }

  if (!is.null(first_release) && !is.na(first_release)) {
    max_score <- max_score + 10
    years <- as.numeric(Sys.Date() - first_release) / 365.25
    n_versions <- length(versions_data$versions %||% timeline)
    band <- maturity_band(years)
    score <- score + band$points

    details$maturity <- list(
      text = paste0(
        band$label, " \u2014 ",
        format_years(years), " on CRAN, ",
        n_versions,
        if (n_versions == 1) " release" else " releases"
      ),
      sentiment = band$sentiment
    )
  } else {
    details$maturity <- unavailable("Release history")
  }

  final_score <- if (max_score > 0) {
    round(score / max_score * 100)
  } else {
    NA_integer_
  }

  list(
    score = final_score,
    details = details,
    weight_available = max_score
  )
}

#' Get a color for the health score
#' @param score Numeric score, or NA when nothing could be scored
#' @return Character, a hex colour
health_score_color <- function(score) {
  if (is.null(score) || length(score) == 0 || is.na(score)) {
    return("#94a3b8")
  }
  if (score >= 75) return("#22c55e")
  if (score >= 50) return("#eab308")
  if (score >= 25) return("#f97316")
  "#ef4444"
}

#' Get a label for the health score
#' @param score Numeric score, or NA when nothing could be scored
#' @return Character label
health_score_label <- function(score) {
  if (is.null(score) || length(score) == 0 || is.na(score)) {
    return("Unavailable")
  }
  if (score >= 75) return("Excellent")
  if (score >= 50) return("Good")
  if (score >= 25) return("Fair")
  "Poor"
}

#' Parse dependency strings into a clean data frame
parse_dependencies <- function(deps_list) {
  if (is.null(deps_list) || length(deps_list) == 0) {
    return(data.frame(
      package = character(),
      version = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    package = names(deps_list),
    version = vapply(
      deps_list,
      function(x) {
        if (is.null(x) || x == "*") "any" else x
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' Build a version history data frame from crandb /all
build_version_history <- function(versions_data) {
  timeline <- versions_data$timeline
  if (is.null(timeline) || length(timeline) == 0) {
    return(NULL)
  }

  versions <- names(timeline)
  dates <- as.Date(substr(unlist(timeline), 1, 10))

  df <- data.frame(
    version = versions,
    date = dates,
    stringsAsFactors = FALSE
  )
  df <- df[order(df$date, decreasing = TRUE), ]
  df$days_since <- as.numeric(Sys.Date() - df$date)
  df
}

#' Order a crandb timeline into a version/date frame, oldest first
#' @param timeline Named list of ISO release timestamps
#' @return Data frame with `version` and `date`, or NULL if empty
version_release_frame <- function(timeline) {
  if (is.null(timeline) || length(timeline) == 0) return(NULL)

  df <- data.frame(
    version = names(timeline),
    date = as.Date(substr(unlist(timeline), 1, 10)),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$date), ]
  rownames(df) <- NULL
  df
}

#' Map each week to the version that was current when the week ended
#'
#' Taking the version current on the last day of the week rather than the
#' first means a release published mid-week still owns that week. Judging
#' from the first day instead loses any release superseded before the next
#' Monday, and any release newer than the last full week, so the chart
#' would silently show fewer versions than the package has released.
#'
#' Two releases inside one week still collapse to the later of them;
#' weekly buckets cannot show both, and the version history table is the
#' complete list.
#'
#' @param weeks Date vector of week start dates
#' @param timeline Named list of ISO release timestamps
#' @return Character vector the same length as weeks, or NULL
weekly_version_map <- function(weeks, timeline) {
  ver_df <- version_release_frame(timeline)
  if (is.null(ver_df) || length(weeks) == 0) return(NULL)

  vapply(weeks + 6, function(w) {
    idx <- which(ver_df$date <= w)
    if (length(idx) == 0) {
      ver_df$version[1]
    } else {
      ver_df$version[max(idx)]
    }
  }, character(1))
}

#' Overlay the live CRAN record onto lagging crandb data
#'
#' crandb rebuilds its index on its own schedule and can sit days behind
#' CRAN, so a freshly published version shows up on CRAN long before it
#' reaches crandb. Where the two disagree, CRAN wins: its DESCRIPTION
#' replaces the metadata, and the new release is appended to the version
#' history so the timeline, release count and recency score see it.
#'
#' @param metadata List from fetch_package_metadata()
#' @param versions List from fetch_package_versions()
#' @param cran List from fetch_cran_description()
#' @return List with `metadata`, `versions` and `stale` (logical)
reconcile_with_cran <- function(metadata, versions, cran) {
  result <- list(
    metadata = metadata, versions = versions, stale = FALSE
  )

  if (is.null(cran) || is.null(cran$Version)) return(result)
  if (is.null(metadata)) return(result)

  cran_ver <- as.character(cran$Version)
  crandb_ver <- as.character(metadata$Version %||% "")

  if (identical(cran_ver, crandb_ver)) return(result)

  # Only move forward: an older CRAN read means the package was archived
  # or the response was unexpected, and crandb stays authoritative.
  if (nzchar(crandb_ver) &&
        utils::compareVersion(cran_ver, crandb_ver) <= 0) {
    return(result)
  }

  result$stale <- TRUE
  result$metadata <- cran

  published <- cran$`Date/Publication` %||% cran$Packaged %||% NULL
  release_date <- if (!is.null(published)) {
    d <- as.Date(substr(published, 1, 10))
    if (is.na(d)) Sys.Date() else d
  } else {
    Sys.Date()
  }
  stamp <- paste0(format(release_date, "%Y-%m-%d"), "T00:00:00+00:00")

  if (is.null(result$versions)) {
    result$versions <- list(versions = list(), timeline = list())
  }
  if (is.null(result$versions$timeline)) {
    result$versions$timeline <- list()
  }
  if (is.null(result$versions$versions)) {
    result$versions$versions <- list()
  }

  if (is.null(result$versions$timeline[[cran_ver]])) {
    result$versions$timeline[[cran_ver]] <- stamp
  }
  if (is.null(result$versions$versions[[cran_ver]])) {
    result$versions$versions[[cran_ver]] <- cran
  }

  result
}
