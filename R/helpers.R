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

#' Format large numbers with commas
format_number <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("N/A")
  }
  formatC(round(x), format = "f", digits = 0, big.mark = ",")
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

#' Calculate a maintenance health score (0-100)
#' Based on recency of updates, download trends, etc.
calculate_health_score <- function(
  metadata, versions_data, download_totals, rev_deps
) {
  score <- 0
  max_score <- 0
  details <- list()

  # 1. Recency of last update (max 30 points)
  max_score <- max_score + 30
  if (!is.null(versions_data)) {
    timeline <- versions_data$timeline
    if (!is.null(timeline) && length(timeline) > 0) {
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
    }
  }

  # 2. Download momentum (max 25 points)
  max_score <- max_score + 25
  monthly <- if (!is.null(download_totals)) {
    download_totals$last_month
  } else {
    NA
  }
  yearly <- if (!is.null(download_totals)) {
    download_totals$last_year
  } else {
    NA
  }
  if (!is.na(monthly) && !is.na(yearly) && yearly > 0) {
    monthly_avg <- yearly / 12
    if (monthly_avg > 0) {
      momentum <- monthly / monthly_avg
      if (momentum >= 1.1) {
        score <- score + 25
        details$momentum <- list(
          text = "Downloads trending up",
          sentiment = "good"
        )
      } else if (momentum >= 0.9) {
        score <- score + 20
        details$momentum <- list(
          text = "Downloads stable",
          sentiment = "good"
        )
      } else if (momentum >= 0.7) {
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
    }
  }

  # 3. Download volume (max 20 points)
  max_score <- max_score + 20
  if (!is.null(monthly) && !is.na(monthly)) {
    if (monthly >= 100000) {
      score <- score + 20
      details$volume <- list(
        text = "Very high download volume",
        sentiment = "good"
      )
    } else if (monthly >= 10000) {
      score <- score + 16
      details$volume <- list(
        text = "High download volume",
        sentiment = "good"
      )
    } else if (monthly >= 1000) {
      score <- score + 12
      details$volume <- list(
        text = "Moderate download volume",
        sentiment = "neutral"
      )
    } else if (monthly >= 100) {
      score <- score + 6
      details$volume <- list(
        text = "Low download volume",
        sentiment = "warn"
      )
    } else {
      score <- score + 2
      details$volume <- list(
        text = "Very low download volume",
        sentiment = "bad"
      )
    }
  }

  # 4. Reverse dependencies (max 15 points)
  max_score <- max_score + 15
  rev_total <- if (!is.null(rev_deps)) {
    rev_deps$total
  } else {
    0
  }
  if (rev_total >= 100) {
    score <- score + 15
    details$ecosystem <- list(
      text = paste0(
        rev_total, " reverse dependencies",
        " \u2014 core ecosystem package"
      ),
      sentiment = "good"
    )
  } else if (rev_total >= 20) {
    score <- score + 12
    details$ecosystem <- list(
      text = paste0(
        rev_total, " reverse dependencies",
        " \u2014 well-established"
      ),
      sentiment = "good"
    )
  } else if (rev_total >= 5) {
    score <- score + 8
    details$ecosystem <- list(
      text = paste0(
        rev_total, " reverse dependencies"
      ),
      sentiment = "neutral"
    )
  } else if (rev_total >= 1) {
    score <- score + 4
    details$ecosystem <- list(
      text = paste0(
        rev_total, " reverse dependency"
      ),
      sentiment = "warn"
    )
  } else {
    details$ecosystem <- list(
      text = "No reverse dependencies",
      sentiment = "bad"
    )
  }

  # 5. Version maturity (max 10 points)
  max_score <- max_score + 10
  if (!is.null(versions_data$versions)) {
    n_versions <- length(versions_data$versions)
    if (n_versions >= 10) {
      score <- score + 10
      details$maturity <- list(
        text = paste0(
          n_versions,
          " releases \u2014 mature package"
        ),
        sentiment = "good"
      )
    } else if (n_versions >= 5) {
      score <- score + 7
      details$maturity <- list(
        text = paste0(n_versions, " releases"),
        sentiment = "neutral"
      )
    } else if (n_versions >= 2) {
      score <- score + 4
      details$maturity <- list(
        text = paste0(
          n_versions,
          " releases \u2014 relatively new"
        ),
        sentiment = "warn"
      )
    } else {
      score <- score + 1
      details$maturity <- list(
        text = "Single release",
        sentiment = "warn"
      )
    }
  }

  final_score <- if (max_score > 0) {
    round(score / max_score * 100)
  } else {
    0
  }

  list(score = final_score, details = details)
}

#' Get a color for the health score
health_score_color <- function(score) {
  if (score >= 75) return("#22c55e")
  if (score >= 50) return("#eab308")
  if (score >= 25) return("#f97316")
  "#ef4444"
}

#' Get a label for the health score
health_score_label <- function(score) {
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
