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
  if (is.na(x) || is.null(x)) return("N/A")
  formatC(x, format = "d", big.mark = ",")
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
        details$recency <- "Updated within last 3 months"
      } else if (days_since <= 180) {
        score <- score + 25
        details$recency <- "Updated within last 6 months"
      } else if (days_since <= 365) {
        score <- score + 18
        details$recency <- "Updated within last year"
      } else if (days_since <= 730) {
        score <- score + 10
        details$recency <- "Updated within last 2 years"
      } else {
        score <- score + 3
        details$recency <- paste0(
          "Last updated ",
          round(days_since / 365, 1), " years ago"
        )
      }
    }
  }

  # 2. Download momentum (max 25 points)
  max_score <- max_score + 25
  monthly <- download_totals$last_month
  yearly <- download_totals$last_year
  if (!is.na(monthly) && !is.na(yearly) && yearly > 0) {
    monthly_avg <- yearly / 12
    if (monthly_avg > 0) {
      momentum <- monthly / monthly_avg
      if (momentum >= 1.1) {
        score <- score + 25
        details$momentum <- "Downloads trending up"
      } else if (momentum >= 0.9) {
        score <- score + 20
        details$momentum <- "Downloads stable"
      } else if (momentum >= 0.7) {
        score <- score + 12
        details$momentum <- "Downloads slightly declining"
      } else {
        score <- score + 5
        details$momentum <- "Downloads declining"
      }
    }
  }

  # 3. Download volume (max 20 points)
  max_score <- max_score + 20
  if (!is.na(monthly)) {
    if (monthly >= 100000) {
      score <- score + 20
      details$volume <- "Very high download volume"
    } else if (monthly >= 10000) {
      score <- score + 16
      details$volume <- "High download volume"
    } else if (monthly >= 1000) {
      score <- score + 12
      details$volume <- "Moderate download volume"
    } else if (monthly >= 100) {
      score <- score + 6
      details$volume <- "Low download volume"
    } else {
      score <- score + 2
      details$volume <- "Very low download volume"
    }
  }

  # 4. Reverse dependencies (max 15 points)
  max_score <- max_score + 15
  rev_total <- rev_deps$total
  if (rev_total >= 100) {
    score <- score + 15
    details$ecosystem <- paste0(
      rev_total, " reverse dependencies",
      " \u2014 core ecosystem package"
    )
  } else if (rev_total >= 20) {
    score <- score + 12
    details$ecosystem <- paste0(
      rev_total,
      " reverse dependencies \u2014 well-established"
    )
  } else if (rev_total >= 5) {
    score <- score + 8
    details$ecosystem <- paste0(
      rev_total, " reverse dependencies"
    )
  } else if (rev_total >= 1) {
    score <- score + 4
    details$ecosystem <- paste0(
      rev_total, " reverse dependency"
    )
  } else {
    details$ecosystem <- "No reverse dependencies"
  }

  # 5. Version maturity (max 10 points)
  max_score <- max_score + 10
  if (!is.null(versions_data$versions)) {
    n_versions <- length(versions_data$versions)
    if (n_versions >= 10) {
      score <- score + 10
      details$maturity <- paste0(
        n_versions, " releases \u2014 mature package"
      )
    } else if (n_versions >= 5) {
      score <- score + 7
      details$maturity <- paste0(n_versions, " releases")
    } else if (n_versions >= 2) {
      score <- score + 4
      details$maturity <- paste0(
        n_versions, " releases \u2014 relatively new"
      )
    } else {
      score <- score + 1
      details$maturity <- "Single release"
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
