# API functions for CRAN and cranlogs data

library(httr2)
library(jsonlite)

#' Fetch package metadata from crandb
#' @param pkg_name Character, package name
#' @return List of package metadata or NULL on failure
fetch_package_metadata <- function(pkg_name) {
  tryCatch({
    resp <- request(
      paste0("https://crandb.r-pkg.org/", pkg_name)
    ) |>
      req_timeout(10) |>
      req_perform()

    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })
}

#' Fetch all versions metadata from crandb
#' @param pkg_name Character, package name
#' @return List with version history or NULL on failure
fetch_package_versions <- function(pkg_name) {
  tryCatch({
    resp <- request(
      paste0("https://crandb.r-pkg.org/", pkg_name, "/all")
    ) |>
      req_timeout(10) |>
      req_perform()

    fromJSON(resp_body_string(resp), simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })
}

#' Fetch download counts from cranlogs
#' @param pkg_name Character, package name
#' @param from Date, start of range
#' @param to Date, end of range
#' @return Data frame with date and count columns or NULL
fetch_daily_downloads <- function(
  pkg_name, from = Sys.Date() - 365, to = Sys.Date() - 1
) {
  tryCatch({
    url <- paste0(
      "https://cranlogs.r-pkg.org/downloads/daily/",
      from, ":", to, "/", pkg_name
    )
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()

    data <- fromJSON(
      resp_body_string(resp), simplifyVector = TRUE
    )

    if (!is.null(data$downloads) &&
          is.data.frame(data$downloads)) {
      df <- data$downloads
      df$date <- as.Date(df$date)
      return(df)
    }
    NULL
  }, error = function(e) {
    NULL
  })
}

#' Fetch total download counts for specific periods
#' @param pkg_name Character, package name
#' @return Named list with download counts
fetch_download_totals <- function(pkg_name) {
  periods <- c("last-day", "last-week", "last-month")

  results <- lapply(periods, function(period) {
    tryCatch({
      url <- paste0(
        "https://cranlogs.r-pkg.org/downloads/total/",
        period, "/", pkg_name
      )
      resp <- request(url) |>
        req_timeout(10) |>
        req_perform()

      data <- fromJSON(
        resp_body_string(resp), simplifyVector = TRUE
      )
      data$downloads
    }, error = function(e) {
      NA_integer_
    })
  })

  # Also get last year total
  year_total <- tryCatch({
    from <- Sys.Date() - 365
    to <- Sys.Date() - 1
    url <- paste0(
      "https://cranlogs.r-pkg.org/downloads/total/",
      from, ":", to, "/", pkg_name
    )
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()

    data <- fromJSON(
      resp_body_string(resp), simplifyVector = TRUE
    )
    data$downloads
  }, error = function(e) {
    NA_integer_
  })

  list(
    last_day = results[[1]],
    last_week = results[[2]],
    last_month = results[[3]],
    last_year = year_total
  )
}

#' Fetch reverse dependencies count from crandb
#' @param pkg_name Character, package name
#' @return List with reverse dependency counts
fetch_reverse_deps <- function(pkg_name) {
  empty <- list(
    total = 0, depends = 0,
    imports = 0, suggests = 0, linking_to = 0
  )
  tryCatch({
    url <- paste0(
      "https://crandb.r-pkg.org/-/revdeps/", pkg_name
    )
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()

    data <- fromJSON(
      resp_body_string(resp), simplifyVector = FALSE
    )

    if (!is.null(data[[pkg_name]])) {
      pkg_data <- data[[pkg_name]]
      n_depends <- length(pkg_data$Depends %||% list())
      n_imports <- length(pkg_data$Imports %||% list())
      n_suggests <- length(pkg_data$Suggests %||% list())
      n_linking <- length(pkg_data$LinkingTo %||% list())

      list(
        total = n_depends + n_imports + n_suggests + n_linking,
        depends = n_depends,
        imports = n_imports,
        suggests = n_suggests,
        linking_to = n_linking
      )
    } else {
      empty
    }
  }, error = function(e) {
    empty
  })
}

#' Search CRAN packages by keyword
#' @param query Character, search term
#' @param limit Integer, max results
#' @return Data frame of matching packages
search_packages <- function(query, limit = 20) {
  tryCatch({
    url <- paste0(
      "https://search.r-pkg.org/package/_search?q=",
      URLencode(query, reserved = TRUE),
      "&size=", limit
    )
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()

    data <- fromJSON(
      resp_body_string(resp), simplifyVector = FALSE
    )

    if (length(data$hits$hits) == 0) return(NULL)

    results <- lapply(data$hits$hits, function(hit) {
      src <- hit$`_source`
      pkg <- src$Package %||% src$package %||% ""
      ttl <- src$Title %||% src$title %||% ""
      ver <- src$Version %||% src$version %||% ""
      mnt <- src$Maintainer %||% src$maintainer %||% ""
      data.frame(
        package = pkg,
        title = ttl,
        version = ver,
        maintainer = gsub("<.*>", "", mnt),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, results)
  }, error = function(e) {
    # Fallback: try exact match via crandb
    meta <- fetch_package_metadata(query)
    if (!is.null(meta)) {
      mnt <- meta$Maintainer %||% ""
      data.frame(
        package = meta$Package %||% query,
        title = meta$Title %||% "",
        version = meta$Version %||% "",
        maintainer = gsub("<.*>", "", mnt),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
}

#' Fetch top downloaded packages from cranlogs
#' @param count Integer, number of packages to return
#' @return Data frame with package and downloads columns
fetch_top_downloads <- function(count = 30) {
  tryCatch({
    url <- paste0(
      "https://cranlogs.r-pkg.org/top/last-month/",
      count
    )
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()

    data <- fromJSON(
      resp_body_string(resp), simplifyVector = TRUE
    )

    # Response structure varies
    downloads <- NULL
    if (is.data.frame(data) &&
          "downloads" %in% names(data)) {
      downloads <- data$downloads[[1]]
    } else if (is.list(data)) {
      if (!is.null(data$downloads) &&
            is.data.frame(data$downloads)) {
        downloads <- data$downloads
      } else if (length(data) > 0 &&
                   !is.null(data[[1]]$downloads)) {
        downloads <- data[[1]]$downloads
      }
    }

    if (!is.null(downloads) &&
          is.data.frame(downloads)) {
      return(downloads)
    }
    NULL
  }, error = function(e) {
    NULL
  })
}

#' Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
