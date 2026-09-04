# API functions for CRAN and cranlogs data

library(httr2)
library(jsonlite)

# Identify the app to the upstream services rather than sending httr2's
# default user agent. These are volunteer-run and appreciate a contact.
CRAN_USER_AGENT <- paste0(
  "cranExploreR (+https://github.com/ces0491/cranExploreR)"
)

# A single package view fans out into several requests, and the same
# package is often revisited within a session (browse, compare, back to
# the explorer). Parsed responses are held in memory for a short while
# so that repeat views cost no further requests. Only successful
# responses reach the cache, so a transient error is retried next view.
CACHE_TTL_SECONDS <- 900
CACHE_MAX_ENTRIES <- 500

response_cache <- new.env(parent = emptyenv())

#' Read a cached value, dropping it if it has expired
#' @param key Character, cache key
#' @return The cached value, or NULL on a miss
cache_get <- function(key) {
  if (!exists(key, envir = response_cache, inherits = FALSE)) {
    return(NULL)
  }
  entry <- get(key, envir = response_cache, inherits = FALSE)
  age <- as.numeric(
    difftime(Sys.time(), entry$stored, units = "secs")
  )
  if (age > CACHE_TTL_SECONDS) {
    rm(list = key, envir = response_cache)
    return(NULL)
  }
  entry$value
}

#' Store a value, making room first if the cache is full
#' @param key Character, cache key
#' @param value Value to store
#' @return The stored value, invisibly to the caller's return
cache_set <- function(key, value) {
  keys <- ls(response_cache, all.names = TRUE)

  if (length(keys) >= CACHE_MAX_ENTRIES) {
    stored <- vapply(
      keys,
      function(k) {
        as.numeric(get(k, envir = response_cache)$stored)
      },
      numeric(1)
    )
    cutoff <- as.numeric(Sys.time()) - CACHE_TTL_SECONDS
    drop <- keys[stored < cutoff]
    if (length(drop) == 0) {
      # Nothing has expired yet, so evict the oldest quarter.
      drop <- keys[order(stored)][seq_len(max(1, length(keys) %/% 4))]
    }
    rm(list = drop, envir = response_cache)
  }

  assign(
    key, list(value = value, stored = Sys.time()),
    envir = response_cache
  )
  value
}

#' Empty the response cache
#' @return NULL, invisibly
cache_clear <- function() {
  rm(
    list = ls(response_cache, all.names = TRUE),
    envir = response_cache
  )
  invisible(NULL)
}

#' Build a configured request
#'
#' An unanswered request removes a factor from the viability score, so
#' retrying keeps a brief upstream wobble from changing what the score
#' says about the package.
#'
#' @param url Character, full request URL
#' @return An httr2 request
cran_request <- function(url) {
  request(url) |>
    req_user_agent(CRAN_USER_AGENT) |>
    req_timeout(10) |>
    req_retry(max_tries = 3)
}

#' Fetch a URL and return the body as text, with caching
#' @param url Character, full request URL
#' @return Character, the response body
fetch_text <- function(url) {
  key <- paste0("text|", url)
  hit <- cache_get(key)
  if (!is.null(hit)) return(hit)

  resp <- req_perform(cran_request(url))
  cache_set(key, resp_body_string(resp))
}

#' Fetch a URL and parse the JSON body, with caching
#' @param url Character, full request URL
#' @param simplify Logical, passed to fromJSON as simplifyVector
#' @return Parsed JSON
fetch_json <- function(url, simplify = FALSE) {
  key <- paste0("json|", simplify, "|", url)
  hit <- cache_get(key)
  if (!is.null(hit)) return(hit)

  resp <- req_perform(cran_request(url))
  cache_set(
    key,
    fromJSON(resp_body_string(resp), simplifyVector = simplify)
  )
}

#' Coerce a cranlogs total response into a single number
#'
#' The shape varies with the endpoint: a named period returns a scalar,
#' a date range returns a one-row frame or a list wrapping one.
#'
#' @param x The downloads element of a parsed cranlogs response
#' @return Numeric of length 1, NA when the value cannot be read
as_download_count <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  if (is.list(x) && !is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(x)) {
    if (nrow(x) == 0 || ncol(x) == 0) return(NA_real_)
    x <- x[[1]]
  }
  if (length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x[1]))
}

#' Fetch package metadata from crandb
#' @param pkg_name Character, package name
#' @return List of package metadata or NULL on failure
fetch_package_metadata <- function(pkg_name) {
  tryCatch(
    fetch_json(paste0("https://crandb.r-pkg.org/", pkg_name)),
    error = function(e) NULL
  )
}

#' Fetch all versions metadata from crandb
#' @param pkg_name Character, package name
#' @return List with version history or NULL on failure
fetch_package_versions <- function(pkg_name) {
  tryCatch(
    fetch_json(
      paste0("https://crandb.r-pkg.org/", pkg_name, "/all")
    ),
    error = function(e) NULL
  )
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
    data <- fetch_json(
      paste0(
        "https://cranlogs.r-pkg.org/downloads/daily/",
        from, ":", to, "/", pkg_name
      ),
      simplify = TRUE
    )

    # Extract downloads data frame from response
    dl <- data$downloads
    if (is.list(dl) && !is.data.frame(dl)) {
      dl <- dl[[1]]
    }
    if (!is.null(dl) && is.data.frame(dl) && nrow(dl) > 0) {
      # Normalize column names (API returns day/downloads)
      if ("day" %in% names(dl)) {
        names(dl)[names(dl) == "day"] <- "date"
      }
      if ("downloads" %in% names(dl)) {
        names(dl)[names(dl) == "downloads"] <- "count"
      }
      dl$date <- as.Date(dl$date)
      return(dl)
    }
    NULL
  }, error = function(e) {
    NULL
  })
}

#' Fetch total download counts for specific periods
#'
#' The 365-day figure is the sum of the daily series, so pass that in
#' when it has already been fetched rather than paying for a request
#' that returns the same number.
#'
#' @param pkg_name Character, package name
#' @param daily_downloads Data frame from fetch_daily_downloads(), or NULL
#' @return Named list with download counts
fetch_download_totals <- function(
  pkg_name, daily_downloads = NULL
) {
  period_total <- function(period) {
    tryCatch(
      as_download_count(
        fetch_json(
          paste0(
            "https://cranlogs.r-pkg.org/downloads/total/",
            period, "/", pkg_name
          ),
          simplify = TRUE
        )$downloads
      ),
      error = function(e) NA_real_
    )
  }

  year_total <- if (is.data.frame(daily_downloads) &&
                      nrow(daily_downloads) > 0) {
    sum(daily_downloads$count, na.rm = TRUE)
  } else {
    period_total(paste0(Sys.Date() - 365, ":", Sys.Date() - 1))
  }

  list(
    last_day = period_total("last-day"),
    last_week = period_total("last-week"),
    last_month = period_total("last-month"),
    last_year = year_total
  )
}

#' Fetch lifetime total downloads
#' @param pkg_name Character, package name
#' @param first_published Date, first publication date
#' @return Numeric total or NA on failure
fetch_lifetime_downloads <- function(
  pkg_name, first_published
) {
  tryCatch(
    as_download_count(
      fetch_json(
        paste0(
          "https://cranlogs.r-pkg.org/downloads/total/",
          first_published, ":",
          Sys.Date() - 1, "/", pkg_name
        ),
        simplify = TRUE
      )$downloads
    ),
    error = function(e) NA_real_
  )
}

#' Fetch reverse dependencies count from crandb
#' @param pkg_name Character, package name
#' @return List with reverse dependency counts, or NULL on failure
fetch_reverse_deps <- function(pkg_name) {
  tryCatch({
    data <- fetch_json(
      paste0("https://crandb.r-pkg.org/-/revdeps/", pkg_name)
    )

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
  }, error = function(e) {
    NULL
  })
}

#' Fetch the current version of every package on CRAN
#'
#' The search index lags CRAN the same way crandb does, so the version it
#' reports alongside each hit can be out of date. One read of the CRAN
#' repository index corrects every result at once, which a per-package
#' DESCRIPTION request could not do for a 50-row page.
#'
#' The repository is named explicitly: `getOption("repos")` may point at a
#' date-pinned Package Manager snapshot on the deployment host, which
#' would reintroduce the staleness this is here to remove.
#'
#' @return Named character vector of versions keyed by package, or NULL
fetch_cran_versions <- function() {
  key <- "cran-index"
  hit <- cache_get(key)
  if (!is.null(hit)) return(hit)

  tryCatch({
    index <- available.packages(
      repos = "https://cran.r-project.org", filters = character()
    )
    if (is.null(index) || nrow(index) == 0) return(NULL)

    versions <- stats::setNames(
      as.character(index[, "Version"]),
      as.character(index[, "Package"])
    )
    versions <- versions[!is.na(versions) & nzchar(versions)]
    if (length(versions) == 0) return(NULL)

    cache_set(key, versions)
  }, error = function(e) {
    NULL
  })
}

#' Replace search-result versions with the current CRAN versions
#'
#' Leaves a row untouched when CRAN has no entry for it, which covers
#' archived packages still present in the search index.
#'
#' @param df Data frame with `package` and `version` columns
#' @param versions Named character vector from fetch_cran_versions()
#' @return The data frame with `version` corrected where possible
apply_cran_versions <- function(df, versions = fetch_cran_versions()) {
  if (is.null(df) || is.null(versions)) return(df)
  if (!all(c("package", "version") %in% names(df))) return(df)

  idx <- match(df$package, names(versions))
  found <- !is.na(idx)
  if (any(found)) {
    df$version[found] <- unname(versions[idx[found]])
  }
  df
}

#' Run a query against the r-pkg search index
#'
#' @param query_string Character, the value of the q parameter, already
#'   escaped as the caller needs it
#' @param limit Integer, max results
#' @return Data frame of matching packages with a `total` attribute
#'   giving the full match count, or NULL when nothing matched
run_package_search <- function(query_string, limit) {
  data <- fetch_json(
    paste0(
      "https://search.r-pkg.org/package/_search?q=",
      query_string, "&size=", limit
    )
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

  df <- do.call(rbind, results)
  total <- data$hits$total %||% nrow(df)

  # The index's own version field can be days behind CRAN.
  df <- apply_cran_versions(df)

  attr(df, "total") <- total
  df
}

#' Search CRAN packages by keyword
#' @param query Character, search term
#' @param limit Integer, max results
#' @return Data frame of matching packages
search_packages <- function(query, limit = 20) {
  tryCatch(
    run_package_search(
      URLencode(query, reserved = TRUE), limit
    ),
    error = function(e) {
      # Fallback: try exact match via crandb
      meta <- fetch_package_metadata(query)
      if (is.null(meta)) return(NULL)

      mnt <- meta$Maintainer %||% ""
      apply_cran_versions(data.frame(
        package = meta$Package %||% query,
        title = meta$Title %||% "",
        version = meta$Version %||% "",
        maintainer = gsub("<.*>", "", mnt),
        stringsAsFactors = FALSE
      ))
    }
  )
}

#' List packages whose name starts with a given letter
#'
#' The index stores the package name under `Package`, and both the field
#' separator and the prefix wildcard have to reach the API unescaped.
#' Percent-encoding them turns the query into a literal term that matches
#' nothing, and because the API answers 200 with an empty result set the
#' failure is silent. The index has no sortable name field, so the page
#' comes back by relevance and is ordered here.
#'
#' @param letter Character, a single letter
#' @param limit Integer, max results
#' @return Data frame of matching packages, or NULL
fetch_packages_by_letter <- function(letter, limit = 50) {
  tryCatch({
    df <- run_package_search(
      paste0("Package:", letter, "*"), limit
    )
    if (is.null(df)) return(NULL)

    total <- attr(df, "total")
    df <- df[order(tolower(df$package)), , drop = FALSE]
    rownames(df) <- NULL
    attr(df, "total") <- total
    df
  }, error = function(e) {
    NULL
  })
}

#' Sample the distribution of monthly downloads across CRAN
#'
#' Calling a package's volume "low" means nothing without knowing what
#' the rest of CRAN looks like. cranlogs' /top endpoint caps at 100
#' packages, so the distribution is estimated from a sample instead.
#'
#' The sample is systematic over the alphabetically sorted package list
#' rather than random, so the percentile a user sees is stable between
#' views and no RNG state is touched. A package's name carries no
#' relationship to its popularity, so this is unbiased for the purpose.
#'
#' @param size Integer, how many packages to sample
#' @param batch Integer, packages per cranlogs request
#' @return Numeric vector of monthly download counts, or NULL
fetch_download_distribution <- function(size = 600, batch = 200) {
  key <- paste0("distribution|", size)
  hit <- cache_get(key)
  if (!is.null(hit)) return(hit)

  tryCatch({
    versions <- fetch_cran_versions()
    if (is.null(versions)) return(NULL)

    pkgs <- sort(names(versions))
    if (length(pkgs) > size) {
      pkgs <- pkgs[round(seq(1, length(pkgs), length.out = size))]
    }

    chunks <- split(pkgs, ceiling(seq_along(pkgs) / batch))
    counts <- unlist(lapply(chunks, function(ch) {
      data <- fetch_json(
        paste0(
          "https://cranlogs.r-pkg.org/downloads/total/last-month/",
          paste(ch, collapse = ",")
        ),
        simplify = TRUE
      )
      if (is.data.frame(data) && "downloads" %in% names(data)) {
        as.numeric(data$downloads)
      } else {
        numeric(0)
      }
    }), use.names = FALSE)

    counts <- counts[!is.na(counts)]
    if (length(counts) == 0) return(NULL)

    cache_set(key, counts)
  }, error = function(e) {
    NULL
  })
}

#' Count reverse dependencies for every package on CRAN
#'
#' The CRAN index carries each package's Depends, Imports, LinkingTo and
#' Suggests, so inverting it gives the whole reverse-dependency graph
#' from one request. crandb's per-package endpoint stays the source for
#' the breakdown shown on screen; this exists to say where a count sits
#' relative to the rest of CRAN.
#'
#' @return Named integer vector of counts keyed by package, or NULL
fetch_revdep_counts <- function() {
  key <- "revdep-counts"
  hit <- cache_get(key)
  if (!is.null(hit)) return(hit)

  tryCatch({
    index <- available.packages(
      repos = "https://cran.r-project.org", filters = character()
    )
    if (is.null(index) || nrow(index) == 0) return(NULL)

    fields <- c("Depends", "Imports", "LinkingTo", "Suggests")
    depended_on <- unlist(lapply(fields, function(f) {
      x <- index[, f]
      x <- x[!is.na(x) & nzchar(x)]
      parts <- unlist(strsplit(x, ",", fixed = TRUE))
      trimws(sub("\\(.*", "", parts))
    }), use.names = FALSE)
    depended_on <- depended_on[nzchar(depended_on)]

    tallied <- table(depended_on)
    counts <- as.integer(tallied[rownames(index)])
    counts[is.na(counts)] <- 0L
    names(counts) <- rownames(index)

    cache_set(key, counts)
  }, error = function(e) {
    NULL
  })
}

#' Assemble the CRAN-wide distributions the score compares against
#'
#' Both are cached, so this is cheap after the first call in a session.
#'
#' @return List with `volume` and `revdeps` numeric vectors
fetch_benchmarks <- function() {
  list(
    volume = fetch_download_distribution(),
    revdeps = fetch_revdep_counts()
  )
}

#' Fetch top downloaded packages from cranlogs
#' @param count Integer, number of packages to return
#' @return Data frame with package and downloads columns
fetch_top_downloads <- function(count = 30) {
  tryCatch({
    data <- fetch_json(
      paste0(
        "https://cranlogs.r-pkg.org/top/last-month/", count
      ),
      simplify = TRUE
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
      # Ensure downloads column is numeric
      if ("downloads" %in% names(downloads)) {
        downloads$downloads <- as.numeric(
          downloads$downloads
        )
      }
      return(downloads)
    }
    NULL
  }, error = function(e) {
    NULL
  })
}

#' Parse a DCF dependency field into the crandb named-list shape
#' @param field Character, e.g. "R (>= 3.6.0), dplyr, stats"
#' @return Named list mapping package to constraint ("*" if none)
parse_dcf_deps <- function(field) {
  if (is.null(field) || is.na(field) || !nzchar(trimws(field))) {
    return(NULL)
  }

  parts <- trimws(strsplit(field, ",")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(NULL)

  open_at <- regexpr("(", parts, fixed = TRUE)
  close_at <- regexpr(")", parts, fixed = TRUE)

  pkgs <- trimws(ifelse(
    open_at > 0, substr(parts, 1, open_at - 1), parts
  ))
  constraints <- trimws(ifelse(
    open_at > 0 & close_at > open_at,
    substr(parts, open_at + 1, close_at - 1),
    "*"
  ))
  constraints[!nzchar(constraints)] <- "*"

  keep <- nzchar(pkgs)
  if (!any(keep)) return(NULL)

  stats::setNames(as.list(constraints[keep]), pkgs[keep])
}

#' Fetch the current DESCRIPTION straight from CRAN
#'
#' crandb lags behind CRAN (it has gone days between syncs), so the
#' version it reports can be stale. CRAN's own DESCRIPTION file is the
#' authoritative record of what is currently published.
#'
#' @param pkg_name Character, package name
#' @return List of metadata in crandb's shape, or NULL on failure
fetch_cran_description <- function(pkg_name) {
  tryCatch({
    txt <- fetch_text(
      paste0(
        "https://cran.r-project.org/web/packages/",
        pkg_name, "/DESCRIPTION"
      )
    )

    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    dcf <- read.dcf(con)

    if (nrow(dcf) == 0) return(NULL)

    fields <- as.list(dcf[1, ])
    fields <- fields[!is.na(unlist(fields))]

    if (is.null(fields$Version) || is.null(fields$Package)) {
      return(NULL)
    }

    for (dep in c("Depends", "Imports", "Suggests", "LinkingTo",
                  "Enhances")) {
      fields[[dep]] <- parse_dcf_deps(fields[[dep]])
    }

    fields
  }, error = function(e) {
    NULL
  })
}

#' Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
