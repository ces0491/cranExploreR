# Load the app's functions into the global environment.
#
# source() with the default local = FALSE evaluates into globalenv(), so
# the functions close over globalenv() and a test can swap one out by
# reassigning the name there. stub() below relies on that.
source("../../R/api_functions.R")
source("../../R/helpers.R")

#' Temporarily replace a global function for the duration of an expression
#' @param name Character, the function to replace
#' @param value The replacement
#' @param code Expression to evaluate with the replacement in place
stub <- function(name, value, code) {
  old <- get(name, envir = globalenv())
  assign(name, value, envir = globalenv())
  on.exit(assign(name, old, envir = globalenv()), add = TRUE)
  force(code)
}

#' Build a fake search-index response for a set of package names
#' @param pkgs Character vector of package names
#' @param total Integer, the reported total match count
#' @return A list shaped like a parsed Elasticsearch response
fake_search_response <- function(pkgs, total = length(pkgs)) {
  list(
    hits = list(
      total = total,
      hits = lapply(pkgs, function(p) {
        list(
          `_source` = list(
            Package = p,
            Title = paste(p, "title"),
            Version = "1.0.0",
            Maintainer = paste0("Someone <", p, "@example.com>")
          )
        )
      })
    )
  )
}

#' Build a crandb-shaped version history
#' @param versions Character vector of version strings
#' @param dates Character vector of ISO dates, same length
#' @return A list with `versions` and `timeline`
fake_versions <- function(versions, dates) {
  list(
    versions = stats::setNames(
      lapply(versions, function(v) list(Version = v)), versions
    ),
    timeline = stats::setNames(
      as.list(paste0(dates, "T00:00:00+00:00")), versions
    )
  )
}
