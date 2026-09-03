stale_results <- function() {
  data.frame(
    package = c("tidylearn", "dplyr", "archivedpkg"),
    title = c("A", "B", "C"),
    version = c("0.4.0", "1.2.0", "9.9.9"),
    maintainer = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )
}

cran_index <- c(tidylearn = "0.5.0", dplyr = "1.2.1")

test_that("stale result versions are corrected against CRAN", {
  df <- apply_cran_versions(stale_results(), cran_index)
  expect_equal(df$version[df$package == "tidylearn"], "0.5.0")
  expect_equal(df$version[df$package == "dplyr"], "1.2.1")
})

test_that("a package absent from CRAN keeps its indexed version", {
  df <- apply_cran_versions(stale_results(), cran_index)
  expect_equal(df$version[df$package == "archivedpkg"], "9.9.9")
})

test_that("other columns are left alone", {
  before <- stale_results()
  after <- apply_cran_versions(before, cran_index)
  expect_equal(after$package, before$package)
  expect_equal(after$title, before$title)
  expect_equal(after$maintainer, before$maintainer)
})

test_that("an unavailable CRAN index leaves results untouched", {
  before <- stale_results()
  expect_equal(apply_cran_versions(before, NULL), before)
})

test_that("a frame without the expected columns passes through", {
  df <- data.frame(package = "dplyr", downloads = 10)
  expect_equal(apply_cran_versions(df, cran_index), df)
  expect_null(apply_cran_versions(NULL, cran_index))
})

test_that("search results are corrected and keep their total", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    fake_search_response(c("tidylearn", "dplyr"), total = 42)
  }, {
    stub("fetch_cran_versions", function() cran_index, {
      search_packages("tidy")
    })
  })

  # fake_search_response reports 1.0.0 for every hit.
  expect_equal(df$version, c("0.5.0", "1.2.1"))
  expect_equal(attr(df, "total"), 42)
})

test_that("letter browse results are corrected too", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    fake_search_response(c("dplyr", "tidylearn"), total = 42)
  }, {
    stub("fetch_cran_versions", function() cran_index, {
      fetch_packages_by_letter("d")
    })
  })

  expect_equal(df$package, c("dplyr", "tidylearn"))
  expect_equal(df$version, c("1.2.1", "0.5.0"))
})

test_that("the CRAN index is cached rather than re-read", {
  cache_clear()
  calls <- 0
  stub("available.packages", function(...) {
    calls <<- calls + 1
    cbind(Package = "dplyr", Version = "1.2.1")
  }, {
    expect_equal(fetch_cran_versions(), c(dplyr = "1.2.1"))
    expect_equal(fetch_cran_versions(), c(dplyr = "1.2.1"))
  })
  expect_equal(calls, 1)
})

test_that("an unreadable CRAN index yields NULL, not an error", {
  cache_clear()
  out <- stub("available.packages", function(...) stop("offline"), {
    fetch_cran_versions()
  })
  expect_null(out)

  cache_clear()
  empty <- stub("available.packages", function(...) {
    matrix(character(0), nrow = 0, ncol = 2,
           dimnames = list(NULL, c("Package", "Version")))
  }, {
    fetch_cran_versions()
  })
  expect_null(empty)
})
