test_that("letter browse sends the field query unescaped", {
  captured <- NULL
  stub("fetch_json", function(url, simplify = FALSE) {
    captured <<- url
    fake_search_response(c("zoo", "Alpha"), total = 1245)
  }, {
    fetch_packages_by_letter("A", limit = 50)
  })

  # Percent-encoding either character turns the prefix query into a
  # literal term, which the API answers with 200 and zero hits.
  expect_true(grepl("q=Package:A*", captured, fixed = TRUE))
  expect_false(grepl("%3A", captured, fixed = TRUE))
  expect_false(grepl("%2A", captured, fixed = TRUE))
  expect_true(grepl("&size=50", captured, fixed = TRUE))
})

test_that("letter browse orders results by name", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    fake_search_response(c("zoo", "Alpha", "beta"))
  }, {
    fetch_packages_by_letter("A")
  })

  expect_equal(df$package, c("Alpha", "beta", "zoo"))
  expect_equal(rownames(df), c("1", "2", "3"))
})

test_that("letter browse reports the full match count", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    fake_search_response(c("Alpha", "beta"), total = 1245)
  }, {
    fetch_packages_by_letter("A")
  })

  expect_equal(attr(df, "total"), 1245)
})

test_that("letter browse returns NULL when nothing matches", {
  out <- stub("fetch_json", function(url, simplify = FALSE) {
    list(hits = list(total = 0, hits = list()))
  }, {
    fetch_packages_by_letter("X")
  })

  expect_null(out)
})

test_that("letter browse returns NULL rather than erroring", {
  out <- stub("fetch_json", function(url, simplify = FALSE) {
    stop("network down")
  }, {
    fetch_packages_by_letter("A")
  })

  expect_null(out)
})

test_that("keyword search percent-encodes the query", {
  captured <- NULL
  stub("fetch_json", function(url, simplify = FALSE) {
    captured <<- url
    fake_search_response("dplyr")
  }, {
    search_packages("machine learning")
  })

  expect_true(grepl("machine%20learning", captured, fixed = TRUE))
})

test_that("keyword search returns the expected columns", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    fake_search_response(c("dplyr", "tidyr"))
  }, {
    search_packages("tidy")
  })

  expect_equal(
    names(df), c("package", "title", "version", "maintainer")
  )
  expect_equal(df$package, c("dplyr", "tidyr"))
  expect_equal(trimws(df$maintainer), c("Someone", "Someone"))
})

test_that("keyword search falls back to crandb on error", {
  df <- stub("fetch_json", function(url, simplify = FALSE) {
    if (grepl("search.r-pkg.org", url, fixed = TRUE)) {
      stop("search down")
    }
    list(
      Package = "dplyr", Title = "A Grammar of Data Manipulation",
      Version = "1.2.1", Maintainer = "Hadley <h@example.com>"
    )
  }, {
    search_packages("dplyr")
  })

  expect_equal(df$package, "dplyr")
  expect_equal(df$version, "1.2.1")
})

test_that("keyword search returns NULL when both routes fail", {
  out <- stub("fetch_json", function(url, simplify = FALSE) {
    stop("everything down")
  }, {
    search_packages("dplyr")
  })

  expect_null(out)
})
