aged <- function(years, releases = 3) {
  first <- Sys.Date() - round(years * 365.25)
  dates <- format(
    seq(first, Sys.Date() - 1, length.out = releases), "%Y-%m-%d"
  )
  fake_versions(paste0("1.", seq_len(releases) - 1), dates)
}

test_that("maturity_band steps with age on CRAN", {
  expect_equal(maturity_band(0.5)$points, 2)
  expect_equal(maturity_band(2)$points, 4)
  expect_equal(maturity_band(4)$points, 6)
  expect_equal(maturity_band(8)$points, 8)
  expect_equal(maturity_band(15)$points, 10)
})

test_that("maturity_band is monotonic", {
  pts <- vapply(
    c(0, 0.99, 1, 2.99, 3, 5.99, 6, 9.99, 10, 30),
    function(y) maturity_band(y)$points, numeric(1)
  )
  expect_false(is.unsorted(pts))
})

test_that("a long-standing package is no longer called new", {
  # The defect: 12% of sampled packages had been on CRAN 8+ years with
  # four releases or fewer, and were labelled "relatively new".
  h <- calculate_health_score(
    NULL, aged(11, releases = 3), list(last_month = 300),
    list(total = 0), NULL
  )
  expect_equal(h$details$maturity$sentiment, "good")
  expect_match(h$details$maturity$text, "^Long-established")
  expect_match(h$details$maturity$text, "11 years on CRAN", fixed = TRUE)
  expect_match(h$details$maturity$text, "3 releases", fixed = TRUE)
})

test_that("a genuinely new package is called new despite many releases", {
  h <- calculate_health_score(
    NULL, aged(0.6, releases = 12), list(last_month = 300),
    list(total = 0), NULL
  )
  expect_match(h$details$maturity$text, "^New to CRAN")
  expect_match(h$details$maturity$text, "12 releases", fixed = TRUE)
})

test_that("release count no longer drives the score", {
  quiet_old <- calculate_health_score(
    NULL, aged(11, releases = 2), list(last_month = 300),
    list(total = 0), NULL
  )
  busy_old <- calculate_health_score(
    NULL, aged(11, releases = 40), list(last_month = 300),
    list(total = 0), NULL
  )
  expect_equal(quiet_old$score, busy_old$score)
})

test_that("age under a year is reported in months", {
  expect_equal(format_years(0.5), "6 months")
  expect_equal(format_years(1 / 12), "1 month")
  expect_equal(format_years(2.34), "2.3 years")
})

test_that("a single release reads in the singular", {
  h <- calculate_health_score(
    NULL, aged(11, releases = 1), list(last_month = 300),
    list(total = 0), NULL
  )
  expect_match(h$details$maturity$text, "1 release$")
})

test_that("maturity is unavailable without a timeline", {
  h <- calculate_health_score(
    NULL, list(versions = list()), list(last_month = 300),
    list(total = 0), NULL
  )
  expect_equal(h$details$maturity$sentiment, "unknown")
  expect_false(grepl("10", h$details$maturity$text, fixed = TRUE))
})
