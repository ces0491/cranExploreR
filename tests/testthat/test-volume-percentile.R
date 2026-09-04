# A distribution shaped like CRAN's: a long tail of quiet packages and a
# few very large ones. Sampling the real thing put the median at roughly
# 280 downloads a month.
cran_shape <- c(
  rep(0, 5), rep(50, 15), rep(200, 30), rep(280, 20),
  rep(400, 15), rep(2000, 10), rep(50000, 4), 1500000
)

# Reverse dependencies: 70% of CRAN has none at all.
revdep_shape <- c(rep(0, 70), rep(2, 21), rep(9, 6), rep(40, 2), 500)

marks <- list(volume = cran_shape, revdeps = revdep_shape)

recent_versions <- function() {
  fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d"))
}

test_that("download_percentile places a value in the distribution", {
  expect_equal(download_percentile(0, cran_shape), 5)
  expect_equal(download_percentile(1500000, cran_shape), 100)
  expect_gt(download_percentile(280, cran_shape), 50)
  expect_lt(download_percentile(50, cran_shape), 50)
})

test_that("download_percentile returns NULL when it cannot place", {
  expect_null(download_percentile(NA, cran_shape))
  expect_null(download_percentile(NULL, cran_shape))
  expect_null(download_percentile(100, NULL))
  expect_null(download_percentile(100, numeric(0)))
})

test_that("percentile_phrase reads correctly at each end", {
  expect_equal(percentile_phrase(99), "in the top 1% of CRAN packages")
  expect_equal(percentile_phrase(100), "in the top 1% of CRAN packages")
  expect_equal(percentile_phrase(94), "in the top 6% of CRAN packages")
  expect_equal(percentile_phrase(4), "in the bottom 4% of CRAN packages")
  expect_equal(percentile_phrase(0), "in the bottom 1% of CRAN packages")
  expect_equal(percentile_phrase(68), "above 68% of CRAN packages")
  expect_null(percentile_phrase(NULL))
})

test_that("share_phrase reports the zero share", {
  expect_equal(share_phrase(revdep_shape), "as for 70% of CRAN packages")
  expect_null(share_phrase(NULL))
  expect_null(share_phrase(numeric(0)))
})

test_that("with_context joins only the parts it is given", {
  expect_equal(with_context("Low"), "Low")
  expect_equal(
    with_context("Low", "345", "/month", "above 68% of CRAN packages"),
    "Low \u2014 345/month, above 68% of CRAN packages"
  )
  expect_equal(with_context("None", NULL, NULL, "as for 70%"),
               "None \u2014 as for 70%")
})

test_that("the volume label carries count and percentile", {
  h <- calculate_health_score(
    NULL, recent_versions(), list(last_month = 400),
    list(total = 0), NULL, marks
  )
  expect_match(h$details$volume$text, "^Modest download volume")
  expect_match(h$details$volume$text, "400/month", fixed = TRUE)
  expect_match(h$details$volume$text, "% of CRAN packages")
})

test_that("without benchmarks the count shows but the percentile does not", {
  # The count is always known; only the comparison needs the sample.
  h <- calculate_health_score(
    NULL, recent_versions(), list(last_month = 400), list(total = 0)
  )
  expect_equal(h$details$volume$text, "Modest download volume \u2014 400/month")
  expect_false(grepl("CRAN packages", h$details$volume$text, fixed = TRUE))
})

test_that("volume bands cut finer where CRAN actually sits", {
  # Powers of ten put everything from 100 to 1,000 in one band; these
  # separate the range that holds most of the repository.
  expect_equal(volume_band(30)$points, 2)
  expect_equal(volume_band(120)$points, 5)
  expect_equal(volume_band(345)$points, 8)
  expect_equal(volume_band(900)$points, 12)
  expect_equal(volume_band(5000)$points, 15)
  expect_equal(volume_band(14566)$points, 18)
  expect_equal(volume_band(1500000)$points, 20)

  # 150/month and 900/month used to score identically.
  expect_false(
    identical(volume_band(150)$points, volume_band(900)$points)
  )
})

test_that("volume bands are monotonic", {
  vals <- c(0, 49, 50, 199, 200, 499, 500, 1999, 2000,
            9999, 10000, 99999, 100000, 1e7)
  pts <- vapply(vals, function(v) volume_band(v)$points, numeric(1))
  expect_false(is.unsorted(pts))
})

test_that("zero reverse dependencies reads as ordinary, not a defect", {
  h <- calculate_health_score(
    NULL, recent_versions(), list(last_month = 400),
    list(total = 0), NULL, marks
  )
  expect_equal(h$details$ecosystem$sentiment, "neutral")
  expect_match(h$details$ecosystem$text, "as for 70% of CRAN packages",
               fixed = TRUE)
})

test_that("a mid-sized dependent count gets its percentile", {
  h <- calculate_health_score(
    NULL, recent_versions(), list(last_month = 400),
    list(total = 40), NULL, marks
  )
  expect_match(h$details$ecosystem$text, "^40 reverse dependencies")
  expect_match(h$details$ecosystem$text, "% of CRAN packages")
})

test_that("a single dependent is described in the singular", {
  h <- calculate_health_score(
    NULL, recent_versions(), list(last_month = 400),
    list(total = 1), NULL, marks
  )
  expect_equal(h$details$ecosystem$text, "1 reverse dependency")
})

test_that("scoring is unaffected by the benchmark context", {
  args <- list(
    NULL, recent_versions(), list(last_month = 400),
    list(total = 0), NULL
  )
  bare <- do.call(calculate_health_score, args)
  ctx <- do.call(calculate_health_score, c(args, list(marks)))
  expect_equal(bare$score, ctx$score)
  expect_equal(bare$weight_available, ctx$weight_available)
})
