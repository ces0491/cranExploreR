test_that("momentum compares daily rates, not period totals", {
  m <- download_momentum(daily_series(200, early_rate = 20, late_rate = 10))
  expect_equal(m$ratio, 0.5)
  expect_equal(m$recent, 10)
  expect_equal(m$baseline, 20)
})

test_that("a young package is not reported as growing while it declines", {
  # tidylearn's shape: ~200 days of history, recent rate below the
  # earlier rate. The old formula divided the period total by 12 months
  # regardless of how many months the data covered, inflating momentum
  # by roughly 365/days and turning this decline into "trending up".
  daily <- daily_series(204, early_rate = 15.7, late_rate = 10.9)
  m <- download_momentum(daily)
  expect_lt(m$ratio, 1)

  h <- calculate_health_score(
    NULL,
    fake_versions("0.5.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 326, last_year = 3064),
    list(total = 0),
    daily
  )
  expect_equal(h$details$momentum$sentiment, "bad")
  expect_match(h$details$momentum$text, "^Downloads declining")

  # The old baseline, for contrast: 3064/12 = 255, against 326 a month.
  expect_gt(326 / (3064 / 12), 1.1)
})

test_that("growth is still reported as growth", {
  daily <- daily_series(200, early_rate = 10, late_rate = 20)
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 600, last_year = 3000), list(total = 0), daily
  )
  expect_equal(h$details$momentum$sentiment, "good")
  expect_match(h$details$momentum$text, "^Downloads trending up")
})

test_that("a flat series reads as stable", {
  daily <- daily_series(200, early_rate = 12, late_rate = 12)
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 360, last_year = 4380), list(total = 0), daily
  )
  expect_equal(h$details$momentum$text, "Downloads stable")
})

test_that("the reported percentage matches the ratio", {
  daily <- daily_series(200, early_rate = 20, late_rate = 10)
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 300, last_year = 4000), list(total = 0), daily
  )
  # ratio 0.5 -> 50% below
  expect_match(h$details$momentum$text, "50% below", fixed = TRUE)
  expect_match(h$details$momentum$text, "prior 170 days", fixed = TRUE)
})

test_that("too little history reports momentum unavailable", {
  expect_null(download_momentum(daily_series(40, 10, 10)))
  expect_null(download_momentum(NULL))
  expect_null(download_momentum(data.frame(a = 1)))

  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 100, last_year = 200), list(total = 0),
    daily_series(40, 10, 10)
  )
  expect_equal(h$details$momentum$sentiment, "unknown")
  # Momentum's 25 points drop out of the denominator.
  expect_equal(h$weight_available, 75)
})

test_that("a zero baseline is not divided by", {
  expect_null(download_momentum(daily_series(200, 0, 5)))
})

test_that("momentum is unavailable when no daily series is supplied", {
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 100, last_year = 1200), list(total = 0)
  )
  expect_equal(h$details$momentum$sentiment, "unknown")
})
