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
  expect_match(h$details$momentum$text, "^Downloads declining — ")

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
  expect_match(h$details$momentum$text, "^Downloads trending up — ")
})

test_that("a flat series reads as stable", {
  daily <- daily_series(200, early_rate = 12, late_rate = 12)
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 360, last_year = 4380), list(total = 0), daily
  )
  expect_match(h$details$momentum$text, "^Downloads stable — ")
})

test_that("the label carries the figures behind the verdict", {
  daily <- daily_series(200, early_rate = 20, late_rate = 10)
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 1, "%Y-%m-%d")),
    list(last_month = 300, last_year = 4000), list(total = 0), daily
  )
  expect_match(h$details$momentum$text, "^Downloads declining — ")

  # The label quotes these; the chart does not draw the baseline.
  m <- download_momentum(daily)
  expect_equal(m$baseline_days, 170)
  expect_equal(round((1 - m$ratio) * 100), 50)
  expect_match(h$details$momentum$text, "50% below the prior 170 days")
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

# Rolling rate, drawn as the chart's trend line ---------------------------

test_that("the trend line ends on the rate the momentum verdict quotes", {
  # Both on their defaults, so a change to the shared window keeps them
  # in step or fails here
  daily <- daily_series(204, early_rate = 15.7, late_rate = 10.9)
  roll <- rolling_rate(daily)
  expect_equal(roll$rate[nrow(roll)], download_momentum(daily)$recent)
  expect_equal(max(roll$date), max(daily$date))
})

test_that("the trend line moves from one rate to the next over a window", {
  daily <- daily_series(200, early_rate = 20, late_rate = 10)
  roll <- rolling_rate(daily, window = 30)
  step <- max(daily$date) - 30

  # Windows wholly before the step sit at the early rate
  expect_true(all(roll$rate[roll$date <= step] == 20))
  # Ten days past the step: 20 days at 20, 10 at 10
  expect_equal(roll$rate[roll$date == step + 10], (20 * 20 + 10 * 10) / 30)
  expect_equal(roll$rate[nrow(roll)], 10)
})

test_that("the trend line starts once a full window has elapsed", {
  daily <- daily_series(100, 5, 5)
  roll <- rolling_rate(daily, window = 30)
  expect_equal(min(roll$date), min(daily$date) + 29)
  expect_equal(nrow(roll), 100 - 29)

  expect_null(rolling_rate(daily_series(29, 5, 5), window = 30))
  expect_null(rolling_rate(NULL))
  expect_null(rolling_rate(data.frame(a = 1)))
})
