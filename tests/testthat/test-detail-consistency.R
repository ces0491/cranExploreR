# Every factor states its band, then the figure behind it. A bare label
# beside four annotated ones reads as missing data rather than as brevity.

full_inputs <- function() {
  list(
    NULL,
    fake_versions(
      c("0.1.0", "0.2.0"),
      format(Sys.Date() - c(400, 20), "%Y-%m-%d")
    ),
    list(last_month = 400),
    list(total = 3),
    daily_series(200, early_rate = 20, late_rate = 10),
    list(
      volume = c(rep(100, 50), rep(500, 50)),
      revdeps = c(rep(0, 70), rep(5, 30))
    )
  )
}

test_that("every factor carries a detail clause", {
  h <- do.call(calculate_health_score, full_inputs())
  for (k in names(h$details)) {
    expect_match(
      h$details[[k]]$text, "\u2014",
      info = paste("factor", k, "has no detail clause")
    )
  }
})

test_that("recency names the release it judged", {
  h <- do.call(calculate_health_score, full_inputs())
  expect_match(h$details$recency$text, "v0.2.0 on ", fixed = TRUE)
  expect_match(
    h$details$recency$text,
    format(Sys.Date() - 20, "%Y-%m-%d"), fixed = TRUE
  )
})

test_that("recency reports the newest release, not the last listed", {
  # Timeline deliberately out of order.
  h <- calculate_health_score(
    NULL,
    fake_versions(
      c("2.0.0", "1.0.0"),
      format(Sys.Date() - c(5, 900), "%Y-%m-%d")
    ),
    list(last_month = 400), list(total = 0)
  )
  expect_match(h$details$recency$text, "v2.0.0", fixed = TRUE)
})

test_that("momentum states the rate and the comparison", {
  h <- do.call(calculate_health_score, full_inputs())
  # 20/day falling to 10/day is a halving.
  expect_match(h$details$momentum$text, "10/day", fixed = TRUE)
  expect_match(h$details$momentum$text, "50% below", fixed = TRUE)
  expect_match(h$details$momentum$text, "prior 170 days", fixed = TRUE)
})

test_that("a flat series reads as level rather than 0% above", {
  h <- calculate_health_score(
    NULL, fake_versions("1.0.0", format(Sys.Date() - 5, "%Y-%m-%d")),
    list(last_month = 400), list(total = 0),
    daily_series(200, early_rate = 12, late_rate = 12)
  )
  expect_match(h$details$momentum$text, "level with the prior",
               fixed = TRUE)
  expect_false(grepl("0% above", h$details$momentum$text, fixed = TRUE))
})

test_that("detail clauses do not change any score", {
  args <- full_inputs()
  h <- do.call(calculate_health_score, args)
  expect_equal(h$weight_available, 100)
  # 0.2.0 landed 20 days ago, so recency takes the top band:
  # 30 recency + 5 momentum + 8 volume + 4 ecosystem + 4 maturity
  expect_equal(h$score, 51)
})

test_that("an unavailable factor stays bare", {
  h <- calculate_health_score(NULL, NULL, NULL, NULL)
  for (k in names(h$details)) {
    expect_equal(h$details[[k]]$sentiment, "unknown")
    expect_false(grepl("\u2014", h$details[[k]]$text))
  }
})
