# A deliberately middling package, so that renormalising over a missing
# factor moves the score somewhere visible rather than staying at 100.
middling_versions <- function() {
  fake_versions(
    c("0.1.0", "0.2.0", "0.3.0"),
    rep(format(Sys.Date() - 400, "%Y-%m-%d"), 3)
  )
}

middling_totals <- list(
  last_day = 3, last_week = 20, last_month = 100, last_year = 1200
)

# Flat rate, so momentum lands in the "stable" band and the
# arithmetic below stays about the other four factors.
middling_daily <- function() daily_series(200, 3.3, 3.3)

test_that("all factors present scores over the full weighting", {
  h <- calculate_health_score(
    NULL, middling_versions(), middling_totals, list(total = 3),
    middling_daily()
  )
  # 10/30 recency + 20/25 momentum + 5/20 volume
  #  + 4/15 ecosystem + 4/10 maturity = 43 of 100
  expect_equal(h$weight_available, 100)
  expect_equal(h$score, 43)
})

test_that("a failed lookup drops its weight instead of scoring zero", {
  h <- calculate_health_score(
    NULL, middling_versions(), middling_totals, NULL,
    middling_daily()
  )
  expect_equal(h$weight_available, 85)
  expect_equal(h$score, 46)
  expect_equal(h$details$ecosystem$sentiment, "unknown")
  expect_match(h$details$ecosystem$text, "unavailable")
})

test_that("zero reverse dependencies is scored, not treated as missing", {
  h <- calculate_health_score(
    NULL, middling_versions(), middling_totals, list(total = 0),
    middling_daily()
  )
  expect_equal(h$weight_available, 100)
  # 70% of CRAN has no dependents, so this is not a red flag.
  expect_equal(h$details$ecosystem$sentiment, "neutral")
  expect_match(h$details$ecosystem$text, "^No reverse dependencies")
})

test_that("missing version history drops recency and maturity", {
  h <- calculate_health_score(
    NULL, NULL, middling_totals, list(total = 3), middling_daily()
  )
  expect_equal(h$weight_available, 60)
  expect_equal(h$details$recency$sentiment, "unknown")
  expect_equal(h$details$maturity$sentiment, "unknown")
})

test_that("missing download data drops momentum and volume", {
  h <- calculate_health_score(
    NULL, middling_versions(), NULL, list(total = 3)
  )
  expect_equal(h$weight_available, 55)
  expect_equal(h$details$momentum$sentiment, "unknown")
  expect_equal(h$details$volume$sentiment, "unknown")
})

test_that("nothing available yields NA rather than a zero score", {
  h <- calculate_health_score(NULL, NULL, NULL, NULL)
  expect_equal(h$weight_available, 0)
  expect_true(is.na(h$score))
  expect_true(all(
    vapply(h$details, function(d) d$sentiment, character(1)) == "unknown"
  ))
})

test_that("every factor is reported whether or not it scored", {
  h <- calculate_health_score(NULL, NULL, NULL, NULL)
  expect_setequal(
    names(h$details),
    c("recency", "momentum", "volume", "ecosystem", "maturity")
  )
})

test_that("a strong package still scores well", {
  h <- calculate_health_score(
    NULL,
    # Maturity is scored on age now, so a strong package needs a
    # long history rather than merely many releases.
    fake_versions(
      paste0("1.", 0:11),
      format(
        Sys.Date() - round(seq(12 * 365, 10, length.out = 12)),
        "%Y-%m-%d"
      )
    ),
    list(
      last_day = 1e4, last_week = 7e4,
      last_month = 3e5, last_year = 3e6
    ),
    list(total = 150),
    daily_series(200, early_rate = 8000, late_rate = 10000)
  )
  expect_equal(h$score, 100)
  expect_equal(health_score_label(h$score), "Excellent")
})

test_that("score colour and label cope with NA", {
  expect_equal(health_score_label(NA), "Unavailable")
  expect_equal(health_score_label(NA_integer_), "Unavailable")
  expect_equal(health_score_color(NA), "#94a3b8")
  expect_equal(health_score_label(80), "Excellent")
  expect_equal(health_score_label(60), "Good")
  expect_equal(health_score_label(30), "Fair")
  expect_equal(health_score_label(10), "Poor")
})

test_that("weight_available distinguishes a partial score", {
  full <- calculate_health_score(
    NULL, middling_versions(), middling_totals, list(total = 3),
    middling_daily()
  )
  partial <- calculate_health_score(
    NULL, middling_versions(), middling_totals, NULL,
    middling_daily()
  )
  # The compare table and the score card both key their caveat off this.
  expect_equal(full$weight_available, 100)
  expect_lt(partial$weight_available, 100)
})
