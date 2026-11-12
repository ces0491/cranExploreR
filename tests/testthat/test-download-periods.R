#' Build a daily series at a constant rate per calendar month
#' @param from,to Character ISO dates bounding the series
#' @param rates Named numeric, downloads per day keyed by "YYYY-MM";
#'   months not named get `default`
#' @param default Numeric, the rate for unnamed months
month_series <- function(from, to, rates = c(), default = 1) {
  dates <- seq(as.Date(from), as.Date(to), by = "day")
  key <- format(dates, "%Y-%m")
  count <- ifelse(key %in% names(rates), rates[key], default)
  data.frame(date = dates, count = unname(count))
}

en_dash <- intToUtf8(0x2013)

# Averages ---------------------------------------------------------------

test_that("daily, weekly and monthly averages scale one rate", {
  daily <- month_series("2026-02-06", "2026-11-10", default = 15)
  expect_equal(average_downloads(daily, "day"), 15)
  expect_equal(average_downloads(daily, "week"), 105)
  expect_equal(average_downloads(daily, "month"), 15 * 365.25 / 12)
  expect_equal(
    average_downloads(daily, "week") / 7,
    average_downloads(daily, "month") / (365.25 / 12)
  )
})

test_that("part-weeks at the ends do not drag the weekly average down", {
  # Fri 6 Feb to Tue 10 Nov 2026: three days in the first week and two
  # in the last. Averaging the weekly totals reads those as whole weeks.
  daily <- month_series("2026-02-06", "2026-11-10", default = 10)
  week <- as.Date(cut(daily$date, "week"))
  mean_of_totals <- mean(tapply(daily$count, week, sum))
  expect_lt(mean_of_totals, 70)
  expect_equal(average_downloads(daily, "week"), 70)
})

test_that("a series shorter than the unit has no average for it", {
  # Five days at 50: scaling to a month would give 1,522 against 250
  # downloads in total
  daily <- month_series("2026-11-06", "2026-11-10", default = 50)
  expect_equal(average_downloads(daily, "day"), 50)
  expect_null(average_downloads(daily, "week"))
  expect_null(average_downloads(daily, "month"))

  week <- month_series("2026-11-04", "2026-11-10", default = 50)
  expect_equal(average_downloads(week, "week"), 350)
  expect_null(average_downloads(week, "month"))
})

test_that("averages skip missing counts and need some data", {
  daily <- month_series("2026-01-01", "2026-01-10", default = 4)
  daily$count[3] <- NA
  expect_equal(average_downloads(daily, "day"), 4)
  expect_null(average_downloads(NULL))
  expect_null(average_downloads(data.frame(a = 1)))
  expect_null(
    average_downloads(data.frame(date = as.Date("2026-01-01"), count = NA))
  )
})

# Peak month -------------------------------------------------------------

test_that("a whole month that wins is reported as complete", {
  daily <- month_series(
    "2026-01-15", "2026-04-10", rates = c("2026-03" = 10)
  )
  pk <- peak_period(daily, "month")
  expect_equal(pk$start, as.Date("2026-03-01"))
  expect_equal(pk$count, 310)
  expect_true(pk$complete)
  expect_equal(peak_period_label(pk, "month"), "310 (Mar 2026)")
})

test_that("a launch month that wins carries the days it covers", {
  # Published 6 Feb: 23 days of February outdo a whole March
  daily <- month_series(
    "2026-02-06", "2026-03-31", rates = c("2026-02" = 20, "2026-03" = 10)
  )
  pk <- peak_period(daily, "month")
  expect_equal(pk$start, as.Date("2026-02-01"))
  expect_equal(pk$count, 23 * 20)
  expect_false(pk$complete)
  expect_equal(pk$first_day, as.Date("2026-02-06"))
  expect_equal(pk$last_day, as.Date("2026-02-28"))
  expect_equal(
    peak_period_label(pk, "month"), paste0("460 (06", en_dash, "28 Feb 2026)")
  )
})

test_that("the current month to date can win and is marked partial", {
  daily <- month_series(
    "2026-08-01", "2026-11-11", rates = c("2026-11" = 50), default = 5
  )
  pk <- peak_period(daily, "month")
  expect_equal(pk$start, as.Date("2026-11-01"))
  expect_equal(pk$count, 11 * 50)
  expect_false(pk$complete)
  expect_equal(pk$last_day, as.Date("2026-11-11"))
})

test_that("month length follows the calendar, leap years included", {
  daily <- month_series(
    "2028-01-01", "2028-03-31", rates = c("2028-02" = 10)
  )
  pk <- peak_period(daily, "month")
  expect_equal(pk$start, as.Date("2028-02-01"))
  expect_equal(pk$count, 290)
  expect_true(pk$complete)
})

test_that("a tie goes to the earlier month", {
  daily <- month_series("2026-04-01", "2026-05-31", default = 0)
  daily$count[daily$date == as.Date("2026-04-10")] <- 7
  daily$count[daily$date == as.Date("2026-05-10")] <- 7
  expect_equal(peak_period(daily, "month")$start, as.Date("2026-04-01"))
})

# Peak week --------------------------------------------------------------

test_that("weeks start on Monday and a whole week reads w/c", {
  # Mon 2 Nov to Sun 8 Nov 2026 is the busy week
  daily <- month_series("2026-10-05", "2026-11-22", default = 1)
  busy <- daily$date >= as.Date("2026-11-02") &
    daily$date <= as.Date("2026-11-08")
  daily$count[busy] <- 30
  pk <- peak_period(daily, "week")
  expect_equal(pk$start, as.Date("2026-11-02"))
  expect_equal(pk$count, 210)
  expect_true(pk$complete)
  expect_equal(peak_period_label(pk, "week"), "210 (w/c 02 Nov 2026)")
})

test_that("a part-week that wins shows the days it covers", {
  # Data ends Tue 10 Nov: the last week has two days, both busy
  daily <- month_series("2026-10-05", "2026-11-10", default = 1)
  daily$count[daily$date >= as.Date("2026-11-09")] <- 100
  pk <- peak_period(daily, "week")
  expect_equal(pk$start, as.Date("2026-11-09"))
  expect_false(pk$complete)
  expect_equal(
    peak_period_label(pk, "week"), paste0("200 (09", en_dash, "10 Nov 2026)")
  )
})

test_that("missing counts are skipped and empty input returns NULL", {
  daily <- month_series("2026-01-01", "2026-02-28", rates = c("2026-02" = 3))
  daily$count[daily$date == as.Date("2026-01-05")] <- NA
  pk <- peak_period(daily, "month")
  expect_equal(pk$start, as.Date("2026-02-01"))
  expect_equal(pk$count, 84)

  expect_null(peak_period(NULL))
  expect_null(peak_period(data.frame(a = 1)))
  expect_null(
    peak_period(data.frame(date = as.Date(character()), count = numeric()))
  )
  expect_equal(peak_period_label(NULL, "week"), "N/A")
})

# Date spans -------------------------------------------------------------

test_that("date spans shorten what they share", {
  d <- as.Date
  expect_equal(date_span(d("2026-11-09"), d("2026-11-09")), "09 Nov 2026")
  expect_equal(
    date_span(d("2026-02-06"), d("2026-02-28")),
    paste0("06", en_dash, "28 Feb 2026")
  )
  expect_equal(
    date_span(d("2026-01-30"), d("2026-02-01")),
    paste0("30 Jan ", en_dash, " 01 Feb 2026")
  )
  expect_equal(
    date_span(d("2025-12-29"), d("2026-01-01")),
    paste0("29 Dec 2025 ", en_dash, " 01 Jan 2026")
  )
})
