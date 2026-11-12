# cranlogs has no row for a day without downloads. These cover filling
# those days in, and why it matters.

#' Stand in for fetch_json() with a cranlogs daily response
#' @param rows Named numeric, downloads keyed by ISO date
#' @param latest Character ISO date the last-day endpoint reports, or NULL
#'   to make that request fail
fake_cranlogs <- function(rows, latest = "2026-11-10") {
  function(url, simplify = FALSE) {
    if (grepl("/total/last-day/", url)) {
      if (is.null(latest)) stop("unavailable")
      return(data.frame(
        start = latest, end = latest, downloads = 0, package = "pkg"
      ))
    }
    list(
      downloads = list(
        data.frame(day = names(rows), downloads = unname(rows))
      ),
      package = "pkg"
    )
  }
}

sparse_rows <- c("2026-11-03" = 2, "2026-11-05" = 4, "2026-11-08" = 1)

test_that("missing days are filled as zeros and rows are kept", {
  dl <- data.frame(
    date = as.Date(names(sparse_rows)), count = unname(sparse_rows)
  )
  filled <- fill_daily_gaps(dl, as.Date("2026-11-01"), as.Date("2026-11-10"))
  expect_equal(nrow(filled), 10)
  expect_equal(sum(filled$count), 7)
  expect_equal(filled$count[filled$date == as.Date("2026-11-04")], 0)

  # A row outside the requested span widens it rather than being dropped
  wide <- fill_daily_gaps(dl, as.Date("2026-11-04"), as.Date("2026-11-06"))
  expect_equal(range(wide$date), as.Date(c("2026-11-03", "2026-11-08")))
})

test_that("the series runs from publication to cranlogs' latest day", {
  daily <- stub(
    "fetch_json", fake_cranlogs(sparse_rows, latest = "2026-11-10"),
    fetch_daily_downloads(
      "pkg", from = as.Date("2026-10-01"), to = as.Date("2026-11-11"),
      since = as.Date("2026-11-01")
    )
  )
  # 1 Nov (published, no downloads) through 10 Nov (latest in cranlogs,
  # no downloads); 11 Nov is not in cranlogs yet so is not a zero
  expect_equal(
    daily$date, seq(as.Date("2026-11-01"), as.Date("2026-11-10"), by = "day")
  )
  expect_equal(average_downloads(daily, "day"), 0.7)
})

test_that("an older package starts at the window, not its publication", {
  daily <- stub(
    "fetch_json", fake_cranlogs(sparse_rows),
    fetch_daily_downloads(
      "pkg", from = as.Date("2026-10-01"), to = as.Date("2026-11-11"),
      since = as.Date("2019-05-01")
    )
  )
  expect_equal(min(daily$date), as.Date("2026-10-01"))
})

test_that("without dates to go on, the rows returned bound the series", {
  daily <- stub(
    "fetch_json", fake_cranlogs(sparse_rows, latest = NULL),
    fetch_daily_downloads(
      "pkg", from = as.Date("2026-10-01"), to = as.Date("2026-11-11")
    )
  )
  expect_equal(range(daily$date), as.Date(c("2026-11-03", "2026-11-08")))
  expect_equal(nrow(daily), 6)
})

test_that("the latest log day is read from the last-day response", {
  expect_equal(
    stub("fetch_json", fake_cranlogs(sparse_rows, "2026-11-10"),
         fetch_latest_log_day("pkg")),
    as.Date("2026-11-10")
  )
  expect_null(
    stub("fetch_json", fake_cranlogs(sparse_rows, NULL),
         fetch_latest_log_day("pkg"))
  )
  expect_null(
    stub("fetch_json", function(url, simplify = FALSE) list(),
         fetch_latest_log_day("pkg"))
  )
})

test_that("a fading package reads as declining once zero days count", {
  # Three a day for 170 days, then three every third day for 30
  days <- seq(as.Date("2026-01-01"), by = "day", length.out = 200)
  i <- seq_along(days)
  full <- data.frame(date = days, count = ifelse(i <= 170 | i %% 3 == 0, 3, 0))
  sparse <- full[full$count > 0, ]

  # Left sparse, the recent days that had downloads look like any other
  expect_equal(download_momentum(sparse)$ratio, 1)

  filled <- fill_daily_gaps(sparse, min(days), max(days))
  expect_equal(filled$count, full$count)
  expect_equal(download_momentum(filled)$ratio, 1 / 3)
})
