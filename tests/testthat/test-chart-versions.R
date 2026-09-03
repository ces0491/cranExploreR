# tidylearn's real release dates, which exercise both cases where a
# release never holds a week boundary.
tl_timeline <- function() {
  fake_versions(
    c("0.1.0", "0.1.1", "0.2.0", "0.3.0", "0.3.1", "0.4.0", "0.5.0"),
    c("2026-02-06", "2026-03-13", "2026-03-16", "2026-04-09",
      "2026-05-19", "2026-08-03", "2026-09-02")
  )$timeline
}

# Mondays, as produced by cut(date, "week").
weeks_to <- function(last) {
  seq(as.Date("2026-02-02"), as.Date(last), by = "week")
}

test_that("version_release_frame orders releases oldest first", {
  df <- version_release_frame(tl_timeline())
  expect_equal(df$version[1], "0.1.0")
  expect_equal(df$version[nrow(df)], "0.5.0")
  expect_false(is.unsorted(df$date))
})

test_that("version_release_frame returns NULL when empty", {
  expect_null(version_release_frame(NULL))
  expect_null(version_release_frame(list()))
})

test_that("each week takes the version current when it began", {
  weeks <- as.Date(c("2026-02-02", "2026-02-09", "2026-04-13"))
  map <- weekly_version_map(weeks, tl_timeline())
  # The first week starts before any release, so it falls back to the
  # oldest known version rather than dropping out of the chart.
  expect_equal(map, c("0.1.0", "0.1.0", "0.3.0"))
})

test_that("a release superseded within the week holds no week", {
  map <- weekly_version_map(weeks_to("2026-08-31"), tl_timeline())
  # 0.1.1 landed Fri 13 Mar and 0.2.0 the following Monday, so no week
  # ever began with 0.1.1 current.
  expect_false("0.1.1" %in% map)
  expect_true("0.2.0" %in% map)
})

test_that("a release newer than the last week holds no week", {
  map <- weekly_version_map(weeks_to("2026-08-31"), tl_timeline())
  expect_false("0.5.0" %in% map)
})

test_that("bands plus markers account for every release", {
  timeline <- tl_timeline()
  weeks <- weeks_to("2026-08-31")
  banded <- unique(weekly_version_map(weeks, timeline))
  ver_df <- version_release_frame(timeline)

  unbanded <- setdiff(ver_df$version, banded)
  in_range <- unbanded[
    ver_df$date[match(unbanded, ver_df$version)] >= min(weeks)
  ]

  expect_setequal(unbanded, c("0.1.1", "0.5.0"))
  expect_setequal(c(banded, in_range), ver_df$version)
  expect_equal(length(banded) + length(in_range), nrow(ver_df))
})

test_that("weekly_version_map copes with a single release", {
  timeline <- fake_versions("1.0.0", "2026-01-05")$timeline
  map <- weekly_version_map(
    as.Date(c("2026-01-05", "2026-01-12")), timeline
  )
  expect_equal(map, c("1.0.0", "1.0.0"))
})

test_that("weekly_version_map returns NULL with nothing to map", {
  expect_null(weekly_version_map(as.Date(character(0)), tl_timeline()))
  expect_null(weekly_version_map(as.Date("2026-01-05"), NULL))
})
