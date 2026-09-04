# tidylearn's real release dates. Two of them, 0.1.1 and 0.5.0, are the
# cases that a start-of-week rule drops: one superseded before the next
# Monday, one published after the last week began.
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

test_that("each week takes the version current when it ended", {
  # Week of Mon 9 Mar ends Sun 15 Mar, by which point 0.1.1 (Fri 13th)
  # is current, so it owns that week rather than being dropped.
  map <- weekly_version_map(as.Date("2026-03-09"), tl_timeline())
  expect_equal(map, "0.1.1")

  # The following week ends after 0.2.0 lands on Mon 16 Mar.
  expect_equal(
    weekly_version_map(as.Date("2026-03-16"), tl_timeline()), "0.2.0"
  )
})

test_that("a week entirely before the first release falls back to it", {
  map <- weekly_version_map(as.Date("2026-01-05"), tl_timeline())
  expect_equal(map, "0.1.0")
})

test_that("a release published mid-week owns that week", {
  # 0.5.0 landed Wed 2 Sep, inside the week beginning Mon 31 Aug.
  map <- weekly_version_map(weeks_to("2026-08-31"), tl_timeline())
  expect_equal(map[length(map)], "0.5.0")
})

test_that("every release in the window gets a band", {
  timeline <- tl_timeline()
  weeks <- weeks_to("2026-08-31")
  banded <- unique(weekly_version_map(weeks, timeline))
  ver_df <- version_release_frame(timeline)

  expect_setequal(banded, ver_df$version)
  expect_equal(length(banded), nrow(ver_df))
})

test_that("the chart needs no marker fallback for tidylearn", {
  timeline <- tl_timeline()
  banded <- unique(weekly_version_map(weeks_to("2026-08-31"), timeline))
  expect_length(setdiff(version_release_frame(timeline)$version, banded), 0)
})

test_that("two releases in one week collapse to the later", {
  # Weekly buckets cannot show both; the version history table can.
  timeline <- fake_versions(
    c("1.0.0", "1.0.1"), c("2026-03-10", "2026-03-12")
  )$timeline
  expect_equal(
    weekly_version_map(as.Date("2026-03-09"), timeline), "1.0.1"
  )
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
