crandb_meta <- function(version = "0.4.0") {
  list(Package = "pkg", Version = version, Title = "Old title")
}

cran_desc <- function(version = "0.5.0", published = "2026-09-02") {
  list(
    Package = "pkg",
    Version = version,
    Title = "New title",
    `Date/Publication` = paste(published, "06:20:02 UTC")
  )
}

old_versions <- function() {
  fake_versions(
    c("0.3.0", "0.4.0"), c("2026-05-19", "2026-08-03")
  )
}

test_that("a newer CRAN version replaces the crandb record", {
  r <- reconcile_with_cran(
    crandb_meta(), old_versions(), cran_desc()
  )
  expect_true(r$stale)
  expect_equal(r$metadata$Version, "0.5.0")
  expect_equal(r$metadata$Title, "New title")
})

test_that("the missing release is added to the version history", {
  r <- reconcile_with_cran(
    crandb_meta(), old_versions(), cran_desc()
  )
  expect_length(r$versions$versions, 3)
  expect_equal(
    substr(r$versions$timeline[["0.5.0"]], 1, 10), "2026-09-02"
  )

  vh <- build_version_history(r$versions)
  expect_equal(vh$version[1], "0.5.0")
  expect_equal(as.character(vh$date[1]), "2026-09-02")
})

test_that("matching versions are left alone", {
  r <- reconcile_with_cran(
    crandb_meta("0.5.0"), old_versions(), cran_desc("0.5.0")
  )
  expect_false(r$stale)
  expect_equal(r$metadata$Title, "Old title")
  expect_length(r$versions$versions, 2)
})

test_that("crandb wins when the CRAN read is older", {
  r <- reconcile_with_cran(
    crandb_meta("2.0.0"), old_versions(), cran_desc("1.0.0")
  )
  expect_false(r$stale)
  expect_equal(r$metadata$Version, "2.0.0")
})

test_that("an unreachable CRAN leaves crandb untouched", {
  r <- reconcile_with_cran(crandb_meta(), old_versions(), NULL)
  expect_false(r$stale)
  expect_equal(r$metadata$Version, "0.4.0")
})

test_that("reconciling with no crandb metadata is a no-op", {
  r <- reconcile_with_cran(NULL, NULL, cran_desc())
  expect_false(r$stale)
  expect_null(r$metadata)
})

test_that("a package absent from crandb still gets a timeline", {
  r <- reconcile_with_cran(crandb_meta(), NULL, cran_desc())
  expect_true(r$stale)
  expect_length(r$versions$timeline, 1)
  expect_equal(names(r$versions$timeline), "0.5.0")
})

test_that("the new release lifts the recency factor", {
  # A crandb record whose newest release is old enough to be scored
  # down, so that picking up the CRAN release visibly changes recency.
  lagging <- fake_versions(
    c("0.3.0", "0.4.0"),
    format(Sys.Date() - c(900, 800), "%Y-%m-%d")
  )

  stale <- calculate_health_score(
    NULL, lagging, list(last_month = 100), list(total = 0)
  )
  expect_equal(stale$details$recency$sentiment, "bad")

  fresh <- reconcile_with_cran(
    crandb_meta(), lagging,
    cran_desc(published = format(Sys.Date() - 1, "%Y-%m-%d"))
  )
  updated <- calculate_health_score(
    NULL, fresh$versions, list(last_month = 100), list(total = 0)
  )

  expect_match(
    updated$details$recency$text, "^Updated within last 3 months — "
  )
  expect_gt(updated$score, stale$score)
})
