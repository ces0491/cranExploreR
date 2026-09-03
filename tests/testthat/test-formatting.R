test_that("format_number handles scalars", {
  expect_equal(format_number(0), "0")
  expect_equal(format_number(1234567), "1,234,567")
  expect_equal(format_number(1234567.4), "1,234,567")
})

test_that("format_number reports missing values as N/A", {
  expect_equal(format_number(NULL), "N/A")
  expect_equal(format_number(numeric(0)), "N/A")
  expect_equal(format_number(NA), "N/A")
  expect_equal(format_number(NA_real_), "N/A")
})

test_that("format_number is vectorised", {
  expect_equal(
    format_number(c(1000, NA, 25)),
    c("1,000", "N/A", "25")
  )
  expect_length(format_number(c(1, 2, 3)), 3)
})

test_that("parse_dcf_deps splits packages from constraints", {
  out <- parse_dcf_deps("R (>= 3.6.0), dplyr, stats (>= 4.0)")
  expect_equal(names(out), c("R", "dplyr", "stats"))
  expect_equal(out$R, ">= 3.6.0")
  expect_equal(out$dplyr, "*")
  expect_equal(out$stats, ">= 4.0")
})

test_that("parse_dcf_deps handles DCF line continuations", {
  out <- parse_dcf_deps("dplyr (>= 1.0.0),\n  ggplot2,\n  tibble")
  expect_equal(names(out), c("dplyr", "ggplot2", "tibble"))
  expect_equal(out$ggplot2, "*")
})

test_that("parse_dcf_deps returns NULL for empty input", {
  expect_null(parse_dcf_deps(NULL))
  expect_null(parse_dcf_deps(NA_character_))
  expect_null(parse_dcf_deps(""))
  expect_null(parse_dcf_deps("   "))
})

test_that("parse_dependencies renders an unconstrained dep as 'any'", {
  df <- parse_dependencies(list(dplyr = ">= 1.0.0", stats = "*"))
  expect_equal(df$package, c("dplyr", "stats"))
  expect_equal(df$version, c(">= 1.0.0", "any"))
})

test_that("parse_dependencies returns an empty frame for no deps", {
  expect_equal(nrow(parse_dependencies(NULL)), 0)
  expect_equal(nrow(parse_dependencies(list())), 0)
})

test_that("as_download_count reads every cranlogs response shape", {
  expect_equal(as_download_count(42), 42)
  expect_equal(as_download_count(list(42)), 42)
  expect_equal(as_download_count(data.frame(downloads = 42)), 42)
  expect_true(is.na(as_download_count(NULL)))
  expect_true(is.na(as_download_count(list())))
})

test_that("build_version_history sorts newest first", {
  vh <- build_version_history(
    fake_versions(
      c("0.1.0", "1.0.0", "0.5.0"),
      c("2024-01-01", "2025-01-01", "2024-06-01")
    )
  )
  expect_equal(vh$version, c("1.0.0", "0.5.0", "0.1.0"))
  expect_true(all(diff(vh$days_since) > 0))
})

test_that("build_version_history returns NULL with no timeline", {
  expect_null(build_version_history(list()))
  expect_null(build_version_history(list(timeline = list())))
})
