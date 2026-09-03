test_that("a stored value is returned on the next read", {
  cache_clear()
  cache_set("k", list(a = 1))
  expect_equal(cache_get("k"), list(a = 1))
})

test_that("a miss returns NULL", {
  cache_clear()
  expect_null(cache_get("never-stored"))
})

test_that("entries past the TTL are dropped", {
  cache_clear()
  assign(
    "k",
    list(value = 1, stored = Sys.time() - CACHE_TTL_SECONDS - 10),
    envir = response_cache
  )
  expect_null(cache_get("k"))
  expect_false(exists("k", envir = response_cache, inherits = FALSE))
})

test_that("entries inside the TTL survive", {
  cache_clear()
  assign(
    "k",
    list(value = 1, stored = Sys.time() - 10),
    envir = response_cache
  )
  expect_equal(cache_get("k"), 1)
})

test_that("the cache does not grow past its limit", {
  cache_clear()
  for (i in seq_len(CACHE_MAX_ENTRIES + 20)) {
    cache_set(paste0("k", i), i)
  }
  n <- length(ls(response_cache, all.names = TRUE))
  expect_lte(n, CACHE_MAX_ENTRIES)
  # The most recent write is still readable after any eviction.
  expect_equal(
    cache_get(paste0("k", CACHE_MAX_ENTRIES + 20)),
    CACHE_MAX_ENTRIES + 20
  )
})

test_that("cache_clear empties the store", {
  cache_set("k", 1)
  cache_clear()
  expect_length(ls(response_cache, all.names = TRUE), 0)
})

test_that("a failed fetch is not cached", {
  cache_clear()
  calls <- 0
  stub("fetch_json", function(url, simplify = FALSE) {
    calls <<- calls + 1
    stop("upstream down")
  }, {
    expect_null(fetch_package_metadata("pkg"))
    expect_null(fetch_package_metadata("pkg"))
  })
  expect_equal(calls, 2)
})

test_that("requests carry a user agent and retry", {
  req <- cran_request("https://example.com")
  expect_true(grepl("cranExploreR", req$options$useragent, fixed = TRUE))
  expect_equal(req$policies$retry_max_tries, 3)
})
