test_that("codelist cache is restored after isolation", {

  cache <- get(
    ".tuiksdmx_codelist_cache",
    envir = asNamespace("tuiksdmx")
  )

  # Preserve the cache for this test.
  local_codelist_cache()

  # Simulate an existing cache entry.
  assign("ORIGINAL", "saved_value", envir = cache)

  run_isolated <- function() {

    local_codelist_cache()

    expect_false(
      exists("ORIGINAL", envir = cache, inherits = FALSE)
    )

    assign("TEMPORARY", "test_value", envir = cache)
  }

  run_isolated()

  expect_identical(
    get("ORIGINAL", envir = cache, inherits = FALSE),
    "saved_value"
  )

  expect_false(
    exists("TEMPORARY", envir = cache, inherits = FALSE)
  )
})
