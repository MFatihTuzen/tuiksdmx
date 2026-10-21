# Offline tests for optional access-token expiration metadata.

testthat::test_that("cached tokens refresh proactively when expiry is reached", {
  calls <- 0L
  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      calls <<- calls + 1L
      token <- paste0("TOKEN_", calls)
      attr(token, "tuik_expires_in") <- 120
      token
    }
  )
  on.exit(tuik_clear_token(api_key = "EXPIRY_TEST_A"), add = TRUE)
  tuik_clear_token(api_key = "EXPIRY_TEST_A")

  testthat::expect_identical(tuik_cached_token("EXPIRY_TEST_A"), "TOKEN_1")
  testthat::expect_identical(tuik_cached_token("EXPIRY_TEST_A"), "TOKEN_1")
  testthat::expect_identical(calls, 1L)

  id <- tuik_token_cache_id("EXPIRY_TEST_A")
  assign(id, Sys.time() - 1, envir = .tuiksdmx_auth$expires_at)
  testthat::expect_identical(tuik_cached_token("EXPIRY_TEST_A"), "TOKEN_2")
  testthat::expect_identical(calls, 2L)
})

testthat::test_that("missing expiry preserves current cache and 401 fallback", {
  calls <- 0L
  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      calls <<- calls + 1L
      paste0("PLAIN_", calls)
    }
  )
  on.exit(tuik_clear_token(api_key = "EXPIRY_TEST_B"), add = TRUE)
  tuik_clear_token(api_key = "EXPIRY_TEST_B")
  testthat::expect_identical(tuik_cached_token("EXPIRY_TEST_B"), "PLAIN_1")
  testthat::expect_identical(tuik_cached_token("EXPIRY_TEST_B"), "PLAIN_1")
  testthat::expect_identical(calls, 1L)
  id <- tuik_token_cache_id("EXPIRY_TEST_B")
  testthat::expect_false(exists(id, envir = .tuiksdmx_auth$expires_at,
                               inherits = FALSE))
})

testthat::test_that("clearing one credential removes only its expiry", {
  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      token <- paste0("TOKEN_", api_key)
      attr(token, "tuik_expires_in") <- 120
      token
    }
  )
  keys <- c("EXPIRY_TEST_C", "EXPIRY_TEST_D")
  on.exit(for (key in keys) tuik_clear_token(api_key = key), add = TRUE)
  for (key in keys) tuik_cached_token(key)
  id_c <- tuik_token_cache_id(keys[[1]])
  id_d <- tuik_token_cache_id(keys[[2]])
  tuik_clear_token(keys[[1]])
  testthat::expect_false(exists(id_c, envir = .tuiksdmx_auth$expires_at,
                               inherits = FALSE))
  testthat::expect_true(exists(id_d, envir = .tuiksdmx_auth$expires_at,
                              inherits = FALSE))
})
