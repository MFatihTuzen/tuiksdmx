
# ============================================================
# test-request.R
#
# HTTP request and credential-specific authentication tests.
#
# All tests are offline and use fake credentials.
# ============================================================


# ------------------------------------------------------------
# Helper: isolate token cache for each request test
# ------------------------------------------------------------

local_request_auth_cache <- function(env = parent.frame()) {

  old_tokens <- .tuiksdmx_auth$tokens

  .tuiksdmx_auth$tokens <- new.env(
    parent = emptyenv()
  )

  withr::defer(
    .tuiksdmx_auth$tokens <- old_tokens,
    envir = env
  )

  invisible(NULL)
}


# ------------------------------------------------------------
# Helper: store a fake token for a fake API key
# ------------------------------------------------------------

set_request_test_token <- function(api_key, token) {

  cache_id <- tuik_token_cache_id(api_key)

  assign(
    cache_id,
    token,
    envir = .tuiksdmx_auth$tokens
  )

  invisible(NULL)
}


# ------------------------------------------------------------
# Explicit API key propagation
# ------------------------------------------------------------

test_that("tuik_perform forwards the explicit API key", {

  local_request_auth_cache()

  withr::local_envvar(
    TUIK_SDMX_API_KEY = "ENV_TEST_KEY"
  )

  received_key <- NULL

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      received_key <<- api_key
      httr2::request("https://example.com/test")
    }
  )

  httr2::local_mocked_responses(
    function(req) {
      httr2::response(status_code = 200)
    }
  )

  resp <- tuik_perform(
    path = "test",
    max_tries = 1,
    api_key = "EXPLICIT_TEST_KEY"
  )

  expect_identical(
    received_key,
    "EXPLICIT_TEST_KEY"
  )

  expect_equal(
    httr2::resp_status(resp),
    200L
  )
})


# ------------------------------------------------------------
# HTTP 401 followed by successful refresh
# ------------------------------------------------------------

test_that("HTTP 401 refreshes only the selected API key", {

  local_request_auth_cache()

  set_request_test_token(
    "TEST_KEY_A",
    "OLD_TOKEN_A"
  )

  set_request_test_token(
    "TEST_KEY_B",
    "TOKEN_B"
  )

  request_count <- 0L
  refresh_count <- 0L
  used_tokens <- character()

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {

      expect_identical(api_key, "TEST_KEY_A")

      used_tokens <<- c(
        used_tokens,
        tuik_cached_token(api_key = api_key)
      )

      httr2::request("https://example.com/test")
    },

    tuik_get_access_token = function(api_key = NULL) {

      refresh_count <<- refresh_count + 1L

      expect_identical(api_key, "TEST_KEY_A")

      "NEW_TOKEN_A"
    }
  )

  httr2::local_mocked_responses(
    function(req) {

      request_count <<- request_count + 1L

      httr2::response(
        status_code = if (request_count == 1L) 401 else 200,
        headers = list(
          "content-type" = "application/json"
        ),
        body = charToRaw("{}")
      )
    }
  )

  expect_message(
    resp <- tuik_perform(
      path = "test",
      max_tries = 1,
      api_key = "TEST_KEY_A"
    ),
    "HTTP 401 received. Refreshing access token and retrying...",
    fixed = TRUE
  )

  expect_equal(request_count, 2L)
  expect_equal(refresh_count, 1L)

  expect_identical(
    used_tokens,
    c("OLD_TOKEN_A", "NEW_TOKEN_A")
  )

  expect_equal(
    httr2::resp_status(resp),
    200L
  )

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_B"),
    "TOKEN_B"
  )
})


# ------------------------------------------------------------
# Two consecutive HTTP 401 responses
# ------------------------------------------------------------

test_that("second HTTP 401 stops and clears only rejected token", {

  local_request_auth_cache()

  set_request_test_token(
    "TEST_KEY_A",
    "OLD_TOKEN_A"
  )

  set_request_test_token(
    "TEST_KEY_B",
    "TOKEN_B"
  )

  request_count <- 0L
  refresh_count <- 0L

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      expect_identical(api_key, "TEST_KEY_A")
      httr2::request("https://example.com/test")
    },

    tuik_get_access_token = function(api_key = NULL) {

      refresh_count <<- refresh_count + 1L

      expect_identical(api_key, "TEST_KEY_A")

      "NEW_TOKEN_A"
    }
  )

  httr2::local_mocked_responses(
    function(req) {

      request_count <<- request_count + 1L

      httr2::response(
        status_code = 401,
        headers = list(
          "content-type" = "application/json"
        ),
        body = charToRaw('{"error":"Unauthorized"}')
      )
    }
  )

  expect_error(
    suppressMessages(
      tuik_perform(
        path = "test",
        max_tries = 1,
        api_key = "TEST_KEY_A"
      )
    ),
    "HTTP status: 401",
    fixed = TRUE
  )

  expect_equal(request_count, 2L)
  expect_equal(refresh_count, 1L)

  id_a <- tuik_token_cache_id("TEST_KEY_A")

  expect_false(
    exists(
      id_a,
      envir = .tuiksdmx_auth$tokens,
      inherits = FALSE
    )
  )

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_B"),
    "TOKEN_B"
  )
})


# ------------------------------------------------------------
# Authentication refresh failure
# ------------------------------------------------------------

test_that("failed HTTP 401 refresh leaves rejected token cleared", {

  local_request_auth_cache()

  set_request_test_token(
    "TEST_KEY_A",
    "OLD_TOKEN_A"
  )

  set_request_test_token(
    "TEST_KEY_B",
    "TOKEN_B"
  )

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      httr2::request("https://example.com/test")
    },

    tuik_get_access_token = function(api_key = NULL) {

      expect_identical(api_key, "TEST_KEY_A")

      stop(
        "MOCK_REFRESH_FAILURE",
        call. = FALSE
      )
    }
  )

  httr2::local_mocked_responses(
    function(req) {
      httr2::response(status_code = 401)
    }
  )

  expect_error(
    suppressMessages(
      tuik_perform(
        path = "test",
        max_tries = 1,
        api_key = "TEST_KEY_A"
      )
    ),
    "MOCK_REFRESH_FAILURE",
    fixed = TRUE
  )

  id_a <- tuik_token_cache_id("TEST_KEY_A")

  expect_false(
    exists(
      id_a,
      envir = .tuiksdmx_auth$tokens,
      inherits = FALSE
    )
  )

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_B"),
    "TOKEN_B"
  )
})


# ------------------------------------------------------------
# HTTP 403: no token refresh
# ------------------------------------------------------------

test_that("HTTP 403 does not trigger token refresh", {

  local_request_auth_cache()

  set_request_test_token(
    "TEST_KEY_A",
    "TOKEN_A"
  )

  request_count <- 0L

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      httr2::request("https://example.com/test")
    },

    tuik_refresh_token = function(api_key = NULL) {
      stop("Refresh must not be called.")
    }
  )

  httr2::local_mocked_responses(
    function(req) {

      request_count <<- request_count + 1L

      httr2::response(
        status_code = 403,
        headers = list(
          "content-type" = "application/json"
        ),
        body = charToRaw('{"error":"Forbidden"}')
      )
    }
  )

  expect_error(
    tuik_perform(
      path = "test",
      max_tries = 1,
      api_key = "TEST_KEY_A"
    ),
    "HTTP status: 403",
    fixed = TRUE
  )

  expect_equal(request_count, 1L)

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_A"),
    "TOKEN_A"
  )
})


# ------------------------------------------------------------
# HTTP error body confidentiality
# ------------------------------------------------------------

test_that("HTTP errors do not expose server response body", {

  local_request_auth_cache()

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      httr2::request("https://example.com/test")
    }
  )

  httr2::local_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 500,
        headers = list(
          "content-type" = "application/json"
        ),
        body = charToRaw(
          '{"error":"Failed with token FAKE_SECRET_TOKEN"}'
        )
      )
    }
  )

  error_message <- tryCatch(
    {
      tuik_perform(
        path = "test",
        max_tries = 1,
        api_key = "TEST_KEY_A"
      )

      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_match(
    error_message,
    "HTTP status: 500",
    fixed = TRUE
  )

  expect_false(
    grepl(
      "FAKE_SECRET_TOKEN",
      error_message,
      fixed = TRUE
    )
  )

  expect_match(
    error_message,
    "Server response body omitted for security.",
    fixed = TRUE
  )
})


# ------------------------------------------------------------
# Diagnostic probe: HTTP 401 handling
# ------------------------------------------------------------

test_that("tuik_probe refreshes the selected token after HTTP 401", {

  local_request_auth_cache()

  set_request_test_token(
    "TEST_KEY_A",
    "OLD_TOKEN_A"
  )

  set_request_test_token(
    "TEST_KEY_B",
    "TOKEN_B"
  )

  request_count <- 0L
  refresh_count <- 0L

  testthat::local_mocked_bindings(
    tuik_build_request = function(
    path, query, accept, api_key = NULL
    ) {
      expect_identical(api_key, "TEST_KEY_A")
      httr2::request("https://example.com/test")
    },

    tuik_get_access_token = function(api_key = NULL) {

      refresh_count <<- refresh_count + 1L

      expect_identical(api_key, "TEST_KEY_A")

      "NEW_TOKEN_A"
    }
  )

  httr2::local_mocked_responses(
    function(req) {

      request_count <<- request_count + 1L

      httr2::response(
        status_code = if (request_count == 1L) 401 else 200
      )
    }
  )

  resp <- tuik_probe(
    path = "test",
    api_key = "TEST_KEY_A"
  )

  expect_equal(request_count, 2L)
  expect_equal(refresh_count, 1L)

  expect_equal(
    httr2::resp_status(resp),
    200L
  )

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_B"),
    "TOKEN_B"
  )
})
