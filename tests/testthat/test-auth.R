
# ============================================================
# test-auth.R
#
# Authentication and credential-specific token cache tests
#
# All tests are offline and use fake credentials.
# ============================================================


# ------------------------------------------------------------
# Helper: isolate authentication cache for each test
# ------------------------------------------------------------

local_test_auth_cache <- function(env = parent.frame()) {

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
# API key resolution
# ------------------------------------------------------------

test_that("API key defaults to environment variable", {

  withr::local_envvar(
    TUIK_SDMX_API_KEY = "ENV_TEST_KEY"
  )

  expect_identical(
    tuik_resolve_api_key(),
    "ENV_TEST_KEY"
  )
})


test_that("explicit API key overrides environment variable", {

  withr::local_envvar(
    TUIK_SDMX_API_KEY = "ENV_TEST_KEY"
  )

  expect_identical(
    tuik_resolve_api_key("EXPLICIT_TEST_KEY"),
    "EXPLICIT_TEST_KEY"
  )
})


test_that("missing API key produces a clear error", {

  withr::local_envvar(
    TUIK_SDMX_API_KEY = NA_character_
  )

  expect_error(
    tuik_get_access_token(),
    "TUIK_SDMX_API_KEY was not found.",
    fixed = TRUE
  )
})


test_that("invalid explicit API keys are rejected", {

  withr::local_envvar(
    TUIK_SDMX_API_KEY = "ENV_TEST_KEY"
  )

  invalid_keys <- list(
    "",
    "   ",
    NA_character_,
    character(),
    c("KEY_A", "KEY_B"),
    123
  )

  for (key in invalid_keys) {

    expect_error(
      tuik_resolve_api_key(key),
      "A valid API key is required.",
      fixed = TRUE
    )
  }
})


# ------------------------------------------------------------
# Token endpoint responses
# ------------------------------------------------------------

expect_sanitized_auth_error <- function(error, message, secrets) {
  expect_s3_class(error, "simpleError")
  expect_identical(conditionMessage(error), message)
  expect_null(conditionCall(error))
  expect_named(unclass(error), c("message", "call"))

  # Inspect output and the entire condition, including its attributes.
  inspected <- paste(
    capture.output(print(error)),
    capture.output(str(error)),
    rawToChar(serialize(error, NULL, ascii = TRUE)),
    collapse = "\n"
  )
  for (secret in secrets) {
    expect_false(grepl(secret, inspected, fixed = TRUE))
  }
}

test_that("authentication HTTP failures discard sensitive conditions", {
  secrets <- c("SYNTHETIC_AUTH_API_KEY", "SYNTHETIC_AUTH_BEARER_TOKEN",
               "SYNTHETIC_AUTH_RESPONSE_BODY")
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 400,
      headers = list("content-type" = "application/json"),
      body = charToRaw(paste0(
        '{"error_description":"', paste(secrets, collapse = " "), '"}'
      ))
    )
  })

  error <- tryCatch(tuik_get_access_token(secrets[[1]]), error = identity)
  expect_sanitized_auth_error(
    error, "Token endpoint request failed.", secrets
  )
})

test_that("authentication transport failures discard sensitive conditions", {
  secrets <- c("SYNTHETIC_TRANSPORT_API_KEY", "SYNTHETIC_TRANSPORT_TOKEN")
  httr2::local_mocked_responses(function(req) {
    stop(structure(
      list(message = paste(secrets, collapse = " "), call = NULL,
           request = req, response = secrets, parent = simpleError(secrets[[2]])),
      class = c("synthetic_auth_error", "error", "condition")
    ))
  })

  error <- tryCatch(tuik_get_access_token(secrets[[1]]), error = identity)
  expect_sanitized_auth_error(
    error, "Token endpoint request failed.", secrets
  )
})

test_that("malformed authentication JSON does not expose response excerpts", {
  secrets <- c("SYNTHETIC_JSON_API_KEY", "SYNTHETIC_JSON_BEARER_TOKEN")
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      body = charToRaw(paste0(
        '{"access_token":"', secrets[[2]], '", INVALID}'
      ))
    )
  })

  error <- tryCatch(tuik_get_access_token(secrets[[1]]), error = identity)
  expect_sanitized_auth_error(
    error, "Token endpoint returned invalid JSON.", secrets
  )
})

test_that("authentication errors do not echo response headers", {
  secrets <- c("SYNTHETIC_HEADER_API_KEY", "SYNTHETIC_HEADER_TOKEN")
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("content-type" = paste("text/html", secrets[[2]])),
      body = charToRaw(paste(secrets, collapse = " "))
    )
  })

  error <- tryCatch(tuik_get_access_token(secrets[[1]]), error = identity)
  expect_sanitized_auth_error(
    error, "Token endpoint did not return JSON.", secrets
  )
})

test_that("valid token response is parsed", {

  mock_response <- httr2::response(
    status_code = 200,
    headers = list(
      "content-type" = "application/json"
    ),
    body = charToRaw(
      '{"access_token":"TEST_ACCESS_TOKEN"}'
    )
  )

  httr2::local_mocked_responses(
    function(req) mock_response
  )

  result <- tuik_get_access_token(
    api_key = "EXPLICIT_TEST_KEY"
  )

  expect_identical(
    result,
    "TEST_ACCESS_TOKEN"
  )
})


test_that("token response without access_token is rejected", {

  mock_response <- httr2::response(
    status_code = 200,
    headers = list(
      "content-type" = "application/json"
    ),
    body = charToRaw(
      '{"token_type":"Bearer"}'
    )
  )

  httr2::local_mocked_responses(
    function(req) mock_response
  )

  expect_error(
    tuik_get_access_token(
      api_key = "TEST_KEY"
    ),
    "access_token was not found in the token response.",
    fixed = TRUE
  )
})


test_that("non-JSON token responses are rejected", {

  mock_response <- httr2::response(
    status_code = 200,
    headers = list(
      "content-type" = "text/html"
    ),
    body = charToRaw(
      "<html>Unexpected response</html>"
    )
  )

  httr2::local_mocked_responses(
    function(req) mock_response
  )

  expect_error(
    tuik_get_access_token(
      api_key = "TEST_KEY"
    ),
    "Token endpoint did not return JSON.",
    fixed = TRUE
  )
})


# ------------------------------------------------------------
# Credential-specific cache identifiers
# ------------------------------------------------------------

test_that("cache identifiers are deterministic and distinct", {

  id_a <- tuik_token_cache_id("TEST_KEY_A")
  id_b <- tuik_token_cache_id("TEST_KEY_B")

  expect_identical(
    id_a,
    tuik_token_cache_id("TEST_KEY_A")
  )

  expect_false(
    identical(id_a, id_b)
  )

  expect_length(
    id_a,
    1L
  )

  expect_equal(
    nchar(id_a),
    64L
  )

  expect_false(
    identical(id_a, "TEST_KEY_A")
  )
})


# ------------------------------------------------------------
# Credential-specific token cache
# ------------------------------------------------------------

test_that("cached token is reused for the same API key", {

  local_test_auth_cache()

  id <- tuik_token_cache_id("TEST_KEY_A")

  assign(
    id,
    "CACHED_TOKEN_A",
    envir = .tuiksdmx_auth$tokens
  )

  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      stop("Token endpoint should not be called.")
    }
  )

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_A"),
    "CACHED_TOKEN_A"
  )
})


test_that("empty cache requests and stores a new token", {

  local_test_auth_cache()

  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      expect_identical(
        api_key,
        "TEST_KEY_A"
      )

      "NEW_TOKEN_A"
    }
  )

  result <- tuik_cached_token(
    api_key = "TEST_KEY_A"
  )

  id <- tuik_token_cache_id("TEST_KEY_A")

  expect_identical(
    result,
    "NEW_TOKEN_A"
  )

  expect_identical(
    get(id, envir = .tuiksdmx_auth$tokens),
    "NEW_TOKEN_A"
  )
})


test_that("different API keys do not share cached tokens", {

  local_test_auth_cache()

  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {

      switch(
        api_key,
        TEST_KEY_A = "TOKEN_A",
        TEST_KEY_B = "TOKEN_B",
        stop("Unexpected API key.")
      )
    }
  )

  token_a <- tuik_cached_token(
    api_key = "TEST_KEY_A"
  )

  token_b <- tuik_cached_token(
    api_key = "TEST_KEY_B"
  )

  expect_identical(token_a, "TOKEN_A")
  expect_identical(token_b, "TOKEN_B")

  expect_identical(
    tuik_cached_token(api_key = "TEST_KEY_A"),
    "TOKEN_A"
  )

  expect_length(
    ls(.tuiksdmx_auth$tokens),
    2L
  )
})


test_that("refresh replaces only the selected token", {

  local_test_auth_cache()

  id_a <- tuik_token_cache_id("TEST_KEY_A")
  id_b <- tuik_token_cache_id("TEST_KEY_B")

  assign(
    id_a,
    "OLD_TOKEN_A",
    envir = .tuiksdmx_auth$tokens
  )

  assign(
    id_b,
    "TOKEN_B",
    envir = .tuiksdmx_auth$tokens
  )

  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {

      expect_identical(
        api_key,
        "TEST_KEY_A"
      )

      "NEW_TOKEN_A"
    }
  )

  expect_invisible(
    tuik_refresh_token(api_key = "TEST_KEY_A")
  )

  expect_identical(
    get(id_a, envir = .tuiksdmx_auth$tokens),
    "NEW_TOKEN_A"
  )

  expect_identical(
    get(id_b, envir = .tuiksdmx_auth$tokens),
    "TOKEN_B"
  )
})


test_that("clear removes only the selected token", {

  local_test_auth_cache()

  id_a <- tuik_token_cache_id("TEST_KEY_A")
  id_b <- tuik_token_cache_id("TEST_KEY_B")

  assign(
    id_a,
    "TOKEN_A",
    envir = .tuiksdmx_auth$tokens
  )

  assign(
    id_b,
    "TOKEN_B",
    envir = .tuiksdmx_auth$tokens
  )

  result <- tuik_clear_token(
    api_key = "TEST_KEY_A"
  )

  expect_null(result)

  expect_false(
    exists(id_a, envir = .tuiksdmx_auth$tokens,
           inherits = FALSE)
  )

  expect_true(
    exists(id_b, envir = .tuiksdmx_auth$tokens,
           inherits = FALSE)
  )
})


test_that("failed refresh preserves the existing token", {

  local_test_auth_cache()

  id <- tuik_token_cache_id("TEST_KEY_A")

  assign(
    id,
    "OLD_TOKEN_A",
    envir = .tuiksdmx_auth$tokens
  )

  testthat::local_mocked_bindings(
    tuik_get_access_token = function(api_key = NULL) {
      stop(
        "MOCK_AUTH_FAILURE",
        call. = FALSE
      )
    }
  )

  expect_error(
    tuik_refresh_token(api_key = "TEST_KEY_A"),
    "MOCK_AUTH_FAILURE",
    fixed = TRUE
  )

  expect_identical(
    get(id, envir = .tuiksdmx_auth$tokens),
    "OLD_TOKEN_A"
  )
})
