
# ============================================================================
# request.R
#
# Low-level HTTP request helpers for the TUIK SDMX Web Service.
#
# Responsibilities:
#   - Building SDMX REST requests
#   - Adding Bearer authentication
#   - Supporting explicit or environment-based API keys
#   - Handling query parameters and Accept headers
#   - Refreshing the correct token after HTTP 401
#   - Applying timeout and retry rules
#
# Higher-level functions should use tuik_perform().
# ============================================================================


# Base URL of the TUIK SDMX REST service
.tuiksdmx_base_url <- "https://nsiws.tuik.gov.tr/rest/"


# ---------------------------------------------------------------------------
# Build an authenticated HTTP request
# ---------------------------------------------------------------------------

#' Build a TUIK SDMX request
#'
#' Creates an authenticated HTTP request for a TUIK SDMX REST endpoint.
#'
#' @param path Character string containing the REST path relative
#'   to the TUIK SDMX base URL.
#' @param query Named list of query parameters.
#' @param accept Optional HTTP Accept header.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return An httr2 request object.
#'
#' @keywords internal
tuik_build_request <- function(
    path,
    query = list(),
    accept = NULL,
    api_key = NULL
) {

  if (
    !is.character(path) ||
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path)
  ) {
    stop(
      "`path` must be a non-empty character string.",
      call. = FALSE
    )
  }

  # Resolve the credential used for this request.
  api_key <- tuik_resolve_api_key(api_key)

  # Obtain the token associated with this credential.
  token <- tuik_cached_token(
    api_key = api_key
  )

  req <- httr2::request(
    paste0(
      .tuiksdmx_base_url,
      path
    )
  ) |>
    httr2::req_headers(
      Authorization = paste0(
        "Bearer ",
        token
      )
    ) |>
    httr2::req_timeout(
      seconds = 30
    )

  if (length(query) > 0) {

    req <- req |>
      httr2::req_url_query(
        !!!query
      )
  }

  if (!is.null(accept)) {

    req <- req |>
      httr2::req_headers(
        Accept = accept
      )
  }

  req
}


# ---------------------------------------------------------------------------
# Perform an authenticated SDMX request
# ---------------------------------------------------------------------------

#' Perform a TUIK SDMX request
#'
#' Performs an authenticated request against the TUIK SDMX
#' REST service.
#'
#' Behaviour:
#'   - Uses an explicitly supplied or environment-based API key
#'   - Retrieves the token associated with that API key
#'   - Retries temporary connection failures
#'   - Refreshes only the selected credential's token on HTTP 401
#'   - Retries the request once after token refresh
#'   - Converts remaining HTTP errors into readable R errors
#'
#' @param path Character string containing the REST path.
#' @param query Named list of query parameters.
#' @param accept Optional HTTP Accept header.
#' @param timeout Request timeout in seconds.
#' @param max_tries Maximum number of attempts for temporary
#'   network failures.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return An httr2 response object.
#'
#' @keywords internal
tuik_perform <- function(
    path,
    query = list(),
    accept = NULL,
    timeout = 60,
    max_tries = 3,
    api_key = NULL
) {

  # Resolve once so that both attempts use the same credential.
  api_key <- tuik_resolve_api_key(api_key)

  perform_once <- function() {

    req <- tuik_build_request(
      path = path,
      query = query,
      accept = accept,
      api_key = api_key
    ) |>
      httr2::req_timeout(
        seconds = timeout
      ) |>
      httr2::req_retry(
        max_tries = max_tries,
        retry_on_failure = TRUE
      ) |>
      httr2::req_error(
        is_error = function(resp) FALSE
      )

    httr2::req_perform(req)
  }

  # First request
  resp <- perform_once()

  status <- httr2::resp_status(resp)

  # Refresh an invalid/expired token and retry once.
  if (status == 401) {

    message(
      "HTTP 401 received. Refreshing access token and retrying..."
    )

    # Only the selected credential's token is cleared.
    tuik_clear_token(
      api_key = api_key
    )

    # Request a new token using the same API key.
    tuik_refresh_token(
      api_key = api_key
    )

    # Rebuild the HTTP request with the new token.
    resp <- perform_once()

    status <- httr2::resp_status(resp)

    if (status == 401) {

      # Do not keep the newly rejected token.
      tuik_clear_token(
        api_key = api_key
      )
    }
  }

  # Convert remaining HTTP errors to readable messages.
  if (status >= 400) {

    content_type <- httr2::resp_header(
      resp,
      "content-type"
    )

    stop(
      paste0(
        "TUIK SDMX request failed.\n",
        "HTTP status: ",
        status,
        "\n",
        "Path: ",
        path,
        "\n",
        "Content-Type: ",
        content_type,
        "\n",
        "Server response body omitted for security."
      ),
      call. = FALSE
    )
  }

  resp
}


# ---------------------------------------------------------------------------
# Diagnostic request helper
# ---------------------------------------------------------------------------

#' Perform a TUIK SDMX request without stopping on HTTP errors
#'
#' Intended mainly for diagnostics, tests and endpoint exploration.
#'
#' Unlike tuik_perform(), HTTP 4xx and 5xx responses are
#' returned instead of converted into R errors.
#'
#' A single token refresh is attempted after HTTP 401.
#'
#' @param path Character string containing the REST path.
#' @param query Named list of query parameters.
#' @param accept Optional HTTP Accept header.
#' @param timeout Request timeout in seconds.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return An httr2 response object.
#'
#' @keywords internal
tuik_probe <- function(
    path,
    query = list(),
    accept = NULL,
    timeout = 60,
    api_key = NULL
) {

  # Keep the credential fixed across both attempts.
  api_key <- tuik_resolve_api_key(api_key)

  perform_once <- function() {

    req <- tuik_build_request(
      path = path,
      query = query,
      accept = accept,
      api_key = api_key
    ) |>
      httr2::req_timeout(
        seconds = timeout
      ) |>
      httr2::req_retry(
        max_tries = 3,
        retry_on_failure = TRUE
      ) |>
      httr2::req_error(
        is_error = function(resp) FALSE
      )

    httr2::req_perform(req)
  }

  resp <- perform_once()

  if (httr2::resp_status(resp) == 401) {

    tuik_clear_token(
      api_key = api_key
    )

    tuik_refresh_token(
      api_key = api_key
    )

    resp <- perform_once()

    if (httr2::resp_status(resp) == 401) {

      tuik_clear_token(
        api_key = api_key
      )
    }
  }

  resp
}
