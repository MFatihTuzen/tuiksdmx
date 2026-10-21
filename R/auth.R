
# ============================================================================
# auth.R
#
# Authentication helpers for the TUIK SDMX Web Service.
#
# API keys can be supplied explicitly or through:
#
#   TUIK_SDMX_API_KEY
#
# in the environment or .Renviron file.
#
# Access tokens are cached separately for each API key.
# Raw API keys are not used as cache identifiers.
# ============================================================================


# ---------------------------------------------------------------------------
# Internal authentication cache
# ---------------------------------------------------------------------------

.tuiksdmx_auth <- new.env(parent = emptyenv())

.tuiksdmx_auth$tokens <- new.env(
  parent = emptyenv()
)

# Optional expiry timestamps (POSIXct), indexed by the same credential hash.
.tuiksdmx_auth$expires_at <- new.env(parent = emptyenv())


# ---------------------------------------------------------------------------
# Resolve API key
# ---------------------------------------------------------------------------

#' Resolve a TUIK SDMX API key
#'
#' Uses an explicitly supplied API key or, when NULL,
#' the TUIK_SDMX_API_KEY environment variable.
#'
#' @param api_key An optional API key.
#'
#' @return A non-empty character string.
#'
#' @keywords internal
tuik_resolve_api_key <- function(api_key = NULL) {

  if (is.null(api_key)) {

    api_key <- Sys.getenv(
      "TUIK_SDMX_API_KEY",
      unset = ""
    )

    if (!nzchar(trimws(api_key))) {

      stop(
        paste0(
          "TUIK_SDMX_API_KEY was not found. ",
          "Set TUIK_SDMX_API_KEY in your environment ",
          "or .Renviron file, or provide `api_key`."
        ),
        call. = FALSE
      )
    }
  }

  if (
    !is.character(api_key) ||
    length(api_key) != 1L ||
    is.na(api_key) ||
    !nzchar(trimws(api_key))
  ) {

    stop(
      paste0(
        "A valid API key is required. ",
        "Provide a non-empty character string."
      ),
      call. = FALSE
    )
  }

  trimws(api_key)
}


# ---------------------------------------------------------------------------
# Credential-specific cache identifier
# ---------------------------------------------------------------------------

#' Create a token cache identifier
#'
#' Creates a SHA-256 identifier from an API key.
#' The raw API key is not used as a cache name.
#'
#' @param api_key A TUIK SDMX API key.
#'
#' @return A SHA-256 hexadecimal character string.
#'
#' @keywords internal
tuik_token_cache_id <- function(api_key) {

  api_key <- tuik_resolve_api_key(api_key)

  digest::digest(
    api_key,
    algo = "sha256",
    serialize = FALSE
  )
}


# ---------------------------------------------------------------------------
# Obtain access token
# ---------------------------------------------------------------------------

#' Get a new TUIK SDMX access token
#'
#' Requests an access token from the TUIK authentication service.
#'
#' @param api_key An optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return An access-token character string. When the server supplies a
#'   positive numeric `expires_in`, it is attached as an internal attribute.
#'
#' @keywords internal
tuik_get_access_token <- function(api_key = NULL) {

  api_key <- tuik_resolve_api_key(api_key)

  req <- httr2::request(
    "https://giris.tuik.gov.tr/realms/web/protocol/openid-connect/token"
  ) |>
    httr2::req_body_form(
      grant_type = "password",
      client_id = "nsi-ws-consumer",
      api_key = api_key
    ) |>
    httr2::req_timeout(
      seconds = 30
    ) |>
    httr2::req_retry(
      max_tries = 3,
      retry_on_failure = TRUE
    )

  resp <- httr2::req_perform(req)

  content_type <- httr2::resp_header(
    resp,
    "content-type"
  )

  if (
    is.null(content_type) ||
    !grepl(
      "json",
      content_type,
      ignore.case = TRUE
    )
  ) {

    stop(
      paste0(
        "Token endpoint did not return JSON. Content-Type: ",
        content_type
      ),
      call. = FALSE
    )
  }

  body <- httr2::resp_body_json(
    resp,
    simplifyVector = TRUE
  )

  if (
    is.null(body$access_token) ||
    !is.character(body$access_token) ||
    length(body$access_token) != 1L ||
    is.na(body$access_token) ||
    !nzchar(body$access_token)
  ) {

    stop(
      "access_token was not found in the token response.",
      call. = FALSE
    )
  }

  token <- body$access_token

  # `expires_in` is optional; never infer a lifetime if it is absent.
  ttl <- body$expires_in
  if (!is.null(ttl) && length(ttl) == 1L &&
      (is.numeric(ttl) || is.character(ttl))) {
    ttl <- suppressWarnings(as.numeric(ttl))
    if (is.finite(ttl) && ttl > 0) {
      attr(token, "tuik_expires_in") <- ttl
    }
  }

  token
}


# ---------------------------------------------------------------------------
# Refresh access token
# ---------------------------------------------------------------------------

#' Refresh a cached TUIK SDMX access token
#'
#' Requests a new access token and updates only the
#' cache entry corresponding to the selected API key.
#'
#' If authentication fails, the existing cache entry
#' remains unchanged.
#'
#' @param api_key An optional API key.
#'
#' @return The new access token invisibly.
#'
#' @keywords internal
tuik_refresh_token <- function(api_key = NULL) {

  api_key <- tuik_resolve_api_key(api_key)

  cache_id <- tuik_token_cache_id(api_key)

  new_token <- tuik_get_access_token(api_key = api_key)

  # Compute expiry only if supplied. Keep the public cached value plain.
  ttl <- attr(new_token, "tuik_expires_in", exact = TRUE)
  new_token <- as.character(new_token)
  expires_at <- NULL
  if (is.numeric(ttl) && length(ttl) == 1L &&
      is.finite(ttl) && ttl > 0) {
    # Refresh slightly before expiry, but retain room for short-lived tokens.
    safety_margin <- min(30, ttl * 0.1)
    expires_at <- Sys.time() + (ttl - safety_margin)
  }

  # No partial cache update if token acquisition fails.
  assign(cache_id, new_token, envir = .tuiksdmx_auth$tokens)
  if (is.null(expires_at)) {
    if (exists(cache_id, envir = .tuiksdmx_auth$expires_at,
               inherits = FALSE)) {
      rm(list = cache_id, envir = .tuiksdmx_auth$expires_at)
    }
  } else {
    assign(cache_id, expires_at, envir = .tuiksdmx_auth$expires_at)
  }

  invisible(new_token)
}


# ---------------------------------------------------------------------------
# Retrieve cached token
# ---------------------------------------------------------------------------

#' Get a cached TUIK SDMX access token
#'
#' Returns the access token associated with the selected
#' API key. If no token is cached, requests a new one.
#'
#' @param api_key An optional API key.
#'
#' @return A character string containing the access token.
#'
#' @keywords internal
tuik_cached_token <- function(api_key = NULL) {

  api_key <- tuik_resolve_api_key(api_key)

  cache_id <- tuik_token_cache_id(api_key)

  missing_token <- !exists(cache_id, envir = .tuiksdmx_auth$tokens,
                           inherits = FALSE)
  expired <- FALSE
  if (!missing_token && exists(cache_id, envir = .tuiksdmx_auth$expires_at,
                               inherits = FALSE)) {
    expires_at <- get(cache_id, envir = .tuiksdmx_auth$expires_at,
                      inherits = FALSE)
    expired <- isTRUE(Sys.time() >= expires_at)
  }

  if (missing_token || expired) {
    tuik_refresh_token(api_key = api_key)
  }

  get(
    cache_id,
    envir = .tuiksdmx_auth$tokens,
    inherits = FALSE
  )
}


# ---------------------------------------------------------------------------
# Clear cached token
# ---------------------------------------------------------------------------

#' Clear a cached TUIK SDMX access token
#'
#' Removes the access token associated with the selected
#' API key without affecting tokens belonging to other keys.
#'
#' @param api_key An optional API key.
#'
#' @return NULL invisibly.
#'
#' @keywords internal
tuik_clear_token <- function(api_key = NULL) {

  api_key <- tuik_resolve_api_key(api_key)

  cache_id <- tuik_token_cache_id(api_key)

  if (exists(
    cache_id,
    envir = .tuiksdmx_auth$tokens,
    inherits = FALSE
  )) {

    rm(
      list = cache_id,
      envir = .tuiksdmx_auth$tokens
    )
  }

  if (exists(cache_id, envir = .tuiksdmx_auth$expires_at,
             inherits = FALSE)) {
    rm(list = cache_id, envir = .tuiksdmx_auth$expires_at)
  }

  invisible(NULL)
}
