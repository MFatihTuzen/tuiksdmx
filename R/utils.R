# ============================================================================
# utils.R
#
# Internal utility functions used across the tuiksdmx package.
#
# These functions are not intended to be called directly by end users.
# ============================================================================


#' Normalise text for searching
#'
#' Converts text to lower case, replaces Turkish-specific characters with
#' ASCII equivalents and normalises whitespace.
#'
#' Examples:
#' "Tuketici Fiyat" -> "tuketici fiyat"
#' "ISGUCU"         -> "isgucu"
#'
#' @param x Character vector.
#'
#' @return A normalised character vector.
#'
#' @keywords internal
tuik_normalize_text <- function(x) {

  x <- dplyr::coalesce(
    as.character(x),
    ""
  )

  # Replace Turkish-specific characters explicitly.
  # This avoids depending on operating-system transliteration behaviour.
  x <- chartr(
    old = "\u00e7\u011f\u0131\u00f6\u015f\u00fc\u00c7\u011e\u0130\u00d6\u015e\u00dc",
    new = "cgiosuCGIOSU",
    x = x
  )

  x <- tolower(x)

  # Replace repeated whitespace with a single space.
  x <- gsub(
    "[[:space:]]+",
    " ",
    x
  )

  trimws(x)
}


#' Create fallback search variants
#'
#' Generates progressively shorter variants of a search term.
#'
#' This is used only when an exact substring search returns no results.
#' It provides a lightweight fallback for cases such as:
#'
#'   "issizlik" -> "issiz"
#'
#' It is deliberately conservative and is not intended to be a linguistic
#' stemmer.
#'
#' @param x Single character string.
#' @param max_trim Maximum number of trailing characters to remove.
#'
#' @return Character vector of candidate search strings.
#'
#' @keywords internal
tuik_search_variants <- function(
    x,
    max_trim = 3
) {

  x <- tuik_normalize_text(x)

  if (!nzchar(x)) {
    return(character())
  }

  words <- strsplit(
    x,
    " ",
    fixed = TRUE
  )[[1]]

  # Do not shorten very short words.
  variants_per_word <- lapply(
    words,
    function(word) {

      result <- word

      if (nchar(word) >= 6) {

        trim_values <- seq_len(
          min(
            max_trim,
            nchar(word) - 4
          )
        )

        result <- c(
          result,
          vapply(
            trim_values,
            function(k) {
              substr(
                word,
                1,
                nchar(word) - k
              )
            },
            character(1)
          )
        )
      }

      unique(result)
    }
  )

  # Generate combinations for multi-word searches.
  combinations <- expand.grid(
    variants_per_word,
    stringsAsFactors = FALSE
  )

  unique(
    apply(
      combinations,
      1,
      paste,
      collapse = " "
    )
  )
}

#' Parse an SDMX-CSV 2.0 response
#'
#' Reads all columns as character to preserve SDMX codes with leading zeros.
#' OBS_VALUE can optionally be converted to numeric when every non-blank value
#' is safely convertible.
#'
#' @param resp An httr2 response containing SDMX-CSV.
#' @param parse_observation Logical. Convert OBS_VALUE to numeric when safe.
#'
#' @return A tibble.
#'
#' @keywords internal
.tuik_parse_sdmx_csv_response <- function(
    resp,
    parse_observation = TRUE
) {
  csv_text <- httr2::resp_body_string(resp)

  if (!nzchar(trimws(csv_text))) {
    return(tibble::tibble())
  }

  data <- readr::read_csv(
    I(csv_text),
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )

  if (
    parse_observation &&
    "OBS_VALUE" %in% names(data)
  ) {
    obs_raw <- data$OBS_VALUE
    obs_numeric <- suppressWarnings(as.numeric(obs_raw))

    invalid_numeric <- (
      !is.na(obs_raw) &
      nzchar(trimws(obs_raw)) &
      is.na(obs_numeric)
    )

    if (any(invalid_numeric)) {
      warning(
        paste0(
          "Some OBS_VALUE entries could not be converted to numeric. ",
          "OBS_VALUE has been kept as character."
        ),
        call. = FALSE
      )
    } else {
      data$OBS_VALUE <- obs_numeric
    }
  }

  data
}

#' Build a credential-specific metadata cache key
#'
#' @param agency Metadata agency.
#' @param id Metadata identifier.
#' @param version Metadata version.
#' @param api_key Optional API key.
#' @return A credential-specific cache key.
#' @keywords internal
.tuik_metadata_cache_key <- function(
    agency,
    id,
    version,
    api_key = NULL
) {
  paste(
    agency,
    id,
    version,
    tuik_token_cache_id(api_key),
    sep = "::"
  )
}
