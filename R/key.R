# ============================================================================
# key.R
#
# Functions for building SDMX data keys from Dataflow profile information.
#
# Main functions:
#   - tuik_key_dimensions()
#   - tuik_build_key()
#   - tuik_missing_filters()
#
# SDMX series keys must follow the exact dimension order defined by the DSD.
#
# TIME_PERIOD is not part of the series key. Time filtering is handled through
# startPeriod and endPeriod query parameters.
#
# Multiple values within one dimension are joined using "+".
# ============================================================================


#' Return the dimensions used in an SDMX series key
#'
#' Extracts ordinary series dimensions from a TÜİK flow profile.
#'
#' TimeDimension components such as TIME_PERIOD are excluded because they are
#' supplied separately through time query parameters.
#'
#' @param profile A `tuik_flow_profile` object.
#'
#' @return A tibble containing the key dimensions in DSD order.
#'
#' @export
tuik_key_dimensions <- function(
    profile
) {

  if (
    is.null(profile$dimensions)
  ) {
    stop(
      "`profile` does not contain dimension information.",
      call. = FALSE
    )
  }

  profile$dimensions |>
    dplyr::filter(
      .data$type == "Dimension"
    ) |>
    dplyr::arrange(
      .data$position
    )
}


#' Identify missing SDMX key filters
#'
#' Compares a filter list with the dimensions required by the DSD.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param filters Named list of dimension selections.
#'
#' @return Character vector containing missing dimension IDs.
#'
#' @export
tuik_missing_filters <- function(
    profile,
    filters
) {

  key_dimensions <- tuik_key_dimensions(
    profile
  )$dimension

  if (is.null(filters)) {
    return(key_dimensions)
  }

  supplied <- names(
    filters
  )

  if (is.null(supplied)) {
    supplied <- character()
  }

  missing <- setdiff(
    key_dimensions,
    supplied
  )

  # A dimension is also considered missing when its supplied value
  # is NULL, empty or entirely NA.
  supplied_required <- intersect(
    key_dimensions,
    supplied
  )

  empty_dimensions <- supplied_required[
    vapply(
      supplied_required,
      function(id) {

        value <- filters[[id]]

        is.null(value) ||
          length(value) == 0 ||
          all(is.na(value)) ||
          all(!nzchar(trimws(as.character(value))))
      },
      logical(1)
    )
  ]

  unique(
    c(
      missing,
      empty_dimensions
    )
  )
}


#' Build an SDMX data key
#'
#' Builds a series key using the exact dimension order defined by the DSD.
#'
#' Each dimension may contain:
#'   - one value, e.g. "M"
#'   - multiple values, e.g. c("01", "02")
#'
#' Multiple values are joined using the SDMX "+" separator.
#'
#' At the current stage, all series dimensions must be supplied explicitly.
#' This is intentional because blank-segment wildcard behaviour has not been
#' reliably confirmed for the TÜİK service.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param filters Named list of dimension selections.
#'
#' @return Character string containing the SDMX key.
#'
#' @examples
#' \dontrun{
#' # Requires TUIK_SDMX_API_KEY in the environment.
#' library(tuiksdmx)
#' profile <- tuik_flow_profile("DF_TUFE_SDMX_TT01")
#'
#' key <- tuik_build_key(
#'   profile,
#'   filters = list(
#'     REF_AREA = "TR",
#'     FREQ = "M",
#'     SINIFLAMA_DUZEYI = "2",
#'     DEGISIM = "1",
#'     OZEL_KAPSAM_TUFE = "_Z",
#'     BASE_PER = "2025",
#'     YAYIM_DONEMI = "2026_01",
#'     COICOP_1999 = "_Z",
#'     COICOP_2018 = c("01", "02"),
#'     INDICATOR = "F_TFE"
#'   )
#' )
#' }
#'
#' @export
tuik_build_key <- function(
    profile,
    filters
) {

  if (
    is.null(filters) ||
    !is.list(filters)
  ) {
    stop(
      "`filters` must be a named list.",
      call. = FALSE
    )
  }

  if (
    is.null(names(filters)) ||
    any(
      !nzchar(
        names(filters)
      )
    )
  ) {
    stop(
      "`filters` must be a named list.",
      call. = FALSE
    )
  }

  key_info <- tuik_key_dimensions(
    profile
  )

  required_dimensions <- key_info$dimension

  # ----------------------------------------------------------
  # Check unknown filter names
  # ----------------------------------------------------------

  unknown <- setdiff(
    names(filters),
    required_dimensions
  )

  if (
    length(unknown) > 0
  ) {
    stop(
      paste0(
        "Unknown filter dimension(s): ",
        paste(
          unknown,
          collapse = ", "
        ),
        "\nValid key dimensions are: ",
        paste(
          required_dimensions,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Check missing dimensions
  # ----------------------------------------------------------

  missing <- tuik_missing_filters(
    profile = profile,
    filters = filters
  )

  if (
    length(missing) > 0
  ) {
    stop(
      paste0(
        "Missing filter dimension(s): ",
        paste(
          missing,
          collapse = ", "
        ),
        "\nAll series dimensions must currently be supplied explicitly."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Build key segments in exact DSD order
  # ----------------------------------------------------------

  segments <- vapply(
    required_dimensions,
    function(dimension_id) {

      values <- filters[[dimension_id]]

      values <- as.character(
        values
      )

      values <- trimws(
        values
      )

      values <- values[
        !is.na(values) &
          nzchar(values)
      ]

      values <- unique(
        values
      )

      if (
        length(values) == 0
      ) {
        stop(
          paste0(
            "No valid value supplied for dimension: ",
            dimension_id
          ),
          call. = FALSE
        )
      }

      paste(
        values,
        collapse = "+"
      )
    },
    character(1)
  )

  paste(
    segments,
    collapse = "."
  )

}


#' Create a filter template for a TÜİK SDMX Dataflow
#'
#' Creates a named list containing all dimensions required for an SDMX
#' series key. Each value is initially NULL and can be filled by the user.
#'
#' TIME_PERIOD is excluded because time filtering is handled separately
#' through startPeriod and endPeriod.
#'
#' @param profile A `tuik_flow_profile` object.
#'
#' @return A named list with one element per series-key dimension.
#'
#' @examples
#' \dontrun{
#' # Requires TUIK_SDMX_API_KEY in the environment.
#' library(tuiksdmx)
#' profile <- tuik_flow_profile("DF_TUFE_SDMX_TT01")
#'
#' filters <- tuik_filter_template(profile)
#'
#' filters$REF_AREA <- "TR"
#' filters$FREQ <- "M"
#' }
#'
#' @export
tuik_filter_template <- function(
    profile
) {

  dimensions <- tuik_key_dimensions(
    profile
  )$dimension

  result <- rep(
    list(NULL),
    length(dimensions)
  )

  names(result) <- dimensions

  result
}


#' Show required filters for a TÜİK SDMX Dataflow
#'
#' Returns the dimensions required to build a series key together with
#' their referenced Codelists.
#'
#' @param profile A `tuik_flow_profile` object.
#'
#' @return A tibble describing required filter dimensions.
#'
#' @export
tuik_filter_dimensions <- function(profile) {
  tuik_key_dimensions(profile) |>
    dplyr::select(
      "position",
      "dimension",
      "codelist_agency",
      "codelist_id",
      "codelist_version"
    )
}

#' Validate SDMX filter values against referenced Codelists
#'
#' Checks whether supplied filter values exist in the Codelists referenced
#' by the Dataflow's DSD.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param filters Named list of dimension selections.
#' @param stop_on_error Logical. If TRUE, stop when invalid codes are found.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with one row per supplied filter value.
#'
#' @export
tuik_validate_filters <- function(
    profile,
    filters,
    stop_on_error = TRUE,
    api_key = NULL
) {

  if (
    is.null(filters) ||
    !is.list(filters) ||
    is.null(names(filters))
  ) {
    stop(
      "`filters` must be a named list.",
      call. = FALSE
    )
  }

  dimensions <- tuik_key_dimensions(
    profile
  )

  # ----------------------------------------------------------
  # Preload related Codelists when the cache is incomplete
  # ----------------------------------------------------------

  required_codelists <- dimensions |>
    dplyr::filter(
      !is.na(.data$codelist_id),
      !is.na(.data$codelist_agency),
      !is.na(.data$codelist_version)
    )

  cache_keys <- purrr::pmap_chr(
    required_codelists |>
      dplyr::select(
        "codelist_agency",
        "codelist_id",
        "codelist_version"
      ),
    function(
    codelist_agency,
    codelist_id,
    codelist_version
    ) {
      .tuik_metadata_cache_key(
        agency = codelist_agency,
        id = codelist_id,
        version = codelist_version,
        api_key = api_key
      )
    }
  )

  cached <- vapply(
    cache_keys,
    function(key) {
      exists(
        key,
        envir = .tuiksdmx_codelist_cache,
        inherits = FALSE
      )
    },
    logical(1)
  )

  if (
    length(cached) > 0 &&
    any(!cached)
  ) {
    tuik_preload_codelists(
      profile = profile,
      api_key = api_key
    )
  }

  unknown <- setdiff(
    names(filters),
    dimensions$dimension
  )

  if (length(unknown) > 0) {
    stop(
      paste0(
        "Unknown filter dimension(s): ",
        paste(
          unknown,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  results <- purrr::map_dfr(
    names(filters),
    function(dimension_id) {

      values <- filters[[dimension_id]]

      if (
        is.null(values) ||
        length(values) == 0
      ) {
        return(
          tibble::tibble(
            dimension = dimension_id,
            value = character(),
            valid = logical(),
            label_tr = character(),
            label_en = character()
          )
        )
      }

      dim_row <- dimensions |>
        dplyr::filter(
          .data$dimension == dimension_id
        )

      values <- unique(
        trimws(
          as.character(values)
        )
      )

      values <- values[
        !is.na(values) &
          nzchar(values)
      ]

      if (
        is.na(dim_row$codelist_id) ||
        is.na(dim_row$codelist_agency)
      ) {

        return(
          tibble::tibble(
            dimension = dimension_id,
            value = values,
            valid = NA,
            label_tr = NA_character_,
            label_en = NA_character_
          )
        )
      }

      codes <- tuik_get_codelist(
        codelist_id = dim_row$codelist_id,
        agency = dim_row$codelist_agency,
        version = dim_row$codelist_version,
        api_key = api_key
      )

      tibble::tibble(
        dimension = dimension_id,
        value = values,
        valid = values %in% codes$code
      ) |>
        dplyr::left_join(
          codes,
          by = c(
            "value" = "code"
          )
        ) |>
        dplyr::select(
          "dimension",
          "value",
          "valid",
          "label_tr",
          "label_en"
        )
    }
  )

  invalid <- results |>
    dplyr::filter(
      .data$valid %in% FALSE
    )

  if (
    stop_on_error &&
    nrow(invalid) > 0
  ) {

    invalid_text <- paste0(
      invalid$dimension,
      "=",
      invalid$value,
      collapse = ", "
    )

    stop(
      paste0(
        "Invalid SDMX filter value(s): ",
        invalid_text
      ),
      call. = FALSE
    )
  }

  results
}
