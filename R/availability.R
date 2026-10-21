# ============================================================================
# availability.R
#
# Helpers for distinguishing theoretical Codelist domains from values actually
# observed in a Dataflow for a bounded time window.
#
# Main function:
#   - tuik_available_codes()
#
# IMPORTANT:
# This function does not claim to return an SDMX ContentConstraint. It derives
# observed availability from returned data. Availability may therefore depend
# on the requested time window and optional filters.
# ============================================================================


#' Retrieve codes observed in a Dataflow for a time window
#'
#' Returns values of a dimension that are actually present in data returned by
#' the selected Dataflow over a bounded period. This is intentionally distinct
#' from `tuik_dimension_codes()`, which returns the theoretical Codelist domain.
#'
#' By default `query_mode = "auto"` uses a server-side key for complete
#' filters. Partial or absent filters require the explicit `allow_all = TRUE`
#' opt-in because the service may return a large `all` response.
#' `query_mode = "all"` also requires `allow_all = TRUE`.
#' Both period bounds are required.
#' Period bounds do not impose a limit on response size or memory usage.
#' This is observed data availability, not an SDMX ContentConstraint.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param dimension Dimension ID whose observed values should be returned.
#' @param start_period Required start period.
#' @param end_period Required end period.
#' @param filters Optional named list of dimension filters. Filter values are
#'   validated against Codelists, and reapplied locally to the returned data.
#' @param labels Logical. If TRUE, attach Turkish and English Codelist labels
#'   when the dimension references a Codelist.
#' @param query_mode `"auto"` (default) uses a complete server-side SDMX
#'   key when possible. `"all"` always requests all series for the period.
#'   The `all` path requires `allow_all = TRUE` in either mode.
#' @param allow_all Logical. Explicitly authorizes a potentially large
#'   `all` download if filters are partial/absent or `query_mode = "all"`.
#'   This does NOT impose a byte, row, memory, or timeout limit.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with observed code values, row counts and optional labels.
#'
#' @examples
#' \dontrun{
#' tuik_available_codes(
#'   profile,
#'   dimension = "COICOP_2018",
#'   start_period = "2026-01",
#'   end_period = "2026-03"
#' )
#'
#' tuik_available_codes(
#'   profile,
#'   dimension = "COICOP_2018",
#'   start_period = "2026-01",
#'   end_period = "2026-03",
#'   filters = list(FREQ = "M", REF_AREA = "TR")
#' )
#' }
#'
#' @export
tuik_available_codes <- function(
    profile,
    dimension,
    start_period,
    end_period,
    filters = NULL,
    labels = TRUE,
    api_key = NULL,
    query_mode = c("auto", "all"),
    allow_all = FALSE
) {
  query_mode <- match.arg(query_mode)
  if (!is.logical(allow_all) || length(allow_all) != 1L || is.na(allow_all)) {
    stop("`allow_all` must be TRUE or FALSE.", call. = FALSE)
  }

  if (
    is.null(profile$dataflow) ||
    nrow(profile$dataflow) != 1
  ) {
    stop(
      "`profile` must contain exactly one Dataflow.",
      call. = FALSE
    )
  }

  if (
    length(dimension) != 1 ||
    is.na(dimension) ||
    !nzchar(trimws(dimension))
  ) {
    stop(
      "`dimension` must be a non-empty character string.",
      call. = FALSE
    )
  }

  validate_period <- function(x, argument) {
    if (
      length(x) != 1 ||
      is.na(x) ||
      !nzchar(trimws(as.character(x)))
    ) {
      stop(
        paste0(
          "`",
          argument,
          "` must be a non-empty character string."
        ),
        call. = FALSE
      )
    }

    trimws(as.character(x))
  }

  start_period <- validate_period(start_period, "start_period")
  end_period <- validate_period(end_period, "end_period")

  all_dimensions <- profile$dimensions$dimension

  if (!(dimension %in% all_dimensions)) {
    stop(
      paste0(
        "Unknown dimension: ",
        dimension,
        "\nAvailable dimensions are: ",
        paste(all_dimensions, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.logical(labels) || length(labels) != 1 || is.na(labels)) {
    stop(
      "`labels` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(filters)) {
    if (
      !is.list(filters) ||
      is.null(names(filters)) ||
      any(!nzchar(names(filters)))
    ) {
      stop(
        "`filters` must be NULL or a named list.",
        call. = FALSE
      )
    }

    tuik_validate_filters(
      profile = profile,
      filters = filters,
      stop_on_error = TRUE,
      api_key = api_key
    )
  }

  key <- .tuik_availability_request_key(
    profile, filters, query_mode, allow_all = allow_all
  )

  dataflow_id <- profile$dataflow$dataflow_id
  path <- paste0("data/", dataflow_id, "/", key)

  resp <- tuik_probe(
    path = path,
    query = list(
      startPeriod = start_period,
      endPeriod = end_period
    ),
    accept = "application/vnd.sdmx.data+csv;version=2.0",
    api_key = api_key
  )

  status <- httr2::resp_status(resp)

  if (status == 404) {
    body <- httr2::resp_body_string(resp)

    if (grepl("NoRecordsFound", body, fixed = TRUE)) {
      return(
        tibble::tibble(
          code = character(),
          n_rows = integer(),
          label_tr = character(),
          label_en = character()
        )
      )
    }
  }

  if (status >= 400) {
    stop(
      paste0(
        "TUIK SDMX availability request failed.\n",
        "HTTP status: ", status, "\n",
        "Path: ", path, "\n",
        "Server response body omitted for security."
      ),
      call. = FALSE
    )
  }

  data <- .tuik_parse_sdmx_csv_response(
    resp,
    parse_observation = FALSE
  )

  if (nrow(data) == 0) {
    return(
      tibble::tibble(
        code = character(),
        n_rows = integer(),
        label_tr = character(),
        label_en = character()
      )
    )
  }

  if (!(dimension %in% names(data))) {
    stop(
      paste0(
        "Dimension '",
        dimension,
        "' was not present in the returned SDMX-CSV data."
      ),
      call. = FALSE
    )
  }

  # Optional partial filters are applied locally because blank-segment partial
  # wildcard keys have not been established as reliable for the TUIK service.
  if (!is.null(filters) && length(filters) > 0) {
    for (filter_dimension in names(filters)) {
      if (!(filter_dimension %in% names(data))) {
        stop(
          paste0(
            "Filter dimension '",
            filter_dimension,
            "' was not present in returned data."
          ),
          call. = FALSE
        )
      }

      values <- unique(
        trimws(
          as.character(filters[[filter_dimension]])
        )
      )
      values <- values[!is.na(values) & nzchar(values)]

      data <- data[
        data[[filter_dimension]] %in% values,
        ,
        drop = FALSE
      ]
    }
  }

  result <- data |>
    dplyr::filter(
      !is.na(.data[[dimension]]),
      nzchar(trimws(.data[[dimension]]))
    ) |>
    dplyr::count(
      code = .data[[dimension]],
      name = "n_rows",
      sort = TRUE
    )

  if (!labels) {
    return(result)
  }

  dim_row <- profile$dimensions |>
    dplyr::filter(
      .data$dimension == !!dimension
    )

  if (
    nrow(dim_row) != 1 ||
    is.na(dim_row$codelist_id) ||
    is.na(dim_row$codelist_agency)
  ) {
    return(
      result |>
        dplyr::mutate(
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

  result |>
    dplyr::left_join(
      codes,
      by = "code"
    )
}


# Internal helper: select an SDMX key without downloading observations.
# Never silently widen a partial/absent filter to an `all` request.
.tuik_availability_request_key <- function(profile, filters, query_mode,
                                           allow_all = FALSE) {
  if (!is.logical(allow_all) || length(allow_all) != 1L || is.na(allow_all)) {
    stop("`allow_all` must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(query_mode) != 1L || is.na(query_mode) ||
      !is.character(query_mode) || !query_mode %in% c("auto", "all")) {
    stop("`query_mode` must be 'auto' or 'all'.", call. = FALSE)
  }
  if (!is.null(filters) && length(filters)) {
    required <- tuik_key_dimensions(profile)$dimension
    unknown <- setdiff(names(filters), required)
    if (length(unknown)) {
      stop("Unknown filter dimension(s): ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
    complete <- !length(setdiff(required, names(filters)))
  } else {
    complete <- FALSE
  }
  if (identical(query_mode, "auto") && complete) {
    return(tuik_build_key(profile, filters))
  }
  if (!allow_all) {
    stop(
      paste0("Observed availability would request all Dataflow series for ",
             "the chosen period. Set `allow_all = TRUE` to authorize ",
             "this potentially large download, or supply complete ",
             "filters with `query_mode = 'auto'`."),
      call. = FALSE
    )
  }
  "all"
}
