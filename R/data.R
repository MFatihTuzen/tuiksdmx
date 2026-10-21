# ============================================================================
# data.R
#
# Functions for retrieving statistical data from the TÜİK SDMX Web Service.
#
# Main function:
#   - tuik_get_data()
#
# Data are requested using:
#   - A Dataflow profile
#   - The `all` key (no filters or explicit local filtering), or a complete key
#   - Optional startPeriod and endPeriod parameters
#
# SDMX-CSV 2.0 is used as the default response format because it maps
# naturally to tabular R data.
# ============================================================================


#' Retrieve data from a TÜİK SDMX Dataflow
#'
#' Retrieves all series with `filters = NULL`, or builds a complete SDMX key
#' from supplied filters. Partial filters require explicit permission via
#' `allow_all = TRUE` or `filter_mode = "local"`: the service's `all` data are retrieved
#' first and filters are applied in R. At least one period bound is required
#' unless `allow_unbounded = TRUE`. This is NOT server-side
#' filtering and can download large responses even for a short period.
#' Complete filters are always queried by SDMX key on the server.
#'
#' @param profile A `tuik_flow_profile` object or a single Dataflow ID.
#'   When an ID is supplied, a profile is resolved automatically with
#'   [tuik_flow_profile()] using the same `api_key`. For explicit Dataflow
#'   agency/version selection, construct a profile first.
#' @param filters NULL to request all series, or a named list of key-dimension
#'   selections. Partial selections require `allow_all = TRUE` (or legacy
#'   `filter_mode = "local"`) before the service's `all` data are downloaded.
#' @param start_period Optional start period, for example "2026-01".
#' @param end_period Optional end period, for example "2026-03".
#' @param parse_observation Logical. If TRUE, attempts to convert OBS_VALUE
#'   to numeric while preserving all SDMX dimensions and attributes as
#'   character columns.
#' @param validate Logical. If TRUE, validate supplied filter values against
#'   the Codelists referenced by the Dataflow DSD before requesting data.
#'   Defaults to TRUE.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#' @param allow_unbounded Logical. Must be TRUE to permit unfiltered or
#'   locally filtered requests with neither period bound supplied. This can download
#'   a very large dataset. Does not limit the server response size.
#' @param labels Language of optional Codelist labels: `"none"` (the
#'   default), `"tr"`, `"en"`, or `"both"`. Raw SDMX codes remain unchanged.
#' @param filter_mode Legacy explicit mode: `"reject"` (default) or `"local"`.
#'   Existing calls remain supported. For new code, prefer `allow_all = TRUE`
#'   to authorize a locally filtered `all` request.
#' @param allow_all Logical, default FALSE. With incomplete `filters`, TRUE
#'   authorizes an `all` request and applies the specified filters locally.
#'   This is the same opt-in used by [tuik_available_codes()]. It DOES NOT
#'   limit transferred bytes, memory, or response time. It does not bypass
#'   `allow_unbounded` when neither period bound is provided.
#' @param label_dimensions Optional character vector of dimension IDs to
#'   decode when `labels` is not `"none"`. If NULL, decode all coded
#'   dimensions present in the downloaded data.
#' @return A tibble containing SDMX observations. If labels are requested,
#'   additional `<DIMENSION>_label_tr` and/or `<DIMENSION>_label_en` columns
#'   are appended. Labels identify Codelist-defined values, not observed
#'   availability.
#'
#' @examples
#' \dontrun{
#' data <- tuik_get_data(
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
#'     COICOP_2018 = "01",
#'     INDICATOR = "F_TFE"
#'   ),
#'   start_period = "2026-01",
#'   end_period = "2026-03"
#' )
#' }
#'
#' @export
tuik_get_data <- function(
    profile,
    filters = NULL,
    start_period = NULL,
    end_period = NULL,
    parse_observation = TRUE,
    validate = TRUE,
    api_key = NULL,
    allow_unbounded = FALSE,
    labels = c("none", "tr", "en", "both"),
    label_dimensions = NULL,
    filter_mode = c("reject", "local"),
    allow_all = FALSE
) {

  labels <- match.arg(labels)
  filter_mode <- match.arg(filter_mode)
  if (!is.logical(allow_all) || length(allow_all) != 1L ||
      is.na(allow_all)) {
    stop("`allow_all` must be TRUE or FALSE.", call. = FALSE)
  }

  # Convenience layer: accept a Dataflow ID without changing the underlying
  # DSD-driven validation, full-key generation, or retrieval behaviour.
  if (is.character(profile)) {
    if (length(profile) != 1L || is.na(profile) ||
        !nzchar(trimws(profile))) {
      stop("`profile` must be a non-empty Dataflow ID or a tuik_flow_profile object.",
           call. = FALSE)
    }
    profile <- tuik_flow_profile(
      dataflow_id = trimws(profile), api_key = api_key
    )
  }
  if (identical(labels, "none") && !is.null(label_dimensions)) {
    stop("`label_dimensions` requires `labels` to be 'tr', 'en', or 'both'.",
         call. = FALSE)
  }

  # ----------------------------------------------------------
  # Validate profile
  # ----------------------------------------------------------

  if (
    is.null(profile$dataflow) ||
    nrow(profile$dataflow) != 1
  ) {
    stop(
      "`profile` must contain exactly one Dataflow.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Validate logical arguments
  # ----------------------------------------------------------

  if (
    length(validate) != 1 ||
    is.na(validate) ||
    !is.logical(validate)
  ) {
    stop(
      "`validate` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    length(parse_observation) != 1 ||
    is.na(parse_observation) ||
    !is.logical(parse_observation)
  ) {
    stop(
      "`parse_observation` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(allow_unbounded) || length(allow_unbounded) != 1L ||
      is.na(allow_unbounded)) {
    stop("`allow_unbounded` must be TRUE or FALSE.", call. = FALSE)
  }

  # ----------------------------------------------------------
  # Validate period arguments
  # ----------------------------------------------------------

  validate_period <- function(
    x,
    argument
  ) {

    if (is.null(x)) {
      return(NULL)
    }

    if (
      length(x) != 1 ||
      is.na(x) ||
      !nzchar(trimws(x))
    ) {
      stop(
        paste0(
          "`",
          argument,
          "` must be NULL or a non-empty character string."
        ),
        call. = FALSE
      )
    }

    trimws(
      as.character(x)
    )
  }

  start_period <- validate_period(
    start_period,
    "start_period"
  )

  end_period <- validate_period(
    end_period,
    "end_period"
  )

  local_filters <- NULL
  if (is.null(filters)) {
    if (is.null(start_period) && is.null(end_period) && !allow_unbounded) {
      stop(
        paste0(
          "Unfiltered download with no period bounds may be very large. ",
          "Supply `start_period` or `end_period`, or explicitly set ",
          "`allow_unbounded = TRUE` to request the full dataset."
        ),
        call. = FALSE
      )
    }
    key <- "all"
  } else {
    if (!is.list(filters) || is.null(names(filters)) ||
        anyNA(names(filters)) || any(!nzchar(names(filters))) ||
        anyDuplicated(names(filters))) {
      stop("`filters` must be a named list with unique dimension names.",
           call. = FALSE)
    }
    required <- tuik_key_dimensions(profile)$dimension
    unknown <- setdiff(names(filters), required)
    if (length(unknown)) {
      stop("Unknown filter dimension(s): ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
    if (!length(filters)) {
      stop("Use `filters = NULL` to request all series.", call. = FALSE)
    }
    invalid_values <- names(filters)[vapply(filters, function(x) {
      !is.character(x) || !length(x) || anyNA(x) ||
        any(!nzchar(trimws(x)))
    }, logical(1))]
    if (length(invalid_values)) {
      stop("Filter values must be nonempty character codes: ",
           paste(invalid_values, collapse = ", "), call. = FALSE)
    }
    missing <- tuik_missing_filters(profile, filters)
    if (length(missing)) {
      if (filter_mode != "local" && !allow_all) {
        stop("Partial filters require `allow_all = TRUE` or `filter_mode = \"local\"`. Missing: ",
             paste(missing, collapse = ", "), call. = FALSE)
      }
      if (is.null(start_period) && is.null(end_period) &&
          !allow_unbounded) {
        stop(
          paste0(
            "Unbounded local partial filtering may download ",
            "the entire Dataflow. Supply a period bound ",
            "or set `allow_unbounded = TRUE`."
          ),
          call. = FALSE
        )
      }
      local_filters <- filters
      key <- "all"
    } else {
      key <- tuik_build_key(profile = profile, filters = filters)
    }
    if (validate) {
      tuik_validate_filters(
        profile = profile, filters = filters,
        stop_on_error = TRUE, api_key = api_key
      )
    }
  }

  # ----------------------------------------------------------
  # Build data endpoint
  # ----------------------------------------------------------

  dataflow_id <- profile$dataflow$dataflow_id

  path <- paste0(
    "data/",
    dataflow_id,
    "/",
    key
  )

  # ----------------------------------------------------------
  # Build query parameters
  # ----------------------------------------------------------

  query <- list()

  if (!is.null(start_period)) {
    query$startPeriod <- start_period
  }

  if (!is.null(end_period)) {
    query$endPeriod <- end_period
  }

  # ----------------------------------------------------------
  # Retrieve SDMX-CSV
  # ----------------------------------------------------------

  resp <- tuik_perform(
    path = path,
    query = query,
    accept = "application/vnd.sdmx.data+csv;version=2.0",
    api_key = api_key
  )

  data <- .tuik_parse_sdmx_csv_response(
    resp,
    parse_observation = parse_observation
  )

  # Local filtering takes place BEFORE label decoding.
  # Important: this does not reduce bytes transferred from the server.
  if (!is.null(local_filters)) {
    for (dimension in names(local_filters)) {
      if (!dimension %in% names(data)) {
        stop("Requested filter dimension absent from response: ",
             dimension, call. = FALSE)
      }
      selected <- unique(trimws(local_filters[[dimension]]))
      data <- data[data[[dimension]] %in% selected, , drop = FALSE]
    }
  }

  if (!identical(labels, "none")) {
    data <- tuik_decode_data(
      data = data,
      profile = profile,
      dimensions = label_dimensions,
      language = labels,
      api_key = api_key
    )
  }

  data
}
