#' Explore a TUIK SDMX Dataflow before downloading observations
#'
#' Displays structural components from a Dataflow's DSD without downloading
#' observations. Series dimensions are selected using `filters`; TIME_PERIOD
#' uses `start_period` and `end_period` in [tuik_get_data()]. A specific coded
#' dimension can be inspected on demand. Codelist entries are theoretical,
#' not necessarily observed in a selected time window.
#'
#' @param dataflow A Dataflow ID or a `tuik_flow_profile` object.
#' @param dimension Optional series dimension ID whose codes to display.
#' @param search Optional search text for `dimension` codes.
#' @param language One of `"both"`, `"tr"`, or `"en"`.
#' @param limit Maximum codes to display for a selected dimension; default 50.
#' @param api_key Optional API key; defaults to `TUIK_SDMX_API_KEY`.
#' @param filters Optional named list of selected series-dimension codes. When
#'   supplied without `dimension`, adds `selected_codes` and `filter_status`
#'   columns to the DSD overview. This reports completeness only; it does not
#'   validate Codelist membership or download observations. Pass the same
#'   filters to [tuik_get_data()], which validates them before retrieval.
#'
#' @return Without `dimension`, a tibble describing DSD components and how to
#'   select them. If `filters` is supplied, also shows selections and missing
#'   series dimensions. With `dimension`, a tibble of Codelist codes and labels.
#' @examples
#' \dontrun{
#' tuik_explore("DF_TUFE_SDMX_TT01")
#' tuik_explore("DF_TUFE_SDMX_TT01", dimension = "DEGISIM")
#' selections <- list(REF_AREA = "TR", FREQ = "M", DEGISIM = "1")
#' tuik_explore("DF_TUFE_SDMX_TT01", filters = selections)
#' data <- tuik_get_data("DF_TUFE_SDMX_TT01", filters = selections,
#'                       filter_mode = "local", start_period = "2026-01",
#'                       end_period = "2026-01")
#' }
#' @export
 tuik_explore <- function(dataflow, dimension = NULL, search = NULL,
                          language = c("both", "tr", "en"), limit = 50,
                          api_key = NULL, filters = NULL) {
  language <- match.arg(language)
  if (inherits(dataflow, "tuik_flow_profile")) {
    profile <- dataflow
  } else if (is.character(dataflow) && length(dataflow) == 1L &&
             !is.na(dataflow) && nzchar(trimws(dataflow))) {
    profile <- tuik_flow_profile(trimws(dataflow), api_key = api_key)
  } else {
    stop("`dataflow` must be a Dataflow ID or tuik_flow_profile object.",
         call. = FALSE)
  }
  if (!is.null(dimension)) {
    if (!is.null(filters)) {
      stop("`filters` is available for the Dataflow overview only; omit `dimension`.",
           call. = FALSE)
    }
    if (!is.character(dimension) || length(dimension) != 1L ||
        is.na(dimension) || !nzchar(trimws(dimension))) {
      stop("`dimension` must be a non-empty character string.", call. = FALSE)
    }
    if (identical(dimension, "TIME_PERIOD")) {
      stop("TIME_PERIOD is selected with `start_period` and `end_period`, not Codelist codes.",
           call. = FALSE)
    }
    if (!(dimension %in% tuik_key_dimensions(profile)$dimension)) {
      stop("Unknown series dimension: ", dimension, call. = FALSE)
    }
    return(tuik_dimension_codes(profile, dimension = dimension,
                                search = search, language = language,
                                limit = limit, api_key = api_key))
  }
  if (!is.null(search)) {
    stop("`search` requires `dimension`.", call. = FALSE)
  }
  dims <- profile$dimensions
  result <- tibble::tibble(
    component = ifelse(dims$type == "TimeDimension", "time", "series_dimension"),
    name = dims$dimension,
    position = dims$position,
    selection = ifelse(dims$type == "TimeDimension",
                       "start_period / end_period", "filters"),
    codelist_id = dims$codelist_id
  )
  add_components <- function(items, role) {
    if (is.null(items) || !nrow(items)) return(NULL)
    id_col <- intersect(c("measure", "attribute", "id", "concept_id"), names(items))
    if (!length(id_col)) return(NULL)
    tibble::tibble(component = role,
                   name = as.character(items[[id_col[[1L]]]]),
                   position = NA_integer_, selection = "not a series filter",
                   codelist_id = NA_character_)
  }
  result <- dplyr::bind_rows(result,
                             add_components(profile$measures, "measure"),
                             add_components(profile$attributes, "attribute"))
  if (is.null(filters)) return(result)

  if (!is.list(filters) || (length(filters) > 0L && is.null(names(filters))) ||
      anyNA(names(filters)) || any(!nzchar(names(filters))) ||
      anyDuplicated(names(filters))) {
    stop("`filters` must be a named list with unique, non-empty names.",
         call. = FALSE)
  }
  key_dimensions <- tuik_key_dimensions(profile)$dimension
  unknown <- setdiff(names(filters), key_dimensions)
  if (length(unknown)) {
    stop("Unknown filter dimension(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  invalid <- names(filters)[vapply(filters, function(x) {
    !is.character(x) || !length(x) || anyNA(x) ||
      any(!nzchar(trimws(x)))
  }, logical(1))]
  if (length(invalid)) {
    stop("Filter values must be non-empty character codes: ",
         paste(invalid, collapse = ", "), call. = FALSE)
  }
  selected_codes <- rep(NA_character_, nrow(result))
  filter_status <- rep("not_applicable", nrow(result))
  filter_status[result$component == "time"] <- "use_period_arguments"
  filter_status[result$component == "series_dimension"] <- "missing"
  for (id in names(filters)) {
    idx <- which(result$component == "series_dimension" & result$name == id)
    selected_codes[idx] <- paste(unique(trimws(filters[[id]])), collapse = ", ")
    filter_status[idx] <- "selected"
  }
  result$selected_codes <- selected_codes
  result$filter_status <- filter_status
  result
}
