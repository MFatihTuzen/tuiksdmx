#' Add SDMX code labels to an observation table
#'
#' Joins dimension code labels from the Codelists referenced by a Dataflow
#' profile. Original SDMX dimension codes and observations are preserved.
#' The labels describe Codelist definitions, not observed availability.
#'
#' @param data A data frame returned by [tuik_get_data()].
#' @param profile A `tuik_flow_profile` object.
#' @param dimensions NULL (all coded dimensions present in `data`) or a
#'   character vector of dimension IDs to decode.
#' @param language One of `"tr"`, `"en"`, or `"both"`.
#' @param api_key Optional API key. If NULL, `TUIK_SDMX_API_KEY` is used.
#'
#' @return A tibble containing the input columns plus `DIMENSION_label_tr`
#'   and/or `DIMENSION_label_en` columns. Unknown code labels remain `NA`.
#' @export
#' @examples
#' \dontrun{
#' decoded <- tuik_decode_data(data, profile, dimensions = c("DEGISIM", "FREQ"))
#' }
tuik_decode_data <- function(data, profile, dimensions = NULL,
                             language = c("tr", "en", "both"),
                             api_key = NULL) {
  language <- match.arg(language)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!inherits(profile, "tuik_flow_profile") ||
      !is.data.frame(profile$dimensions)) {
    stop("`profile` must be a tuik_flow_profile object.", call. = FALSE)
  }

  metadata <- profile$dimensions
  coded <- metadata$dimension[
    !is.na(metadata$codelist_id) &
      !is.na(metadata$codelist_agency) &
      !is.na(metadata$codelist_version)
  ]
  if (is.null(dimensions)) {
    dimensions <- intersect(coded, names(data))
  } else {
    if (!is.character(dimensions) || anyNA(dimensions) ||
        any(!nzchar(dimensions)) || anyDuplicated(dimensions)) {
      stop("`dimensions` must be a vector of unique, non-empty names.",
           call. = FALSE)
    }
    unavailable <- setdiff(dimensions, coded)
    if (length(unavailable)) {
      stop("Dimension(s) do not reference a Codelist: ",
           paste(unavailable, collapse = ", "), call. = FALSE)
    }
    absent <- setdiff(dimensions, names(data))
    if (length(absent)) {
      stop("Dimension(s) missing from `data`: ",
           paste(absent, collapse = ", "), call. = FALSE)
    }
  }

  suffixes <- switch(language, tr = "label_tr", en = "label_en",
                     both = c("label_tr", "label_en"))
  output <- tibble::as_tibble(data)
  for (dimension in dimensions) {
    if (!is.character(output[[dimension]])) {
      stop("Dimension `", dimension, "` must be character; SDMX codes ",
           "must not be converted to numbers.", call. = FALSE)
    }
    new_names <- paste0(dimension, "_", suffixes)
    collisions <- intersect(new_names, names(output))
    if (length(collisions)) {
      stop("Label column(s) already exist: ",
           paste(collisions, collapse = ", "), call. = FALSE)
    }

    codes <- tuik_dimension_codes(
      profile, dimension = dimension, limit = NULL, api_key = api_key
    )
    # Matching (rather than a many-to-many join) guarantees one output row
    # per input row, even if a malformed Codelist has repeated code IDs.
    if (anyDuplicated(codes$code)) {
      stop("Duplicate code IDs in Codelist for `", dimension, "`.",
           call. = FALSE)
    }
    index <- match(output[[dimension]], codes$code)
    for (i in seq_along(suffixes)) {
      output[[new_names[[i]]]] <- codes[[suffixes[[i]]]][index]
    }
  }
  output
}
