
# ============================================================================
# codelists.R
#
# Functions for retrieving and searching TÜİK SDMX Codelists.
#
# Main functions:
#   - tuik_get_codelist()
#   - tuik_dimension_codes()
#
# A Codelist defines the coded values that may be used by a dimension
# or attribute.
# ============================================================================

# ============================================================================
# In-memory Codelist cache
#
# Codelists can be relatively large and are frequently reused across
# dimensions and validation operations. Cached Codelists are reused during
# the current R session to avoid repeated network requests.
# ============================================================================

.tuiksdmx_codelist_cache <- new.env(
  parent = emptyenv()
)


#' Parse a TÜİK SDMX Codelist node
#'
#' @param codelist_node An xml2 Codelist node.
#'
#' @return A tibble with code, Turkish label and English label.
#'
#' @keywords internal
.tuik_parse_codelist_node <- function(codelist_node) {

  code_nodes <- xml2::xml_find_all(
    codelist_node,
    "./*[local-name()='Code']"
  )

  if (length(code_nodes) == 0) {
    return(
      tibble::tibble(
        code = character(),
        label_tr = character(),
        label_en = character()
      )
    )
  }

  purrr::map_dfr(
    seq_along(code_nodes),
    function(i) {

      node <- code_nodes[[i]]
      code <- xml2::xml_attr(node, "id")

      name_nodes <- xml2::xml_find_all(
        node,
        "./*[local-name()='Name']"
      )

      languages <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) {
          xml2::xml_attr(name_nodes[[j]], "lang")
        }
      )

      labels <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) {
          xml2::xml_text(name_nodes[[j]])
        }
      )

      pick_lang <- function(language) {
        if (language %in% languages) {
          labels[match(language, languages)]
        } else {
          NA_character_
        }
      }

      tibble::tibble(
        code = code,
        label_tr = pick_lang("tr"),
        label_en = pick_lang("en")
      )
    }
  )
}


#' Retrieve a TÜİK SDMX Codelist
#'
#' Downloads and parses an SDMX Codelist.
#'
#' Results are cached in memory for the current R session.
#'
#' @param codelist_id Codelist identifier.
#' @param agency Codelist agency.
#' @param version Codelist version.
#' @param refresh Logical. If TRUE, ignore the cache and download the
#'   Codelist again.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with code, Turkish label and English label.
#'
#' @export
tuik_get_codelist <- function(
    codelist_id,
    agency,
    version = "latest",
    refresh = FALSE,
    api_key = NULL
) {

  if (
    length(codelist_id) != 1 ||
    is.na(codelist_id) ||
    !nzchar(codelist_id)
  ) {
    stop(
      "`codelist_id` must be a non-empty character string.",
      call. = FALSE
    )
  }

  if (
    length(agency) != 1 ||
    is.na(agency) ||
    !nzchar(agency)
  ) {
    stop(
      "`agency` must be a non-empty character string.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Create a credential-specific cache key
  # ----------------------------------------------------------

  cache_key <- .tuik_metadata_cache_key(
    agency = agency,
    id = codelist_id,
    version = version,
    api_key = api_key
  )

  # ----------------------------------------------------------
  # Return cached Codelist when available
  # ----------------------------------------------------------

  if (
    !refresh &&
    exists(
      cache_key,
      envir = .tuiksdmx_codelist_cache,
      inherits = FALSE
    )
  ) {
    return(
      get(
        cache_key,
        envir = .tuiksdmx_codelist_cache,
        inherits = FALSE
      )
    )
  }

  # ----------------------------------------------------------
  # Retrieve Codelist
  # ----------------------------------------------------------

  path <- paste0(
    "codelist/",
    agency,
    "/",
    codelist_id,
    "/",
    version
  )

  resp <- tuik_perform(
    path = path,
    api_key = api_key
  )

  xml <- httr2::resp_body_xml(resp)

  codelist_node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='Codelist']"
  )

  if (inherits(codelist_node, "xml_missing")) {
    stop(
      "No Codelist element was found in the response.",
      call. = FALSE
    )
  }

  result <- .tuik_parse_codelist_node(
    codelist_node
  )

  # ----------------------------------------------------------
  # Store parsed Codelist in memory
  # ----------------------------------------------------------

  assign(
    cache_key,
    result,
    envir = .tuiksdmx_codelist_cache
  )

  result
}


#' Retrieve codes for a DSD dimension
#'
#' Finds the Codelist referenced by a dimension and returns its codes.
#'
#' Optionally searches within code values and Turkish/English labels.
#'
#' @param dimensions Dimension table returned by `tuik_dimensions()` or a
#'   `tuik_flow_profile` object.
#' @param dimension Dimension ID, for example "FREQ" or "COICOP_2018".
#' @param search Optional search term.
#' @param language Search language: "both", "tr", or "en".
#' @param limit Maximum number of rows to return. Use NULL for all rows.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble containing the dimension codes.
#'
#' @examples
#' \dontrun{
#' tuik_dimension_codes(
#'   dimensions,
#'   "FREQ"
#' )
#'
#' tuik_dimension_codes(
#'   dimensions,
#'   "COICOP_2018",
#'   search = "food"
#' )
#' }
#'
#' @export
tuik_dimension_codes <- function(
    dimensions,
    dimension,
    search = NULL,
    language = c("both", "tr", "en"),
    limit = 50,
    api_key = NULL
) {

  language <- match.arg(language)

  if (inherits(dimensions, "tuik_flow_profile")) {
    dimensions <- dimensions$dimensions
  }

  if (
    length(dimension) != 1 ||
    is.na(dimension) ||
    !nzchar(dimension)
  ) {
    stop(
      "`dimension` must be a non-empty character string.",
      call. = FALSE
    )
  }

  dim_row <- dimensions |>
    dplyr::filter(
      .data$dimension == !!dimension
    )

  if (nrow(dim_row) == 0) {
    stop(
      paste0(
        "Dimension not found: ",
        dimension
      ),
      call. = FALSE
    )
  }

  if (nrow(dim_row) > 1) {
    stop(
      paste0(
        "Multiple rows found for dimension: ",
        dimension
      ),
      call. = FALSE
    )
  }

  if (
    is.na(dim_row$codelist_id) ||
    is.na(dim_row$codelist_agency)
  ) {
    stop(
      paste0(
        "Dimension '",
        dimension,
        "' does not reference a Codelist."
      ),
      call. = FALSE
    )
  }

  codes <- tuik_get_codelist(
    codelist_id = dim_row$codelist_id,
    agency = dim_row$codelist_agency,
    version = dim_row$codelist_version,
    api_key = api_key
  )

  # ----------------------------------------------------------
  # Optional search
  # ----------------------------------------------------------
  if (!is.null(search)) {

    search_norm <- tuik_normalize_text(search)

    codes <- codes |>
      dplyr::mutate(
        search_code = tuik_normalize_text(.data$code),
        search_tr = tuik_normalize_text(.data$label_tr),
        search_en = tuik_normalize_text(.data$label_en)
      )

    if (language == "tr") {

      codes <- codes |>
        dplyr::filter(
          grepl(search_norm, .data$search_code, fixed = TRUE) |
            grepl(search_norm, .data$search_tr, fixed = TRUE)
        )

    } else if (language == "en") {

      codes <- codes |>
        dplyr::filter(
          grepl(search_norm, .data$search_code, fixed = TRUE) |
            grepl(search_norm, .data$search_en, fixed = TRUE)
        )

    } else {

      codes <- codes |>
        dplyr::filter(
          grepl(search_norm, .data$search_code, fixed = TRUE) |
            grepl(search_norm, .data$search_tr, fixed = TRUE) |
            grepl(search_norm, .data$search_en, fixed = TRUE)
        )
    }

    codes <- codes |>
      dplyr::select(
        "code",
        "label_tr",
        "label_en"
      )
  }



  # ----------------------------------------------------------
  # Apply row limit
  # ----------------------------------------------------------

  if (!is.null(limit)) {

    codes <- codes |>
      dplyr::slice_head(
        n = limit
      )
  }

  codes
}


#' Clear cached Codelists
#'
#' Removes all Codelists stored in the current R session cache.
#'
#' @return NULL invisibly.
#'
#' @keywords internal
tuik_clear_codelist_cache <- function() {

  rm(
    list = ls(
      envir = .tuiksdmx_codelist_cache,
      all.names = TRUE
    ),
    envir = .tuiksdmx_codelist_cache
  )

  invisible(NULL)
}


#' Preload Codelists used by a Dataflow
#'
#' Retrieves descendant structures for a Dataflow and stores all returned
#' Codelists in the in-memory Codelist cache.
#'
#' This is more efficient than downloading each Codelist separately when
#' several dimensions need to be validated.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param refresh Logical. If TRUE, overwrite already cached Codelists.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return The number of Codelists stored in the cache, invisibly.
#'
#' @keywords internal
tuik_preload_codelists <- function(
    profile,
    refresh = FALSE,
    api_key = NULL
) {

  if (
    is.null(profile$dataflow) ||
    nrow(profile$dataflow) != 1
  ) {
    stop(
      "`profile` must contain exactly one Dataflow.",
      call. = FALSE
    )
  }

  flow <- profile$dataflow

  path <- paste0(
    "dataflow/",
    flow$agency,
    "/",
    flow$dataflow_id,
    "/",
    flow$dataflow_version
  )

  resp <- tuik_perform(
    path = path,
    query = list(
      references = "descendants"
    ),
    api_key = api_key
  )

  xml <- httr2::resp_body_xml(resp)

  codelist_nodes <- xml2::xml_find_all(
    xml,
    ".//*[local-name()='Codelist']"
  )

  if (length(codelist_nodes) == 0) {
    return(invisible(0L))
  }

  stored <- 0L

  for (i in seq_along(codelist_nodes)) {

    node <- codelist_nodes[[i]]
    attrs <- xml2::xml_attrs(node)

    agency <- unname(
      attrs["agencyID"]
    )

    codelist_id <- unname(
      attrs["id"]
    )

    version <- unname(
      attrs["version"]
    )

    if (
      is.na(agency) ||
      is.na(codelist_id) ||
      is.na(version)
    ) {
      next
    }

    cache_key <- .tuik_metadata_cache_key(
      agency = agency,
      id = codelist_id,
      version = version,
      api_key = api_key
    )

    if (
      !refresh &&
      exists(
        cache_key,
        envir = .tuiksdmx_codelist_cache,
        inherits = FALSE
      )
    ) {
      next
    }

    result <- .tuik_parse_codelist_node(
      node
    )

    assign(
      cache_key,
      result,
      envir = .tuiksdmx_codelist_cache
    )

    stored <- stored + 1L
  }

  invisible(stored)
}
