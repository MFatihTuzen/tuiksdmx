# ============================================================================
# dataflows.R
#
# Functions for discovering and retrieving TÜİK SDMX Dataflows.
#
# Main functions:
#   - tuik_dataflows()
#   - tuik_get_dataflow()
#
# A Dataflow represents a published statistical data flow and references
# the Data Structure Definition (DSD) used by that flow.
# ============================================================================

# ============================================================================
# In-memory Dataflow catalogue cache
#
# The catalogue is relatively large and does not need to be downloaded again
# for every search operation. The cache lives only for the current R session.
# ============================================================================

.tuiksdmx_dataflow_cache <- new.env(
  parent = emptyenv()
)

.tuiksdmx_dataflow_cache$catalogue <- NULL


#' List TÜİK SDMX Dataflows
#'
#' Retrieves the current Dataflow catalogue published by TÜİK.
#'
#' The returned table contains:
#'   - Dataflow agency, ID and version
#'   - Final/non-final status
#'   - Turkish and English names
#'   - Referenced DSD agency, ID and version
#'
#' Results are cached in memory for the current R session. Use
#' `refresh = TRUE` to force a new request to the TÜİK SDMX service.
#'
#' @param agency Dataflow agency. Defaults to "TR".
#' @param version Version selector. Defaults to "latest".
#' @param refresh Logical. If TRUE, ignore the cached catalogue and
#'   retrieve a fresh copy from the service.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with one row per Dataflow.
#'
#' @examples
#' \dontrun{
#' flows <- tuik_dataflows()
#'
#' # Force a fresh download
#' flows <- tuik_dataflows(refresh = TRUE)
#' }
#'
#' @export
tuik_dataflows <- function(
    agency = "TR",
    version = "latest",
    refresh = FALSE,
    api_key = NULL
) {

  # ----------------------------------------------------------
  # Use cache only for the default catalogue request
  #
  # The cache key is kept explicit so that the function can
  # later support other agencies or version selectors safely.
  # ----------------------------------------------------------

  api_key <- tuik_resolve_api_key(api_key)

  cache_key <- paste(
    agency,
    version,
    tuik_token_cache_id(api_key),
    sep = "::"
  )

  cached <- .tuiksdmx_dataflow_cache$catalogue

  if (
    !refresh &&
    !is.null(cached) &&
    identical(
      cached$key,
      cache_key
    )
  ) {
    return(
      cached$data
    )
  }

  # ----------------------------------------------------------
  # Retrieve catalogue
  # ----------------------------------------------------------

  path <- paste0(
    "dataflow/",
    agency,
    "/all/",
    version
  )

  resp <- tuik_perform(
    path = path,
    api_key = api_key
  )

  xml <- httr2::resp_body_xml(
    resp
  )

  flow_nodes <- xml2::xml_find_all(
    xml,
    ".//*[local-name()='Dataflow']"
  )

  # ----------------------------------------------------------
  # Empty response
  # ----------------------------------------------------------

  if (length(flow_nodes) == 0) {

    result <- tibble::tibble(
      agency = character(),
      dataflow_id = character(),
      dataflow_version = character(),
      is_final = logical(),
      name_tr = character(),
      name_en = character(),
      dsd_agency = character(),
      dsd_id = character(),
      dsd_version = character()
    )

    .tuiksdmx_dataflow_cache$catalogue <- list(
      key = cache_key,
      data = result
    )

    return(result)
  }

  # ----------------------------------------------------------
  # Parse Dataflows
  # ----------------------------------------------------------

  result <- purrr::map_dfr(
    seq_along(flow_nodes),
    function(i) {

      node <- flow_nodes[[i]]

      attrs <- xml2::xml_attrs(
        node
      )

      # ------------------------------------------------------
      # Multilingual names
      # ------------------------------------------------------

      name_nodes <- xml2::xml_find_all(
        node,
        "./*[local-name()='Name']"
      )

      languages <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) {

          xml2::xml_attr(
            name_nodes[[j]],
            "lang"
          )
        }
      )

      labels <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) {

          xml2::xml_text(
            name_nodes[[j]]
          )
        }
      )

      name_tr <-
        if ("tr" %in% languages) {

          labels[
            match(
              "tr",
              languages
            )
          ]

        } else {

          NA_character_
        }

      name_en <-
        if ("en" %in% languages) {

          labels[
            match(
              "en",
              languages
            )
          ]

        } else {

          NA_character_
        }

      # ------------------------------------------------------
      # Referenced DSD
      # ------------------------------------------------------

      dsd_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='Structure']/*[local-name()='Ref']"
      )

      if (
        inherits(
          dsd_ref,
          "xml_missing"
        )
      ) {

        dsd_agency <- NA_character_
        dsd_id <- NA_character_
        dsd_version <- NA_character_

      } else {

        dsd_attrs <- xml2::xml_attrs(
          dsd_ref
        )

        dsd_agency <-
          unname(
            dsd_attrs["agencyID"]
          )

        dsd_id <-
          unname(
            dsd_attrs["id"]
          )

        dsd_version <-
          unname(
            dsd_attrs["version"]
          )
      }

      # ------------------------------------------------------
      # Final status
      # ------------------------------------------------------

      is_final_raw <- unname(
        attrs["isFinal"]
      )

      is_final <-
        if (
          is.null(is_final_raw) ||
          is.na(is_final_raw)
        ) {

          NA

        } else {

          identical(
            tolower(is_final_raw),
            "true"
          )
        }

      # ------------------------------------------------------
      # Single Dataflow row
      # ------------------------------------------------------

      tibble::tibble(
        agency =
          unname(
            attrs["agencyID"]
          ),

        dataflow_id =
          unname(
            attrs["id"]
          ),

        dataflow_version =
          unname(
            attrs["version"]
          ),

        is_final =
          is_final,

        name_tr =
          name_tr,

        name_en =
          name_en,

        dsd_agency =
          dsd_agency,

        dsd_id =
          dsd_id,

        dsd_version =
          dsd_version
      )
    }
  )

  # ----------------------------------------------------------
  # Store in session cache
  # ----------------------------------------------------------

  .tuiksdmx_dataflow_cache$catalogue <- list(
    key = cache_key,
    data = result
  )

  result
}


#' Retrieve a single TÜİK SDMX Dataflow
#'
#' Retrieves metadata for one Dataflow and its referenced DSD.
#'
#' @param dataflow_id Dataflow identifier.
#' @param agency Dataflow agency. Defaults to "TR".
#' @param version Dataflow version. Defaults to "latest".
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A one-row tibble.
#'
#' @examples
#' \dontrun{
#' tuik_get_dataflow(
#'   "DF_TUFE_SDMX_TT01",
#'   version = "1.0"
#' )
#' }
#'
#' @export
tuik_get_dataflow <- function(
    dataflow_id,
    agency = "TR",
    version = "latest",
    api_key = NULL
) {

  if (
    length(dataflow_id) != 1 ||
    is.na(dataflow_id) ||
    !nzchar(dataflow_id)
  ) {
    stop(
      "`dataflow_id` must be a non-empty character string.",
      call. = FALSE
    )
  }

  path <- paste0(
    "dataflow/",
    agency,
    "/",
    dataflow_id,
    "/",
    version
  )

  resp <- tuik_perform(
    path = path,
    api_key = api_key
  )

  xml <- httr2::resp_body_xml(
    resp
  )

  flow_node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='Dataflow']"
  )

  if (
    inherits(
      flow_node,
      "xml_missing"
    )
  ) {
    stop(
      paste0(
        "Dataflow not found in response: ",
        agency,
        ":",
        dataflow_id,
        "(",
        version,
        ")"
      ),
      call. = FALSE
    )
  }

  attrs <- xml2::xml_attrs(
    flow_node
  )

  # ----------------------------------------------------------
  # Multilingual names
  # ----------------------------------------------------------

  name_nodes <- xml2::xml_find_all(
    flow_node,
    "./*[local-name()='Name']"
  )

  languages <- purrr::map_chr(
    seq_along(name_nodes),
    function(i) {
      xml2::xml_attr(
        name_nodes[[i]],
        "lang"
      )
    }
  )

  labels <- purrr::map_chr(
    seq_along(name_nodes),
    function(i) {
      xml2::xml_text(
        name_nodes[[i]]
      )
    }
  )

  name_tr <-
    if ("tr" %in% languages) {
      labels[
        match(
          "tr",
          languages
        )
      ]
    } else {
      NA_character_
    }

  name_en <-
    if ("en" %in% languages) {
      labels[
        match(
          "en",
          languages
        )
      ]
    } else {
      NA_character_
    }

  # ----------------------------------------------------------
  # DSD reference
  # ----------------------------------------------------------

  dsd_ref <- xml2::xml_find_first(
    flow_node,
    ".//*[local-name()='Structure']/*[local-name()='Ref']"
  )

  if (
    inherits(
      dsd_ref,
      "xml_missing"
    )
  ) {

    dsd_agency <- NA_character_
    dsd_id <- NA_character_
    dsd_version <- NA_character_

  } else {

    dsd_attrs <- xml2::xml_attrs(
      dsd_ref
    )

    dsd_agency <-
      unname(
        dsd_attrs["agencyID"]
      )

    dsd_id <-
      unname(
        dsd_attrs["id"]
      )

    dsd_version <-
      unname(
        dsd_attrs["version"]
      )
  }

  is_final_raw <- unname(
    attrs["isFinal"]
  )

  is_final <-
    if (
      is.na(is_final_raw) ||
      is.null(is_final_raw)
    ) {
      NA
    } else {
      identical(
        tolower(is_final_raw),
        "true"
      )
    }

  tibble::tibble(
    agency =
      unname(
        attrs["agencyID"]
      ),

    dataflow_id =
      unname(
        attrs["id"]
      ),

    dataflow_version =
      unname(
        attrs["version"]
      ),

    is_final =
      is_final,

    name_tr =
      name_tr,

    name_en =
      name_en,

    dsd_agency =
      dsd_agency,

    dsd_id =
      dsd_id,

    dsd_version =
      dsd_version
  )
}

#' Clear the cached Dataflow catalogue
#'
#' Removes the in-memory Dataflow catalogue cache.
#'
#' This is mainly useful when the catalogue is known to have changed or
#' during development and testing.
#'
#' @return NULL invisibly.
#'
#' @keywords internal
tuik_clear_dataflow_cache <- function() {

  .tuiksdmx_dataflow_cache$catalogue <- NULL

  invisible(NULL)
}
