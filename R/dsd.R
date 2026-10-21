# ============================================================================
# dsd.R
#
# Functions for retrieving and parsing TÜİK SDMX Data Structure Definitions.
#
# Main functions:
#   - tuik_get_dsd()
#   - tuik_dimensions()
#   - tuik_measures()
#   - tuik_attributes()
#
# A Data Structure Definition (DSD) describes the structure of a statistical
# dataset, including dimensions, measures and attributes.
# ============================================================================


#' Retrieve a TÜİK SDMX Data Structure Definition
#'
#' Retrieves a DSD from the TÜİK SDMX Web Service.
#'
#' @param dsd_id Data Structure Definition identifier.
#' @param agency DSD agency. Defaults to "TR".
#' @param version DSD version. Defaults to "latest".
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return An xml2 XML document.
#'
#' @examples
#' \dontrun{
#' dsd <- tuik_get_dsd(
#'   "DSD_TUFE",
#'   version = "1.12"
#' )
#' }
#'
#' @export
tuik_get_dsd <- function(
    dsd_id,
    agency = "TR",
    version = "latest",
    api_key = NULL
) {

  if (
    length(dsd_id) != 1 ||
    is.na(dsd_id) ||
    !nzchar(dsd_id)
  ) {
    stop(
      "`dsd_id` must be a non-empty character string.",
      call. = FALSE
    )
  }

  path <- paste0(
    "datastructure/",
    agency,
    "/",
    dsd_id,
    "/",
    version
  )

  resp <- tuik_perform(
    path = path,
    api_key = api_key
  )

  httr2::resp_body_xml(
    resp
  )
}


#' Extract dimensions from a TÜİK SDMX DSD
#'
#' Parses Dimension and TimeDimension components from a DSD.
#'
#' @param dsd XML document returned by `tuik_get_dsd()`.
#'
#' @return A tibble describing DSD dimensions.
#'
#' @export
tuik_dimensions <- function(
    dsd
) {

  dsd_node <- xml2::xml_find_first(
    dsd,
    ".//*[local-name()='DataStructure']"
  )

  if (
    inherits(
      dsd_node,
      "xml_missing"
    )
  ) {
    stop(
      "No DataStructure element was found in the supplied XML.",
      call. = FALSE
    )
  }

  dimension_nodes <- xml2::xml_find_all(
    dsd_node,
    ".//*[local-name()='DimensionList']/*[
      local-name()='Dimension' or
      local-name()='TimeDimension'
    ]"
  )

  purrr::map_dfr(
    seq_along(dimension_nodes),
    function(i) {

      node <- dimension_nodes[[i]]

      attrs <- xml2::xml_attrs(
        node
      )

      concept_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='ConceptIdentity']/*[local-name()='Ref']"
      )

      concept_attrs <-
        if (
          inherits(
            concept_ref,
            "xml_missing"
          )
        ) {
          character()
        } else {
          xml2::xml_attrs(
            concept_ref
          )
        }

      codelist_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='Enumeration']/*[local-name()='Ref']"
      )

      if (
        inherits(
          codelist_ref,
          "xml_missing"
        )
      ) {

        codelist_agency <- NA_character_
        codelist_id <- NA_character_
        codelist_version <- NA_character_

      } else {

        codelist_attrs <- xml2::xml_attrs(
          codelist_ref
        )

        codelist_agency <-
          unname(
            codelist_attrs["agencyID"]
          )

        codelist_id <-
          unname(
            codelist_attrs["id"]
          )

        codelist_version <-
          unname(
            codelist_attrs["version"]
          )
      }

      tibble::tibble(
        position =
          as.integer(
            unname(
              attrs["position"]
            )
          ),

        dimension =
          unname(
            attrs["id"]
          ),

        type =
          xml2::xml_name(
            node
          ),

        concept_id =
          unname(
            concept_attrs["id"]
          ),

        concept_scheme =
          unname(
            concept_attrs[
              "maintainableParentID"
            ]
          ),

        concept_scheme_version =
          unname(
            concept_attrs[
              "maintainableParentVersion"
            ]
          ),

        concept_agency =
          unname(
            concept_attrs["agencyID"]
          ),

        codelist_agency =
          codelist_agency,

        codelist_id =
          codelist_id,

        codelist_version =
          codelist_version
      )
    }
  ) |>
    dplyr::arrange(
      .data$position
    )
}


#' Extract measures from a TÜİK SDMX DSD
#'
#' Parses measure components from a DSD.
#'
#' @param dsd XML document returned by `tuik_get_dsd()`.
#'
#' @return A tibble describing DSD measures.
#'
#' @export
tuik_measures <- function(
    dsd
) {

  dsd_node <- xml2::xml_find_first(
    dsd,
    ".//*[local-name()='DataStructure']"
  )

  if (
    inherits(
      dsd_node,
      "xml_missing"
    )
  ) {
    stop(
      "No DataStructure element was found in the supplied XML.",
      call. = FALSE
    )
  }

  measure_nodes <- xml2::xml_find_all(
    dsd_node,
    ".//*[local-name()='MeasureList']/*"
  )

  purrr::map_dfr(
    seq_along(measure_nodes),
    function(i) {

      node <- measure_nodes[[i]]

      attrs <- xml2::xml_attrs(
        node
      )

      concept_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='ConceptIdentity']/*[local-name()='Ref']"
      )

      concept_attrs <-
        if (
          inherits(
            concept_ref,
            "xml_missing"
          )
        ) {
          character()
        } else {
          xml2::xml_attrs(
            concept_ref
          )
        }

      tibble::tibble(
        measure =
          unname(
            attrs["id"]
          ),

        type =
          xml2::xml_name(
            node
          ),

        concept_id =
          unname(
            concept_attrs["id"]
          ),

        concept_scheme =
          unname(
            concept_attrs[
              "maintainableParentID"
            ]
          ),

        concept_scheme_version =
          unname(
            concept_attrs[
              "maintainableParentVersion"
            ]
          ),

        concept_agency =
          unname(
            concept_attrs["agencyID"]
          )
      )
    }
  )
}


#' Extract attributes from a TÜİK SDMX DSD
#'
#' Parses attribute components and their coded representations from a DSD.
#'
#' @param dsd XML document returned by `tuik_get_dsd()`.
#'
#' @return A tibble describing DSD attributes.
#'
#' @export
tuik_attributes <- function(
    dsd
) {

  dsd_node <- xml2::xml_find_first(
    dsd,
    ".//*[local-name()='DataStructure']"
  )

  if (
    inherits(
      dsd_node,
      "xml_missing"
    )
  ) {
    stop(
      "No DataStructure element was found in the supplied XML.",
      call. = FALSE
    )
  }

  attribute_nodes <- xml2::xml_find_all(
    dsd_node,
    ".//*[local-name()='AttributeList']/*[local-name()='Attribute']"
  )

  purrr::map_dfr(
    seq_along(attribute_nodes),
    function(i) {

      node <- attribute_nodes[[i]]

      attrs <- xml2::xml_attrs(
        node
      )

      # --------------------------------------------------------
      # Concept identity
      # --------------------------------------------------------

      concept_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='ConceptIdentity']/*[local-name()='Ref']"
      )

      concept_attrs <-
        if (
          inherits(
            concept_ref,
            "xml_missing"
          )
        ) {
          character()
        } else {
          xml2::xml_attrs(
            concept_ref
          )
        }

      # --------------------------------------------------------
      # Codelist representation
      # --------------------------------------------------------

      codelist_ref <- xml2::xml_find_first(
        node,
        ".//*[local-name()='Enumeration']/*[local-name()='Ref']"
      )

      if (
        inherits(
          codelist_ref,
          "xml_missing"
        )
      ) {

        codelist_agency <- NA_character_
        codelist_id <- NA_character_
        codelist_version <- NA_character_

      } else {

        codelist_attrs <- xml2::xml_attrs(
          codelist_ref
        )

        codelist_agency <-
          unname(
            codelist_attrs["agencyID"]
          )

        codelist_id <-
          unname(
            codelist_attrs["id"]
          )

        codelist_version <-
          unname(
            codelist_attrs["version"]
          )
      }

      # --------------------------------------------------------
      # Attribute relationship
      # --------------------------------------------------------

      relationship_node <- xml2::xml_find_first(
        node,
        ".//*[local-name()='AttributeRelationship']/*[1]"
      )

      relationship_type <-
        if (
          inherits(
            relationship_node,
            "xml_missing"
          )
        ) {
          NA_character_
        } else {
          xml2::xml_name(
            relationship_node
          )
        }

      relationship_ref <- xml2::xml_find_first(
        relationship_node,
        ".//*[local-name()='Ref']"
      )

      relationship_target <-
        if (
          inherits(
            relationship_ref,
            "xml_missing"
          )
        ) {
          NA_character_
        } else {
          xml2::xml_attr(
            relationship_ref,
            "id"
          )
        }

      # --------------------------------------------------------
      # Single attribute row
      # --------------------------------------------------------

      tibble::tibble(
        attribute =
          unname(
            attrs["id"]
          ),

        assignment_status =
          unname(
            attrs["assignmentStatus"]
          ),

        concept_id =
          unname(
            concept_attrs["id"]
          ),

        concept_scheme =
          unname(
            concept_attrs[
              "maintainableParentID"
            ]
          ),

        concept_scheme_version =
          unname(
            concept_attrs[
              "maintainableParentVersion"
            ]
          ),

        concept_agency =
          unname(
            concept_attrs["agencyID"]
          ),

        codelist_agency =
          codelist_agency,

        codelist_id =
          codelist_id,

        codelist_version =
          codelist_version,

        relationship_type =
          relationship_type,

        relationship_target =
          relationship_target
      )
    }
  )
}
