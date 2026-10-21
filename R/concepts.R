# ============================================================================
# concepts.R
#
# Functions for retrieving and interpreting TÜİK SDMX ConceptSchemes.
#
# Main functions:
#   - tuik_get_concept_scheme()
#   - tuik_concepts()
#   - tuik_search_concepts()
#
# Concepts describe the semantic meaning of DSD components. Codelists define
# the coded values that a component may take; ConceptSchemes explain what the
# component itself means.
# ============================================================================

.tuiksdmx_concept_cache <- new.env(parent = emptyenv())


#' Parse a TÜİK SDMX ConceptScheme node
#'
#' @param concept_scheme_node An xml2 ConceptScheme node.
#'
#' @return A tibble with one row per Concept.
#'
#' @keywords internal
.tuik_parse_concept_scheme_node <- function(concept_scheme_node) {
  scheme_attrs <- xml2::xml_attrs(concept_scheme_node)

  scheme_agency <- unname(scheme_attrs["agencyID"])
  scheme_id <- unname(scheme_attrs["id"])
  scheme_version <- unname(scheme_attrs["version"])

  concept_nodes <- xml2::xml_find_all(
    concept_scheme_node,
    "./*[local-name()='Concept']"
  )

  if (length(concept_nodes) == 0) {
    return(
      tibble::tibble(
        concept_agency = character(),
        concept_scheme = character(),
        concept_scheme_version = character(),
        concept_id = character(),
        label_tr = character(),
        label_en = character(),
        description_tr = character(),
        description_en = character()
      )
    )
  }

  purrr::map_dfr(
    seq_along(concept_nodes),
    function(i) {
      node <- concept_nodes[[i]]
      concept_id <- xml2::xml_attr(node, "id")

      name_nodes <- xml2::xml_find_all(
        node,
        "./*[local-name()='Name']"
      )
      name_lang <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) xml2::xml_attr(name_nodes[[j]], "lang")
      )
      name_text <- purrr::map_chr(
        seq_along(name_nodes),
        function(j) xml2::xml_text(name_nodes[[j]])
      )

      description_nodes <- xml2::xml_find_all(
        node,
        "./*[local-name()='Description']"
      )
      description_lang <- purrr::map_chr(
        seq_along(description_nodes),
        function(j) xml2::xml_attr(description_nodes[[j]], "lang")
      )
      description_text <- purrr::map_chr(
        seq_along(description_nodes),
        function(j) xml2::xml_text(description_nodes[[j]])
      )

      pick_lang <- function(languages, values, language) {
        if (language %in% languages) {
          values[match(language, languages)]
        } else {
          NA_character_
        }
      }

      tibble::tibble(
        concept_agency = scheme_agency,
        concept_scheme = scheme_id,
        concept_scheme_version = scheme_version,
        concept_id = concept_id,
        label_tr = pick_lang(name_lang, name_text, "tr"),
        label_en = pick_lang(name_lang, name_text, "en"),
        description_tr = pick_lang(
          description_lang,
          description_text,
          "tr"
        ),
        description_en = pick_lang(
          description_lang,
          description_text,
          "en"
        )
      )
    }
  )
}


#' Retrieve a TÜİK SDMX ConceptScheme
#'
#' Downloads and parses a ConceptScheme. Results are cached in memory for the
#' current R session.
#'
#' @param concept_scheme_id ConceptScheme identifier.
#' @param agency ConceptScheme agency.
#' @param version ConceptScheme version. Defaults to "latest".
#' @param refresh Logical. If TRUE, ignore the cache and download again.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with Concept IDs and multilingual labels/descriptions.
#'
#' @export
tuik_get_concept_scheme <- function(
    concept_scheme_id,
    agency,
    version = "latest",
    refresh = FALSE,
    api_key = NULL
) {
  if (
    length(concept_scheme_id) != 1 ||
    is.na(concept_scheme_id) ||
    !nzchar(concept_scheme_id)
  ) {
    stop(
      "`concept_scheme_id` must be a non-empty character string.",
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

  cache_key <- .tuik_metadata_cache_key(
    agency = agency,
    id = concept_scheme_id,
    version = version,
    api_key = api_key
  )

  if (
    !refresh &&
    exists(
      cache_key,
      envir = .tuiksdmx_concept_cache,
      inherits = FALSE
    )
  ) {
    return(
      get(
        cache_key,
        envir = .tuiksdmx_concept_cache,
        inherits = FALSE
      )
    )
  }

  path <- paste0(
    "conceptscheme/",
    agency,
    "/",
    concept_scheme_id,
    "/",
    version
  )

  resp <- tuik_perform(path = path, api_key = api_key)
  xml <- httr2::resp_body_xml(resp)

  concept_scheme_node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  if (inherits(concept_scheme_node, "xml_missing")) {
    stop(
      "No ConceptScheme element was found in the response.",
      call. = FALSE
    )
  }

  result <- .tuik_parse_concept_scheme_node(concept_scheme_node)

  assign(
    cache_key,
    result,
    envir = .tuiksdmx_concept_cache
  )

  result
}


#' Preload ConceptSchemes used by a Dataflow
#'
#' Retrieves descendant structures for a Dataflow and stores all returned
#' ConceptSchemes in the in-memory ConceptScheme cache.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param refresh Logical. If TRUE, overwrite already cached ConceptSchemes.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return The number of ConceptSchemes stored in the cache, invisibly.
#'
#' @keywords internal
tuik_preload_concepts <- function(
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
    query = list(references = "descendants"),
    api_key = api_key
  )

  xml <- httr2::resp_body_xml(resp)

  scheme_nodes <- xml2::xml_find_all(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  if (length(scheme_nodes) == 0) {
    return(invisible(0L))
  }

  stored <- 0L

  for (i in seq_along(scheme_nodes)) {
    node <- scheme_nodes[[i]]
    attrs <- xml2::xml_attrs(node)

    agency <- unname(attrs["agencyID"])
    scheme_id <- unname(attrs["id"])
    version <- unname(attrs["version"])

    if (
      is.na(agency) ||
      is.na(scheme_id) ||
      is.na(version)
    ) {
      next
    }
    cache_key <- .tuik_metadata_cache_key(
      agency = agency,
      id = scheme_id,
      version = version,
      api_key = api_key
    )

    if (
      !refresh &&
      exists(
        cache_key,
        envir = .tuiksdmx_concept_cache,
        inherits = FALSE
      )
    ) {
      next
    }

    assign(
      cache_key,
      .tuik_parse_concept_scheme_node(node),
      envir = .tuiksdmx_concept_cache
    )

    stored <- stored + 1L
  }

  invisible(stored)
}


#' Retrieve concepts used by a TÜİK Dataflow
#'
#' Resolves the concepts referenced by dimensions, measures and attributes in
#' a flow profile and adds their Turkish/English labels and descriptions.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param refresh Logical. If TRUE, refresh ConceptScheme metadata.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A tibble with component-to-concept mappings and semantic metadata.
#'
#' @export
tuik_concepts <- function(
    profile,
    refresh = FALSE,
    api_key = NULL
) {
  if (is.null(profile$dimensions)) {
    stop(
      "`profile` does not contain dimension information.",
      call. = FALSE
    )
  }

  dimension_refs <- profile$dimensions |>
    dplyr::transmute(
      component_type = "dimension",
      component_id = .data$dimension,
      concept_id = .data$concept_id,
      concept_scheme = .data$concept_scheme,
      concept_scheme_version = .data$concept_scheme_version,
      concept_agency = .data$concept_agency
    )

  measure_refs <- profile$measures |>
    dplyr::transmute(
      component_type = "measure",
      component_id = .data$measure,
      concept_id = .data$concept_id,
      concept_scheme = .data$concept_scheme,
      concept_scheme_version = .data$concept_scheme_version,
      concept_agency = .data$concept_agency
    )

  attribute_refs <- profile$attributes |>
    dplyr::transmute(
      component_type = "attribute",
      component_id = .data$attribute,
      concept_id = .data$concept_id,
      concept_scheme = .data$concept_scheme,
      concept_scheme_version = .data$concept_scheme_version,
      concept_agency = .data$concept_agency
    )

  refs <- dplyr::bind_rows(
    dimension_refs,
    measure_refs,
    attribute_refs
  ) |>
    dplyr::filter(
      !is.na(.data$concept_id),
      !is.na(.data$concept_scheme),
      !is.na(.data$concept_agency)
    )

  if (nrow(refs) == 0) {
    return(
      refs |>
        dplyr::mutate(
          label_tr = character(),
          label_en = character(),
          description_tr = character(),
          description_en = character()
        )
    )
  }

  scheme_refs <- refs |>
    dplyr::distinct(
      .data$concept_agency,
      .data$concept_scheme,
      .data$concept_scheme_version
    )

  cache_keys <- purrr::pmap_chr(
    scheme_refs,
    function(
    concept_agency,
    concept_scheme,
    concept_scheme_version
    ) {
      .tuik_metadata_cache_key(
        agency = concept_agency,
        id = concept_scheme,
        version = concept_scheme_version,
        api_key = api_key
      )
    }
  )

  cached <- vapply(
    cache_keys,
    function(key) {
      exists(
        key,
        envir = .tuiksdmx_concept_cache,
        inherits = FALSE
      )
    },
    logical(1)
  )

  if (
    refresh ||
    (length(cached) > 0 && any(!cached))
  ) {
    tuik_preload_concepts(
      profile = profile,
      refresh = refresh,
      api_key = api_key
    )
  }

  concept_metadata <- purrr::pmap_dfr(
    scheme_refs,
    function(
    concept_agency,
    concept_scheme,
    concept_scheme_version
    ) {
      tuik_get_concept_scheme(
        concept_scheme_id = concept_scheme,
        agency = concept_agency,
        version = concept_scheme_version,
        refresh = FALSE,
        api_key = api_key
      )
    }
  )

  refs |>
    dplyr::left_join(
      concept_metadata,
      by = c(
        "concept_agency",
        "concept_scheme",
        "concept_scheme_version",
        "concept_id"
      )
    ) |>
    dplyr::arrange(
      factor(
        .data$component_type,
        levels = c("dimension", "measure", "attribute")
      ),
      .data$component_id
    )
}


#' Search concepts used by a TÜİK Dataflow
#'
#' Searches component IDs, concept IDs, labels and descriptions in the concepts
#' referenced by a flow profile.
#'
#' @param profile A `tuik_flow_profile` object.
#' @param query Search text.
#' @param language One of "both", "tr", or "en".
#' @param limit Maximum number of results. Use NULL for all matches.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A filtered concept tibble.
#'
#' @export
tuik_search_concepts <- function(
    profile,
    query,
    language = c("both", "tr", "en"),
    limit = 50,
    api_key = NULL
) {
  language <- match.arg(language)

  if (
    length(query) != 1 ||
    is.na(query) ||
    !nzchar(trimws(query))
  ) {
    stop(
      "`query` must be a non-empty character string.",
      call. = FALSE
    )
  }

  concepts <- tuik_concepts(profile,  api_key = api_key)
  query_norm <- tuik_normalize_text(query)

  searchable <- concepts |>
    dplyr::mutate(
      search_component = tuik_normalize_text(.data$component_id),
      search_concept = tuik_normalize_text(.data$concept_id),
      search_tr = tuik_normalize_text(
        paste(.data$label_tr, .data$description_tr)
      ),
      search_en = tuik_normalize_text(
        paste(.data$label_en, .data$description_en)
      )
    )

  matched <- if (language == "tr") {
    grepl(query_norm, searchable$search_component, fixed = TRUE) |
      grepl(query_norm, searchable$search_concept, fixed = TRUE) |
      grepl(query_norm, searchable$search_tr, fixed = TRUE)
  } else if (language == "en") {
    grepl(query_norm, searchable$search_component, fixed = TRUE) |
      grepl(query_norm, searchable$search_concept, fixed = TRUE) |
      grepl(query_norm, searchable$search_en, fixed = TRUE)
  } else {
    grepl(query_norm, searchable$search_component, fixed = TRUE) |
      grepl(query_norm, searchable$search_concept, fixed = TRUE) |
      grepl(query_norm, searchable$search_tr, fixed = TRUE) |
      grepl(query_norm, searchable$search_en, fixed = TRUE)
  }

  result <- concepts[matched, , drop = FALSE]

  if (!is.null(limit)) {
    result <- dplyr::slice_head(result, n = limit)
  }

  result
}


#' Clear cached ConceptSchemes
#'
#' @return NULL invisibly.
#'
#' @keywords internal
tuik_clear_concept_cache <- function() {
  rm(
    list = ls(
      envir = .tuiksdmx_concept_cache,
      all.names = TRUE
    ),
    envir = .tuiksdmx_concept_cache
  )

  invisible(NULL)
}
