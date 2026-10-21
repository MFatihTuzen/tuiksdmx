# ============================================================================
# search.R
#
# Search helpers for discovering TÜİK SDMX Dataflows.
#
# Main function:
#   - tuik_search_dataflows()
#
# The goal is to let users discover published statistical datasets without
# knowing Dataflow IDs in advance.
# ============================================================================

#' Search TÜİK SDMX Dataflows
#'
#' Searches the TÜİK SDMX Dataflow catalogue using a keyword or phrase.
#'
#' Search fields:
#'   - Dataflow ID
#'   - Turkish Dataflow name
#'   - English Dataflow name
#'   - Referenced DSD ID
#'
#' Search is case-insensitive and tolerant of Turkish characters.
#'
#' For example:
#'
#'   "tüketici fiyat"
#'   "tuketici fiyat"
#'
#' are treated equivalently.
#'
#' If no exact substring match is found, an optional conservative fallback
#' search is performed using shortened word variants. This may help with
#' expressions such as "işsizlik" when catalogue labels contain "işsiz".
#'
#' @param query Character string containing the search term.
#' @param language Which label language should be searched.
#'   One of "both", "tr", or "en". Defaults to "both".
#' @param final_only Logical. If TRUE, return only final Dataflows.
#' @param limit Maximum number of results. Use NULL for all matches.
#' @param fuzzy Logical. If TRUE, perform fallback matching when the exact
#'   search returns no results.
#' @param refresh Logical. If TRUE, refresh the Dataflow catalogue.
#' @param dataflows Optional Dataflow catalogue supplied by the user.
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used
#'   when retrieving the catalogue.
#'
#' @return A tibble containing matching Dataflows.
#'
#' @examples
#' \dontrun{
#' tuik_search_dataflows("tüketici fiyat")
#'
#' tuik_search_dataflows("tuketici fiyat")
#'
#' tuik_search_dataflows("işsizlik")
#'
#' tuik_search_dataflows(
#'   "consumer price",
#'   language = "en"
#' )
#' }
#'
#' @export
tuik_search_dataflows <- function(
    query,
    language = c("both", "tr", "en"),
    final_only = FALSE,
    limit = 25,
    fuzzy = TRUE,
    refresh = FALSE,
    dataflows = NULL,
    api_key = NULL
) {

  # ----------------------------------------------------------
  # Validate arguments
  # ----------------------------------------------------------

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

  language <- match.arg(
    language
  )

  if (
    !is.null(limit) &&
    (
      length(limit) != 1 ||
      is.na(limit) ||
      limit < 1
    )
  ) {
    stop(
      "`limit` must be NULL or a positive number.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Retrieve catalogue
  # ----------------------------------------------------------

  if (is.null(dataflows)) {

    dataflows <- tuik_dataflows(
      refresh = refresh,
      api_key = api_key
    )
  }

  # ----------------------------------------------------------
  # Normalise searchable fields
  # ----------------------------------------------------------

  query_norm <- tuik_normalize_text(
    query
  )

  search_data <- dataflows |>
    dplyr::mutate(
      search_id = tuik_normalize_text(.data$dataflow_id),
      search_tr = tuik_normalize_text(.data$name_tr),
      search_en = tuik_normalize_text(.data$name_en),
      search_dsd = tuik_normalize_text(.data$dsd_id)
    )

  # ----------------------------------------------------------
  # Internal matching helper
  # ----------------------------------------------------------

  find_matches <- function(
    search_term
  ) {

    if (language == "tr") {

      matched <-
        grepl(
          search_term,
          search_data$search_id,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_tr,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_dsd,
          fixed = TRUE
        )

    } else if (language == "en") {

      matched <-
        grepl(
          search_term,
          search_data$search_id,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_en,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_dsd,
          fixed = TRUE
        )

    } else {

      matched <-
        grepl(
          search_term,
          search_data$search_id,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_tr,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_en,
          fixed = TRUE
        ) |
        grepl(
          search_term,
          search_data$search_dsd,
          fixed = TRUE
        )
    }

    which(
      matched
    )
  }

  # ----------------------------------------------------------
  # Exact normalised search
  # ----------------------------------------------------------

  match_rows <- find_matches(
    query_norm
  )

  match_method <- "exact"

  search_term_used <- query_norm

  # ----------------------------------------------------------
  # Fallback search
  #
  # Only used if the exact search found nothing.
  # ----------------------------------------------------------

  if (
    length(match_rows) == 0 &&
    fuzzy
  ) {

    variants <- tuik_search_variants(
      query_norm
    )

    # Do not retry the original query.
    variants <- setdiff(
      variants,
      query_norm
    )

    for (variant in variants) {

      candidate_rows <- find_matches(
        variant
      )

      if (length(candidate_rows) > 0) {

        match_rows <- candidate_rows

        match_method <- "fallback"

        search_term_used <- variant

        break
      }
    }
  }

  # ----------------------------------------------------------
  # No matches
  # ----------------------------------------------------------

  if (length(match_rows) == 0) {

    return(
      dataflows[0, ] |>
        dplyr::mutate(
          match_method = character(),
          search_term_used = character()
        )
    )
  }

  # ----------------------------------------------------------
  # Build results
  # ----------------------------------------------------------

  result <- search_data[
    match_rows,
    ,
    drop = FALSE
  ]

  if (final_only) {
    result <- result |>
      dplyr::filter(
        .data$is_final %in% TRUE
      )
  }

  # ----------------------------------------------------------
  # Ranking
  # ----------------------------------------------------------

  result <- result |>
    dplyr::mutate(

      match_score =
        dplyr::case_when(

          search_id == search_term_used ~ 1L,

          startsWith(
            search_id,
            search_term_used
          ) ~ 2L,

          grepl(
            search_term_used,
            search_id,
            fixed = TRUE
          ) ~ 3L,

          grepl(
            search_term_used,
            search_tr,
            fixed = TRUE
          ) ~ 4L,

          grepl(
            search_term_used,
            search_en,
            fixed = TRUE
          ) ~ 5L,

          grepl(
            search_term_used,
            search_dsd,
            fixed = TRUE
          ) ~ 6L,

          TRUE ~ 99L
        ),

      match_method =
        match_method,

      search_term_used =
        search_term_used
    ) |>
    dplyr::arrange(
      .data$match_score,
      dplyr::desc(.data$is_final),
      .data$dataflow_id
    )

  # ----------------------------------------------------------
  # Limit results
  # ----------------------------------------------------------

  if (!is.null(limit)) {

    result <- result |>
      dplyr::slice_head(
        n = limit
      )
  }

  # ----------------------------------------------------------
  # Return clean output
  # ----------------------------------------------------------

  result |>
    dplyr::select(
      "agency",
      "dataflow_id",
      "dataflow_version",
      "is_final",
      "name_tr",
      "name_en",
      "dsd_agency",
      "dsd_id",
      "dsd_version",
      "match_method",
      "search_term_used"
    )
}
