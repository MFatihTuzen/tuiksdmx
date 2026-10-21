# ============================================================================
# profile.R
#
# High-level profile helpers for TÜİK SDMX Dataflows.
#
# Main function:
#   - tuik_flow_profile()
#
# A flow profile combines:
#   - Dataflow metadata
#   - Referenced DSD
#   - Dimensions
#   - Measures
#   - Attributes
#
# This provides a convenient object that can later be used for building
# SDMX keys and retrieving filtered data.
# ============================================================================


#' Build a profile for a TÜİK SDMX Dataflow
#'
#' Retrieves a Dataflow, follows its referenced DSD and parses its
#' structural components.
#'
#' @param dataflow_id Dataflow identifier.
#' @param agency Dataflow agency. Defaults to "TR".
#' @param version Dataflow version. Defaults to "latest".
#' @param api_key Optional API key. If NULL, the
#'   TUIK_SDMX_API_KEY environment variable is used.
#'
#' @return A list with the following components:
#'   - dataflow
#'   - dsd
#'   - dimensions
#'   - measures
#'   - attributes
#'
#' @examples
#' \dontrun{
#' profile <- tuik_flow_profile(
#'   "DF_TUFE_SDMX_TT01",
#'   version = "1.0"
#' )
#'
#' profile$dimensions
#' }
#'
#' @export
tuik_flow_profile <- function(
    dataflow_id,
    agency = "TR",
    version = "latest",
    api_key = NULL
) {

  # ----------------------------------------------------------
  # Retrieve Dataflow metadata
  # ----------------------------------------------------------

  flow <- tuik_get_dataflow(
    dataflow_id = dataflow_id,
    agency = agency,
    version = version,
    api_key = api_key
  )

  if (
    nrow(flow) != 1
  ) {
    stop(
      "Expected exactly one Dataflow record.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Validate referenced DSD
  # ----------------------------------------------------------

  if (
    is.na(flow$dsd_id) ||
    is.na(flow$dsd_agency) ||
    is.na(flow$dsd_version)
  ) {
    stop(
      paste0(
        "Dataflow '",
        dataflow_id,
        "' does not contain a complete DSD reference."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Retrieve DSD
  # ----------------------------------------------------------

  dsd <- tuik_get_dsd(
    dsd_id = flow$dsd_id,
    agency = flow$dsd_agency,
    version = flow$dsd_version,
    api_key = api_key
  )

  # ----------------------------------------------------------
  # Parse structural components
  # ----------------------------------------------------------

  dimensions <- tuik_dimensions(
    dsd
  )

  measures <- tuik_measures(
    dsd
  )

  attributes <- tuik_attributes(
    dsd
  )

  # ----------------------------------------------------------
  # Create summary
  # ----------------------------------------------------------

  summary <- tibble::tibble(
    item = c(
      "Dataflow",
      "DSD",
      "Dimensions",
      "Measures",
      "Attributes"
    ),
    value = c(
      paste0(
        flow$agency,
        ":",
        flow$dataflow_id,
        "(",
        flow$dataflow_version,
        ")"
      ),
      paste0(
        flow$dsd_agency,
        ":",
        flow$dsd_id,
        "(",
        flow$dsd_version,
        ")"
      ),
      as.character(
        nrow(dimensions)
      ),
      as.character(
        nrow(measures)
      ),
      as.character(
        nrow(attributes)
      )
    )
  )

  # ----------------------------------------------------------
  # Return profile object
  # ----------------------------------------------------------

  result <- list(
    summary = summary,
    dataflow = flow,
    dsd = dsd,
    dimensions = dimensions,
    measures = measures,
    attributes = attributes
  )

  class(result) <- c(
    "tuik_flow_profile",
    class(result)
  )

  result
}
