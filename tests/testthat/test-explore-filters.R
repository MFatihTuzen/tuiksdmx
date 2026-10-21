# Offline tests for the optional selection summary in tuik_explore().
explore_filters_profile <- function() {
  structure(list(
    dimensions = tibble::tibble(
      position = c(1L, 2L, NA_integer_),
      dimension = c("REF_AREA", "FREQ", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "TimeDimension"),
      codelist_id = c("CL_GEO", "CL_FREQ", NA_character_)
    ),
    measures = tibble::tibble(measure = "OBS_VALUE"),
    attributes = tibble::tibble(attribute = "OBS_STATUS")
  ), class = "tuik_flow_profile")
}

testthat::test_that("explore shows partial selections and time separately", {
  p <- explore_filters_profile()
  f <- list(FREQ = c("M", "A"))
  out <- tuik_explore(p, filters = f)
  testthat::expect_identical(out$selected_codes[out$name == "FREQ"], "M, A")
  testthat::expect_identical(out$filter_status[out$name == "FREQ"], "selected")
  testthat::expect_identical(out$filter_status[out$name == "REF_AREA"], "missing")
  testthat::expect_identical(out$filter_status[out$name == "TIME_PERIOD"],
                             "use_period_arguments")
  testthat::expect_identical(out$filter_status[out$name == "OBS_VALUE"],
                             "not_applicable")
  testthat::expect_identical(f, list(FREQ = c("M", "A")))
})

testthat::test_that("explore preserves original output without filters", {
  p <- explore_filters_profile()
  out <- tuik_explore(p)
  testthat::expect_false("filter_status" %in% names(out))
  testthat::expect_identical(out$selection[out$name == "TIME_PERIOD"],
                             "start_period / end_period")
})

testthat::test_that("explore rejects malformed and non-key selections", {
  p <- explore_filters_profile()
  testthat::expect_error(tuik_explore(p, filters = list(TIME_PERIOD = "2026-01")),
                         "Unknown filter")
  testthat::expect_error(tuik_explore(p, filters = list(FREQ = "")),
                         "non-empty")
  testthat::expect_error(tuik_explore(p, filters = list(FREQ = 1)),
                         "character")
  testthat::expect_error(tuik_explore(p, filters = list(FREQ = NA_character_)),
                         "character")
  testthat::expect_error(tuik_explore(p, filters = list(FREQ = "M", FREQ = "A")),
                         "unique")
  testthat::expect_error(tuik_explore(p, filters = list(X = "M")),
                         "Unknown filter")
  testthat::expect_error(tuik_explore(p, dimension = "FREQ", filters = list(FREQ = "M")),
                         "overview")
})
