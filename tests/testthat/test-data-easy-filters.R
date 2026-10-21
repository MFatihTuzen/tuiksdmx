# Regression tests for the compact, explicitly authorized partial-filter path.

testthat::test_that("bounded partial filters accept allow_all and apply locally", {
  profile <- structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = 1:3,
      dimension = c("REF_AREA", "FREQ", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = list(), ...) {
      testthat::expect_identical(path, "data/DF_TEST/all")
      testthat::expect_identical(query, list(startPeriod = "2026-01"))
      "MOCK_RESPONSE"
    },
    .tuik_parse_sdmx_csv_response = function(...) {
      tibble::tibble(REF_AREA = c("TR", "DE", "TR"),
                     FREQ = c("M", "M", "A"),
                     TIME_PERIOD = c("2026-01", "2026-01", "2026"),
                     OBS_VALUE = c(1, 2, 3))
    }
  )
  result <- tuik_get_data(
    profile, filters = list(REF_AREA = "TR"),
    start_period = "2026-01", allow_all = TRUE, validate = FALSE
  )
  testthat::expect_identical(result$OBS_VALUE, c(1, 3))
})

testthat::test_that("partial filters require an explicit opt-in", {
  profile <- structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = 1:3,
      dimension = c("REF_AREA", "FREQ", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")
  testthat::local_mocked_bindings(
    tuik_perform = function(...) stop("Unexpected network request")
  )
  testthat::expect_error(
    tuik_get_data(profile, filters = list(REF_AREA = "TR"),
                  start_period = "2026-01", validate = FALSE),
    "allow_all"
  )
  testthat::expect_error(
    tuik_get_data(profile, filters = list(REF_AREA = "TR"),
                  allow_all = TRUE, validate = FALSE),
    "allow_unbounded"
  )
  testthat::expect_error(
    tuik_get_data(profile, filters = list(REF_AREA = "TR"),
                  start_period = "2026-01", allow_all = NA, validate = FALSE),
    "allow_all"
  )
})
