
test_that("tuik_get_data forwards api_key to validation and retrieval", {

  profile <- structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = c(1L, 2L),
      dimension = c("FREQ", "TIME_PERIOD"),
      type = c("Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")

  validation_calls <- 0L
  request_calls <- 0L

  expected <- tibble::tibble(
    FREQ = "M",
    TIME_PERIOD = "2026-01",
    OBS_VALUE = 123.4
  )

  testthat::local_mocked_bindings(
    tuik_validate_filters = function(
    profile, filters, stop_on_error = TRUE,
    api_key = NULL, ...
    ) {
      validation_calls <<- validation_calls + 1L
      expect_identical(api_key, "TEST_KEY_A")
      expect_true(stop_on_error)
      invisible(NULL)
    },

    tuik_build_key = function(profile, filters, ...) {
      expect_identical(filters, list(FREQ = "M"))
      "M"
    },

    tuik_perform = function(
    path, query = NULL, accept = NULL,
    api_key = NULL, ...
    ) {
      request_calls <<- request_calls + 1L
      expect_identical(api_key, "TEST_KEY_A")
      expect_identical(path, "data/DF_TEST/M")
      expect_identical(
        query,
        list(
          startPeriod = "2026-01",
          endPeriod = "2026-02"
        )
      )
      expect_identical(
        accept,
        "application/vnd.sdmx.data+csv;version=2.0"
      )
      "MOCK_RESPONSE"
    },

    .tuik_parse_sdmx_csv_response = function(
    resp, parse_observation = TRUE, ...
    ) {
      expect_identical(resp, "MOCK_RESPONSE")
      expect_true(parse_observation)
      expected
    }
  )

  result <- tuik_get_data(
    profile = profile,
    filters = list(FREQ = "M"),
    start_period = "2026-01",
    end_period = "2026-02",
    api_key = "TEST_KEY_A"
  )

  expect_identical(validation_calls, 1L)
  expect_identical(request_calls, 1L)
  expect_identical(result, expected)
})


test_that("tuik_get_data skips validation when requested", {

  profile <- structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = c(1L, 2L),
      dimension = c("FREQ", "TIME_PERIOD"),
      type = c("Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")

  testthat::local_mocked_bindings(
    tuik_validate_filters = function(...) {
      stop("Validation must not be called.")
    },

    tuik_build_key = function(...) "M",

    tuik_perform = function(path, api_key = NULL, ...) {
      expect_identical(api_key, "TEST_KEY_B")
      "MOCK_RESPONSE"
    },

    .tuik_parse_sdmx_csv_response = function(...) {
      tibble::tibble(OBS_VALUE = "12")
    }
  )

  result <- tuik_get_data(
    profile = profile,
    filters = list(FREQ = "M"),
    validate = FALSE,
    parse_observation = FALSE,
    api_key = "TEST_KEY_B"
  )

  expect_identical(result$OBS_VALUE, "12")
})
