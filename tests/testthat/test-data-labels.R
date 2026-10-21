# Offline tests for the optional `labels` integration in tuik_get_data().
# Place at tests/testthat/test-data-labels.R.

make_data_labels_profile <- function() {
  structure(
    list(dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
         dimensions = tibble::tibble(dimension = "FREQ")),
    class = "tuik_flow_profile"
  )
}

mock_label_data_request <- function(path, query = list(), accept = NULL,
                                    api_key = NULL, ...) {
  testthat::expect_identical(path, "data/DF_TEST/all")
  testthat::expect_identical(query$startPeriod, "2024")
  structure(list(), class = "mock_response")
}

mock_label_data_parse <- function(resp, parse_observation = TRUE) {
  tibble::tibble(FREQ = c("A", "A"),
                 TIME_PERIOD = c("2024", "2024"),
                 OBS_VALUE = c(12.5, 15.5))
}

testthat::test_that("default get_data still returns unchanged raw SDMX data", {
  testthat::local_mocked_bindings(
    tuik_perform = mock_label_data_request,
    .tuik_parse_sdmx_csv_response = mock_label_data_parse,
    tuik_decode_data = function(...) stop("decode must not be called")
  )
  out <- tuik_get_data(make_data_labels_profile(), start_period = "2024")
  testthat::expect_named(out, c("FREQ", "TIME_PERIOD", "OBS_VALUE"))
  testthat::expect_identical(out$OBS_VALUE, c(12.5, 15.5))
})

testthat::test_that("labels option delegates to decoder with language and key", {
  received <- NULL
  fake_decode <- function(data, profile, dimensions = NULL,
                          language = "tr", api_key = NULL) {
    received <<- list(dimensions = dimensions, language = language,
                      api_key = api_key)
    data$FREQ_label_tr <- c("Yıllık", "Yıllık")
    data
  }
  testthat::local_mocked_bindings(
    tuik_perform = mock_label_data_request,
    .tuik_parse_sdmx_csv_response = mock_label_data_parse,
    tuik_decode_data = fake_decode
  )
  out <- tuik_get_data(
    make_data_labels_profile(), start_period = "2024",
    labels = "tr", label_dimensions = "FREQ", api_key = "placeholder-test-key"
  )
  testthat::expect_identical(received, list(
    dimensions = "FREQ", language = "tr", api_key = "placeholder-test-key"
  ))
  testthat::expect_identical(out$OBS_VALUE, c(12.5, 15.5))
  testthat::expect_identical(out$FREQ_label_tr, c("Yıllık", "Yıllık"))
})

testthat::test_that("labels parameters reject inconsistent choices", {
  p <- make_data_labels_profile()
  testthat::expect_error(
    tuik_get_data(p, labels = "fr", start_period = "2024"),
    "arg"
  )
  testthat::expect_error(
    tuik_get_data(p, label_dimensions = "FREQ", start_period = "2024"),
    "requires"
  )
})
