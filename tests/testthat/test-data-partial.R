# Offline tests for explicit local partial filtering and optional period bounds.
# Place at tests/testthat/test-data-partial.R.

partial_profile <- function() {
  structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = 1:4,
      dimension = c("REF_AREA", "FREQ", "CATEGORY", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")
}

partial_parse <- function(resp, parse_observation = TRUE) {
  tibble::tibble(
    REF_AREA = c("TR", "TR", "DE", "TR", "TR"),
    FREQ = c("M", "M", "M", "A", "M"),
    CATEGORY = c("01", "02", "01", "01", "03"),
    TIME_PERIOD = c("2026-01", "2026-01", "2026-01", "2026", "2026-01"),
    OBS_VALUE = c(10, 20, 30, 40, 50)
  )
}

partial_request <- function(path, query = list(), accept = NULL,
                            api_key = NULL, ...) {
  testthat::expect_identical(path, "data/DF_TEST/all")
  testthat::expect_identical(query$startPeriod, "2026-01")
  testthat::expect_identical(query$endPeriod, "2026-01")
  structure(list(), class = "mock_response")
}

testthat::test_that("local filtering requires explicit mode and unbounded opt-in", {
  p <- partial_profile()
  f <- list(REF_AREA = "TR")
  testthat::expect_error(
    tuik_get_data(p, filters = f, start_period = "2026-01",
                  end_period = "2026-01", validate = FALSE),
    "filter_mode"
  )
  testthat::expect_error(
    tuik_get_data(p, filters = f,
                  filter_mode = "local", validate = FALSE),
    "allow_unbounded"
  )
  testthat::expect_error(
    tuik_get_data(p, filters = list(NOT_DIM = "TR"),
                  start_period = "2026-01", end_period = "2026-01",
                  filter_mode = "local", validate = FALSE),
    "Unknown filter"
  )
})

testthat::test_that("local filters use all, then select requested codes", {
  testthat::local_mocked_bindings(
    tuik_perform = partial_request,
    .tuik_parse_sdmx_csv_response = partial_parse
  )
  out <- tuik_get_data(
    partial_profile(),
    filters = list(REF_AREA = "TR", CATEGORY = c("01", "03")),
    start_period = "2026-01", end_period = "2026-01",
    filter_mode = "local", validate = FALSE
  )
  testthat::expect_identical(out$CATEGORY, c("01", "01", "03"))
  testthat::expect_identical(out$OBS_VALUE, c(10, 40, 50))
  testthat::expect_s3_class(out, "tbl_df")
})

testthat::test_that("local filtering can produce zero rows without dropping columns", {
  testthat::local_mocked_bindings(
    tuik_perform = partial_request,
    .tuik_parse_sdmx_csv_response = partial_parse
  )
  out <- tuik_get_data(
    partial_profile(), filters = list(CATEGORY = "99"),
    start_period = "2026-01", end_period = "2026-01",
    filter_mode = "local", validate = FALSE
  )
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_true("OBS_VALUE" %in% names(out))
})

testthat::test_that("local filtering happens before optional decoding", {
  received <- NULL
  decode_mock <- function(data, profile, dimensions = NULL,
                          language = "tr", api_key = NULL) {
    received <<- nrow(data)
    data$CATEGORY_label_tr <- rep("Category", nrow(data))
    data
  }
  testthat::local_mocked_bindings(
    tuik_perform = partial_request,
    .tuik_parse_sdmx_csv_response = partial_parse,
    tuik_decode_data = decode_mock
  )
  out <- tuik_get_data(
    partial_profile(), filters = list(REF_AREA = "TR", FREQ = "M"),
    start_period = "2026-01", end_period = "2026-01",
    filter_mode = "local", validate = FALSE,
    labels = "tr", label_dimensions = "CATEGORY"
  )
  testthat::expect_identical(received, 3L)
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true("CATEGORY_label_tr" %in% names(out))
})


testthat::test_that("local filtering permits a start-only period", {
  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = list(), ...) {
      testthat::expect_identical(path, "data/DF_TEST/all")
      testthat::expect_identical(query, list(startPeriod = "2026-01"))
      "MOCK_RESPONSE"
    },
    .tuik_parse_sdmx_csv_response = partial_parse
  )
  out <- tuik_get_data(
    partial_profile(), filters = list(REF_AREA = "TR"),
    start_period = "2026-01", filter_mode = "local", validate = FALSE
  )
  testthat::expect_identical(out$OBS_VALUE, c(10, 20, 40, 50))
})

testthat::test_that("local filtering permits an end-only period", {
  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = list(), ...) {
      testthat::expect_identical(path, "data/DF_TEST/all")
      testthat::expect_identical(query, list(endPeriod = "2026-01"))
      "MOCK_RESPONSE"
    },
    .tuik_parse_sdmx_csv_response = partial_parse
  )
  out <- tuik_get_data(
    partial_profile(), filters = list(CATEGORY = "03"),
    end_period = "2026-01", filter_mode = "local", validate = FALSE
  )
  testthat::expect_identical(out$OBS_VALUE, 50)
})

testthat::test_that("unbounded local filtering requires and honors explicit opt-in", {
  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = list(), ...) {
      testthat::expect_identical(path, "data/DF_TEST/all")
      testthat::expect_identical(query, list())
      "MOCK_RESPONSE"
    },
    .tuik_parse_sdmx_csv_response = partial_parse
  )
  out <- tuik_get_data(
    partial_profile(), filters = list(REF_AREA = "TR"),
    filter_mode = "local", allow_unbounded = TRUE, validate = FALSE
  )
  testthat::expect_identical(out$OBS_VALUE, c(10, 20, 40, 50))
})
