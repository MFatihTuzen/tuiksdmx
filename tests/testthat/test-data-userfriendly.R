# Offline regression tests for Dataflow-ID convenience usage.

testthat::test_that("Dataflow ID resolves a profile and performs bounded all retrieval", {
  calls <- 0L
  p <- structure(list(dataflow = tibble::tibble(dataflow_id = "DF_TEST")),
                 class = "tuik_flow_profile")
  testthat::local_mocked_bindings(
    tuik_flow_profile = function(dataflow_id, api_key = NULL, ...) {
      calls <<- calls + 1L
      testthat::expect_identical(dataflow_id, "DF_TEST")
      testthat::expect_identical(api_key, "TEST_KEY_A")
      p
    },
    tuik_perform = function(path, query = list(), api_key = NULL, ...) {
      testthat::expect_identical(path, "data/DF_TEST/all")
      testthat::expect_identical(query, list(startPeriod = "2026-01", endPeriod = "2026-01"))
      testthat::expect_identical(api_key, "TEST_KEY_A")
      "MOCK_RESPONSE"
    },
    .tuik_parse_sdmx_csv_response = function(...) {
      tibble::tibble(FREQ = "M", OBS_VALUE = 123)
    }
  )
  result <- tuik_get_data(" DF_TEST ", start_period = "2026-01",
                          end_period = "2026-01", api_key = "TEST_KEY_A")
  testthat::expect_identical(calls, 1L)
  testthat::expect_identical(result$OBS_VALUE, 123)
})

testthat::test_that("Dataflow ID cannot silently request unbounded all data", {
  p <- structure(list(dataflow = tibble::tibble(dataflow_id = "DF_TEST")),
                 class = "tuik_flow_profile")
  testthat::local_mocked_bindings(
    tuik_flow_profile = function(...) p,
    tuik_perform = function(...) stop("Network request must not happen.")
  )
  testthat::expect_error(tuik_get_data("DF_TEST"), "allow_unbounded")
})

testthat::test_that("Invalid Dataflow IDs are rejected before lookup", {
  testthat::local_mocked_bindings(
    tuik_flow_profile = function(...) stop("Lookup should not happen.")
  )
  for (id in list("", "  ", NA_character_, character(), c("a", "b"))) {
    testthat::expect_error(tuik_get_data(id), "profile")
  }
})

testthat::test_that("Existing profile objects skip metadata lookup", {
  p <- structure(list(dataflow = tibble::tibble(dataflow_id = "DF_TEST")),
                 class = "tuik_flow_profile")
  testthat::local_mocked_bindings(
    tuik_flow_profile = function(...) stop("Unexpected profile lookup."),
    tuik_perform = function(...) "MOCK_RESPONSE",
    .tuik_parse_sdmx_csv_response = function(...) tibble::tibble(OBS_VALUE = "5")
  )
  out <- tuik_get_data(p, start_period = "2026-01", end_period = "2026-01")
  testthat::expect_identical(out$OBS_VALUE, "5")
})
