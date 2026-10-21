# Updated prior key-selection tests for safe availability policy.

availability_key_profile <- function() {
  structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = 1:4,
      dimension = c("REF_AREA", "FREQ", "CATEGORY", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")
}

testthat::test_that("availability requires explicit consent for all", {
  p <- availability_key_profile()
  f <- list(REF_AREA = "TR", FREQ = "M", CATEGORY = "01")
  testthat::expect_error(
    .tuik_availability_request_key(p, f, "all"), "allow_all"
  )
  testthat::expect_identical(
    .tuik_availability_request_key(p, f, "all", allow_all = TRUE), "all"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, NULL, "auto"), "allow_all"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, list(REF_AREA = "TR"), "auto"),
    "allow_all"
  )
})

testthat::test_that("availability auto mode builds complete key", {
  p <- availability_key_profile()
  f <- list(REF_AREA = "TR", FREQ = "M", CATEGORY = c("01", "03"))
  testthat::expect_identical(
    .tuik_availability_request_key(p, f, "auto"), "TR.M.01+03"
  )
})

testthat::test_that("availability rejects unknown dimensions in auto mode", {
  p <- availability_key_profile()
  testthat::expect_error(
    .tuik_availability_request_key(p, list(REF_AREA = "TR", OTHER = "X"),
                                   "auto"),
    "Unknown filter dimension"
  )
})
