# Offline policy tests; no API keys or HTTP calls.

availability_safe_profile <- function() {
  structure(list(
    dataflow = tibble::tibble(dataflow_id = "DF_TEST"),
    dimensions = tibble::tibble(
      position = 1:4,
      dimension = c("REF_AREA", "FREQ", "CATEGORY", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "Dimension", "TimeDimension")
    )
  ), class = "tuik_flow_profile")
}

testthat::test_that("complete filters select a server-side key", {
  p <- availability_safe_profile()
  filters <- list(REF_AREA = "TR", FREQ = "M", CATEGORY = c("01", "03"))
  testthat::expect_identical(
    .tuik_availability_request_key(p, filters, "auto"),
    "TR.M.01+03"
  )
})

testthat::test_that("partial or absent filters require explicit opt-in", {
  p <- availability_safe_profile()
  testthat::expect_error(
    .tuik_availability_request_key(p, NULL, "auto"), "allow_all"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, list(REF_AREA = "TR"), "auto"),
    "allow_all"
  )
  testthat::expect_identical(
    .tuik_availability_request_key(p, NULL, "auto", allow_all = TRUE), "all"
  )
  testthat::expect_identical(
    .tuik_availability_request_key(p, list(REF_AREA = "TR"), "auto",
                                   allow_all = TRUE), "all"
  )
})

testthat::test_that("forced all mode requires opt-in even for full filters", {
  p <- availability_safe_profile()
  filters <- list(REF_AREA = "TR", FREQ = "M", CATEGORY = "01")
  testthat::expect_error(
    .tuik_availability_request_key(p, filters, "all"), "allow_all"
  )
  testthat::expect_identical(
    .tuik_availability_request_key(p, filters, "all", allow_all = TRUE),
    "all"
  )
})

testthat::test_that("unsafe or invalid option values are rejected", {
  p <- availability_safe_profile()
  testthat::expect_error(
    .tuik_availability_request_key(p, NULL, "auto", allow_all = NA),
    "allow_all"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, NULL, "auto", allow_all = 1),
    "allow_all"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, NULL, "automatic"), "query_mode"
  )
  testthat::expect_error(
    .tuik_availability_request_key(p, list(WRONG = "x"), "auto"),
    "Unknown filter dimension"
  )
})
