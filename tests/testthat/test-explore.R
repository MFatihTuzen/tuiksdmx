testthat::test_that("explore lists series and time without downloading data", {
  p <- structure(list(
    dimensions = tibble::tibble(
      position = c(1L, 2L, NA_integer_),
      dimension = c("REF_AREA", "FREQ", "TIME_PERIOD"),
      type = c("Dimension", "Dimension", "TimeDimension"),
      codelist_id = c("CL_GEO", "CL_FREQ", NA_character_)
    ),
    measures = tibble::tibble(measure = "OBS_VALUE"),
    attributes = tibble::tibble(attribute = "OBS_STATUS")
  ), class = "tuik_flow_profile")
  out <- tuik_explore(p)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_identical(out$selection[out$name == "TIME_PERIOD"],
                             "start_period / end_period")
  testthat::expect_identical(out$selection[out$name == "FREQ"], "filters")
  testthat::expect_true(all(c("OBS_VALUE", "OBS_STATUS") %in% out$name))
  testthat::expect_error(tuik_explore(p, dimension = "TIME_PERIOD"),
                         "start_period")
  testthat::expect_error(tuik_explore(p, search = "monthly"), "requires")
})

testthat::test_that("explore passes through Codelist lookup without observations", {
  p <- structure(list(
    dimensions = tibble::tibble(position = c(1L, NA_integer_),
      dimension = c("FREQ", "TIME_PERIOD"),
      type = c("Dimension", "TimeDimension"),
      codelist_id = c("CL_FREQ", NA_character_))
  ), class = "tuik_flow_profile")
  testthat::local_mocked_bindings(
    tuik_dimension_codes = function(dimensions, dimension, search = NULL,
                                    language = "both", limit = 50,
                                    api_key = NULL) {
      testthat::expect_identical(dimensions, p)
      testthat::expect_identical(dimension, "FREQ")
      testthat::expect_identical(search, "month")
      testthat::expect_identical(api_key, "TEST_KEY_A")
      tibble::tibble(code = "M", label_tr = "Aylık", label_en = "Monthly")
    }
  )
  out <- tuik_explore(p, dimension = "FREQ", search = "month",
                      api_key = "TEST_KEY_A")
  testthat::expect_identical(out$code, "M")
})
