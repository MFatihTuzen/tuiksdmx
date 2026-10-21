
test_that("filters must be a named list", {

  expect_error(
    tuik_validate_filters(
      profile = NULL,
      filters = c("M", "TR")
    ),
    regexp = "`filters` must be a named list",
    fixed = TRUE
  )

})

test_that("valid codelist values are accepted", {

  profile <- test_freq_profile()

  testthat::local_mocked_bindings(
    tuik_preload_codelists = function(...) NULL,
    tuik_get_codelist = function(...) test_freq_codelist()
  )

  result <- tuik_validate_filters(
    profile = profile,
    filters = list(FREQ = "M"),
    api_key = "TEST_KEY_A"
  )

  expect_identical(result$dimension, "FREQ")
  expect_identical(result$value, "M")
  expect_identical(result$valid, TRUE)
  expect_identical(result$label_en, "Monthly")
})

test_that("invalid codelist values are rejected", {

  profile <- test_freq_profile()

  testthat::local_mocked_bindings(
    tuik_preload_codelists = function(...) NULL,
    tuik_get_codelist = function(...) test_freq_codelist()
  )

  expect_error(
    tuik_validate_filters(
      profile = profile,
      filters = list(FREQ = "XYZ"),
      api_key = "TEST_KEY_A"
    ),
    regexp = "Invalid SDMX filter value\\(s\\): FREQ=XYZ"
  )
})

test_that("invalid values can be returned without stopping", {

  profile <- test_freq_profile()

  testthat::local_mocked_bindings(
    tuik_preload_codelists = function(...) NULL,
    tuik_get_codelist = function(...) test_freq_codelist()
  )

  result <- tuik_validate_filters(
    profile = profile,
    filters = list(FREQ = c("M", "XYZ")),
    stop_on_error = FALSE,
    api_key = "TEST_KEY_A"
  )

  expect_identical(result$value, c("M", "XYZ"))
  expect_identical(result$valid, c(TRUE, FALSE))
  expect_identical(result$label_en, c("Monthly", NA_character_))
})
