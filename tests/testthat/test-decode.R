# Offline tests for tuik_decode_data().
# Place this file at tests/testthat/test-decode.R.

make_decode_fixture <- function() {
  profile <- structure(
    list(dimensions = tibble::tibble(
      dimension = c("FREQ", "SEX", "TIME_PERIOD"),
      codelist_id = c("CL_SIKLIK", "CL_CINSIYET", NA_character_),
      codelist_agency = c("TR", "TR", NA_character_),
      codelist_version = c("1.0", "1.1", NA_character_)
    )),
    class = "tuik_flow_profile"
  )
  data <- tibble::tibble(
    FREQ = c("M", "A", "M", "X"),
    SEX = c("1", "2", "1", "_T"),
    TIME_PERIOD = c("2026-01", "2026", "2026-02", "2026"),
    OBS_VALUE = c(10.5, 20.0, NA_real_, 100)
  )
  list(profile = profile, data = data)
}

mock_decode_codes <- function(profile, dimension, limit = NULL, api_key = NULL) {
  switch(
    dimension,
    FREQ = tibble::tibble(
      code = c("M", "A"),
      label_tr = c("Aylık", "Yıllık"),
      label_en = c("Monthly", "Annual")
    ),
    SEX = tibble::tibble(
      code = c("1", "2", "_T"),
      label_tr = c("Erkek", "Kadın", "Toplam"),
      label_en = c("Male", "Female", "Total")
    ),
    stop("Unexpected dimension in mock: ", dimension)
  )
}

testthat::test_that("decode preserves observations and adds Turkish labels", {
  fixture <- make_decode_fixture()
  testthat::local_mocked_bindings(tuik_dimension_codes = mock_decode_codes)

  out <- tuik_decode_data(
    fixture$data, fixture$profile, dimensions = "FREQ", language = "tr"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), nrow(fixture$data))
  testthat::expect_identical(out$OBS_VALUE, fixture$data$OBS_VALUE)
  testthat::expect_identical(out$TIME_PERIOD, fixture$data$TIME_PERIOD)
  testthat::expect_identical(out$FREQ, fixture$data$FREQ)
  testthat::expect_identical(
    out$FREQ_label_tr, c("Aylık", "Yıllık", "Aylık", NA_character_)
  )
  testthat::expect_false("FREQ_label_en" %in% names(out))
})

testthat::test_that("decode handles both languages and all coded dimensions", {
  fixture <- make_decode_fixture()
  testthat::local_mocked_bindings(tuik_dimension_codes = mock_decode_codes)

  out <- tuik_decode_data(
    fixture$data, fixture$profile, language = "both"
  )

  testthat::expect_true(all(c(
    "FREQ_label_tr", "FREQ_label_en", "SEX_label_tr", "SEX_label_en"
  ) %in% names(out)))
  testthat::expect_false("TIME_PERIOD_label_tr" %in% names(out))
  testthat::expect_identical(
    out$SEX_label_en, c("Male", "Female", "Male", "Total")
  )
  testthat::expect_equal(nrow(out), nrow(fixture$data))
})

testthat::test_that("decode validates requested dimensions", {
  fixture <- make_decode_fixture()
  testthat::expect_error(
    tuik_decode_data(fixture$data, fixture$profile, dimensions = "TIME_PERIOD"),
    "do not reference a Codelist"
  )
  testthat::expect_error(
    tuik_decode_data(fixture$data, fixture$profile, dimensions = "UNKNOWN"),
    "do not reference a Codelist"
  )
  testthat::expect_error(
    tuik_decode_data(fixture$data, fixture$profile, dimensions = c("FREQ", "FREQ")),
    "unique, non-empty"
  )
  without_sex <- fixture$data[, setdiff(names(fixture$data), "SEX")]
  testthat::expect_error(
    tuik_decode_data(without_sex, fixture$profile, dimensions = "SEX"),
    "missing from"
  )
})

testthat::test_that("decode rejects collisions and non-character SDMX codes", {
  fixture <- make_decode_fixture()
  testthat::local_mocked_bindings(tuik_dimension_codes = mock_decode_codes)

  collision <- fixture$data
  collision$FREQ_label_tr <- "existing"
  testthat::expect_error(
    tuik_decode_data(collision, fixture$profile, dimensions = "FREQ"),
    "already exist"
  )

  wrong_type <- fixture$data
  wrong_type$SEX <- as.integer(c(1, 2, 1, 0))
  testthat::expect_error(
    tuik_decode_data(wrong_type, fixture$profile, dimensions = "SEX"),
    "must be character"
  )
})

testthat::test_that("decode rejects duplicate code identifiers", {
  fixture <- make_decode_fixture()
  duplicated_codes <- function(profile, dimension, limit = NULL, api_key = NULL) {
    tibble::tibble(
      code = c("M", "M"),
      label_tr = c("Aylık", "Tekrar"),
      label_en = c("Monthly", "Duplicate")
    )
  }
  testthat::local_mocked_bindings(tuik_dimension_codes = duplicated_codes)
  testthat::expect_error(
    tuik_decode_data(fixture$data, fixture$profile, dimensions = "FREQ"),
    "Duplicate code IDs"
  )
})
