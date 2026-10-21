
test_that("tuik_available_codes forwards api_key through all operations", {

  profile <- list(
    dataflow = tibble::tibble(
      dataflow_id = "DF_TEST"
    ),
    dimensions = tibble::tibble(
      position = c(1L, 2L),
      dimension = c("FREQ", "REF_AREA"),
      type = c("Dimension", "Dimension"),
      codelist_id = c("CL_FREQ", "CL_AREA"),
      codelist_agency = c("TR", "TR"),
      codelist_version = c("1.0", "1.0")
    )
  )

  validation_calls <- 0L
  probe_calls <- 0L
  codelist_calls <- 0L

  observed <- tibble::tibble(
    FREQ = c("M", "M", "M"),
    REF_AREA = c("TR", "TR", "US"),
    OBS_VALUE = c("100", "101", "99")
  )

  testthat::local_mocked_bindings(
    tuik_validate_filters = function(
    profile, filters, stop_on_error = TRUE,
    api_key = NULL, ...
    ) {
      validation_calls <<- validation_calls + 1L
      expect_identical(api_key, "TEST_KEY_A")
      expect_identical(filters, list(FREQ = "M"))
      expect_true(stop_on_error)
      invisible(NULL)
    },

    tuik_probe = function(
    path, query = NULL, accept = NULL,
    api_key = NULL, ...
    ) {
      probe_calls <<- probe_calls + 1L
      expect_identical(api_key, "TEST_KEY_A")
      expect_identical(path, "data/DF_TEST/all")
      expect_identical(
        query,
        list(
          startPeriod = "2026-01",
          endPeriod = "2026-03"
        )
      )
      expect_identical(
        accept,
        "application/vnd.sdmx.data+csv;version=2.0"
      )

      httr2::response(
        status_code = 200L,
        body = charToRaw("mock")
      )
    },

    .tuik_parse_sdmx_csv_response = function(
    resp, parse_observation = TRUE, ...
    ) {
      expect_false(parse_observation)
      observed
    },

    tuik_get_codelist = function(
    codelist_id, agency, version,
    api_key = NULL, ...
    ) {
      codelist_calls <<- codelist_calls + 1L
      expect_identical(api_key, "TEST_KEY_A")
      expect_identical(codelist_id, "CL_AREA")
      expect_identical(agency, "TR")
      expect_identical(version, "1.0")

      tibble::tibble(
        code = c("TR", "US"),
        label_tr = c("Türkiye", "ABD"),
        label_en = c("Türkiye", "United States")
      )
    }
  )

  result <- tuik_available_codes(
    profile = profile,
    dimension = "REF_AREA",
    start_period = "2026-01",
    end_period = "2026-03",
    filters = list(FREQ = "M"),
    labels = TRUE,
    api_key = "TEST_KEY_A",
    allow_all = TRUE
  )

  expect_identical(validation_calls, 1L)
  expect_identical(probe_calls, 1L)
  expect_identical(codelist_calls, 1L)

  expect_equal(sum(result$n_rows), 3L)
  expect_setequal(result$code, c("TR", "US"))
  expect_identical(
    result$n_rows[match("TR", result$code)],
    2L
  )
  expect_identical(
    result$label_en[match("US", result$code)],
    "United States"
  )
})


test_that("tuik_available_codes handles NoRecordsFound", {

  profile <- list(
    dataflow = tibble::tibble(
      dataflow_id = "DF_TEST"
    ),
    dimensions = tibble::tibble(
      dimension = "FREQ"
    )
  )

  testthat::local_mocked_bindings(
    tuik_probe = function(path, api_key = NULL, ...) {
      expect_identical(api_key, "TEST_KEY_B")

      httr2::response(
        status_code = 404L,
        body = charToRaw("NoRecordsFound")
      )
    },

    tuik_get_codelist = function(...) {
      stop("Codelist lookup must not be called.")
    }
  )

  result <- tuik_available_codes(
    profile = profile,
    dimension = "FREQ",
    start_period = "2026-01",
    end_period = "2026-03",
    api_key = "TEST_KEY_B",
    allow_all = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_identical(
    names(result),
    c("code", "n_rows", "label_tr", "label_en")
  )
})
