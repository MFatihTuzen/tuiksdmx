
test_that("SDMX key follows DSD dimension order", {

  profile <- structure(
    list(
      dimensions = tibble::tibble(
        position = c(1L, 2L, 3L, 4L),
        dimension = c(
          "FREQ", "REF_AREA", "INDICATOR", "TIME_PERIOD"
        ),
        type = c(
          "Dimension", "Dimension",
          "Dimension", "TimeDimension"
        )
      )
    ),
    class = "tuik_flow_profile"
  )

  filters <- list(
    INDICATOR = "CPI",
    REF_AREA = "TR",
    FREQ = "M"
  )

  result <- tuik_build_key(
    profile = profile,
    filters = filters
  )

  expect_identical(
    result,
    "M.TR.CPI"
  )
})

test_that("SDMX key supports multiple dimension values", {

  profile <- structure(
    list(
      dimensions = tibble::tibble(
        position = c(1L, 2L, 3L),
        dimension = c("FREQ", "REF_AREA", "INDICATOR"),
        type = rep("Dimension", 3)
      )
    ),
    class = "tuik_flow_profile"
  )

  filters <- list(
    FREQ = "M",
    REF_AREA = "TR",
    INDICATOR = c("CPI", "PPI")
  )

  result <- tuik_build_key(profile, filters)

  expect_identical(result, "M.TR.CPI+PPI")
})

test_that("SDMX key rejects missing series dimensions", {

  profile <- structure(
    list(
      dimensions = tibble::tibble(
        position = 1:3,
        dimension = c("FREQ", "REF_AREA", "INDICATOR"),
        type = rep("Dimension", 3)
      )
    ),
    class = "tuik_flow_profile"
  )

  filters <- list(
    FREQ = "M",
    INDICATOR = "CPI"
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    regexp = "Missing filter dimension\\(s\\): REF_AREA"
  )
})

test_that("SDMX key rejects unknown filter dimensions", {

  profile <- structure(
    list(
      dimensions = tibble::tibble(
        position = 1:3,
        dimension = c("FREQ", "REF_AREA", "INDICATOR"),
        type = rep("Dimension", 3)
      )
    ),
    class = "tuik_flow_profile"
  )

  filters <- list(
    FREQ = "M",
    REF_AREA = "TR",
    INDICATOR = "CPI",
    REGION = "ANKARA"
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    regexp = "Unknown filter dimension\\(s\\): REGION"
  )
})

test_that("tuik_build_key respects DSD dimension order", {

  profile <- test_freq_profile()

  profile$dimensions <- tibble::tibble(
    position = c(1L, 2L, 3L),
    dimension = c("FREQ", "REF_AREA", "INDICATOR"),
    type = c("Dimension", "Dimension", "Dimension"),
    codelist_agency = c("TR", "TR", "TR"),
    codelist_id = c("CL_FREQ", "CL_AREA", "CL_INDICATOR"),
    codelist_version = c("1.0", "1.0", "1.0")
  )

  # Filtreler DSD sırasından farklı giriliyor.
  filters <- list(
    INDICATOR = "EMP",
    FREQ = "M",
    REF_AREA = "TR"
  )

  result <- tuik_build_key(
    profile = profile,
    filters = filters
  )

  expect_identical(
    result,
    "M.TR.EMP"
  )
})

test_that("tuik_build_key cleans and combines multiple values", {

  profile <- test_freq_profile()

  filters <- list(
    FREQ = c(" M ", "Q", "M", NA_character_, "")
  )

  result <- tuik_build_key(
    profile = profile,
    filters = filters
  )

  expect_identical(
    result,
    "M+Q"
  )
})

test_that("tuik_build_key rejects empty dimension values", {

  profile <- test_freq_profile()

  filters <- list(
    FREQ = c(NA_character_, "", "   ")
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    "No valid value supplied for dimension: FREQ",
    fixed = TRUE
  )
})

test_that("tuik_build_key rejects unknown filter dimensions", {

  profile <- test_freq_profile()

  filters <- list(
    FREQ = "M",
    REGION = "TR"
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    paste0(
      "Unknown filter dimension(s): REGION",
      "\nValid key dimensions are: FREQ"
    ),
    fixed = TRUE
  )
})

test_that("tuik_build_key rejects missing filter dimensions", {

  profile <- test_freq_profile()

  profile$dimensions <- tibble::tibble(
    position = c(1L, 2L),
    dimension = c("FREQ", "REF_AREA"),
    type = c("Dimension", "Dimension"),
    codelist_agency = c("TR", "TR"),
    codelist_id = c("CL_FREQ", "CL_AREA"),
    codelist_version = c("1.0", "1.0")
  )

  # REF_AREA filtresi kasıtlı olarak verilmedi.
  filters <- list(
    FREQ = "M"
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    paste0(
      "Missing filter dimension(s): REF_AREA",
      "\nAll series dimensions must currently be supplied explicitly."
    ),
    fixed = TRUE
  )
})

test_that("tuik_build_key requires filters to be a list", {

  profile <- test_freq_profile()

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = "M"
    ),
    "`filters` must be a named list.",
    fixed = TRUE
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = NULL
    ),
    "`filters` must be a named list.",
    fixed = TRUE
  )
})

test_that("tuik_build_key rejects unnamed filter lists", {

  profile <- test_freq_profile()

  # Liste elemanlarının isimleri yok.
  expect_error(
    tuik_build_key(
      profile = profile,
      filters = list("M")
    ),
    "`filters` must be a named list.",
    fixed = TRUE
  )

  # Bir liste elemanının ismi boş.
  filters <- stats::setNames(
    list("M", "Q"),
    c("FREQ", "")
  )

  expect_error(
    tuik_build_key(
      profile = profile,
      filters = filters
    ),
    "`filters` must be a named list.",
    fixed = TRUE
  )
})
