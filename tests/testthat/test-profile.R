test_that("tuik_flow_profile requires exactly one Dataflow", {

  # API çağrısını taklit et: hiç Dataflow bulunamadı
  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) {
      tibble::tibble()
    }
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "Expected exactly one Dataflow record.",
    fixed = TRUE
  )
})

test_that("tuik_flow_profile rejects multiple Dataflow records", {

  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) {
      tibble::tibble(
        dataflow_id = c("DF_A", "DF_B")
      )
    }
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "Expected exactly one Dataflow record.",
    fixed = TRUE
  )
})

test_that("tuik_flow_profile rejects missing DSD identifiers", {

  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) {
      tibble::tibble(
        agency = "TR",
        dataflow_id = "DF_TEST",
        dataflow_version = "1.0",
        dsd_id = NA_character_,
        dsd_agency = "TR",
        dsd_version = "1.0"
      )
    }
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "Dataflow 'DF_TEST' does not contain a complete DSD reference.",
    fixed = TRUE
  )
})

test_that("tuik_flow_profile rejects missing DSD agency or version", {

  # DSD kurum bilgisi eksik
  flow_missing_agency <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "1.0",
    dsd_id = "DSD_TEST",
    dsd_agency = NA_character_,
    dsd_version = "1.0"
  )

  # DSD sürüm bilgisi eksik
  flow_missing_version <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "1.0",
    dsd_id = "DSD_TEST",
    dsd_agency = "TR",
    dsd_version = NA_character_
  )

  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) flow_missing_agency
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "Dataflow 'DF_TEST' does not contain a complete DSD reference.",
    fixed = TRUE
  )

  # Mock fonksiyonunun döndürdüğü kaydı değiştir
  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) flow_missing_version
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "Dataflow 'DF_TEST' does not contain a complete DSD reference.",
    fixed = TRUE
  )
})

test_that("tuik_flow_profile creates a structured profile", {

  flow <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "1.0",
    dsd_id = "DSD_TEST",
    dsd_agency = "TR",
    dsd_version = "1.0"
  )

  dsd <- test_dsd_xml()

  # Gerçek API çağrılarını taklit et
  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) flow,
    tuik_get_dsd = function(...) dsd
  )

  result <- tuik_flow_profile("DF_TEST")

  # Profile sınıfı
  expect_s3_class(result, "tuik_flow_profile")

  # Profile bileşenleri
  expect_named(
    result,
    c(
      "summary",
      "dataflow",
      "dsd",
      "dimensions",
      "measures",
      "attributes"
    )
  )

  # Dataflow ve DSD bilgileri
  expect_identical(result$dataflow, flow)
  expect_identical(result$dsd, dsd)

  # DSD boyutları
  expect_equal(nrow(result$dimensions), 3L)

  expect_identical(
    result$dimensions$dimension,
    c("FREQ", "REF_AREA", "TIME_PERIOD")
  )

  # Özet tablo
  expect_identical(
    result$summary$item,
    c("Dataflow", "DSD", "Dimensions", "Measures", "Attributes")
  )

  expect_identical(
    result$summary$value[1:3],
    c("TR:DF_TEST(1.0)", "TR:DSD_TEST(1.0)", "3")
  )
})

test_that("tuik_flow_profile counts all DSD components correctly", {

  flow <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "1.0",
    dsd_id = "DSD_FULL",
    dsd_agency = "TR",
    dsd_version = "1.0"
  )

  # Dimension, measure ve attribute içeren örnek DSD
  dsd <- xml2::read_xml('
    <Structure xmlns:str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure">
      <str:DataStructure id="DSD_FULL">
        <str:DataStructureComponents>

          <str:DimensionList>
            <str:Dimension id="FREQ" position="1"/>
            <str:TimeDimension id="TIME_PERIOD" position="2"/>
          </str:DimensionList>

          <str:MeasureList>
            <str:PrimaryMeasure id="OBS_VALUE"/>
          </str:MeasureList>

          <str:AttributeList>
            <str:Attribute id="OBS_STATUS" assignmentStatus="Conditional"/>
          </str:AttributeList>

        </str:DataStructureComponents>
      </str:DataStructure>
    </Structure>
  ')

  # Gerçek API çağrılarını taklit et
  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) flow,
    tuik_get_dsd = function(...) dsd
  )

  result <- tuik_flow_profile("DF_TEST")

  expect_identical(
    result$summary$item,
    c("Dataflow", "DSD", "Dimensions", "Measures", "Attributes")
  )

  expect_identical(
    result$summary$value,
    c(
      "TR:DF_TEST(1.0)",
      "TR:DSD_FULL(1.0)",
      "2",
      "1",
      "1"
    )
  )

  expect_equal(nrow(result$dimensions), 2L)
  expect_equal(nrow(result$measures), 1L)
  expect_equal(nrow(result$attributes), 1L)
})

test_that("tuik_flow_profile forwards Dataflow and DSD identifiers", {

  flow <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "2.0",
    dsd_id = "DSD_TEST",
    dsd_agency = "TR",
    dsd_version = "1.5"
  )

  dsd <- test_dsd_xml()

  testthat::local_mocked_bindings(

    tuik_get_dataflow = function(
    dataflow_id,
    agency,
    version,
    api_key = NULL
    ) {

      expect_identical(dataflow_id, "DF_TEST")
      expect_identical(agency, "TR")
      expect_identical(version, "2.0")
      expect_identical(api_key, "TEST_API_KEY")

      flow
    },

    tuik_get_dsd = function(
    dsd_id,
    agency,
    version,
    api_key = NULL
    ) {

      expect_identical(dsd_id, "DSD_TEST")
      expect_identical(agency, "TR")
      expect_identical(version, "1.5")
      expect_identical(api_key, "TEST_API_KEY")

      dsd
    }
  )

  result <- tuik_flow_profile(
    dataflow_id = "DF_TEST",
    agency = "TR",
    version = "2.0",
    api_key = "TEST_API_KEY"
  )

  expect_s3_class(result, "tuik_flow_profile")
})

test_that("tuik_flow_profile propagates DSD retrieval errors", {

  flow <- tibble::tibble(
    agency = "TR",
    dataflow_id = "DF_TEST",
    dataflow_version = "1.0",
    dsd_id = "DSD_TEST",
    dsd_agency = "TR",
    dsd_version = "1.0"
  )

  testthat::local_mocked_bindings(
    tuik_get_dataflow = function(...) flow,

    tuik_get_dsd = function(...) {
      stop("MOCK_DSD_FAILURE", call. = FALSE)
    }
  )

  expect_error(
    tuik_flow_profile("DF_TEST"),
    "MOCK_DSD_FAILURE",
    fixed = TRUE
  )
})
