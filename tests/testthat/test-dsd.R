test_that("key dimensions exclude time and follow DSD order", {

  profile <- structure(
    list(
      dimensions = tibble::tibble(
        position = c(3L, 4L, 1L, 2L),
        dimension = c(
          "INDICATOR", "TIME_PERIOD", "FREQ", "REF_AREA"
        ),
        type = c(
          "Dimension", "TimeDimension",
          "Dimension", "Dimension"
        )
      )
    ),
    class = "tuik_flow_profile"
  )

  result <- tuik_key_dimensions(profile)

  expect_identical(
    result$dimension,
    c("FREQ", "REF_AREA", "INDICATOR")
  )

  expect_identical(
    result$position,
    c(1L, 2L, 3L)
  )

  expect_false("TIME_PERIOD" %in% result$dimension)
})

test_that("missing dimension information is rejected", {

  profile <- structure(
    list(),
    class = "tuik_flow_profile"
  )

  expect_error(
    tuik_key_dimensions(profile),
    regexp = "does not contain dimension information",
    fixed = TRUE
  )
})

test_that("DSD dimensions are parsed from XML", {

  dsd <- test_dsd_xml()

  result <- tuik_dimensions(dsd)

  expect_identical(
    result$dimension,
    c("FREQ", "REF_AREA", "TIME_PERIOD")
  )

  expect_identical(
    result$position,
    c(1L, 2L, 3L)
  )

  expect_identical(
    result$type,
    c("Dimension", "Dimension", "TimeDimension")
  )

  expect_identical(
    result$codelist_id[2],
    "CL_AREA"
  )

  expect_identical(
    result$codelist_version[2],
    "1.0"
  )
})

test_that("DSD without DataStructure is rejected", {

  empty_dsd <- xml2::read_xml('
    <Structure>
      <Structures/>
    </Structure>
  ')

  expect_error(
    tuik_dimensions(empty_dsd),
    regexp = "No DataStructure element was found",
    fixed = TRUE
  )
})

test_that("missing DSD metadata is represented as NA", {

  dsd <- test_dsd_xml()

  result <- tuik_dimensions(dsd)

  # FREQ has a concept reference but no codelist.
  freq <- result[result$dimension == "FREQ", ]

  expect_identical(freq$concept_id, "FREQ")
  expect_identical(freq$concept_agency, "TR")
  expect_true(is.na(freq$codelist_id))

  # REF_AREA has a codelist but no concept reference.
  area <- result[result$dimension == "REF_AREA", ]

  expect_true(is.na(area$concept_id))
  expect_identical(area$codelist_id, "CL_AREA")

  # TIME_PERIOD has neither reference.
  time <- result[result$dimension == "TIME_PERIOD", ]

  expect_true(is.na(time$concept_id))
  expect_true(is.na(time$codelist_id))
})

test_that("DSD measures are parsed from XML", {

  dsd <- test_measure_xml()

  result <- tuik_measures(dsd)

  expect_equal(nrow(result), 1L)

  expect_identical(
    result$measure,
    "OBS_VALUE"
  )

  expect_identical(
    result$type,
    "PrimaryMeasure"
  )

  expect_identical(
    result$concept_id,
    "OBS_VALUE"
  )

  expect_identical(
    result$concept_scheme,
    "CS_TEST"
  )

  expect_identical(
    result$concept_scheme_version,
    "1.0"
  )

  expect_identical(
    result$concept_agency,
    "TR"
  )
})

test_that("measures without concept references retain their information", {

  dsd <- xml2::read_xml('
    <Structure>
      <DataStructure id="DSD_TEST">
        <DataStructureComponents>
          <MeasureList>
            <PrimaryMeasure id="OBS_VALUE"/>
          </MeasureList>
        </DataStructureComponents>
      </DataStructure>
    </Structure>
  ')

  result <- tuik_measures(dsd)

  expect_equal(nrow(result), 1L)

  expect_identical(result$measure, "OBS_VALUE")
  expect_identical(result$type, "PrimaryMeasure")

  expect_true(all(is.na(
    result[, c(
      "concept_id",
      "concept_scheme",
      "concept_scheme_version",
      "concept_agency"
    )]
  )))
})

test_that("measures require a DataStructure element", {

  dsd <- xml2::read_xml('
    <Structure>
      <Structures/>
    </Structure>
  ')

  expect_error(
    tuik_measures(dsd),
    regexp = "No DataStructure element was found",
    fixed = TRUE
  )
})

test_that("DSD attributes are parsed from XML", {

  dsd <- test_attribute_xml()

  result <- tuik_attributes(dsd)

  expect_equal(nrow(result), 1L)

  expect_identical(result$attribute, "OBS_STATUS")
  expect_identical(result$assignment_status, "Conditional")

  expect_identical(result$concept_id, "OBS_STATUS")
  expect_identical(result$concept_scheme, "CS_TEST")
  expect_identical(result$concept_scheme_version, "1.0")
  expect_identical(result$concept_agency, "TR")

  expect_identical(result$codelist_agency, "TR")
  expect_identical(result$codelist_id, "CL_OBS_STATUS")
  expect_identical(result$codelist_version, "1.0")

  expect_identical(result$relationship_type, "PrimaryMeasure")
  expect_identical(result$relationship_target, "OBS_VALUE")
})

test_that("attributes without metadata references retain their information", {

  dsd <- xml2::read_xml('
    <Structure>
      <DataStructure id="DSD_TEST">
        <DataStructureComponents>
          <AttributeList>
            <Attribute
              id="OBS_STATUS"
              assignmentStatus="Conditional"
            />
          </AttributeList>
        </DataStructureComponents>
      </DataStructure>
    </Structure>
  ')

  result <- tuik_attributes(dsd)

  expect_equal(nrow(result), 1L)

  expect_identical(result$attribute, "OBS_STATUS")
  expect_identical(result$assignment_status, "Conditional")

  missing_fields <- c(
    "concept_id",
    "concept_scheme",
    "concept_scheme_version",
    "concept_agency",
    "codelist_agency",
    "codelist_id",
    "codelist_version",
    "relationship_type",
    "relationship_target"
  )

  expect_true(all(is.na(result[, missing_fields])))
})

test_that("attributes require a DataStructure element", {

  dsd <- xml2::read_xml('
    <Structure>
      <Structures/>
    </Structure>
  ')

  expect_error(
    tuik_attributes(dsd),
    regexp = "No DataStructure element was found",
    fixed = TRUE
  )
})
