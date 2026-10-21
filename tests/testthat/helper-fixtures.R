# Create a minimal flow profile for testing frequency codes.
test_freq_profile <- function() {

  structure(
    list(
      dimensions = tibble::tibble(
        position = 1L,
        dimension = "FREQ",
        type = "Dimension",
        codelist_agency = "TR",
        codelist_id = "CL_FREQ",
        codelist_version = "1.0"
      )
    ),
    class = "tuik_flow_profile"
  )
}

# Create a sample frequency codelist for offline tests.
test_freq_codelist <- function() {

  tibble::tibble(
    code = c("M", "Q", "A"),
    label_tr = c("Aylik", "Uc Aylik", "Yillik"),
    label_en = c("Monthly", "Quarterly", "Annual")
  )
}

local_codelist_cache <- function(env = parent.frame()) {

  cache <- get(
    ".tuiksdmx_codelist_cache",
    envir = asNamespace("tuiksdmx")
  )

  old_values <- as.list(
    cache,
    all.names = TRUE
  )

  withr::defer(
    {
      rm(
        list = ls(cache, all.names = TRUE),
        envir = cache
      )

      list2env(
        old_values,
        envir = cache
      )
    },
    envir = env
  )

  rm(
    list = ls(cache, all.names = TRUE),
    envir = cache
  )

  invisible(NULL)
}

# Create a sample Dataflow catalogue for offline search tests.
test_dataflows <- function() {

  tibble::tibble(
    agency = c("TR", "TR", "TR"),
    dataflow_id = c("DF_TUFE", "DF_ISSIZLIK", "DF_SANAYI"),
    dataflow_version = c("1.0", "1.0", "1.0"),
    is_final = c(TRUE, TRUE, FALSE),
    name_tr = c(
      "Tüketici Fiyat Endeksi",
      "İşsizlik Oranı",
      "Sanayi Üretim Endeksi"
    ),
    name_en = c(
      "Consumer Price Index",
      "Unemployment Rate",
      "Industrial Production Index"
    ),
    dsd_agency = c("TR", "TR", "TR"),
    dsd_id = c("DSD_TUFE", "DSD_ISSIZLIK", "DSD_SANAYI"),
    dsd_version = c("1.0", "1.0", "1.0")
  )
}

# Create a minimal SDMX-ML DSD document for offline tests.
test_dsd_xml <- function() {

  xml2::read_xml('
<Structure xmlns="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message"
           xmlns:str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure"
           xmlns:com="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common">
  <str:Structures>
    <str:DataStructures>
      <str:DataStructure id="DSD_TEST">
        <str:DataStructureComponents>
          <str:DimensionList>
            <str:Dimension id="REF_AREA" position="2">
              <str:LocalRepresentation>
                <str:Enumeration>
                  <Ref agencyID="TR" id="CL_AREA" version="1.0"/>
                </str:Enumeration>
              </str:LocalRepresentation>
            </str:Dimension>
            <str:Dimension id="FREQ" position="1">
              <str:ConceptIdentity>
                <Ref id="FREQ" agencyID="TR"/>
              </str:ConceptIdentity>
            </str:Dimension>
            <str:TimeDimension id="TIME_PERIOD" position="3"/>
          </str:DimensionList>
        </str:DataStructureComponents>
      </str:DataStructure>
    </str:DataStructures>
  </str:Structures>
</Structure>
')
}

# Create a minimal SDMX-ML Measure document for offline tests.
test_measure_xml <- function() {

  xml2::read_xml('
<Structure xmlns:str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure"
           xmlns:com="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common">
  <str:DataStructure id="DSD_TEST">
    <str:DataStructureComponents>
      <str:MeasureList>
        <str:PrimaryMeasure id="OBS_VALUE">
          <str:ConceptIdentity>
            <Ref
              id="OBS_VALUE"
              agencyID="TR"
              maintainableParentID="CS_TEST"
              maintainableParentVersion="1.0"
            />
          </str:ConceptIdentity>
        </str:PrimaryMeasure>
      </str:MeasureList>
    </str:DataStructureComponents>
  </str:DataStructure>
</Structure>
')
}

# Create a minimal SDMX-ML Attribute document for offline tests.
test_attribute_xml <- function() {

  xml2::read_xml('
<Structure xmlns:str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure">
  <str:DataStructure id="DSD_TEST">
    <str:DataStructureComponents>
      <str:AttributeList>
        <str:Attribute id="OBS_STATUS" assignmentStatus="Conditional">
          <str:ConceptIdentity>
            <Ref
              id="OBS_STATUS"
              agencyID="TR"
              maintainableParentID="CS_TEST"
              maintainableParentVersion="1.0"
            />
          </str:ConceptIdentity>
          <str:LocalRepresentation>
            <str:Enumeration>
              <Ref agencyID="TR" id="CL_OBS_STATUS" version="1.0"/>
            </str:Enumeration>
          </str:LocalRepresentation>
          <str:AttributeRelationship>
            <str:PrimaryMeasure>
              <Ref id="OBS_VALUE"/>
            </str:PrimaryMeasure>
          </str:AttributeRelationship>
        </str:Attribute>
      </str:AttributeList>
    </str:DataStructureComponents>
  </str:DataStructure>
</Structure>
')
}

# Create a minimal SDMX Codelist XML document for offline tests.
test_codelist_xml <- function() {

  xml2::read_xml('
    <Structure>
      <Codelist id="CL_FREQ" agencyID="TR" version="1.0">

        <Code id="M">
          <Name xml:lang="tr">Aylık</Name>
          <Name xml:lang="en">Monthly</Name>
        </Code>

        <Code id="Q">
          <Name xml:lang="tr">Üç Aylık</Name>
          <Name xml:lang="en">Quarterly</Name>
        </Code>

        <Code id="A">
          <Name xml:lang="tr">Yıllık</Name>
          <Name xml:lang="en">Annual</Name>
        </Code>

      </Codelist>
    </Structure>
  ')
}

# Create a minimal SDMX ConceptScheme XML document for offline tests.
test_concept_scheme_xml <- function() {

  xml2::read_xml('
    <Structure>
      <ConceptScheme id="CS_TEST" agencyID="TR" version="1.0">

        <Concept id="FREQ">
          <Name xml:lang="tr">Frekans</Name>
          <Name xml:lang="en">Frequency</Name>
          <Description xml:lang="tr">Verinin zaman sıklığı</Description>
          <Description xml:lang="en">Frequency of observations</Description>
        </Concept>

        <Concept id="REF_AREA">
          <Name xml:lang="tr">Referans Bölge</Name>
          <Name xml:lang="en">Reference Area</Name>
          <Description xml:lang="tr">Coğrafi referans alanı</Description>
          <Description xml:lang="en">Geographical reference area</Description>
        </Concept>

      </ConceptScheme>
    </Structure>
  ')
}
