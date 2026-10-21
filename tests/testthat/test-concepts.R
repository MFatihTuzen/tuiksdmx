test_concept_cache_key <- function(id, version = "1.0") {
  .tuik_metadata_cache_key(
    agency = "TR",
    id = id,
    version = version,
    api_key = "TEST_KEY_A"
  )
}

test_that("ConceptScheme metadata and multilingual labels are parsed", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- test_concept_scheme_xml()

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  result <- .tuik_parse_concept_scheme_node(node)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)

  expect_named(
    result,
    c(
      "concept_agency",
      "concept_scheme",
      "concept_scheme_version",
      "concept_id",
      "label_tr",
      "label_en",
      "description_tr",
      "description_en"
    )
  )

  expect_identical(result$concept_agency, c("TR", "TR"))
  expect_identical(result$concept_scheme, c("CS_TEST", "CS_TEST"))
  expect_identical(result$concept_scheme_version, c("1.0", "1.0"))

  expect_identical(result$concept_id, c("FREQ", "REF_AREA"))

  expect_identical(
    result$label_tr,
    c("Frekans", "Referans Bölge")
  )

  expect_identical(
    result$label_en,
    c("Frequency", "Reference Area")
  )

  expect_identical(
    result$description_tr,
    c("Verinin zaman sıklığı", "Coğrafi referans alanı")
  )

  expect_identical(
    result$description_en,
    c("Frequency of observations", "Geographical reference area")
  )
})

test_that("missing Concept names and descriptions are returned as NA", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- xml2::read_xml('
    <Structure>
      <ConceptScheme id="CS_TEST" agencyID="TR" version="1.0">

        <Concept id="FREQ">
          <Name xml:lang="tr">Frekans</Name>
          <Description xml:lang="en">Frequency of observations</Description>
        </Concept>

        <Concept id="REF_AREA">
          <Name xml:lang="en">Reference Area</Name>
          <Description xml:lang="tr">Coğrafi referans alanı</Description>
        </Concept>

      </ConceptScheme>
    </Structure>
  ')

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  result <- .tuik_parse_concept_scheme_node(node)

  expect_equal(nrow(result), 2L)

  expect_identical(
    result$concept_id,
    c("FREQ", "REF_AREA")
  )

  expect_identical(
    result$label_tr,
    c("Frekans", NA_character_)
  )

  expect_identical(
    result$label_en,
    c(NA_character_, "Reference Area")
  )

  expect_identical(
    result$description_tr,
    c(NA_character_, "Coğrafi referans alanı")
  )

  expect_identical(
    result$description_en,
    c("Frequency of observations", NA_character_)
  )
})

test_that("empty ConceptSchemes return an empty tibble", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- xml2::read_xml('
    <Structure>
      <ConceptScheme
        id="CS_EMPTY"
        agencyID="TR"
        version="1.0"
      />
    </Structure>
  ')

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  result <- .tuik_parse_concept_scheme_node(node)

  expect_s3_class(result, "tbl_df")

  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 8L)

  expect_named(
    result,
    c(
      "concept_agency",
      "concept_scheme",
      "concept_scheme_version",
      "concept_id",
      "label_tr",
      "label_en",
      "description_tr",
      "description_en"
    )
  )

  expect_true(
    all(vapply(result, is.character, logical(1)))
  )
})

test_that("tuik_get_concept_scheme rejects empty and missing identifiers", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  expect_error(
    tuik_get_concept_scheme("", agency = "TR"),
    "`concept_scheme_id` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_concept_scheme("CS_TEST", agency = ""),
    "`agency` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_concept_scheme(NA_character_, agency = "TR"),
    "`concept_scheme_id` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_concept_scheme("CS_TEST", agency = NA_character_),
    "`agency` must be a non-empty character string.",
    fixed = TRUE
  )
})

test_that("tuik_get_concept_scheme returns cached data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  # Test sonunda önbelleği eski durumuna getir
  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_CACHE_TEST", "1.0")

  had_key <- exists(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  if (had_key) {
    old_value <- get(
      cache_key,
      envir = cache,
      inherits = FALSE
    )
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Örnek ConceptScheme verisi
  expected <- tibble::tibble(
    concept_agency = "TR",
    concept_scheme = "CS_CACHE_TEST",
    concept_scheme_version = "1.0",
    concept_id = "FREQ",
    label_tr = "Frekans",
    label_en = "Frequency",
    description_tr = "Verinin zaman sıklığı",
    description_en = "Frequency of observations"
  )

  # Veriyi önbelleğe yerleştir
  assign(
    cache_key,
    expected,
    envir = cache
  )

  # API'ye gitmeden önbellekten oku
  result <- tuik_get_concept_scheme(
    concept_scheme_id = "CS_CACHE_TEST",
    agency = "TR",
    version = "1.0"
  )

  expect_identical(result, expected)
})

test_that("concept scheme refresh bypasses cached data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_REFRESH_TEST", "1.0")

  # Test sonunda mevcut önbellek durumunu geri yükle
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Önbelleğe eski veri yerleştir
  assign(
    cache_key,
    tibble::tibble(concept_id = "OLD"),
    envir = cache
  )

  # Gerçek API çağrısını taklit et
  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {

      expect_identical(
        path,
        "conceptscheme/TR/CS_REFRESH_TEST/1.0"
      )

      stop("MOCK_API_CALLED", call. = FALSE)
    }
  )

  # refresh = TRUE önbelleği atlamalı
  expect_error(
    tuik_get_concept_scheme(
      concept_scheme_id = "CS_REFRESH_TEST",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "MOCK_API_CALLED",
    fixed = TRUE
  )
})

test_that("concept scheme refresh updates cached data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_TEST", "1.0")

  # Test sonunda mevcut önbellek durumunu geri yükle
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Önbellekteki eski veri
  old_data <- tibble::tibble(concept_id = "OLD")

  assign(cache_key, old_data, envir = cache)

  # Başarılı API yanıtını taklit et
  xml <- test_concept_scheme_xml()

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {

      expect_identical(
        path,
        "conceptscheme/TR/CS_TEST/1.0"
      )

      mock_response
    }
  )

  # Eski önbelleği atlayarak yeni veriyi getir
  result <- tuik_get_concept_scheme(
    concept_scheme_id = "CS_TEST",
    agency = "TR",
    version = "1.0",
    refresh = TRUE
  )

  # Yeni Concept bilgilerini kontrol et
  expect_identical(
    result$concept_id,
    c("FREQ", "REF_AREA")
  )

  expect_identical(
    result$label_en,
    c("Frequency", "Reference Area")
  )

  # Önbelleğin yeni veriyle güncellendiğini kontrol et
  cached_data <- get(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  expect_identical(cached_data, result)
  expect_false(identical(cached_data, old_data))
})

test_that("tuik_get_concept_scheme rejects XML without ConceptScheme", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- xml2::read_xml('
    <Structure>
      <ConceptSchemes/>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {
      mock_response
    }
  )

  expect_error(
    tuik_get_concept_scheme(
      concept_scheme_id = "CS_MISSING_TEST",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "No ConceptScheme element was found in the response.",
    fixed = TRUE
  )
})

test_that("failed concept scheme refresh preserves cached data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_FAILED_REFRESH", "1.0")

  # Önceki önbellek kaydını koru
  had_key <- exists(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  if (had_key) {
    old_value <- get(
      cache_key,
      envir = cache,
      inherits = FALSE
    )
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Önbelleğe eski veriyi yerleştir
  old_data <- tibble::tibble(
    concept_id = "FREQ",
    label_tr = "Frekans",
    label_en = "Frequency"
  )

  assign(cache_key, old_data, envir = cache)

  # Başarısız API isteğini taklit et
  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {
      stop("MOCK_API_FAILURE", call. = FALSE)
    }
  )

  # Yenileme işlemi hata üretmeli
  expect_error(
    tuik_get_concept_scheme(
      concept_scheme_id = "CS_FAILED_REFRESH",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "MOCK_API_FAILURE",
    fixed = TRUE
  )

  # Eski önbellek verisi korunmalı
  cached_data <- get(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  expect_identical(cached_data, old_data)
})

test_that("tuik_clear_concept_cache removes all cached entries", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache

  # Test öncesindeki önbelleği koru
  old_cache <- as.list(cache, all.names = TRUE)

  withr::defer({
    rm(
      list = ls(cache, all.names = TRUE),
      envir = cache
    )
    list2env(old_cache, envir = cache)
  })

  # Test kayıtlarını oluştur
  assign("TEST_A", 1, envir = cache)
  assign("TEST_B", 2, envir = cache)
  assign(".TEST_HIDDEN", 3, envir = cache)

  expect_true(
    all(
      c("TEST_A", "TEST_B", ".TEST_HIDDEN") %in%
        ls(cache, all.names = TRUE)
    )
  )

  # Önbelleği temizle
  result <- tuik_clear_concept_cache()

  expect_length(
    ls(cache, all.names = TRUE),
    0L
  )

  expect_null(result)

  # Boş önbellekte tekrar çalışabilmeli
  expect_invisible(tuik_clear_concept_cache())
})


test_that("tuik_preload_concepts requires exactly one Dataflow", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  # Dataflow alanı bulunmuyor
  expect_error(
    tuik_preload_concepts(list()),
    "`profile` must contain exactly one Dataflow.",
    fixed = TRUE
  )

  # Dataflow tablosu boş
  expect_error(
    tuik_preload_concepts(
      list(dataflow = tibble::tibble())
    ),
    "`profile` must contain exactly one Dataflow.",
    fixed = TRUE
  )

  # Birden fazla Dataflow bulunuyor
  expect_error(
    tuik_preload_concepts(
      list(
        dataflow = tibble::tibble(
          dataflow_id = c("DF_A", "DF_B")
        )
      )
    ),
    "`profile` must contain exactly one Dataflow.",
    fixed = TRUE
  )
})

test_that("tuik_preload_concepts stores multiple ConceptSchemes", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  old_cache <- as.list(cache, all.names = TRUE)

  withr::defer({
    rm(list = ls(cache, all.names = TRUE), envir = cache)
    list2env(old_cache, envir = cache)
  })

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  xml <- xml2::read_xml('
    <Structure>
      <ConceptSchemes>
        <ConceptScheme id="CS_A" agencyID="TR" version="1.0">
          <Concept id="FREQ">
            <Name xml:lang="tr">Frekans</Name>
            <Name xml:lang="en">Frequency</Name>
          </Concept>
        </ConceptScheme>

        <ConceptScheme id="CS_B" agencyID="TR" version="1.0">
          <Concept id="REF_AREA">
            <Name xml:lang="tr">Referans Bölge</Name>
            <Name xml:lang="en">Reference Area</Name>
          </Concept>
        </ConceptScheme>
      </ConceptSchemes>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = NULL, ...) {

      expect_identical(
        path,
        "dataflow/TR/DF_TEST/1.0"
      )

      expect_identical(
        query,
        list(references = "descendants")
      )

      mock_response
    }
  )

  stored <- tuik_preload_concepts(
    profile,
    refresh = TRUE
  )

  expect_identical(stored, 2L)

  expect_true(
    exists(test_concept_cache_key("CS_A", "1.0"), envir = cache, inherits = FALSE)
  )

  expect_true(
    exists(test_concept_cache_key("CS_B", "1.0"), envir = cache, inherits = FALSE)
  )

  scheme_a <- get(test_concept_cache_key("CS_A", "1.0"), envir = cache, inherits = FALSE)
  scheme_b <- get(test_concept_cache_key("CS_B", "1.0"), envir = cache, inherits = FALSE)

  expect_identical(scheme_a$concept_id, "FREQ")
  expect_identical(scheme_a$label_en, "Frequency")

  expect_identical(scheme_b$concept_id, "REF_AREA")
  expect_identical(scheme_b$label_tr, "Referans Bölge")
})

test_that("preload skips cached ConceptSchemes when refresh is FALSE", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_TEST", "1.0")

  # Önceki kaydı koru
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Önbellekte eski Concept bilgisi
  old_data <- tibble::tibble(
    concept_id = "OLD",
    label_en = "Old concept"
  )

  assign(cache_key, old_data, envir = cache)

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  # API yanıtında aynı ConceptScheme için yeni veri bulunuyor
  xml <- test_concept_scheme_xml()

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = NULL, ...) {
      mock_response
    }
  )

  stored <- tuik_preload_concepts(
    profile,
    refresh = FALSE
  )

  cached_data <- get(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  expect_identical(stored, 0L)
  expect_identical(cached_data, old_data)
})

test_that("preload updates cached ConceptSchemes when refresh is TRUE", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_TEST", "1.0")

  # Test öncesindeki önbellek kaydını koru
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Önbellekte eski veri
  old_data <- tibble::tibble(
    concept_id = "OLD",
    label_en = "Old concept"
  )

  assign(cache_key, old_data, envir = cache)

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  # Yeni ConceptScheme XML belgesi
  xml <- test_concept_scheme_xml()

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = NULL, ...) {
      mock_response
    }
  )

  stored <- tuik_preload_concepts(
    profile,
    refresh = TRUE
  )

  cached_data <- get(
    cache_key,
    envir = cache,
    inherits = FALSE
  )

  expect_identical(stored, 1L)

  expect_identical(
    cached_data$concept_id,
    c("FREQ", "REF_AREA")
  )

  expect_identical(
    cached_data$label_en,
    c("Frequency", "Reference Area")
  )

  expect_false(identical(cached_data, old_data))
})

test_that("preload skips ConceptSchemes with missing identifiers", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  old_cache <- as.list(cache, all.names = TRUE)

  withr::defer({
    rm(list = ls(cache, all.names = TRUE), envir = cache)
    list2env(old_cache, envir = cache)
  })

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  xml <- xml2::read_xml('
    <Structure>
      <ConceptSchemes>
        <ConceptScheme id="CS_A" agencyID="TR" version="1.0">
          <Concept id="FREQ"/>
        </ConceptScheme>

        <ConceptScheme id="CS_B" version="1.0">
          <Concept id="REF_AREA"/>
        </ConceptScheme>

        <ConceptScheme agencyID="TR" version="1.0">
          <Concept id="TIME_PERIOD"/>
        </ConceptScheme>

        <ConceptScheme id="CS_D" agencyID="TR">
          <Concept id="OBS_VALUE"/>
        </ConceptScheme>
      </ConceptSchemes>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = NULL, ...) {
      mock_response
    }
  )

  stored <- tuik_preload_concepts(
    profile,
    refresh = TRUE
  )

  # Yalnızca CS_A kaydedilmiş olmalı
  expect_identical(stored, 1L)

  cache_keys <- ls(cache, all.names = TRUE)

  expect_identical(
    cache_keys,
    test_concept_cache_key("CS_A", "1.0")
  )

  valid_data <- get(
    test_concept_cache_key("CS_A", "1.0"),
    envir = cache,
    inherits = FALSE
  )

  expect_identical(valid_data$concept_id, "FREQ")
})

test_that("preload returns zero when no ConceptSchemes are present", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  xml <- xml2::read_xml('
    <Structure>
      <ConceptSchemes/>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, query = NULL, ...) {
      mock_response
    }
  )

  result <- withVisible(
    tuik_preload_concepts(profile, refresh = TRUE)
  )

  expect_identical(result$value, 0L)
  expect_false(result$visible)
})

test_that("tuik_concepts joins dimension references with cached metadata", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  # Test sonrasında önbelleği eski durumuna getir
  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_TEST", "1.0")

  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Concept metadata bilgilerini örnek XML'den oluştur
  xml <- test_concept_scheme_xml()

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  metadata <- .tuik_parse_concept_scheme_node(node)

  assign(cache_key, metadata, envir = cache)

  # Tek dimension içeren örnek profile
  profile <- list(
    dimensions = tibble::tibble(
      dimension = "FREQ",
      concept_id = "FREQ",
      concept_scheme = "CS_TEST",
      concept_scheme_version = "1.0",
      concept_agency = "TR"
    ),

    measures = tibble::tibble(
      measure = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    ),

    attributes = tibble::tibble(
      attribute = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    )
  )

  result <- tuik_concepts(profile)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)

  expect_identical(result$component_type, "dimension")
  expect_identical(result$component_id, "FREQ")
  expect_identical(result$concept_id, "FREQ")

  expect_identical(result$label_tr, "Frekans")
  expect_identical(result$label_en, "Frequency")

  expect_identical(
    result$description_en,
    "Frequency of observations"
  )
})

test_that("tuik_concepts joins and orders all component types", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_COMPONENTS", "1.0")

  # Test öncesindeki önbellek durumunu koru
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Örnek Concept metadata
  metadata <- tibble::tibble(
    concept_agency = rep("TR", 3),
    concept_scheme = rep("CS_COMPONENTS", 3),
    concept_scheme_version = rep("1.0", 3),
    concept_id = c("FREQ", "OBS_VALUE", "OBS_STATUS"),
    label_tr = c("Frekans", "Gözlem Değeri", "Gözlem Durumu"),
    label_en = c("Frequency", "Observation Value", "Observation Status"),
    description_tr = rep(NA_character_, 3),
    description_en = rep(NA_character_, 3)
  )

  assign(cache_key, metadata, envir = cache)

  # Üç bileşen içeren örnek profile
  profile <- list(
    dimensions = tibble::tibble(
      dimension = "FREQ",
      concept_id = "FREQ",
      concept_scheme = "CS_COMPONENTS",
      concept_scheme_version = "1.0",
      concept_agency = "TR"
    ),

    measures = tibble::tibble(
      measure = "OBS_VALUE",
      concept_id = "OBS_VALUE",
      concept_scheme = "CS_COMPONENTS",
      concept_scheme_version = "1.0",
      concept_agency = "TR"
    ),

    attributes = tibble::tibble(
      attribute = "OBS_STATUS",
      concept_id = "OBS_STATUS",
      concept_scheme = "CS_COMPONENTS",
      concept_scheme_version = "1.0",
      concept_agency = "TR"
    )
  )

  result <- tuik_concepts(profile)

  expect_equal(nrow(result), 3L)

  expect_identical(
    result$component_type,
    c("dimension", "measure", "attribute")
  )

  expect_identical(
    result$component_id,
    c("FREQ", "OBS_VALUE", "OBS_STATUS")
  )

  expect_identical(
    result$concept_id,
    c("FREQ", "OBS_VALUE", "OBS_STATUS")
  )

  expect_identical(
    result$label_tr,
    c("Frekans", "Gözlem Değeri", "Gözlem Durumu")
  )

  expect_identical(
    result$label_en,
    c("Frequency", "Observation Value", "Observation Status")
  )
})

test_that("tuik_concepts excludes components with missing concept references", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  cache <- .tuiksdmx_concept_cache
  cache_key <- test_concept_cache_key("CS_TEST", "1.0")

  # Önceki önbellek durumunu koru
  had_key <- exists(cache_key, envir = cache, inherits = FALSE)

  if (had_key) {
    old_value <- get(cache_key, envir = cache, inherits = FALSE)
  }

  withr::defer({
    if (had_key) {
      assign(cache_key, old_value, envir = cache)
    } else if (exists(cache_key, envir = cache, inherits = FALSE)) {
      rm(list = cache_key, envir = cache)
    }
  })

  # Örnek Concept metadata
  xml <- test_concept_scheme_xml()

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='ConceptScheme']"
  )

  metadata <- .tuik_parse_concept_scheme_node(node)

  assign(cache_key, metadata, envir = cache)

  # Bir geçerli, bir eksik Concept referansı
  profile <- list(
    dimensions = tibble::tibble(
      dimension = c("FREQ", "REF_AREA"),
      concept_id = c("FREQ", NA_character_),
      concept_scheme = c("CS_TEST", NA_character_),
      concept_scheme_version = c("1.0", NA_character_),
      concept_agency = c("TR", NA_character_)
    ),

    measures = tibble::tibble(
      measure = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    ),

    attributes = tibble::tibble(
      attribute = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    )
  )

  result <- tuik_concepts(profile)

  expect_equal(nrow(result), 1L)

  expect_identical(result$component_type, "dimension")
  expect_identical(result$component_id, "FREQ")
  expect_identical(result$concept_id, "FREQ")
  expect_identical(result$label_tr, "Frekans")

  # Orijinal profile değişmemeli
  expect_identical(
    profile$dimensions$dimension,
    c("FREQ", "REF_AREA")
  )
})

test_that("tuik_concepts returns an empty table when no valid references exist", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  profile <- list(
    dimensions = tibble::tibble(
      dimension = "FREQ",
      concept_id = NA_character_,
      concept_scheme = NA_character_,
      concept_scheme_version = NA_character_,
      concept_agency = NA_character_
    ),

    measures = tibble::tibble(
      measure = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    ),

    attributes = tibble::tibble(
      attribute = character(),
      concept_id = character(),
      concept_scheme = character(),
      concept_scheme_version = character(),
      concept_agency = character()
    )
  )

  result <- tuik_concepts(profile)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 10L)

  expect_named(
    result,
    c(
      "component_type",
      "component_id",
      "concept_id",
      "concept_scheme",
      "concept_scheme_version",
      "concept_agency",
      "label_tr",
      "label_en",
      "description_tr",
      "description_en"
    )
  )

  expect_true(
    all(vapply(result, is.character, logical(1)))
  )
})

test_that("tuik_concepts requires dimension information", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  expect_error(
    tuik_concepts(list()),
    "`profile` does not contain dimension information.",
    fixed = TRUE
  )

  expect_error(
    tuik_concepts(
      list(dimensions = NULL)
    ),
    "`profile` does not contain dimension information.",
    fixed = TRUE
  )
})

test_that("tuik_search_concepts supports Turkish and English searches", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  concepts <- tibble::tibble(
    component_type = c("dimension", "dimension", "measure"),
    component_id = c("FREQ", "REF_AREA", "OBS_VALUE"),
    concept_id = c("FREQ", "REF_AREA", "OBS_VALUE"),
    label_tr = c("Frekans", "Referans Bölge", "Gözlem Değeri"),
    label_en = c("Frequency", "Reference Area", "Observation Value"),
    description_tr = c(
      "Verinin zaman sıklığı",
      "Coğrafi referans alanı",
      "Gözlem değeri"
    ),
    description_en = c(
      "Frequency of observations",
      "Geographical reference area",
      "Observed value"
    )
  )

  # Gerçek profile, önbellek veya API kullanılmasın
  testthat::local_mocked_bindings(
    tuik_concepts = function(profile, api_key = NULL) concepts
  )

  result_tr <- tuik_search_concepts(
    profile = list(),
    query = "frekans",
    language = "tr"
  )

  result_en <- tuik_search_concepts(
    profile = list(),
    query = "reference",
    language = "en"
  )

  expect_equal(nrow(result_tr), 1L)
  expect_identical(result_tr$component_id, "FREQ")
  expect_identical(result_tr$label_tr, "Frekans")

  expect_equal(nrow(result_en), 1L)
  expect_identical(result_en$component_id, "REF_AREA")
  expect_identical(result_en$label_en, "Reference Area")
})

test_that("concept search supports both languages and identifier matching", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  concepts <- tibble::tibble(
    component_type = c("dimension", "dimension", "measure"),
    component_id = c("FREQ", "REF_AREA", "OBS_VALUE"),
    concept_id = c("FREQ", "REF_AREA", "OBS_VALUE"),
    label_tr = c("Frekans", "Referans Bölge", "Gözlem Değeri"),
    label_en = c("Frequency", "Reference Area", "Observation Value"),
    description_tr = c(
      "Verinin zaman sıklığı",
      "Coğrafi referans alanı",
      "Gözlem değeri"
    ),
    description_en = c(
      "Frequency of observations",
      "Geographical reference area",
      "Observed value"
    )
  )

  testthat::local_mocked_bindings(
    tuik_concepts = function(profile, api_key = NULL) concepts
  )

  # İngilizce açıklama, both seçeneğiyle aranabilmeli
  result_both <- tuik_search_concepts(
    profile = list(),
    query = "observation",
    language = "both"
  )

  # Dil seçeneğinden bağımsız olarak Concept kimliği aranabilmeli
  result_id <- tuik_search_concepts(
    profile = list(),
    query = "OBS_VALUE",
    language = "tr"
  )

  expect_equal(nrow(result_both), 2L)
  expect_identical(result_both$component_id,c("FREQ", "OBS_VALUE"))
  expect_equal(nrow(result_id), 1L)
  expect_identical(result_id$concept_id,"OBS_VALUE")
})

test_that("concept search respects limit and preserves result order", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  concepts <- tibble::tibble(
    component_type = rep("dimension", 4),
    component_id = c("D1", "D2", "D3", "D4"),
    concept_id = c("C1", "C2", "C3", "C4"),
    label_tr = c(
      "İstihdam Oranı",
      "İşsizlik Oranı",
      "Katılım Oranı",
      "Doluluk Oranı"
    ),
    label_en = c(
      "Employment Rate",
      "Unemployment Rate",
      "Participation Rate",
      "Occupancy Rate"
    ),
    description_tr = rep(NA_character_, 4),
    description_en = rep(NA_character_, 4)
  )

  # API ve önbellekten bağımsız test
  testthat::local_mocked_bindings(
    tuik_concepts = function(profile, api_key = NULL) concepts
  )

  # İlk iki eşleşmeyi al
  result_limited <- tuik_search_concepts(
    profile = list(),
    query = "rate",
    language = "en",
    limit = 2
  )

  # Tüm eşleşmeleri al
  result_all <- tuik_search_concepts(
    profile = list(),
    query = "rate",
    language = "en",
    limit = NULL
  )

  expect_equal(nrow(result_limited), 2L)

  expect_identical(
    result_limited$component_id,
    c("D1", "D2")
  )

  expect_equal(nrow(result_all), 4L)

  expect_identical(
    result_all$component_id,
    c("D1", "D2", "D3", "D4")
  )
})

test_that("concept search rejects empty and missing queries", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  expect_error(
    tuik_search_concepts(
      profile = list(),
      query = ""
    ),
    "`query` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_search_concepts(
      profile = list(),
      query = "   "
    ),
    "`query` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_search_concepts(
      profile = list(),
      query = NA_character_
    ),
    "`query` must be a non-empty character string.",
    fixed = TRUE
  )
})

test_that("ConceptScheme cache is isolated by API key", {

  withr::local_envvar(
    TUIK_SDMX_API_KEY = "TEST_KEY_A"
  )

  cache <- .tuiksdmx_concept_cache

  key_a <- .tuik_metadata_cache_key(
    agency = "TR",
    id = "CS_ISOLATION",
    version = "1.0",
    api_key = "TEST_KEY_A"
  )

  key_b <- .tuik_metadata_cache_key(
    agency = "TR",
    id = "CS_ISOLATION",
    version = "1.0",
    api_key = "TEST_KEY_B"
  )

  expect_false(identical(key_a, key_b))

  # Yalnızca bu testin kullandığı kayıtları koru.
  old_entries <- lapply(
    c(key_a, key_b),
    function(key) {
      if (exists(key, envir = cache, inherits = FALSE)) {
        get(key, envir = cache, inherits = FALSE)
      } else {
        NULL
      }
    }
  )

  existed <- vapply(
    c(key_a, key_b),
    exists,
    logical(1),
    envir = cache,
    inherits = FALSE
  )

  withr::defer({
    keys <- c(key_a, key_b)

    for (i in seq_along(keys)) {
      if (existed[[i]]) {
        assign(keys[[i]], old_entries[[i]], envir = cache)
      } else if (exists(keys[[i]], envir = cache, inherits = FALSE)) {
        rm(list = keys[[i]], envir = cache)
      }
    }
  })

  if (exists(key_b, envir = cache, inherits = FALSE)) {
    rm(list = key_b, envir = cache)
  }

  cached_a <- tibble::tibble(
    concept_agency = "TR",
    concept_scheme = "CS_ISOLATION",
    concept_scheme_version = "1.0",
    concept_id = "FREQ",
    label_tr = "Frekans",
    label_en = "Frequency",
    description_tr = NA_character_,
    description_en = NA_character_
  )

  assign(key_a, cached_a, envir = cache)

  # A anahtarı mevcut cache kaydını okuyabilmeli.
  result_a <- tuik_get_concept_scheme(
    concept_scheme_id = "CS_ISOLATION",
    agency = "TR",
    version = "1.0",
    api_key = "TEST_KEY_A"
  )

  expect_identical(result_a, cached_a)

  # B anahtarı A'nın cache sonucunu kullanamamalı.
  # Ağ erişimi yerine kontrollü mock kullanıyoruz.
  testthat::local_mocked_bindings(
    tuik_perform = function(path, api_key = NULL, ...) {
      expect_identical(api_key, "TEST_KEY_B")
      stop("CACHE_MISS_FOR_KEY_B", call. = FALSE)
    }
  )

  expect_error(
    tuik_get_concept_scheme(
      concept_scheme_id = "CS_ISOLATION",
      agency = "TR",
      version = "1.0",
      api_key = "TEST_KEY_B"
    ),
    "CACHE_MISS_FOR_KEY_B",
    fixed = TRUE
  )

  expect_false(
    exists(key_b, envir = cache, inherits = FALSE)
  )

  expect_identical(
    get(key_a, envir = cache, inherits = FALSE),
    cached_a
  )
})
