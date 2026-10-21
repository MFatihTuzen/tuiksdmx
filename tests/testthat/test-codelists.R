test_codelist_cache_key <- function(id, version = "1.0") {
  .tuik_metadata_cache_key(
    agency = "TR",
    id = id,
    version = version,
    api_key = "TEST_KEY_A"
  )
}

test_that("codelist codes and multilingual labels are parsed", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- test_codelist_xml()

  codelist_node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='Codelist']"
  )

  result <- .tuik_parse_codelist_node(codelist_node)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3L)

  expect_named(
    result,
    c("code", "label_tr", "label_en")
  )

  expect_identical(
    result$code,
    c("M", "Q", "A")
  )

  expect_identical(
    result$label_tr,
    c("Aylık", "Üç Aylık", "Yıllık")
  )

  expect_identical(
    result$label_en,
    c("Monthly", "Quarterly", "Annual")
  )
})

test_that("missing codelist language labels are returned as NA", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- xml2::read_xml('
    <Structure>
      <Codelist id="CL_TEST">

        <Code id="M">
          <Name xml:lang="tr">Aylık</Name>
        </Code>

        <Code id="Q">
          <Name xml:lang="en">Quarterly</Name>
        </Code>

        <Code id="A">
          <Name xml:lang="tr">Yıllık</Name>
          <Name xml:lang="en">Annual</Name>
        </Code>

      </Codelist>
    </Structure>
  ')

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='Codelist']"
  )

  result <- .tuik_parse_codelist_node(node)

  expect_identical(result$code, c("M", "Q", "A"))

  expect_identical(
    result$label_tr,
    c("Aylık", NA_character_, "Yıllık")
  )

  expect_identical(
    result$label_en,
    c(NA_character_, "Quarterly", "Annual")
  )

  expect_equal(nrow(result), 3L)
})

test_that("empty codelists return an empty tibble", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  xml <- xml2::read_xml('
    <Structure>
      <Codelist id="CL_EMPTY" agencyID="TR" version="1.0"/>
    </Structure>
  ')

  node <- xml2::xml_find_first(
    xml,
    ".//*[local-name()='Codelist']"
  )

  result <- .tuik_parse_codelist_node(node)

  expect_s3_class(result, "tbl_df")

  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 3L)

  expect_named(
    result,
    c("code", "label_tr", "label_en")
  )

  expect_identical(result$code, character())
  expect_identical(result$label_tr, character())
  expect_identical(result$label_en, character())
})

test_that("tuik_get_codelist rejects empty and missing identifiers", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  expect_error(
    tuik_get_codelist("", agency = "TR"),
    "`codelist_id` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_codelist("CL_FREQ", agency = ""),
    "`agency` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_codelist(NA_character_, agency = "TR"),
    "`codelist_id` must be a non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    tuik_get_codelist("CL_FREQ", agency = NA_character_),
    "`agency` must be a non-empty character string.",
    fixed = TRUE
  )
})

test_that("tuik_get_codelist returns cached data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  expected <- tibble::tibble(
    code = c("M", "Q", "A"),
    label_tr = c("Aylık", "Üç Aylık", "Yıllık"),
    label_en = c("Monthly", "Quarterly", "Annual")
  )

  cache_key <- test_codelist_cache_key("CL_TEST_CACHE", "1.0")

  assign(
    cache_key,
    expected,
    envir = .tuiksdmx_codelist_cache
  )

  result <- tuik_get_codelist(
    codelist_id = "CL_TEST_CACHE",
    agency = "TR",
    version = "1.0"
  )

  expect_identical(result, expected)
})

test_that("refresh bypasses cached codelist data", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  old_data <- tibble::tibble(
    code = "OLD",
    label_tr = "Eski",
    label_en = "Old"
  )

  cache_key <- test_codelist_cache_key("CL_REFRESH_TEST", "1.0")

  assign(
    cache_key,
    old_data,
    envir = .tuiksdmx_codelist_cache
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {
      expect_identical(
        path,
        "codelist/TR/CL_REFRESH_TEST/1.0"
      )

      stop("MOCK_API_CALLED", call. = FALSE)
    }
  )

  expect_error(
    tuik_get_codelist(
      codelist_id = "CL_REFRESH_TEST",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "MOCK_API_CALLED",
    fixed = TRUE
  )
})

test_that("refresh updates the codelist cache after a successful response", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  # Önbellekteki eski veri
  old_data <- tibble::tibble(
    code = "OLD",
    label_tr = "Eski",
    label_en = "Old"
  )

  cache_key <- test_codelist_cache_key("CL_FREQ", "1.0")

  assign(
    cache_key,
    old_data,
    envir = .tuiksdmx_codelist_cache
  )

  # API'den geleceğini varsaydığımız yeni XML
  xml <- test_codelist_xml()

  mock_response <- httr2::response(
    status_code = 200,
    headers = list(
      "content-type" = "application/xml"
    ),
    body = charToRaw(as.character(xml))
  )

  # Gerçek API isteğinin yerine sahte yanıt kullan
  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {

      expect_identical(
        path,
        "codelist/TR/CL_FREQ/1.0"
      )

      mock_response
    }
  )

  # Önbellekte eski veri olmasına rağmen yenile
  result <- tuik_get_codelist(
    codelist_id = "CL_FREQ",
    agency = "TR",
    version = "1.0",
    refresh = TRUE
  )

  # Yeni verinin ayrıştırıldığını kontrol et
  expect_identical(
    result$code,
    c("M", "Q", "A")
  )

  expect_identical(
    result$label_en,
    c("Monthly", "Quarterly", "Annual")
  )

  # Önbelleğin yeni veriyle güncellendiğini kontrol et
  cached_data <- get(
    cache_key,
    envir = .tuiksdmx_codelist_cache,
    inherits = FALSE
  )

  expect_identical(cached_data, result)
  expect_false(identical(cached_data, old_data))
})

test_that("tuik_get_codelist rejects XML without a Codelist element", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  xml <- xml2::read_xml('
    <Structure>
      <Codelists/>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list(
      "content-type" = "application/xml"
    ),
    body = charToRaw(as.character(xml))
  )

  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {
      mock_response
    }
  )

  expect_error(
    tuik_get_codelist(
      codelist_id = "CL_MISSING_TEST",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "No Codelist element was found in the response.",
    fixed = TRUE
  )
})

test_that("failed refresh preserves existing codelist cache", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  old_data <- tibble::tibble(
    code = "M",
    label_tr = "Aylık",
    label_en = "Monthly"
  )

  cache_key <- test_codelist_cache_key("CL_FAILED_REFRESH", "1.0")

  assign(
    cache_key,
    old_data,
    envir = .tuiksdmx_codelist_cache
  )

  # Başarısız API isteğini taklit et
  testthat::local_mocked_bindings(
    tuik_perform = function(path, ...) {
      stop("MOCK_API_FAILURE", call. = FALSE)
    }
  )

  # Yenileme başarısız olmalı
  expect_error(
    tuik_get_codelist(
      codelist_id = "CL_FAILED_REFRESH",
      agency = "TR",
      version = "1.0",
      refresh = TRUE
    ),
    "MOCK_API_FAILURE",
    fixed = TRUE
  )

  # Eski önbellek kaydı korunmalı
  cached_data <- get(
    cache_key,
    envir = .tuiksdmx_codelist_cache,
    inherits = FALSE
  )

  expect_identical(cached_data, old_data)
})

test_that("tuik_clear_codelist_cache removes all cached entries", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  cache <- .tuiksdmx_codelist_cache

  # Test kayıtlarını oluştur
  assign("TEST_A", 1, envir = cache)
  assign("TEST_B", 2, envir = cache)
  assign(".TEST_HIDDEN", 3, envir = cache)

  # Kayıtların bulunduğunu doğrula
  expect_true(
    all(c("TEST_A", "TEST_B", ".TEST_HIDDEN") %in%
          ls(cache, all.names = TRUE))
  )

  # Önbelleği temizle
  result <- tuik_clear_codelist_cache()

  # Bütün kayıtlar silinmiş olmalı
  expect_length(
    ls(cache, all.names = TRUE),
    0L
  )

  # Fonksiyon görünmez NULL döndürmeli
  expect_null(result)
  expect_invisible(tuik_clear_codelist_cache())
})

test_that("tuik_preload_codelists requires exactly one Dataflow", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  # Dataflow alanı bulunmuyor
  expect_error(
    tuik_preload_codelists(list()),
    "`profile` must contain exactly one Dataflow.",
    fixed = TRUE
  )

  # Dataflow tablosu boş
  expect_error(
    tuik_preload_codelists(
      list(dataflow = tibble::tibble())
    ),
    "`profile` must contain exactly one Dataflow.",
    fixed = TRUE
  )

  # Birden fazla Dataflow bulunuyor
  expect_error(
    tuik_preload_codelists(
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

test_that("tuik_preload_codelists stores multiple codelists", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  # Tek Dataflow içeren örnek profile
  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  # İki codelist içeren örnek SDMX yanıtı
  xml <- xml2::read_xml('
    <Structure>
      <Codelists>
        <Codelist id="CL_FREQ" agencyID="TR" version="1.0">
          <Code id="M">
            <Name xml:lang="tr">Aylık</Name>
            <Name xml:lang="en">Monthly</Name>
          </Code>
        </Codelist>

        <Codelist id="CL_AREA" agencyID="TR" version="1.0">
          <Code id="TR">
            <Name xml:lang="tr">Türkiye</Name>
            <Name xml:lang="en">Türkiye</Name>
          </Code>
        </Codelist>
      </Codelists>
    </Structure>
  ')

  mock_response <- httr2::response(
    status_code = 200,
    headers = list("content-type" = "application/xml"),
    body = charToRaw(as.character(xml))
  )

  # Gerçek API isteğini taklit et
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

  # Toplu yüklemeyi gerçekleştir
  stored <- tuik_preload_codelists(
    profile,
    refresh = TRUE
  )

  cache <- .tuiksdmx_codelist_cache

  # İki codelist kaydedilmiş olmalı
  expect_identical(stored, 2L)

  expect_true(
    exists(test_codelist_cache_key("CL_FREQ", "1.0"), envir = cache, inherits = FALSE)
  )

  expect_true(
    exists(test_codelist_cache_key("CL_AREA", "1.0"), envir = cache, inherits = FALSE)
  )

  # Kaydedilen kodları doğrula
  freq <- get(test_codelist_cache_key("CL_FREQ", "1.0"), envir = cache)
  area <- get(test_codelist_cache_key("CL_AREA", "1.0"), envir = cache)

  expect_identical(freq$code, "M")
  expect_identical(freq$label_en, "Monthly")

  expect_identical(area$code, "TR")
  expect_identical(area$label_tr, "Türkiye")
})

test_that("preload skips cached codelists when refresh is FALSE", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  # Önceden kaydedilmiş codelist
  old_data <- tibble::tibble(
    code = "OLD",
    label_tr = "Eski",
    label_en = "Old"
  )

  cache_key <- test_codelist_cache_key("CL_FREQ", "1.0")

  assign(
    cache_key,
    old_data,
    envir = .tuiksdmx_codelist_cache
  )

  # API yanıtında aynı codelist için yeni veri bulunuyor
  xml <- xml2::read_xml('
    <Structure>
      <Codelist id="CL_FREQ" agencyID="TR" version="1.0">
        <Code id="M">
          <Name xml:lang="tr">Aylık</Name>
          <Name xml:lang="en">Monthly</Name>
        </Code>
      </Codelist>
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

  stored <- tuik_preload_codelists(
    profile,
    refresh = FALSE
  )

  cached_data <- get(
    cache_key,
    envir = .tuiksdmx_codelist_cache,
    inherits = FALSE
  )

  expect_identical(stored, 0L)
  expect_identical(cached_data, old_data)
})

test_that("preload updates cached codelists when refresh is TRUE", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  old_data <- tibble::tibble(
    code = "OLD",
    label_tr = "Eski",
    label_en = "Old"
  )

  cache_key <- test_codelist_cache_key("CL_FREQ", "1.0")

  assign(
    cache_key,
    old_data,
    envir = .tuiksdmx_codelist_cache
  )

  # API'den gelen yeni codelist
  xml <- xml2::read_xml('
    <Structure>
      <Codelist id="CL_FREQ" agencyID="TR" version="1.0">
        <Code id="M">
          <Name xml:lang="tr">Aylık</Name>
          <Name xml:lang="en">Monthly</Name>
        </Code>
      </Codelist>
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

  stored <- tuik_preload_codelists(
    profile,
    refresh = TRUE
  )

  cached_data <- get(
    cache_key,
    envir = .tuiksdmx_codelist_cache,
    inherits = FALSE
  )

  expect_identical(stored, 1L)
  expect_identical(cached_data$code, "M")
  expect_identical(cached_data$label_tr, "Aylık")
  expect_false(identical(cached_data, old_data))
})

test_that("preload skips codelists with missing identifiers", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  xml <- xml2::read_xml('
    <Structure>
      <Codelists>
        <Codelist id="CL_A" agencyID="TR" version="1.0">
          <Code id="M"/>
        </Codelist>

        <Codelist id="CL_B" version="1.0">
          <Code id="Q"/>
        </Codelist>

        <Codelist agencyID="TR" version="1.0">
          <Code id="A"/>
        </Codelist>

        <Codelist id="CL_D" agencyID="TR">
          <Code id="W"/>
        </Codelist>
      </Codelists>
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

  stored <- tuik_preload_codelists(
    profile,
    refresh = TRUE
  )

  cache <- .tuiksdmx_codelist_cache

  # Yalnızca geçerli codelist kaydedilmeli
  expect_identical(stored, 1L)

  expect_true(
    exists(test_codelist_cache_key("CL_A", "1.0"), envir = cache, inherits = FALSE)
  )

  expect_false(
    exists(test_codelist_cache_key("CL_B", "1.0"), envir = cache, inherits = FALSE)
  )

  expect_false(
    exists(test_codelist_cache_key("CL_D", "latest"), envir = cache, inherits = FALSE)
  )

  valid_data <- get(
    test_codelist_cache_key("CL_A", "1.0"),
    envir = cache,
    inherits = FALSE
  )

  expect_identical(valid_data$code, "M")
})

test_that("preload returns zero when no codelists are present", {
  withr::local_envvar(TUIK_SDMX_API_KEY = "TEST_KEY_A")

  local_codelist_cache()

  profile <- list(
    dataflow = tibble::tibble(
      agency = "TR",
      dataflow_id = "DF_TEST",
      dataflow_version = "1.0"
    )
  )

  xml <- xml2::read_xml('
    <Structure>
      <Codelists/>
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
    tuik_preload_codelists(profile, refresh = TRUE)
  )

  expect_identical(result$value, 0L)
  expect_false(result$visible)
})

test_that("codelist cache is isolated by API key", {

  local_codelist_cache()

  cached_a <- tibble::tibble(
    code = "A",
    label_tr = "Anahtar A",
    label_en = "Key A"
  )

  key_a <- .tuik_metadata_cache_key(
    agency = "TR",
    id = "CL_ISOLATION",
    version = "1.0",
    api_key = "TEST_KEY_A"
  )

  key_b <- .tuik_metadata_cache_key(
    agency = "TR",
    id = "CL_ISOLATION",
    version = "1.0",
    api_key = "TEST_KEY_B"
  )

  expect_false(identical(key_a, key_b))

  assign(
    key_a,
    cached_a,
    envir = .tuiksdmx_codelist_cache
  )

  # A anahtarı mevcut cache kaydını kullanabilmeli.
  result_a <- tuik_get_codelist(
    codelist_id = "CL_ISOLATION",
    agency = "TR",
    version = "1.0",
    api_key = "TEST_KEY_A"
  )

  expect_identical(result_a, cached_a)

  # B anahtarı A'nın cache sonucunu alamamalı.
  # Gerçek HTTP isteğini kontrollü bir hata ile taklit ediyoruz.
  testthat::local_mocked_bindings(
    tuik_perform = function(path, api_key = NULL, ...) {
      expect_identical(api_key, "TEST_KEY_B")
      stop("CACHE_MISS_FOR_KEY_B", call. = FALSE)
    }
  )

  expect_error(
    tuik_get_codelist(
      codelist_id = "CL_ISOLATION",
      agency = "TR",
      version = "1.0",
      api_key = "TEST_KEY_B"
    ),
    "CACHE_MISS_FOR_KEY_B",
    fixed = TRUE
  )

  expect_false(
    exists(
      key_b,
      envir = .tuiksdmx_codelist_cache,
      inherits = FALSE
    )
  )

  expect_identical(
    get(key_a, envir = .tuiksdmx_codelist_cache),
    cached_a
  )
})
