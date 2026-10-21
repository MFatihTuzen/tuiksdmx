test_that("Dataflows can be searched without Turkish characters", {

  dataflows <- test_dataflows()

  result <- tuik_search_dataflows(
    query = "issizlik",
    dataflows = dataflows
  )

  expect_equal(nrow(result), 1L)
  expect_identical(result$dataflow_id, "DF_ISSIZLIK")
  expect_identical(result$name_tr, "İşsizlik Oranı")
})

test_that("Dataflows can be searched using English names", {

  dataflows <- test_dataflows()

  result <- tuik_search_dataflows(
    query = "consumer",
    language = "en",
    dataflows = dataflows
  )

  expect_equal(nrow(result), 1L)
  expect_identical(result$dataflow_id, "DF_TUFE")
  expect_identical(result$name_en, "Consumer Price Index")
})

test_that("final_only excludes non-final Dataflows", {

  dataflows <- test_dataflows()

  result_all <- tuik_search_dataflows(
    query = "sanayi",
    final_only = FALSE,
    dataflows = dataflows
  )

  result_final <- tuik_search_dataflows(
    query = "sanayi",
    final_only = TRUE,
    dataflows = dataflows
  )

  expect_identical(result_all$dataflow_id, "DF_SANAYI")
  expect_equal(nrow(result_final), 0L)
  expect_identical(names(result_final), names(result_all))
})

test_that("final Dataflows are ranked first for equal matches", {

  dataflows <- test_dataflows()

  dataflows$name_tr <- c(
    "Tüketici Fiyat Endeksi",
    "İşsizlik Oranı",
    "Sanayi Fiyat Endeksi"
  )

  result <- tuik_search_dataflows(
    query = "fiyat",
    dataflows = dataflows
  )

  expect_identical(
    result$dataflow_id,
    c("DF_TUFE", "DF_SANAYI")
  )

  expect_identical(
    result$is_final,
    c(TRUE, FALSE)
  )
})

test_that("match quality takes priority over final status", {

  dataflows <- test_dataflows()

  dataflows$dataflow_id <- c(
    "DF_FIYAT",
    "DF_ISSIZLIK",
    "DF_SANAYI"
  )

  dataflows$name_tr <- c(
    "Tüketici Endeksi",
    "İşsizlik Oranı",
    "Sanayi Fiyat Endeksi"
  )

  dataflows$is_final <- c(FALSE, TRUE, TRUE)

  result <- tuik_search_dataflows(
    query = "fiyat",
    dataflows = dataflows
  )

  expect_identical(
    result$dataflow_id,
    c("DF_FIYAT", "DF_SANAYI")
  )

  expect_identical(
    result$is_final,
    c(FALSE, TRUE)
  )
})
