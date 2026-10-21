test_that("Turkish text is normalized for searching", {

  result <- tuik_normalize_text(
    c("Tüketici Fiyat", "İŞGÜCÜ", "işsizlik")
  )

  expect_identical(
    result,
    c("tuketici fiyat", "isgucu", "issizlik")
  )
})

test_that("missing text values become empty strings", {

  result <- tuik_normalize_text(
    c("İşgücü", NA_character_, "TÜFE")
  )

  expect_identical(
    result,
    c("isgucu", "", "tufe")
  )
})
