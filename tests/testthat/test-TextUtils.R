context("CleanText")

testthat::test_that("errors", {
  testthat::expect_equal(
    CleanText("The boy was tall",
              lemmatize = TRUE),
    "the boy be tall"
  )

  testthat::expect_warning(
    CleanText("N20 - (MKs)",
              delete = c(),
              lemmatize = TRUE)
  )
})

context("CleanKeywords")

testthat::test_that("errors", {
  testthat::expect_true(
    all(CleanKeywords("<keyword A><Keyword B>")[[1]] ==
          c("keyword a", "keyword b"))
  )

  testthat::expect_true(
    all(CleanKeywords("<keyword (MORE)><Keyword-B>")[[1]] ==
          c("keyword (more)", "keyword b"))
  )
})
