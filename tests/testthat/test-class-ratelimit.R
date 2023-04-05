test_that("Rate class initialization works", {
  rate_limit_response <- testthat::test_path(
    "test_data",
    "rate_limit_response.RDS"
  )
  test_ratelimit_response <- readRDS(rate_limit_response)

  rate_limit_test_obj <- asRate(test_ratelimit_response)

  testthat::expect_true(checkmate::test_class(rate_limit_test_obj,
    classes =
      c("Rate", "Item", "R6")
  ))

  # Check if all the expected fields are filled
  testthat::expect_equal(rate_limit_test_obj$rate$limit, 1000L)
  testthat::expect_equal(rate_limit_test_obj$rate$remaining, 1000L)
  testthat::expect_equal(
    rate_limit_test_obj$rate$reset,
    "2023-03-31 11:12:32 CEST"
  )
  testthat::expect_equal(rate_limit_test_obj$instance$limit, 25L)
  testthat::expect_equal(rate_limit_test_obj$instance$remaining, 25L)
})
