test_that("Rate class initialization works", {
  # Item object creation works
  testthat::expect_no_error(asRate(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_rate_limit_obj,
    classes = c("Item", "Rate"),
    public = c(
      "rate", "instance", "print"
    )
  )
})

test_that("Rate print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_rate_limit_obj$print())
})
