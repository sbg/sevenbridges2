test_that("Division initialization works", {
  # Item object creation works
  testthat::expect_no_error(asDivision(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_division_obj,
    classes = c("Item", "Division"),
    public = c(
      "id", "name", "print", "reload"
    )
  )
})

test_that("Division print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_no_error(setup_division_obj$print())
  testthat::expect_snapshot(setup_division_obj$print())
})
