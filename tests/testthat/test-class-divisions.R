test_that("Divisions initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Divisions$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_divisions_obj,
    classes = c("Resource", "Divisions"),
    public = c("URL", "query", "get")
  )
})

test_that("Divisions get() throws error when expected", {
  # Setup test parameters for test
  test_bad_id <- list(id = 123)
  test_missing_id <- list(id = NULL)

  # Get fails when id 'division_id' parameter is invalid
  testthat::expect_error(
    do.call(setup_divisions_obj$get, test_bad_id),
    regexp = "Assertion on 'id' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_divisions_obj$get, test_missing_id),
    regexp = "Please provide the 'id' parameter.",
    fixed = TRUE
  )
})
