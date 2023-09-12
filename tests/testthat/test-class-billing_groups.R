test_that("Billing_groups initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Billing_groups$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_billing_groups_obj,
    classes = c("Resource", "Billing_groups"),
    public = c("URL", "query", "get")
  )
})

test_that("Billing_groups get() throws error when expected", {
  # Setup test parameters for test
  test_bad_id <- list(id = 123)
  test_missing_id <- list(id = NULL)

  # Get fails when id param is invalid
  testthat::expect_error(
    do.call(setup_billing_groups_obj$get, test_bad_id),
    regexp = "Assertion on 'id' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_billing_groups_obj$get, test_missing_id),
    regexp = "Please provide id parameter!",
    fixed = TRUE
  )
})
