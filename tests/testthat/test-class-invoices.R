test_that("Invoices initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Invoices$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_invoices_obj,
    classes = c("Resource", "Invoices"),
    public = c("URL", "query", "get")
  )
})

test_that("Invoices query() throws error when expected", {
  # Setup test parameters for test
  test_billing_group_missing <- list(billing_group = NULL)
  test_bad_billing_group <- list(billing_group = 123)

  # Query fails when billing group is missing
  testthat::expect_error(
    do.call(setup_invoices_obj$query, test_billing_group_missing)
  )

  # Query fails when billing group param is invalid
  testthat::expect_error(
    do.call(setup_invoices_obj$query, test_bad_billing_group)
  )
})

test_that("Invoices get() throws error when expected", {
  # Setup test parameters for test
  test_bad_id <- list(id = 123)
  test_missing_id <- list(id = NULL)

  # Get fails when id param is invalid
  testthat::expect_error(
    do.call(setup_invoices_obj$get, test_bad_id),
    regexp = "Assertion on 'id' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_invoices_obj$get, test_missing_id),
    regexp = "Please provide id parameter!",
    fixed = TRUE
  )
})
