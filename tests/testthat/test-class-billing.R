test_that("Billing initialization works", {
  # Item object creation works
  testthat::expect_no_error(asBilling(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_billing_obj,
    classes = c("Item", "Billing"),
    public = c(
      "id",
      "owner",
      "name",
      "type",
      "pending",
      "disabled",
      "balance",
      "print",
      "reload",
      "analysis_breakdown",
      "storage_breakdown",
      "egress_breakdown"
    )
  )
})

test_that("Billing print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_billing_obj$print())
})


test_that("Breakdown queries throws error when needed", {
  # Setup test parameters for test
  test_bad_date_from <- list(date_from = 1)
  test_bad_date_to <- list(date_to = 1)
  test_bad_invoice <- list(invoice = 1)

  ## Analysis breakdown tests
  # Test bad date_from parameter
  testthat::expect_error(do.call(setup_billing_obj$analysis_breakdown, test_bad_date_from)) # nolint

  # Test bad date_to parameter
  testthat::expect_error(do.call(setup_billing_obj$analysis_breakdown, test_bad_date_to)) # nolint

  # Test bad invoice parameter
  testthat::expect_error(do.call(setup_billing_obj$analysis_breakdown, test_bad_invoice)) # nolint

  ## Storage breakdown tests
  # Test bad date_from parameter
  testthat::expect_error(do.call(setup_billing_obj$storage_breakdown, test_bad_date_from)) # nolint

  # Test bad date_to parameter
  testthat::expect_error(do.call(setup_billing_obj$storage_breakdown, test_bad_date_to)) # nolint

  # Test bad invoice parameter
  testthat::expect_error(do.call(setup_billing_obj$storage_breakdown, test_bad_invoice)) # nolint

  ## Egress breakdown tests
  # Test bad date_from parameter
  testthat::expect_error(do.call(setup_billing_obj$egress_breakdown, test_bad_date_from)) # nolint

  # Test bad date_to parameter
  testthat::expect_error(do.call(setup_billing_obj$egress_breakdown, test_bad_date_to)) # nolint

  # Test bad invoice parameter
  testthat::expect_error(do.call(setup_billing_obj$egress_breakdown, test_bad_invoice)) # nolint
})
