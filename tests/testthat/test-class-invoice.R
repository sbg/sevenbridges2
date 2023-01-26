test_that("Invoice initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating an invoice object
  test_invoice_response <- readRDS(testthat::test_path("test_data", "single_invoice_response.RDS"))

  # Create invoice object
  test_invoice_object <- asInvoice(x = test_invoice_response, auth = test_auth_obj)

  testthat::expect_true(checkmate::test_class(test_invoice_object, classes = c("Invoice", "Item", "R6")))

  # Check if all the expected fields are filled
  testthat::expect_equal(test_invoice_object$invoice_id, "0123456789")
  testthat::expect_equal(test_invoice_object$pending, FALSE)
  testthat::expect_equal(test_invoice_object$approval_date, "2020-01-01T00:00:00Z")

  testthat::expect_true(checkmate::checkClass(test_invoice_object$invoice_period, classes = "list"))
  testthat::expect_equal(test_invoice_object$invoice_period$from, "2020-01-01T11:00:00Z")
  testthat::expect_equal(test_invoice_object$invoice_period$to, "2020-01-31T23:59:59Z")

  testthat::expect_true(checkmate::checkClass(test_invoice_object$analysis_costs, classes = "list"))
  testthat::expect_equal(test_invoice_object$analysis_costs$currency, "USD")
  testthat::expect_equal(test_invoice_object$analysis_costs$amount, "1244.1")


  testthat::expect_true(checkmate::checkClass(test_invoice_object$storage_costs, classes = "list"))
  testthat::expect_equal(test_invoice_object$storage_costs$currency, "USD")
  testthat::expect_equal(test_invoice_object$storage_costs$amount, "117.4")

  testthat::expect_true(checkmate::checkClass(test_invoice_object$total, classes = "list"))
  testthat::expect_equal(test_invoice_object$total$currency, "USD")
  testthat::expect_equal(test_invoice_object$total$amount, "1361.5")

  testthat::expect_equal(test_invoice_object$href, "https://api.sbgenomics.com/v2/billing/invoices/0123456789")

  # Check if superclass field auth is as expected
  testthat::expect_equal(test_invoice_object$auth$platform, "aws-us")
  testthat::expect_equal(test_invoice_object$auth$url, "https://api.sbgenomics.com/v2/")
})
