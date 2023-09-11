test_that("Invoice initialization works", {
  # Item object creation works
  testthat::expect_no_error(asInvoice(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_invoice_obj,
    classes = c("Item", "Invoice"),
    public = c(
      "approval_date", "pending", "id", "href", "invoice_period",
      "analysis_costs", "total"
    )
  )
})

test_that("Invoice print method works", {
  testthat::expect_snapshot(setup_invoice_obj$print())
})
