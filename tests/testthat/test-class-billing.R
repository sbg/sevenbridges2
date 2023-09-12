test_that("Billing initialization works", {
  # Item object creation works
  testthat::expect_no_error(asBilling(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_billing_obj,
    classes = c("Item", "Billing"),
    public = c(
      "id", "owner", "name", "type", "pending", "disabled", "balance", "print",
      "reload", "analysis_breakdown", "storage_breakdown", "egress_breakdown"
    )
  )
})

test_that("Billing print method works", {
  testthat::expect_snapshot(setup_billing_obj$print())
})
