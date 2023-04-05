test_that("Billing group initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a billing group object
  test_billing_group_response <-
    readRDS(testthat::test_path("test_data", "ravenclaw_test_resp.RDS"))

  # Create billing group object
  test_billing_group <- asBilling(
    x = test_billing_group_response,
    auth = test_auth_obj
  )

  testthat::expect_true(
    checkmate::test_class(test_billing_group,
      classes = c("Billing", "Item", "R6")
    )
  )

  # Check if all the expected fields are filled
  testthat::expect_equal(
    test_billing_group$id,
    "asdfg123-1234-1234-ab12-7e7e7e777abc"
  )
  testthat::expect_equal(test_billing_group$owner, "luna_lovegood")
  testthat::expect_equal(test_billing_group$name, "Ravenclaw Test")
  testthat::expect_equal(test_billing_group$type, "regular")
  testthat::expect_equal(test_billing_group$pending, FALSE)
  testthat::expect_equal(test_billing_group$disabled, FALSE)
  # nolint start
  testthat::expect_equal(test_billing_group$href, "https://api.sbgenomics.com/v2/billing/groups/asdfg123-1234-1234-ab12-7e7e7e777abc")
  # nolint end
  testthat::expect_equal(test_billing_group$balance$currency, "Galleon [ʛ]")
  testthat::expect_equal(test_billing_group$balance$amount, 33333.3)


  # Check if superclass field auth is as expected
  testthat::expect_equal(test_billing_group$auth$platform, "aws-us")
  testthat::expect_equal(
    test_billing_group$auth$url,
    "https://api.sbgenomics.com/v2/"
  )
})







test_that("Function asBillingList works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a billing group object
  test_billing_group_response <-
    readRDS(testthat::test_path("test_data", "ravenclaw_test_resp.RDS"))

  # Create a list with 2 copies of test_billing_group_response
  test_billing_group_resp_list <- list(
    items = rep(list(test_billing_group_response), 2)
  )

  # Create a list of billing group objects using the asBillingList helper
  # function
  test_billing_group_list <- asBillingList(
    x = test_billing_group_resp_list,
    auth = test_auth_obj
  )


  for (test_billing_group in test_billing_group_list) {
    testthat::expect_true(
      checkmate::test_class(test_billing_group,
        classes = c("Billing", "Item", "R6")
      )
    )

    # Check if all the expected fields are filled
    testthat::expect_equal(
      test_billing_group$id,
      "asdfg123-1234-1234-ab12-7e7e7e777abc"
    )
    testthat::expect_equal(test_billing_group$owner, "luna_lovegood")
    testthat::expect_equal(test_billing_group$name, "Ravenclaw Test")
    testthat::expect_equal(test_billing_group$type, "regular")
    testthat::expect_equal(test_billing_group$pending, FALSE)
    testthat::expect_equal(test_billing_group$disabled, FALSE)
    # nolint start
    testthat::expect_equal(test_billing_group$href, "https://api.sbgenomics.com/v2/billing/groups/asdfg123-1234-1234-ab12-7e7e7e777abc")
    # nolint end
    testthat::expect_equal(test_billing_group$balance$currency, "Galleon [ʛ]")
    testthat::expect_equal(test_billing_group$balance$amount, 33333.3)


    # Check if superclass field auth is as expected
    testthat::expect_equal(test_billing_group$auth$platform, "aws-us")
    testthat::expect_equal(
      test_billing_group$auth$url,
      "https://api.sbgenomics.com/v2/"
    )
  }
})
