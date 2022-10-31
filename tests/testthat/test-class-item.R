test_that("Item initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a user object
  test_user_response <- readRDS(testthat::test_path("test_data", "luna_resp.RDS"))

  # Create Item object
  test_item <- Item$new(href = "https://api.sbgenomics.com/v2/users/luna_lovegood",
                        response = test_user_response,
                        auth = test_auth_obj)

  testthat::expect_true(checkmate::test_class(test_item, classes = c("Item", "R6")))

  testthat::expect_equal(test_item$href, "https://api.sbgenomics.com/v2/users/luna_lovegood")

  # Check if the response field is as expected
  keys <- names(test_user_response)
  # Compare the two lists
  expect_equal(test_item$response[keys], test_user_response[keys])

  # Check auth field contents
  testthat::expect_equal(test_item$auth$platform, "aws-us")
  testthat::expect_equal(test_item$auth$url, "https://api.sbgenomics.com/v2/")
  testthat::expect_equal(test_item$auth$from, "direct")
  testthat::expect_equal(test_item$auth$sysenv_token, NULL)
  testthat::expect_equal(test_item$auth$sysenv_url, NULL)
  testthat::expect_equal(test_item$auth$config_file, NULL)
  testthat::expect_equal(test_item$auth$authorization, FALSE)
  testthat::expect_equal(test_item$auth$fs, NULL)
  testthat::expect_equal(test_item$auth$profile_name, NULL)
})
