test_that("User print method works", {
  user_obj_file <- testthat::test_path("test_data", "luna_lovegood_user_obj.RDS")
  test_user <- readRDS(user_obj_file)
  testthat::expect_snapshot(test_user$print())
})

test_that("User initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a user object
  test_user_response <- readRDS(testthat::test_path("test_data", "luna_resp.RDS"))

  # Create user object
  test_user <- asUser(x = test_user_response, auth = test_auth_obj)

  testthat::expect_true(checkmate::test_class(test_user, classes = c("User", "Item", "R6")))

  # Check if all the expected fields are filled
  testthat::expect_equal(test_user$country, "United Kingdom")
  testthat::expect_equal(test_user$affiliation, "Hogwarts")
  testthat::expect_equal(test_user$username, "luna_lovegood")
  testthat::expect_equal(test_user$first_name, "Luna")
  testthat::expect_equal(test_user$last_name, "Lovegood")
  testthat::expect_equal(test_user$email, "luna.lovegood@hogwarts.com")
  testthat::expect_equal(test_user$href, "https://api.sbgenomics.com/v2/users/luna_lovegood")

  # Check if superclass field auth is as expected
  testthat::expect_equal(test_user$auth$platform, "aws-us")
  testthat::expect_equal(test_user$auth$url, "https://api.sbgenomics.com/v2/")
})
