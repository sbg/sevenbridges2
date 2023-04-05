test_that("Project member initialization works", {
  # Load predefined response needed for creating a member object
  test_member_response <- readRDS(testthat::test_path(
    "test_data",
    "add_project_member_response.RDS"
  ))

  # Create member object using the asMember helper function
  test_member <- asMember(x = test_member_response)

  # Check if all the expected fields are filled
  testthat::expect_equal(test_member$type, "USER")
  testthat::expect_equal(test_member$email, "ginny.weasley@hogwarts.com")
  testthat::expect_equal(test_member$username, "ginny_weasley")
  testthat::expect_equal(test_member$id, "ginny_weasley")
  # nolint start
  testthat::expect_equal(test_member$href, "https://api.sbgenomics.com/v2/projects/luna_lovegood/nargles-project/members/")
  # nolint end

  # Get permissions environment from test_member object
  permissions <- test_member$permissions

  # Convert Permission env to a list and keep only those elements that are
  # logical
  permissions_list <- as.list(permissions)
  permissions_list <- purrr::keep(permissions_list, .p = is.logical)

  # Set expected permissions list
  expected_permissions_list <- list(
    admin = FALSE,
    execute = TRUE,
    copy = TRUE,
    read = TRUE,
    write = TRUE,
    response = NA
  )

  keys <- names(permissions_list)

  # Compare the two lists
  testthat::expect_equal(
    permissions_list[keys],
    expected_permissions_list[keys]
  )
})

test_that("Project print method works", {
  member_obj_file <- testthat::test_path(
    "test_data",
    "ginny_weasley_member_object.RDS"
  )
  test_member <- readRDS(member_obj_file)
  testthat::expect_snapshot(test_member$print())
})




test_that("Function asMemberList works", {
  # Load predefined response needed for creating a member object
  test_member_response <- readRDS(testthat::test_path(
    "test_data",
    "add_project_member_response.RDS"
  ))

  # Create a list with 2 copies of test_member_response
  test_member_responses_list <- list(items = rep(list(test_member_response), 2))

  # Create a list of member objects using the asMemberList helper function
  test_members_list <- asMemberList(x = test_member_responses_list, auth = NULL)

  for (test_member in test_members_list) {
    # Check if all the expected fields are filled
    testthat::expect_equal(test_member$type, "USER")
    testthat::expect_equal(test_member$email, "ginny.weasley@hogwarts.com")
    testthat::expect_equal(test_member$username, "ginny_weasley")
    testthat::expect_equal(test_member$id, "ginny_weasley")
    # nolint start
    testthat::expect_equal(test_member$href, "https://api.sbgenomics.com/v2/projects/luna_lovegood/nargles-project/members/")
    # nolint end

    # Get permissions environment from test_member object
    permissions <- test_member$permissions

    # Convert Permission env to a list and keep only those elements that are
    # logical
    permissions_list <- as.list(permissions)
    permissions_list <- purrr::keep(permissions_list, .p = is.logical)

    # Set expected permissions list
    expected_permissions_list <- list(
      admin = FALSE,
      execute = TRUE,
      copy = TRUE,
      read = TRUE,
      write = TRUE,
      response = NA
    )

    keys <- names(permissions_list)

    # Compare the two lists
    testthat::expect_equal(
      permissions_list[keys],
      expected_permissions_list[keys]
    )
  }
})
