test_that("Project initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a project object
  test_project_response <- readRDS(testthat::test_path(
    "test_data",
    "get_project_response.RDS"
  ))

  # Create project object using the asProject helper function
  test_project <- asProject(x = test_project_response, auth = test_auth_obj)

  testthat::expect_true(checkmate::test_class(test_project,
    classes = c("Project", "Item", "R6")
  ))

  # Check if all the expected fields are filled
  testthat::expect_equal(test_project$category, "PRIVATE")
  testthat::expect_equal(test_project$modified_on, "2022-12-20T16:09:20Z")
  testthat::expect_equal(test_project$created_on, "2022-12-20T16:09:20Z")
  testthat::expect_equal(test_project$created_by, "luna_lovegood")
  testthat::expect_equal(test_project$root_folder, "12a1ab12345a12345a12345a")
  testthat::expect_equal(test_project$type, "v2")
  testthat::expect_equal(
    test_project$billing_group,
    "ab12345a-123a-1234-1234-a1ab12a12ab1"
  )
  testthat::expect_equal(test_project$name, "nargles-project")
  testthat::expect_equal(test_project$id, "luna_lovegood/nargles-project")
  # nolint start
  testthat::expect_equal(test_project$href, "https://api.sbgenomics.com/v2/projects/luna_lovegood/nargles-project")
  # nolint end

  # Get settings list from test_project object
  settings_list <- test_project$settings

  # Set expected settings list
  expected_settings_list <- list(
    locked = FALSE,
    controlled = FALSE,
    location = "aws:us-east-1",
    use_interruptible_instances = TRUE,
    use_memoization = FALSE,
    intermediate_files = list(duration = 24, retention = "LIMITED"),
    allow_network_access = FALSE,
    use_elastic_disk = FALSE
  )

  keys <- names(settings_list)

  # Compare the two lists
  expect_equal(settings_list[keys], expected_settings_list[keys])

  # Get permissions list from test project object
  permissions_list <- test_project$permissions

  # # Set expected permissions list
  expected_permissions_list <- list(
    write = TRUE,
    read = TRUE,
    copy = TRUE,
    execute = TRUE,
    admin = TRUE
  )

  permissions_keys <- names(permissions_list)

  # Compare the two lists
  expect_equal(
    permissions_list[permissions_keys],
    expected_permissions_list[permissions_keys]
  )

  # Check if superclass field auth is as expected
  testthat::expect_equal(test_project$auth$platform, "aws-us")
  testthat::expect_equal(
    test_project$auth$url,
    "https://api.sbgenomics.com/v2/"
  )
})

test_that("Project print method works", {
  project_obj_file <- testthat::test_path(
    "test_data",
    "luna_lovegood_project_obj.RDS"
  )
  test_project <- readRDS(project_obj_file)
  testthat::expect_snapshot(test_project$print())
})


test_that("Project detailed_print method works", {
  project_obj_file <- testthat::test_path(
    "test_data",
    "luna_lovegood_project_obj.RDS"
  )
  test_project <- readRDS(project_obj_file)
  testthat::expect_snapshot(test_project$detailed_print())
})


test_that("Project list_apps method works", {
  test_query_terms_bad <-
    list(
      query_term = list(1, 2, 3)
    )
  test_id_bad <-
    list(
      id = 1,
      limit = 10,
      offset = 3
    )
  # Query fails when term is bad
  testthat::expect_error(
    do.call(setup_project_obj$list_apps, test_query_terms_bad),
    regexp = "Assertion on 'query_terms' failed: May only contain the following types: {character}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )
  # Query fails when bad id is provided
  testthat::expect_error(
    do.call(setup_project_obj$list_apps, test_id_bad),
    regexp = "Assertion on 'id' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})


test_that("Project create_app method works", {
  test_no_raw_no_path <- list(raw = NULL, from_path = NULL)
  test_both_raw_and_path <-
    list(raw = list("1", "2", "3"), from_path = "cwl_path")
  test_no_name <-
    list(
      raw = list("1", "2", "3"),
      name = NULL
    )
  # Create app fails when no raw cwl or no cwl path are provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_no_raw_no_path),
    regexp = "App raw body OR file path must be provided!",
    fixed = TRUE
  )
  # Create app fails when both raw cwl and cwl path are provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_both_raw_and_path),
    regexp = "Either raw body OR file path should be provided!",
    fixed = TRUE
  )
  # Create app fails when no app name is provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_no_name),
    regexp = "Name parameter must be provided!",
    fixed = TRUE
  )
})


test_that("Function asProjectList works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))

  # Load predefined response needed for creating a project object
  test_project_response <- readRDS(testthat::test_path(
    "test_data",
    "get_project_response.RDS"
  ))

  # Create a list with 2 copies of test_project_response
  test_project_responses_list <- list(
    items = rep(list(test_project_response), 2)
  )

  # Create a list of project objects using the asProjectList helper function
  test_project_list <- asProjectList(
    x = test_project_responses_list,
    auth = test_auth_obj
  )

  for (test_project in test_project_list) {
    testthat::expect_true(checkmate::test_class(test_project,
      classes = c("Project", "Item", "R6")
    ))

    # Check if all the expected fields are filled
    testthat::expect_equal(test_project$category, "PRIVATE")
    testthat::expect_equal(test_project$modified_on, "2022-12-20T16:09:20Z")
    testthat::expect_equal(test_project$created_on, "2022-12-20T16:09:20Z")
    testthat::expect_equal(test_project$created_by, "luna_lovegood")
    testthat::expect_equal(test_project$root_folder, "12a1ab12345a12345a12345a")
    testthat::expect_equal(test_project$type, "v2")
    testthat::expect_equal(
      test_project$billing_group,
      "ab12345a-123a-1234-1234-a1ab12a12ab1"
    )
    testthat::expect_equal(test_project$name, "nargles-project")
    testthat::expect_equal(test_project$id, "luna_lovegood/nargles-project")
    # nolint start
    testthat::expect_equal(test_project$href, "https://api.sbgenomics.com/v2/projects/luna_lovegood/nargles-project")
    # nolint end

    # Get settings list from test_project object
    settings_list <- test_project$settings

    # Set expected settings list
    expected_settings_list <- list(
      locked = FALSE,
      controlled = FALSE,
      location = "aws:us-east-1",
      use_interruptible_instances = TRUE,
      use_memoization = FALSE,
      intermediate_files = list(duration = 24, retention = "LIMITED"),
      allow_network_access = FALSE,
      use_elastic_disk = FALSE
    )

    keys <- names(settings_list)

    # Compare the two lists
    expect_equal(settings_list[keys], expected_settings_list[keys])


    # Get permissions list from test project object
    permissions_list <- test_project$permissions

    # # Set expected permissions list
    expected_permissions_list <- list(
      write = TRUE,
      read = TRUE,
      copy = TRUE,
      execute = TRUE,
      admin = TRUE
    )

    permissions_keys <- names(permissions_list)

    # Compare the two lists
    expect_equal(
      permissions_list[permissions_keys],
      expected_permissions_list[permissions_keys]
    )

    # Check if superclass field auth is as expected
    testthat::expect_equal(test_project$auth$platform, "aws-us")
    testthat::expect_equal(
      test_project$auth$url,
      "https://api.sbgenomics.com/v2/"
    )
  }
})
