test_that("Apps initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Apps$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_apps_obj,
    classes = c("Resource", "Apps"),
    public = c("URL", "query", "get", "copy", "create")
  )
})

test_that("Apps query works", {
  # Setup test parameters for test
  test_no_project <- list(project = NULL, visibility = "private")
  test_bad_visibility <-
    list(project = setup_project_obj, visibility = "foo")
  test_query_terms_bad <-
    list(
      project = setup_project_obj,
      visibility = "private",
      query_term = list(1, 2, 3)
    )
  test_id_bad <-
    list(
      project = setup_project_obj,
      visibility = "private",
      id = 1
    )

  # Query fails when no project is provided
  testthat::expect_error(do.call(setup_apps_obj$query, test_no_project))

  # Query fails when no visibility parameter is provided
  testthat::expect_error(do.call(setup_apps_obj$query, test_bad_visibility))

  # Query fails when term is bad
  testthat::expect_error(do.call(setup_apps_obj$query, test_query_terms_bad))

  # Query fails when bad id is provided
  testthat::expect_error(do.call(setup_apps_obj$query, test_id_bad))
})

test_that("Apps get works", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)
  test_bad_revision <- list(id = "foo", revision = "bar")

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_apps_obj$get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_apps_obj$get, test_bad_id))

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_apps_obj$get, test_bad_revision))
})

test_that("Apps copy works", {
  # Setup test parameters for test
  test_no_app <- list(app = NULL, project = "foo")
  test_no_project <- list(app = "foo", project = NULL)
  test_bad_app <- list(app = 1, project = "bar")
  test_bad_project <- list(app = "foo", project = 1)
  test_bad_name <- list(
    app = "foo",
    project = "bar",
    name = 1
  )
  test_bad_strategy <-
    list(
      id = "foo",
      revision = "bar",
      strategy = "bad_str"
    )

  # Copy fails when no app is provided
  testthat::expect_error(do.call(setup_apps_obj$copy, test_no_app),
    regexp = "App parameter must be provided!",
    fixed = TRUE
  )

  # Copy fails when no project is provided
  testthat::expect_error(
    do.call(setup_apps_obj$copy, test_no_project),
    regexp = "Project parameter must be provided!",
    fixed = TRUE
  )

  # Copy fails when bad app is provided
  testthat::expect_error(do.call(setup_apps_obj$copy, test_bad_app))

  # Copy fails when bad project is provided
  testthat::expect_error(do.call(setup_apps_obj$copy, test_bad_project))

  # Copy fails when bad name is provided
  testthat::expect_error(do.call(setup_apps_obj$copy, test_bad_name))

  # Copy fails when bad strategy is provided
  testthat::expect_error(do.call(setup_apps_obj$copy, test_bad_strategy))
})

test_that("Apps create works", {
  # Setup test parameters for test
  test_no_raw_no_path <- list(raw = NULL, from_path = NULL)
  test_both_raw_and_path <-
    list(raw = list("1", "2", "3"), from_path = "cwl_path")
  test_no_project <-
    list(raw = list("1", "2", "3"), project = NULL)
  test_no_name <-
    list(
      raw = list("1", "2", "3"),
      project = "project_id",
      name = NULL
    )
  test_no_file_path <- list(
    from_path = "cwl_path",
    project = "project_id",
    name = "app_name"
  )
  test_bad_raw <- list(
    raw = 1,
    project = "project_id",
    name = "app_name"
  )
  test_bad_name <-
    list(
      raw = "cwl_string",
      project = "project_id",
      name = 1
    )

  # Create app fails when no raw cwl or no cwl path are provided
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_no_raw_no_path),
    regexp = "App raw body OR file path must be provided!",
    fixed = TRUE
  )

  # Create app fails when both raw cwl and cwl path are provided
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_both_raw_and_path),
    regexp = "Either raw body OR file path should be provided!",
    fixed = TRUE
  )

  # Create app fails when no project is provided
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_no_project),
    regexp = "Project parameter must be provided!",
    fixed = TRUE
  )

  # Create app fails when no app name is provided
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_no_name),
    regexp = "Name parameter must be provided!",
    fixed = TRUE
  )

  # Create app fails when wrong cwl format is provided
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_bad_raw)
  )

  # Create app fails when file path does not exist
  testthat::expect_error(
    do.call(setup_apps_obj$create, test_no_file_path)
  )

  # Create app fails when wrong cwl format is provided
  testthat::expect_error(do.call(setup_apps_obj$create, test_bad_name))
})
