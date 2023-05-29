test_that("App initialization works", {
  # Resource object creation works
  testthat::expect_no_error(App$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_app_obj,
    classes = c("Item", "App"),
    public = c("URL", "id", "project", "name", "revision", "copy_of", "latest_revision", "raw") # nolint
  )
})

test_that("App print method works", {
  testthat::expect_snapshot(setup_app_obj$print())
})

test_that("App copy method works", {
  # Setup test parameters for test
  test_no_project <- list(project = NULL)
  test_bad_project <- list(project = 1)
  test_bad_name <- list(
    project = setup_project_obj,
    name = 1
  )
  test_bad_strategy <- list(
    project = setup_project_obj,
    strategy = "bad_str"
  )
  test_bad_use_revision <- list(
    project = setup_project_obj,
    use_revision = "bar"
  )

  # Copy fails when no app is provided
  testthat::expect_error(do.call(setup_app_obj$copy, test_no_project),
    regexp = "Project parameter must be provided!",
    fixed = TRUE
  )

  # Copy fails when bad project is provided
  testthat::expect_error(do.call(setup_app_obj$copy, test_bad_project))

  # Copy fails when bad name is provided
  testthat::expect_error(do.call(setup_app_obj$copy, test_bad_name))

  # Copy fails when bad strategy is provided
  testthat::expect_error(do.call(setup_app_obj$copy, test_bad_strategy))

  # Copy fails when bad use_revision is provided
  testthat::expect_error(do.call(setup_app_obj$copy, test_bad_use_revision))
})


test_that("App get_revision method works", {
  # Setup test parameters for test
  test_bad_revision <- list(
    revision = "bar"
  )
  test_bad_in_place <- list(
    in_place = "bar"
  )

  # Get revision fails when bad revision is provided
  testthat::expect_error(do.call(setup_app_obj$get_revision, test_bad_revision)) # nolint

  # Get revision fails when bad in_place is provided
  testthat::expect_error(do.call(setup_app_obj$get_revision, test_bad_in_place)) # nolint
})


test_that("App create_revision method works", {
  # Setup test parameters for test
  test_no_raw_no_path <- list(raw = NULL, from_path = NULL)
  test_both_raw_and_path <-
    list(raw = "cwl_string", from_path = "cwl_path")
  test_bad_raw <- list(
    raw = 1
  )
  test_bad_file_path <- list(
    from_path = 1
  )
  test_nonexisting_file_path <- list(
    from_path = "invalid/path/to/file"
  )
  test_bad_raw_format <- list(
    raw_format = "BED"
  )
  test_bad_in_place <- list(
    in_place = "bar"
  )

  # Create revision fails when no raw cwl or no cwl path are provided
  # nolint start
  testthat::expect_error(
    do.call(setup_app_obj$create_revision, test_no_raw_no_path),
    regexp = "Both parameters raw and from_path are missing. Please provide one of them.",
    fixed = TRUE
  )
  # nolint end

  # Create revision fails when both raw cwl and cwl path are provided
  # nolint start
  testthat::expect_error(
    do.call(setup_app_obj$create_revision, test_both_raw_and_path),
    regexp = "Both parameters raw and from_path are provided. Please use only one of them.",
    fixed = TRUE
  )
  # nolint end

  # Create revision fails when bad raw parameter is provided
  testthat::expect_error(do.call(setup_app_obj$create_revision, test_bad_raw)) # nolint

  # Create revision fails when bad file_path is provided
  testthat::expect_error(do.call(setup_app_obj$create_revision, test_bad_file_path)) # nolint


  # Create revision fails when non-existing file_path is provided
  testthat::expect_error(
    do.call(setup_app_obj$create_revision, test_nonexisting_file_path),
    regexp = "File invalid/path/to/file does not exist.",
    fixed = TRUE
  )

  # Create revision fails when bad raw_format is provided
  testthat::expect_error(do.call(setup_app_obj$create_revision, test_bad_raw_format)) # nolint

  # Create revision fails when bad raw_format is provided
  testthat::expect_error(do.call(setup_app_obj$create_revision, test_bad_raw_format)) # nolint

  # Create revision fails when bad in_place is provided
  testthat::expect_error(do.call(setup_app_obj$get_revision, test_bad_in_place)) # nolint
})
