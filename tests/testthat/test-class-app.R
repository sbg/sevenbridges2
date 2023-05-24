test_that("App print method works", {
  app_obj <- testthat::test_path(
    "test_data",
    "app_object.RDS"
  )
  test_app <- readRDS(app_obj)
  testthat::expect_snapshot(test_app$print())
})


test_that("App copy method works", {
  app_obj <- testthat::test_path(
    "test_data",
    "app_object.RDS"
  )
  test_app <- readRDS(app_obj)


  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)

  # Negative test use cases for project parameter
  testthat::expect_error(test_app$copy(project = NULL))
  invalid_project_params <- list(1, TRUE, list(a = "test"), test_file)
  for (invalid_param in invalid_project_params) {
    testthat::expect_error(test_app$copy(project = invalid_param))
  }

  # Negative test use cases for name parameter
  # nolint start
  testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", name = NULL))
  invalid_project_params <- list(1, TRUE, list(a = "test"), test_file)
  for (invalid_param in invalid_project_params) {
    testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", name = invalid_param))
  }
  # nolint end

  # Negative test use cases for strategy parameter
  # nolint start
  testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", strategy = NULL), "Please provide the copy strategy.")
  invalid_strategy_params <- list(1, TRUE, list(a = "test"), test_file, "test_strategy")
  for (invalid_param in invalid_strategy_params) {
    testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", name = invalid_param))
  }
  # nolint end

  # Negative test use cases for use_revision parameter
  # nolint start
  testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", use_revision = NULL))
  invalid_use_revision_params <- list(1, list(a = "test"), c(TRUE, FALSE), "test_revision")
  for (invalid_param in invalid_use_revision_params) {
    testthat::expect_error(test_app$copy(project = "luna_lovegood/nargles-project", use_revision = invalid_param))
  }
  # nolint end
})



test_that("App get_revision method works", {
  app_obj <- testthat::test_path(
    "test_data",
    "app_object.RDS"
  )
  test_app <- readRDS(app_obj)

  # Negative test use cases for revision parameter
  testthat::expect_error(test_app$get_revision(revision = NULL))
  invalid_revision_number_params <- list("test_revision", TRUE, list(a = "test"), c(1, 2, 3)) # nolint
  for (invalid_param in invalid_revision_number_params) {
    testthat::expect_error(test_app$get_revision(revision = invalid_param)) # nolint
  }
})


test_that("App create_revision method works", {
  app_obj <- testthat::test_path(
    "test_data",
    "app_object.RDS"
  )
  test_app <- readRDS(app_obj)

  # Negative test use cases for raw parameter
  invalid_raw_params <- list(1, "test_raw", TRUE, c(1, 2, 3)) # nolint
  for (invalid_param in invalid_raw_params) {
    testthat::expect_error(test_app$create_revision(raw = invalid_param)) # nolint
  }

  # Negative test use cases for file_path parameter
  # nolint start
  invalid_file_path_params <- list(1, 3.14, list(a = "test"), TRUE, c(1, 2, 3))
  for (invalid_param in invalid_file_path_params) {
    testthat::expect_error(test_app$create_revision(file_path = invalid_file_path_params))
  }
  # nolint end

  # Negative test use case for both params (raw, file_path) missing
  # nolint start
  testthat::expect_error(test_app$create_revision(raw = NULL, file_path = NULL), "Both parameters raw and file_path are missing. Please provide one of them.")
  # nolint end

  # Negative test use case for both params (raw, file_path) provided
  # nolint start
  testthat::expect_error(test_app$create_revision(raw = list(class = "CommandLineTool"), file_path = "path/to/test_app.cwl"), "Both parameters raw and file_path are provided. Please use only one of them.")
  # nolint end
})
