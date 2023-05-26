test_that("App print method works", {
  testthat::expect_snapshot(setup_app_obj$print())
})

test_that("App copy method works", {
  # Negative test use cases for project parameter
  testthat::expect_error(setup_app_obj$copy(project = NULL))
  invalid_project_params <- list(1, TRUE, list(a = "test"), setup_file_obj)
  for (invalid_param in invalid_project_params) {
    testthat::expect_error(setup_app_obj$copy(project = invalid_param))
  }

  # Negative test use cases for name parameter
  # nolint start
  invalid_name_params <- list(1, TRUE, list(a = "test"), setup_file_obj)
  for (invalid_param in invalid_name_params) {
    testthat::expect_error(setup_app_obj$copy(project = setup_project_obj, name = invalid_param))
  }
  # nolint end

  # Negative test use cases for strategy parameter
  # nolint start
  testthat::expect_error(setup_app_obj$copy(project = setup_project_obj, strategy = NULL), "Please provide the copy strategy.")
  invalid_strategy_params <- list(1, TRUE, list(a = "test"), setup_file_obj, "test_strategy")
  for (invalid_param in invalid_strategy_params) {
    testthat::expect_error(setup_app_obj$copy(project = setup_project_obj, strategy = invalid_param))
  }
  # nolint end

  # Negative test use cases for use_revision parameter
  # nolint start
  testthat::expect_error(setup_app_obj$copy(project = setup_project_obj, use_revision = NULL))
  invalid_use_revision_params <- list(1, list(a = "test"), c(TRUE, FALSE), "test_revision")
  for (invalid_param in invalid_use_revision_params) {
    testthat::expect_error(setup_app_obj$copy(project = setup_project_obj, use_revision = invalid_param))
  }
  # nolint end
})



test_that("App get_revision method works", {
  # Negative test use cases for revision parameter
  testthat::expect_error(setup_app_obj$get_revision(revision = NULL))
  invalid_revision_number_params <- list("test_revision", TRUE, list(a = "test"), c(1, 2, 3)) # nolint
  for (invalid_param in invalid_revision_number_params) {
    testthat::expect_error(setup_app_obj$get_revision(revision = invalid_param)) # nolint
  }

  # Negative test use cases for in_place parameter
  invalid_in_place_params <- c(NULL, NA, c(TRUE, FALSE), 1, "text", setup_file_obj) # nolint
  for (invalid_param in invalid_in_place_params) {
    testthat::expect_error(setup_app_obj$get_revision(revision = 2, in_place = invalid_param)) # nolint
  }
})


test_that("App create_revision method works", {
  # Negative test use cases for raw parameter
  invalid_raw_params <- list(1, "test_raw", TRUE, c(1, 2, 3))
  for (invalid_param in invalid_raw_params) {
    testthat::expect_error(setup_app_obj$create_revision(raw = invalid_param)) # nolint
  }

  # Negative test use cases for from_path parameter
  # nolint start
  invalid_from_path_params <- list(1, 3.14, list(a = "test"), TRUE, c(1, 2, 3), "invalid/path/to/file")
  for (invalid_param in invalid_from_path_params) {
    testthat::expect_error(setup_app_obj$create_revision(from_path = invalid_param)) # nolint
  }
  # nolint end

  # Negative test use case for both params (raw, from_path) missing
  # nolint start
  testthat::expect_error(setup_app_obj$create_revision(raw = NULL, from_path = NULL))
  # "Both parameters raw and from_path are missing. Please provide one of them.", fixed = TRUE)

  # nolint end

  # Negative test use case for both params (raw, from_path) provided
  # nolint start
  testthat::expect_error(setup_app_obj$create_revision(raw = setup_raw_cwl, from_path = setup_app_path))
  # "Both parameters raw and from_path are provided. Please use only one of them.", fixed = TRUE)
  # nolint end

  # Negative test use cases for raw_format parameter
  invalid_raw_format_params <- c(NULL, NA, c(TRUE, FALSE), 1, "text", setup_file_obj) # nolint
  for (invalid_param in invalid_raw_format_params) {
    testthat::expect_error(setup_app_obj$create_revision(raw = setup_raw_cwl, raw_format = invalid_param)) # nolint
  }

  # Negative test use cases for in_place parameter
  invalid_in_place_params <- c(NULL, NA, c(TRUE, FALSE), 1, "text", setup_file_obj) # nolint
  for (invalid_param in invalid_in_place_params) {
    testthat::expect_error(setup_app_obj$create_revision(raw = setup_raw_cwl, in_place = invalid_param)) # nolint
  }
})
