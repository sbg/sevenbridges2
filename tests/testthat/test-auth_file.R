testthat::test_that("Getting file by id fails when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path(
    "test_data",
    "auth_obj_files.RDS"
  ))

  invalid_ids <- c("", NULL, 34323223424)
  for (id in invalid_ids) {
    testthat::expect_error(test_auth_obj$get_file(id))
  }
})

testthat::test_that("Coping files into a project fails when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth_obj_files.RDS"))

  # Invalid files parameter
  invalid_files_obj <- c("", NULL, list(), 232424, "232424242")
  for (id in invalid_files_obj) {
    testthat::expect_error(test_auth_obj$copy_files(list(id), "dest-project"))
  }

  # Invalid project parameter
  invalid_project_obj <- c("", NULL, list(), 232424, NA)
  example_file_obj <- File$new(id = "file-id", name = "file-name", type = "file")
  for (id in invalid_project_obj) {
    testthat::expect_error(test_auth_obj$copy_files(list(example_file_obj), id))
  }
})

testthat::test_that("Create folder fails when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth_obj_files.RDS"))

  # Test when setting both parent and project params
  testthat::expect_error(
    test_auth_obj$create_folder("my-new-folder",
      parent = "parent-folder",
      project = "destination-project"
    ),
    "You should specify a project name or a parent folder ID, not both."
  )

  # Test when not setting either parent or project params
  testthat::expect_error(
    test_auth_obj$create_folder("my-new-folder"),
    "Both the project name and parent folder ID are missing. You need to provide one of them."
  )

  # Setting invalid parent param
  invalid_parent_param <- c("", NULL, list(), 232424, NA)
  for (id in invalid_parent_param) {
    testthat::expect_error(test_auth_obj$create_folder("my-new-folder", parent = id))
  }
  example_file_obj <- File$new(id = "file-id", name = "file-name", type = "file")
  testthat::expect_error(
    test_auth_obj$create_folder("my-new-folder", parent = example_file_obj),
    "The provided parent object is not a folder."
  )

  # Setting invalid project param
  invalid_project_obj <- c("", NULL, list(), 232424, NA, example_file_obj)
  for (id in invalid_project_obj) {
    testthat::expect_error(test_auth_obj$create_folder("my-new-folder", project = id))
  }
})
