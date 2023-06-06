test_that("File print method works", {
  testthat::expect_snapshot(setup_file_obj$print())
})


test_that("File detailed_print method works", {
  testthat::expect_snapshot(setup_file_obj$detailed_print())
})

test_that("File update_details method works", {
  # Negative test use cases for name parameter
  testthat::expect_error(setup_file_obj$update_details(name = 1))
  testthat::expect_error(setup_file_obj$update_details(name = NULL))
  testthat::expect_error(setup_file_obj$update_details(name = TRUE))
  testthat::expect_error(setup_file_obj$update_details(name = list(a = "tet"))) # nolint

  # Negative test use cases for metadata parameter
  testthat::expect_error(setup_file_obj$update_details(metadata = "test"))
  testthat::expect_error(setup_file_obj$update_details(metadata = 1))
  testthat::expect_error(setup_file_obj$update_details(metadata = NULL))
  testthat::expect_error(setup_file_obj$update_details(metadata = TRUE))
  testthat::expect_error(setup_file_obj$update_details(metadata = c("test")))

  # Negative test use cases for tags parameter
  testthat::expect_error(setup_file_obj$update_details(tags = "test"))
  testthat::expect_error(setup_file_obj$update_details(tags = 1))
  testthat::expect_error(setup_file_obj$update_details(tags = NULL))
  testthat::expect_error(setup_file_obj$update_details(tags = TRUE))
  testthat::expect_error(setup_file_obj$update_details(tags = list(a = "test"))) # nolint
})

test_that("File set_metadata method works", {
  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(setup_file_obj$set_metadata(), "Metadata fields are missing. You need to provide at least one.")
  # nolint end
  testthat::expect_error(setup_file_obj$set_metadata(metadata = "test"))
  testthat::expect_error(setup_file_obj$set_metadata(metadata = 1))
  testthat::expect_error(setup_file_obj$set_metadata(metadata = NULL))
  testthat::expect_error(setup_file_obj$set_metadata(metadata = TRUE))
  testthat::expect_error(setup_file_obj$set_metadata(metadata = c("test")))
})

test_that("File copy_to method works", {
  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(setup_file_obj$copy_to(), "Project parameter is missing. You need to provide one.")
  # nolint end
  testthat::expect_error(setup_file_obj$copy_to(project = setup_project_obj, name = 1)) # nolint
  testthat::expect_error(setup_file_obj$copy_to(project = setup_project_obj, name = TRUE)) # nolint
  testthat::expect_error(setup_file_obj$copy_to(
    project = setup_project_obj,
    name = c("test")
  ))
  testthat::expect_error(setup_file_obj$copy_to(
    project = setup_project_obj,
    name = list("test")
  ))
})

test_that("File move_to_folder method works", {
  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(setup_file_obj$move_to_folder(), "Parent folder is missing. You need to provide one.")
  # nolint end
  testthat::expect_error(setup_file_obj$move_to_folder(
    parent = setup_folder_obj,
    name = 1
  ))
  testthat::expect_error(setup_file_obj$move_to_folder(
    parent = setup_folder_obj,
    name = TRUE
  ))
  testthat::expect_error(setup_file_obj$move_to_folder(
    parent = setup_folder_obj,
    name = c("test")
  ))
  testthat::expect_error(setup_file_obj$move_to_folder(
    parent = setup_folder_obj,
    name = list("test")
  ))
})
