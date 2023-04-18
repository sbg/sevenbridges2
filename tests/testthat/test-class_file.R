test_that("File print method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)
  testthat::expect_snapshot(test_file$print())
})


test_that("File detailed_print method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)
  testthat::expect_snapshot(test_file$detailed_print())
})

test_that("File update_details method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)

  # Negative test use cases for name parameter
  testthat::expect_error(test_file$update_details(name = 1))
  testthat::expect_error(test_file$update_details(name = NULL))
  testthat::expect_error(test_file$update_details(name = TRUE))
  testthat::expect_error(test_file$update_details(name = c("test")))
  testthat::expect_error(test_file$update_details(name = list(a = "tet")))

  # Negative test use cases for metadata parameter
  testthat::expect_error(test_file$update_details(metadata = "test"))
  testthat::expect_error(test_file$update_details(metadata = 1))
  testthat::expect_error(test_file$update_details(metadata = NULL))
  testthat::expect_error(test_file$update_details(metadata = TRUE))
  testthat::expect_error(test_file$update_details(metadata = c("test")))

  # Negative test use cases for tags parameter
  testthat::expect_error(test_file$update_details(tags = "test"))
  testthat::expect_error(test_file$update_details(tags = 1))
  testthat::expect_error(test_file$update_details(tags = NULL))
  testthat::expect_error(test_file$update_details(tags = TRUE))
  testthat::expect_error(test_file$update_details(tags = list(a = "test")))
})

test_that("File set_metadata method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)

  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(test_file$set_metadata(), "Metadata fields are missing. You need to provide at least one.")
  # nolint end
  testthat::expect_error(test_file$set_metadata(metadata = "test"))
  testthat::expect_error(test_file$set_metadata(metadata = 1))
  testthat::expect_error(test_file$set_metadata(metadata = NULL))
  testthat::expect_error(test_file$set_metadata(metadata = TRUE))
  testthat::expect_error(test_file$set_metadata(metadata = c("test")))
})

test_that("File copy_to method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)

  project_obj <- testthat::test_path(
    "test_data",
    "luna_lovegood_project_obj.RDS"
  )
  test_project <- readRDS(project_obj)

  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(file$copy_to(), "Project parameter is missing. You need to provide one.")
  # nolint end
  testthat::expect_error(file$copy_to(project = project_obj, name = 1))
  testthat::expect_error(file$copy_to(project = project_obj, name = TRUE))
  testthat::expect_error(file$copy_to(project = project_obj, name = c("test")))
  testthat::expect_error(file$copy_to(
    project = project_obj,
    name = list("test")
  ))
})

test_that("File move_to_folder method works", {
  file_obj <- testthat::test_path(
    "test_data",
    "file_object.RDS"
  )
  test_file <- readRDS(file_obj)

  folder_obj <- testthat::test_path(
    "test_data",
    "folder_object.RDS"
  )
  test_folder <- readRDS(folder_obj)

  # Negative test use cases for metadata parameter
  # nolint start
  testthat::expect_error(file$move_to_folder(), "Parent folder is missing. You need to provide one.")
  # nolint end
})
