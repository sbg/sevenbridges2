test_that("File initialization works", {
  # Item object creation works
  testthat::expect_no_error(asFile(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_file_obj,
    classes = c("Item", "File"),
    public = c(
      "secondary_files", "type", "parent", "url", "metadata",
      "tags", "origin", "storage", "modified_on", "created_on",
      "project", "size", "name", "id", "URL", "submit_export",
      "download", "delete", "list_contents", "move_to_folder",
      "set_metadata", "get_metadata", "get_download_url", "copy_to",
      "add_tag", "update", "detailed_print", "print", "reload"
    )
  )
})

test_that("File print method works", {
  testthat::expect_snapshot(setup_file_obj$print())
})


test_that("File detailed_print method works", {
  testthat::expect_snapshot(setup_file_obj$detailed_print())
})

test_that("File update method throws error when expected", {
  # Setup test params for testing
  bad_name_param <- list(name = 123)
  bad_metadata_param <- list(metadata = 123)
  bad_tags_param <- list(tags = 123)

  # Test with invalid name param
  testthat::expect_error(
    do.call(setup_file_obj$update, bad_name_param),
    regexp = "Assertion on 'name' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # Test with invalid metadata param
  testthat::expect_error(
    do.call(setup_file_obj$update, bad_metadata_param),
    regexp = "Metadata parameter must be a named list of key-value pairs. For example: metadata <- list(metadata_key_1 = 'metadata_value_1', metadata_key_2 = 'metadata_value_2')", # nolint
    fixed = TRUE
  )

  # Test with invalid tags param
  testthat::expect_error(
    do.call(setup_file_obj$update, bad_tags_param),
    regexp = "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')", # nolint
    fixed = TRUE
  )

  # Test with all missing input params
  testthat::expect_error(
    do.call(setup_file_obj$update, list()),
    regexp = "Please provide updated information.", # nolint
    fixed = TRUE
  )
})

test_that("File add_tag method throws error when expected", {
  bad_tags_param <- list(tags = 123)
  missing_tag <- list(tags = NULL)
  bad_overwrite_param <- list(tags = list("tag1"), overwrite = 123)

  # Test with invalid tags param
  testthat::expect_error(
    do.call(setup_file_obj$add_tag, bad_tags_param),
    regexp = "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')", # nolint
    fixed = TRUE
  )
  # Test with missing tags param
  testthat::expect_error(
    do.call(setup_file_obj$add_tag, missing_tag),
    regexp = "Tags parameter is missing. You need to provide at least one.", # nolint
    fixed = TRUE
  )
  # Test with bad overwrite param
  testthat::expect_error(
    do.call(setup_file_obj$add_tag, bad_overwrite_param),
    regexp = "Assertion on 'overwrite' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("File copy_to method throws error when expected", {
  # Setup test params for testing
  missing_project_param <- list(project = NULL)
  bad_project_param1 <- list(project = 123)
  bad_project_param2 <- list(project = setup_app_obj)
  bad_name_param <- list(name = 123)

  # Test with missing project param
  testthat::expect_error(
    do.call(setup_file_obj$copy_to, missing_project_param),
    regexp = "Project parameter is missing. You need to provide one.", # nolint
    fixed = TRUE
  )

  # Test with bad project param
  testthat::expect_error(
    do.call(setup_file_obj$copy_to, bad_project_param1),
    regexp = "Assertion on 'project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$copy_to, bad_project_param2),
    regexp = "Assertion on 'project' failed: Must inherit from class 'Project', but has classes 'App','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Test with bad name param
  testthat::expect_error(
    do.call(setup_file_obj$copy_to, bad_name_param),
    regexp = "Project parameter is missing. You need to provide one.", # nolint
    fixed = TRUE
  )
})

test_that("File set_metadata method throws error when expected", {
  # Setup test params for testing
  missing_metadata_param <- list(metadata = NULL)
  bad_metadata_param <- list(metadata = 123)
  bad_overwrite_param <- list(
    metadata = list("field" = "value"),
    overwrite = 123
  )

  # Test with missing metadata param
  testthat::expect_error(
    do.call(setup_file_obj$set_metadata, missing_metadata_param),
    regexp = "Metadata fields are missing. You need to provide at least one.", # nolint
    fixed = TRUE
  )

  # Test with bad metadata param
  testthat::expect_error(
    do.call(setup_file_obj$set_metadata, bad_metadata_param),
    regexp = "Metadata parameter must be a named list of key-value pairs. For example: metadata <- list(metadata_key_1 = 'metadata_value_1', metadata_key_2 = 'metadata_value_2')", # nolint
    fixed = TRUE
  )

  # Test with bad overwrite param
  testthat::expect_error(
    do.call(setup_file_obj$set_metadata, bad_overwrite_param),
    regexp = "Assertion on 'overwrite' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("File move_to_folder method throws error when expected", {
  # Setup test params for testing
  missing_parent_param <- list(parent = NULL)
  bad_parent_param1 <- list(parent = 123)
  bad_parent_param2 <- list(parent = setup_file_obj)
  bad_name_param <- list(parent = "parent-id", name = 123)

  # Test with missing parent param
  testthat::expect_error(
    do.call(setup_file_obj$move_to_folder, missing_parent_param),
    regexp = "Parent folder is missing. You need to provide one.", # nolint
    fixed = TRUE
  )
  # Test with bad parent param
  testthat::expect_error(
    do.call(setup_file_obj$move_to_folder, bad_parent_param1),
    regexp = "Assertion on 'parent' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$move_to_folder, bad_parent_param2),
    regexp = "Parent must be a folder, not a file!", # nolint
    fixed = TRUE
  )
  # Test with bad name param
  testthat::expect_error(
    do.call(setup_file_obj$move_to_folder, bad_name_param),
    regexp = "Assertion on 'name' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("File download method throws error when expected", {
  # Setup test params for testing
  setup_file_obj$url <- "dowload/url"
  bad_directory_path_param <- list(directory_path = "non/existing/path")
  missing_filename_param <- list(
    directory_path = ".",
    filename = NULL
  )
  bad_filename_param <- list(
    directory_path = ".",
    filename = 123
  )
  bad_retry_param1 <- list(
    directory_path = ".",
    filename = "data.txt", retry_count = "23"
  )
  bad_retry_param2 <- list(
    directory_path = ".",
    filename = "data.txt", retry_count = -1
  )
  missing_retry_param <- list(
    directory_path = ".",
    filename = "data.txt", retry_count = NULL
  )
  bad_timeout_param1 <- list(
    directory_path = ".",
    filename = "data.txt",
    retry_count = 5, retry_timeout = "60"
  )
  bad_timeout_param2 <- list(
    directory_path = ".",
    filename = "data.txt",
    retry_count = 5, retry_timeout = -1
  )
  missing_timeout_param <- list(
    directory_path = ".",
    filename = "data.txt",
    retry_count = 5, retry_timeout = NULL
  )

  # Test with bad directory path
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_directory_path_param),
    regexp = "Destination directory non/existing/path does not exist.", # nolint
    fixed = TRUE
  )
  # Test with missing filename
  testthat::expect_error(
    do.call(setup_file_obj$download, missing_filename_param),
    regexp = "The filename parameter is missing.", # nolint
    fixed = TRUE
  )
  # Test with bad filename
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_filename_param),
    regexp = "The filename parameter should be a length-one string.", # nolint
    fixed = TRUE
  )
  # Test with bad retry param
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_retry_param1),
    regexp = "retry_count parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_retry_param2),
    regexp = "retry_count parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$download, missing_retry_param),
    regexp = "retry_count parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
  # Test with bad timeout param
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_timeout_param1),
    regexp = "retry_timeout parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$download, bad_timeout_param2),
    regexp = "retry_timeout parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_file_obj$download, missing_timeout_param),
    regexp = "retry_timeout parameter must be a positive integer number.", # nolint
    fixed = TRUE
  )
})
