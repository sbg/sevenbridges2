test_that("Exports initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Exports$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_exports_obj,
    classes = c("Resource", "Exports"),
    public = c(
      "URL", "query", "get", "submit_export"
    )
  )
})

test_that("Exports get() throws error when needed", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_exports_obj$get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_exports_obj$get, test_bad_id))
})

test_that("Exports query() throws error when needed", {
  # 1. Test with invalid volume type
  bad_volume_obj <- list(volume = setup_project_obj)
  bad_volume_type <- list(volume = 1234)

  # Get fails when volume is of wrong class
  testthat::expect_error(do.call(setup_exports_obj$query, bad_volume_obj),
    regexp = "Assertion on 'volume' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Get fails when volume is of wrong data type
  testthat::expect_error(do.call(setup_exports_obj$query, bad_volume_type),
    regexp = "Assertion on 'volume' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 2. Test with invalid state type
  bad_state_class <- list(state = setup_file_obj)
  bad_state_type <- list(state = 1234)
  invalid_state_values <- list(state = c("FAILED", "run"))
  outofrange_state_values <- list(
    state = c("PENDING", "RUNNING", "COMPLETED", "FAILED", "run")
  )

  testthat::expect_error(do.call(setup_exports_obj$query, bad_state_class),
    regexp = "Assertion on 'state' failed: Must be of type 'character', not 'File/Item/R6'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(do.call(setup_exports_obj$query, bad_state_type),
    regexp = "Assertion on 'state' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(do.call(setup_exports_obj$query, invalid_state_values),
    regexp = "Assertion on 'state' failed: Must be a subset of {'PENDING','RUNNING','COMPLETED','FAILED'}, but has additional elements {'run'}.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_exports_obj$query, outofrange_state_values),
    regexp = "Assertion on 'state' failed: Must have length <= 4, but has length 5.", # nolint
    fixed = TRUE
  )
})

test_that("Exports submit_export() throws error when needed", {
  # 1. Test with source file missing and of wrong type/class
  no_file <- list(source_file = NULL)
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, no_file),
    regexp = "Source file must be provided as a string or File object!", # nolint
    fixed = TRUE
  )
  bad_file_class <- list(source_file = setup_project_obj)
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_file_class),
    regexp = "Assertion on 'source_file' failed: Must inherit from class 'File', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  folder_type_file <- list(source_file = setup_folder_obj)
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, folder_type_file),
    regexp = "Folders cannot be exported. Please, provide single file id or File object with type = 'file'.", # nolint
    fixed = TRUE
  )

  # 2. Test with missing destination_volume or of wrong class and type
  no_dest_volume <- list(
    source_file = "file-id"
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, no_dest_volume),
    regexp = "Destination volume must be provided as a string or Volume object!", # nolint
    fixed = TRUE
  )
  bad_volume_class <- list(
    destination_volume = setup_project_obj,
    source_file = "file-id"
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_volume_class),
    regexp = "Assertion on 'destination_volume' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  bad_volume_type <- list(
    destination_volume = 1234,
    source_file = "file-id"
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_volume_type),
    regexp = "Assertion on 'destination_volume' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. Test with destination_location missing and of wrong type/class
  no_location <- list(
    source_file = "file-id",
    destination_volume = "volume-id",
    destination_location = NULL
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, no_location),
    regexp = "Destination location name must be provided as a string!",
    fixed = TRUE
  )
  bad_location_type <- list(
    source_file = "file-id",
    destination_volume = "volume-id",
    destination_location = 1234
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_location_type),
    regexp = "Assertion on 'destination_location' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )

  # 4. Test with bad overwrite parameter
  bad_overwrite_type <- list(
    source_file = "file-id",
    destination_volume = "volume-id",
    destination_location = "file_name.txt",
    overwrite = "TRUE"
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_overwrite_type),
    regexp = "Assertion on 'overwrite' failed: Must be of type 'logical' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
  # 5. Test with bad copy_only parameter
  bad_copyonly_type <- list(
    source_file = "file-id",
    destination_volume = "volume-id",
    destination_location = "file_name.txt",
    copy_only = 1234
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_copyonly_type),
    regexp = "Assertion on 'copy_only' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # 6. Test with bad properties parameter
  bad_properties_type <- list(
    source_file = "file-id",
    destination_volume = "volume-id",
    destination_location = "file_name.txt",
    properties = "list"
  )
  testthat::expect_error(
    do.call(setup_exports_obj$submit_export, bad_properties_type),
    regexp = "Assertion on 'properties' failed: Must be of type 'list' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
})
