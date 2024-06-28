test_that("prepare_items_for_bulk_export() throws error when needed", {
  # 1. Test with files parameter being set as NULL or of some non-list type
  files_null <- NULL
  testthat::expect_error(
    prepare_items_for_bulk_export(files = files_null, destination_volume = "volume-id"), # nolint
    regexp = "Parameter 'files' is missing. Please provide either a list of File objects, or a list of file IDs.", # nolint
    fixed = TRUE
  )
  files_non_list <- c(123, 456)
  testthat::expect_error(
    prepare_items_for_bulk_export(files = files_non_list, destination_volume = "volume-id"), # nolint
    regexp = "Assertion on 'files' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  # 2. Test with wrong file type/class
  valid_file_class <- setup_file_obj
  invalid_file_class <- setup_project_obj
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(valid_file_class, invalid_file_class), destination_volume = "volume-id"), # nolint
    regexp = "Assertion on 'files' failed: May only contain the following types: {character,File}, but element 2 has type 'Project,Item,R6'.", # nolint
    fixed = TRUE
  )

  folder_type_file <- setup_folder_obj
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(valid_file_class, folder_type_file), destination_volume = "volume-id"), # nolint
    regexp = "Provided list contains folder objects, which cannot be exported. Please make sure to remove all folder objects (type = 'folder') and try again.", # nolint
    fixed = TRUE
  )

  # 3. Test with missing destination_volume
  file_id_1 <- "file_1_id"
  file_id_2 <- "file_2_id"
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2)), # nolint
    regexp = "Missing 'destination_volume' parameter. Destination volume must be provided either as a string or a Volume object.", # nolint
    fixed = TRUE
  )

  bad_destination_volume_type <- 1234
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2), destination_volume = bad_destination_volume_type), # nolint
    regexp = "Assertion on 'destination_volume' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2), destination_volume = setup_project_obj), # nolint
    regexp = "Assertion on 'destination_volume' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  # 4. Test with destination_location_prefix missing of wrong type
  bad_location_prefix <- 1234
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2), destination_volume = "volume-id", destination_location_prefix = bad_location_prefix), # nolint
    regexp = "Assertion on 'destination_location_prefix' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # 5. Test with bad overwrite parameter
  bad_overwrite_type <- "TRUE"
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2), destination_volume = "volume-id", overwrite = bad_overwrite_type), # nolint
    regexp = "Assertion on 'overwrite' failed: Must be of type 'logical' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )

  # 6. Test with bad properties parameter
  bad_properties_type <- "list"
  testthat::expect_error(
    prepare_items_for_bulk_export(files = list(file_id_1, file_id_2), destination_volume = "volume-id", properties = bad_properties_type), # nolint
    regexp = "Assertion on 'properties' failed: Must be of type 'list' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
})

test_that("prepare_items_for_bulk_export() works as expected", {
  file_1 <- setup_file_obj
  file_2 <- setup_file_obj
  destination_volume <- "volume-id"
  overwrite <- TRUE
  properties <- list(
    sse_algorithm = "AES256"
  )

  expected_output_list <- list(
    list(
      "source_file" = file_1,
      "destination_volume" = destination_volume,
      "destination_location" = file_1$name, # nolint
      "overwrite" = overwrite,
      "properties" = properties
    ),
    list(
      "source_file" = file_2,
      "destination_volume" = destination_volume,
      "destination_location" = file_2$name, # nolint
      "overwrite" = overwrite,
      "properties" = properties
    )
  )

  output <- prepare_items_for_bulk_export(
    files = list(file_1, file_2), # nolint
    destination_volume = destination_volume, # nolint-
    destination_location_prefix = NULL,
    overwrite = overwrite,
    properties = properties
  )

  testthat::expect_equal(output, expected_output_list)
})
