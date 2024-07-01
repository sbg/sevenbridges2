test_that("prepare_volume_import_item() throws error when needed", {
  # 1. Test with volume_item parameter being set as NULL
  volume_item_null <- NULL
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = volume_item_null,
      destination_field_name = "destination_project"
    ),
    regexp = "Parameter 'volume_item' is missing. Please provide either a VolumeFile or a VolumePrefix object.", # nolint
    fixed = TRUE
  )

  # 2. Test with a wrong volume_item type/class
  volume_item_bad_type <- 1234
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = volume_item_bad_type,
      destination_field_name = "destination_project"
    ),
    regexp = "Please make sure that the provided 'volume_item' parameter is either a VolumeFile or a VolumePrefix object.", # nolint
    fixed = TRUE
  )

  volume_item_bad_class <- setup_project_obj
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = volume_item_bad_class,
      destination_field_name = "destination_project"
    ),
    regexp = "Please make sure that the provided 'volume_item' parameter is either a VolumeFile or a VolumePrefix object.", # nolint
    fixed = TRUE
  )

  # 3. Test with missing and wrong destination_field_name parameter
  valid_volume_item <- setup_volume_file_obj
  dest_field_name_missing <- NULL
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = dest_field_name_missing
    ), # nolint
    regexp = "Parameter 'destination_field_name' is missing. It should be one of the following two strings: 'destination_project' or 'destination_parent'", # nolint
    fixed = TRUE
  )

  dest_field_name_bad_type <- 1234
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = dest_field_name_bad_type
    ), # nolint
    regexp = "Assertion on 'destination_field_name' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )

  dest_field_name_invalid_value <- "destination_unknown"
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = dest_field_name_invalid_value
    ), # nolint
    regexp = "Assertion on 'destination_field_name' failed: Must be a subset of {'destination_project','destination_parent'}, but has additional elements {'destination_unknown'}.", # nolint
    fixed = TRUE
  )

  # 4. Test with destination parameter missing of wrong type
  valid_volume_item <- setup_volume_file_obj
  valid_destination_field_name <- "destination_project"
  destination_missing <- NULL
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = valid_destination_field_name, # nolint
      destination = destination_missing
    ),
    regexp = "Parameter 'destination' is missing.", # nolint
    fixed = TRUE
  )

  destination_bad_type <- 1234
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = valid_destination_field_name, # nolint
      destination = destination_bad_type
    ),
    regexp = "Assertion on 'destination' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )

  # 5. Test with bad autorename parameter
  valid_volume_item <- setup_volume_file_obj
  valid_destination_field_name <- "destination_project"
  valid_destination <- "23182321u9323jk1d321"
  bad_autorename_type <- "TRUE"
  # nolint start
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = valid_destination_field_name,
      destination = valid_destination, autorename = bad_autorename_type
    ),
    regexp = "Assertion on 'autorename' failed: Must be of type 'logical', not 'character'.",
    fixed = TRUE
  )
  # nolint end

  # 6. Test with bad preserve_folder_structure parameter
  bad_preserve_folder_str_type <- "TRUE"
  # nolint start
  testthat::expect_error(
    prepare_volume_import_item(
      volume_item = valid_volume_item,
      destination_field_name = valid_destination_field_name,
      destination = valid_destination,
      preserve_folder_structure = bad_preserve_folder_str_type
    ),
    regexp = "Assertion on 'preserve_folder_structure' failed: Must be of type 'logical', not 'character'.",
    fixed = TRUE
  )
  # nolint end
})

test_that("prepare_volume_import_item() works as expected provided with a VolumeFile", { # nolint
  valid_volume_item <- setup_volume_file_obj
  destination_field_name <- "destination_project"
  destination <- "23182321u9323jk1d321"
  autorename <- TRUE

  expected_output_list <- list(
    "source_volume" = valid_volume_item$volume,
    "source_location" = valid_volume_item$location,
    "autorename" = TRUE,
    "destination_project" = "23182321u9323jk1d321"
  )

  output <- prepare_volume_import_item(
    volume_item = valid_volume_item,
    destination_field_name = destination_field_name,
    autorename = autorename,
    destination = destination
  )

  testthat::expect_equal(output, expected_output_list)
})

test_that("prepare_volume_import_item() works as expected provided with a VolumePrefix", { # nolint
  valid_volume_item <- setup_volume_prefix_obj
  destination_field_name <- "destination_parent"
  destination <- "23182321u9323jk1d321"
  autorename <- TRUE
  preserve_folder_structure <- TRUE

  expected_output_list <- list(
    "source_volume" = valid_volume_item$volume,
    "source_location" = valid_volume_item$prefix,
    "preserve_folder_structure" = TRUE,
    "autorename" = TRUE,
    "destination_parent" = "23182321u9323jk1d321"
  )

  output <- prepare_volume_import_item(
    volume_item = valid_volume_item,
    destination_field_name = destination_field_name,
    destination = destination,
    autorename = autorename,
    preserve_folder_structure = preserve_folder_structure
  )

  testthat::expect_equal(output, expected_output_list)
})

test_that("prepare_items_for_bulk_import() throws error when needed", {
  # 1. Test with volume_items parameter being set as NULL or some non-list type
  volume_items_null <- NULL
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = volume_items_null),
    regexp = "Parameter 'volume_items' is missing. Please provide a list of VolumeFile or VolumePrefix objects.", # nolint
    fixed = TRUE
  )

  volume_items_non_list <- c(123, 456)
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = volume_items_non_list),
    regexp = "Assertion on 'volume_items' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  # 2. Test with wrong file type/class
  valid_volume_items_class <- setup_volume_file_obj
  invalid_volume_items_class <- setup_project_obj
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_items_class, invalid_volume_items_class)), # nolint
    regexp = "Assertion on 'volume_items' failed: May only contain the following types: {VolumeFile,VolumePrefix}, but element 2 has type 'Project,Item,R6'.", # nolint
    fixed = TRUE
  )

  # 3. Test with destination_project and destination_parent both missing
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(setup_volume_file_obj)), # nolint
    regexp = "Please specify either the 'destination_project' or 'destination_parent' parameter.", # nolint
    fixed = TRUE
  )

  # 4. Test with destination_project and destination_parent both provided
  valid_volume_item <- setup_volume_file_obj
  valid_destination_project <- setup_project_obj
  valid_destination_parent <- setup_folder_obj
  # nolint start
  testthat::expect_error(
    prepare_items_for_bulk_import(
      volume_items = list(valid_volume_item),
      destination_project = valid_destination_project,
      destination_parent = valid_destination_parent
    ),
    regexp = "Either destination project or parent parameter must be provided, not both.",
    fixed = TRUE
  )
  # nolint end

  # 5. Test with invalid destination_project type/class
  valid_volume_item <- setup_volume_file_obj

  bad_destination_project_type <- 1234
  # nolint start
  testthat::expect_error(
    prepare_items_for_bulk_import(
      volume_items = list(valid_volume_item),
      destination_project = bad_destination_project_type
    ),
    regexp = "Assertion on 'destination_project' failed: Must be of type 'character', not 'double'.",
    fixed = TRUE
  )
  # nolint end

  # nolint start
  bad_destination_project_class <- setup_file_obj
  testthat::expect_error(
    prepare_items_for_bulk_import(
      volume_items = list(valid_volume_item),
      destination_project = bad_destination_project_class
    ),
    regexp = "Assertion on 'destination_project' failed: Must inherit from class 'Project', but has classes 'File','Item','R6'.",
    fixed = TRUE
  )
  # nolint end

  # 6. Test with invalid destination_parent type/class
  bad_destination_parent_type <- 1234
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_item), destination_parent = bad_destination_parent_type), # nolint
    regexp = "Assertion on 'destination_parent' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  bad_dest_parent_class_project <- setup_project_obj
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_item), destination_parent = bad_dest_parent_class_project), # nolint
    regexp = "Assertion on 'destination_parent' failed: Must inherit from class 'File', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  bad_dest_parent_class_file <- setup_file_obj
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_item), destination_parent = bad_dest_parent_class_file), # nolint
    regexp = "Destination parent directory parameter must contain folder ID or File object with type = 'folder'.", # nolint
    fixed = TRUE
  )

  # 7. Test with bad autorename type
  valid_volume_item <- setup_volume_file_obj
  valid_destination_project <- setup_project_obj
  bad_autorename_type <- "FALSE"
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_item), destination_project = valid_destination_project, autorename = bad_autorename_type), # nolint
    regexp = "Assertion on 'autorename' failed: Must be of type 'logical', not 'character'.", # nolint
    fixed = TRUE
  )

  # 8. Test with bad preserve_folder_structure type
  valid_volume_item <- setup_volume_file_obj
  valid_destination_parent <- setup_folder_obj
  bad_preserve_folder_str_type <- "TRUE"
  testthat::expect_error(
    prepare_items_for_bulk_import(volume_items = list(valid_volume_item), destination_parent = valid_destination_parent, preserve_folder_structure = bad_preserve_folder_str_type), # nolint
    regexp = "Assertion on 'preserve_folder_structure' failed: Must be of type 'logical', not 'character'.", # nolint
    fixed = TRUE
  )
})

test_that("prepare_items_for_bulk_import() works as expected provided with the list of VolumeFile objects", { # nolint
  volume_file_obj_1 <- setup_volume_file_obj
  volume_file_obj_2 <- setup_volume_file_obj
  destination_project <- setup_project_obj
  autorename <- TRUE

  expected_output_list <- list(
    list(
      "source_volume" = volume_file_obj_1$volume,
      "source_location" = volume_file_obj_1$location,
      "autorename" = TRUE,
      "destination_project" = setup_project_obj$id
    ),
    list(
      "source_volume" = volume_file_obj_1$volume,
      "source_location" = volume_file_obj_1$location,
      "autorename" = TRUE,
      "destination_project" = setup_project_obj$id
    )
  )

  output <- prepare_items_for_bulk_import(
    volume_items = list(volume_file_obj_1, volume_file_obj_2),
    destination_project = destination_project,
    autorename = autorename
  )

  testthat::expect_equal(output, expected_output_list)
})

test_that("prepare_items_for_bulk_import() works as expected provided with the list of VolumePrefix objects", { # nolint
  volume_prefix_obj_1 <- setup_volume_prefix_obj
  volume_prefix_obj_2 <- setup_volume_prefix_obj
  destination_parent <- setup_folder_obj
  autorename <- TRUE
  preserve_folder_structure <- TRUE

  expected_output_list <- list(
    list(
      "source_volume" = volume_prefix_obj_1$volume,
      "source_location" = volume_prefix_obj_1$prefix,
      "preserve_folder_structure" = TRUE,
      "autorename" = TRUE,
      "destination_parent" = setup_folder_obj$id
    ),
    list(
      "source_volume" = volume_prefix_obj_2$volume,
      "source_location" = volume_prefix_obj_2$prefix,
      "preserve_folder_structure" = TRUE,
      "autorename" = TRUE,
      "destination_parent" = setup_folder_obj$id
    )
  )

  output <- prepare_items_for_bulk_import(
    volume_items = list(volume_prefix_obj_1, volume_prefix_obj_2),
    destination_parent = destination_parent,
    autorename = autorename,
    preserve_folder_structure = preserve_folder_structure
  )

  testthat::expect_equal(output, expected_output_list)
})
