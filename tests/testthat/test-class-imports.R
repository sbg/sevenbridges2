test_that("Imports initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Imports$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_imports_obj,
    classes = c("Resource", "Imports"),
    public = c(
      "URL", "query", "get", "submit_import"
    )
  )
})

test_that("Imports get() throws error when needed", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_imports_obj$get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_imports_obj$get, test_bad_id))
})

test_that("Imports query() throws error when needed", {
  # 1. Test with invalid volume type
  bad_volume_obj <- list(volume = setup_project_obj)
  bad_volume_type <- list(volume = 1234)

  # Get fails when volume is of wrong class
  testthat::expect_error(do.call(setup_imports_obj$query, bad_volume_obj),
    regexp = "Assertion on 'volume' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Get fails when volume is of wrong data type
  testthat::expect_error(do.call(setup_imports_obj$query, bad_volume_type),
    regexp = "Assertion on 'volume' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 2. Test with invalid project type
  bad_project_obj <- list(project = setup_file_obj)
  bad_project_type <- list(project = 1234)

  # Get fails when project is of wrong class
  testthat::expect_error(do.call(setup_imports_obj$query, bad_project_obj),
    regexp = "Assertion on 'project' failed: Must inherit from class 'Project', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Get fails when project is of wrong data type
  testthat::expect_error(do.call(setup_imports_obj$query, bad_project_type),
    regexp = "Assertion on 'project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. Test with invalid state type
  bad_state_class <- list(state = setup_file_obj)
  bad_state_type <- list(state = 1234)
  invalid_state_values <- list(state = c("FAILED", "run"))
  outofrange_state_values <- list(
    state = c("PENDING", "RUNNING", "COMPLETED", "FAILED", "run")
  )

  testthat::expect_error(do.call(setup_imports_obj$query, bad_state_class),
    regexp = "Assertion on 'state' failed: Must be of type 'character', not 'File/Item/R6'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(do.call(setup_imports_obj$query, bad_state_type),
    regexp = "Assertion on 'state' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(do.call(setup_imports_obj$query, invalid_state_values),
    regexp = "Assertion on 'state' failed: Must be a subset of {'PENDING','RUNNING','COMPLETED','FAILED'}, but has additional elements {'run'}.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_imports_obj$query, outofrange_state_values),
    regexp = "Assertion on 'state' failed: Must have length <= 4, but has length 5.", # nolint
    fixed = TRUE
  )
})

test_that("Imports submit_import() throws error when needed", {
  # 1. Test with volume missing, but location set as string
  bad_volume_and_location <- list(
    source_volume = NULL,
    source_location = "location"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_volume_and_location),
    regexp = "Volume ID must be provided as string or Volume object.",
    fixed = TRUE
  )

  # 2. Test with volume of wrong class and type
  bad_volume_class <- list(
    source_volume = setup_project_obj,
    source_location = "location"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_volume_class),
    regexp = "Assertion on 'source_volume' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  bad_volume_type <- list(
    source_volume = 1234,
    source_location = "location"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_volume_type),
    regexp = "Assertion on 'source_volume' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 3. Test with location missing and of wrong type/class
  no_location <- list(source_volume = "volume-id", source_location = NULL)
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, no_location),
    regexp = "Source file/folder location/prefix must be provided as a string.",
    fixed = TRUE
  )
  bad_location_class_w_volume <- list(
    source_volume = "volume-id",
    source_location = setup_file_obj
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_location_class_w_volume),
    regexp = "Assertion on 'source_location' failed: Must be of type 'string', not 'File/Item/R6'", # nolint
    fixed = TRUE
  )
  bad_location_type <- list(
    source_volume = "volume-id",
    source_location = 1234
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_location_type),
    regexp = "Assertion on 'source_location' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  # 4. Test with project and parent params when both missing or both provided
  proj_parent_missing <- list(
    source_volume = "volume-id",
    source_location = "location"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, proj_parent_missing),
    regexp = "Please provide either destination project or parent parameter.",
    fixed = TRUE
  )
  proj_parent_provided <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = "project-id",
    destination_parent = "parent-id"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, proj_parent_provided),
    regexp = "Either destination project or parent parameter must be provided, not both.", # nolint
    fixed = TRUE
  )
  # 5. Test with invalid project parameter
  bad_project_class <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_file_obj
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_project_class),
    regexp = "Assertion on 'destination_project' failed: Must inherit from class 'Project', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )
  bad_project_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = 1234
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_project_type),
    regexp = "Assertion on 'destination_project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 6. Test with bad parent class/type and when File class with 'file' type
  bad_parent_class <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = setup_project_obj
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_parent_class),
    regexp = "Assertion on 'destination_parent' failed: Must inherit from class 'File', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  parent_file_class_type_file <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = setup_file_obj
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, parent_file_class_type_file),
    regexp = "Destination parent directory parameter must contain folder id or File object with type = 'folder'.", # nolint
    fixed = TRUE
  )
  bad_parent_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = 1234
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_parent_type),
    regexp = "Assertion on 'destination_parent' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 7. Test with bad name parameter
  bad_name_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    name = 1234
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_name_type),
    regexp = "Assertion on 'name' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # 8. Test with bad overwrite parameter
  bad_overwrite_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    overwrite = "TRUE"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_overwrite_type),
    regexp = "Assertion on 'overwrite' failed: Must be of type 'logical' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
  # 9. Test with bad autorename parameter
  bad_autorename_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    autorename = 1234
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_autorename_type),
    regexp = "Assertion on 'autorename' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # 10. Test with bad preserve_folder_structure parameter
  bad_preserve_f_structure_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    preserve_folder_structure = "FALSE"
  )
  testthat::expect_error(
    do.call(setup_imports_obj$submit_import, bad_preserve_f_structure_type),
    regexp = "Assertion on 'preserve_folder_structure' failed: Must be of type 'logical' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
})

test_that("Imports bulk_get() throws error when needed", {
  # Setup test parameters for test
  test_no_id <- list(imports = NULL)
  test_bad_id <- list(imports = 1)
  test_bad_id_nums <- list(imports = list(1, 2, 3))
  test_bad_id_objs <- list(imports = list(setup_project_obj, setup_file_obj))

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_imports_obj$bulk_get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_imports_obj$bulk_get, test_bad_id))
  testthat::expect_error(do.call(setup_imports_obj$bulk_get, test_bad_id_nums))
  testthat::expect_error(do.call(setup_imports_obj$bulk_get, test_bad_id_objs))
})

test_that("Imports bulk_submit_import() throws error when needed", {
  # 1. Test with items parameter being set as NULL or of some non-list type
  items_null <- list(items = NULL)
  testthat::expect_error(
    do.call(setup_imports_obj$bulk_submit_import, items_null),
    regexp = "Items parameter should be set as a nested list of information on files/folders you want to import.", # nolint
    fixed = TRUE
  )
  items_non_list <- list(items = c("import"))
  testthat::expect_error(
    do.call(setup_imports_obj$bulk_submit_import, items_non_list),
    regexp = "Assertion on 'items' failed: Must be of type 'list', not 'character'.", # nolint
    fixed = TRUE
  )

  # 2. Test with volume missing, but location set as string
  bad_volume_and_location <- list(
    source_volume = NULL,
    source_location = "location"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_volume_and_location)),
    regexp = "Volume ID must be provided as string or Volume object in element 1.", # nolint
    fixed = TRUE
  )

  # 3. Test with volume of wrong class and type
  bad_volume_class <- list(
    source_volume = setup_project_obj,
    source_location = "location"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_volume_class)),
    regexp = "Assertion on 'item[[\"source_volume\"]]' failed: Must inherit from class 'Volume', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  bad_volume_type <- list(
    source_volume = 1234,
    source_location = "location"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_volume_type)),
    regexp = "Assertion on 'item[[\"source_volume\"]]' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 4. Test with location missing and of wrong type/class
  no_location <- list(source_volume = "volume-id", source_location = NULL)
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(no_location)),
    regexp = "Source file/folder location/prefix must be provided as a string in element 1.", # nolint
    fixed = TRUE
  )
  bad_location_class_w_volume <- list(
    source_volume = "volume-id",
    source_location = setup_file_obj
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_location_class_w_volume)), # nolint
    regexp = "Assertion on 'item[[\"source_location\"]]' failed: Must be of type 'string', not 'File/Item/R6'", # nolint
    fixed = TRUE
  )
  bad_location_type <- list(
    source_volume = "volume-id",
    source_location = 1234
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_location_type)),
    regexp = "Assertion on 'item[[\"source_location\"]]' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  # 5. Test with project and parent params when both missing or both provided
  ## in element 1
  proj_parent_missing <- list(
    source_volume = "volume-id",
    source_location = "location"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(proj_parent_missing)),
    regexp = "Please provide either destination project or parent parameter in element 1.", # nolint
    fixed = TRUE
  )
  ## in element 2
  proj_parent_missing <- list(
    list(
      source_volume = "volume-id",
      source_location = "location",
      destination_project = "project-id"
    ),
    list(
      source_volume = "volume-id",
      source_location = "location"
    )
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = proj_parent_missing),
    regexp = "Please provide either destination project or parent parameter in element 2.", # nolint
    fixed = TRUE
  )
  proj_parent_provided <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = "project-id",
    destination_parent = "parent-id"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(proj_parent_provided)),
    regexp = "Either destination project or parent parameter must be provided in element 1, not both.", # nolint
    fixed = TRUE
  )
  # 6. Test with invalid project parameter
  bad_project_class <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_file_obj
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_project_class)),
    regexp = "Assertion on 'item[[\"destination_project\"]]' failed: Must inherit from class 'Project', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )
  bad_project_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = 1234
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_project_type)),
    regexp = "Assertion on 'item[[\"destination_project\"]]' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 7. Test with bad parent class/type and when File class with 'file' type
  bad_parent_class <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = setup_project_obj
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_parent_class)),
    regexp = "Assertion on 'item[[\"destination_parent\"]]' failed: Must inherit from class 'File', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
  parent_file_class_type_file <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = setup_file_obj
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(parent_file_class_type_file)), # nolint
    regexp = "Destination parent directory parameter must contain folder id or File object with type = 'folder' in element 1.", # nolint
    fixed = TRUE
  )
  bad_parent_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_parent = 1234
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_parent_type)),
    regexp = "Assertion on 'item[[\"destination_parent\"]]' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # 8. Test with bad name parameter
  bad_name_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    name = 1234
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_name_type)),
    regexp = "Assertion on 'item[[\"name\"]]' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # 9. Test with bad autorename parameter
  bad_autorename_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    autorename = 1234
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_autorename_type)),
    regexp = "Assertion on 'item[[\"autorename\"]]' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # 10. Test with bad preserve_folder_structure parameter
  bad_preserve_f_structure_type <- list(
    source_volume = "volume-id",
    source_location = "location",
    destination_project = setup_project_obj,
    preserve_folder_structure = "FALSE"
  )
  testthat::expect_error(
    setup_imports_obj$bulk_submit_import(items = list(bad_preserve_f_structure_type)), # nolint
    regexp = "Assertion on 'item[[\"preserve_folder_structure\"]]' failed: Must be of type 'logical' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
})
