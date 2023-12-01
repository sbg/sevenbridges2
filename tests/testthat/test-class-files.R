test_that("Files initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Files$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_files_obj,
    classes = c("Resource", "Files"),
    public = c("URL", "query", "get", "copy", "create_folder")
  )
})

test_that("Files query() throws error when expected", {
  # Setup test parameters for test
  test_project_parent_missing <- list(project = NULL, parent = NULL)
  test_proj_parent_not_missing <- list(
    project = "project-id",
    parent = "parent-id"
  )
  test_bad_project1 <- list(project = 123)
  test_bad_project2 <- list(project = setup_app_obj)
  test_bad_parent1 <- list(parent = 123)
  test_bad_parent2 <- list(parent = setup_app_obj)

  test_bad_name <- list(name = 123)
  test_bad_metadata <- list(project = "project-id", metadata = 123)
  test_bad_origin1 <- list(project = "project-id", origin = 123)
  test_bad_origin2 <- list(project = "project-id", origin = setup_file_obj)
  test_bad_tag <- list(project = "project-id", tag = 12)

  # Query fails when project and parent param are both missing
  testthat::expect_error(
    do.call(setup_files_obj$query, test_project_parent_missing),
    regexp = "No project or parent directory was defined. You must provide one of the two!", # nolint
    fixed = TRUE
  )

  # Query fails when project and parent param are both not missing
  testthat::expect_error(
    do.call(setup_files_obj$query, test_proj_parent_not_missing),
    regexp = "Project and parent parameters are mutually exclusive. You must provide one of the two, not both.", # nolint
    fixed = TRUE
  )

  # Query fails when project param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_project1),
    regexp = "Assertion on 'project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_project2),
    regexp = "Assertion on 'project' failed: Must inherit from class 'Project', but has classes 'App','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Query fails when parent param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_parent1),
    regexp = "Assertion on 'parent' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_parent2),
    regexp = "Assertion on 'parent' failed: Must inherit from class 'File', but has classes 'App','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Query fails when name param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_name),
    regexp = "Assertion on 'name' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # Query fails when metadata param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_metadata),
    regexp = "Metadata parameter must be a named list of key-value pairs. For example: metadata <- list(metadata_key_1 = 'metadata_value_1', metadata_key_2 = 'metadata_value_2')", # nolint
    fixed = TRUE
  )

  # Query fails when origin param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_origin1),
    regexp = "Assertion on 'origin' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # Query fails when origin param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_origin2),
    regexp = "Assertion on 'origin' failed: Must inherit from class 'Task', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  # Query fails when origin param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$query, test_bad_tag),
    regexp = "Assertion on 'tag' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Files get() throws error when expected", {
  # Setup test parameters for test
  test_bad_id <- list(id = 123)
  test_missing_id <- list(id = NULL)

  # Get fails when id param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$get, test_bad_id),
    regexp = "Assertion on 'id' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$get, test_missing_id),
    regexp = "Please provide id parameter!", # nolint
    fixed = TRUE
  )
})

test_that("Files delete() throws error when needed", {
  # Setup test parameters for test
  test_no_file <- list(file = NULL)
  test_bad_file <- list(file = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_files_obj$delete, test_no_file))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_files_obj$delete, test_bad_file))
})

test_that("Files copy() throws error when expected", {
  missing_files_dest_proj_params <- list(
    files = NULL,
    destination_project = NULL
  )
  bad_files_param1 <- list(
    files = 123,
    destination_project = "dest_proj"
  )
  bad_files_param2 <- list(
    files = list(123, 234),
    destination_project = "dest_proj"
  )
  bad_files_param3 <- list(
    files = list(setup_project_obj),
    destination_project = "dest_proj"
  )

  bad_project_param1 <- list(
    files = list(setup_file_obj),
    destination_project = 123
  )
  bad_project_param2 <- list(
    files = list(setup_file_obj),
    destination_project = setup_file_obj
  )

  # Copy fails when params are missing
  testthat::expect_error(
    do.call(setup_files_obj$copy, missing_files_dest_proj_params),
    regexp = "Parameter 'files' or 'destination_project' is missing. You need to provide both of them.", # nolint
    fixed = TRUE
  )

  # Copy fails when files param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$copy, bad_files_param1),
    regexp = "Assertion on 'files' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$copy, bad_files_param2),
    regexp = "Assertion on 'files' failed: May only contain the following types: {File}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$copy, bad_files_param3),
    regexp = "Assertion on 'files' failed: May only contain the following types: {File}, but element 1 has type 'Project,Item,R6'.", # nolint
    fixed = TRUE
  )

  # Copy fails when project param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$copy, bad_project_param1),
    regexp = "Assertion on 'destination_project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$copy, bad_project_param2),
    regexp = "Assertion on 'destination_project' failed: Must inherit from class 'Project', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )
})

test_that("Files create_folder() throws error when expected", {
  missing_name_param <- list(name = NULL)
  bad_name_param1 <- list(name = 123)
  bad_name_param2 <- list(name = "Folder name")
  bad_name_param3 <- list(name = "__start_invalid")

  missing_parent_project <- list(
    name = "folder_name",
    parent = NULL,
    project = NULL
  )
  not_missing_parent_project <- list(
    name = "folder_name",
    parent = "parent-id",
    project = "project-id"
  )

  bad_parent_param1 <- list(
    name = "folder_name",
    parent = 123
  )
  bad_parent_param2 <- list(
    name = "folder_name",
    parent = setup_app_obj
  )
  bad_parent_param3 <- list(
    name = "folder_name",
    parent = setup_file_obj
  )

  bad_project_param1 <- list(
    name = "folder_name",
    project = 123
  )
  bad_project_param2 <- list(
    name = "folder_name",
    project = setup_app_obj
  )

  # Copy fails when name is missing
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, missing_name_param),
    regexp = "Please, provide the folder's name.", # nolint
    fixed = TRUE
  )

  # Copy fails when name param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_name_param1),
    regexp = "Assertion on 'name' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_name_param2),
    regexp = "The folder name cannot contain spaces in the name.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_name_param3),
    regexp = "The folder name cannot start with \"__\"", # nolint
    fixed = TRUE
  )

  # Copy fails when project and parent are both missing
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, missing_parent_project),
    regexp = "Both the project name and parent folder ID are missing. You need to provide one of them.", # nolint
    fixed = TRUE
  )

  # Copy fails when project and parent are both not missing
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, not_missing_parent_project),
    regexp = "You should specify a project or a parent folder, not both.", # nolint
    fixed = TRUE
  )

  # Copy fails when parent param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_parent_param1),
    regexp = "Assertion on 'parent' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_parent_param2),
    regexp = "Assertion on 'parent' failed: Must inherit from class 'File', but has classes 'App','Item','R6'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_parent_param3),
    regexp = "The provided parent object is not a folder.", # nolint
    fixed = TRUE
  )

  # Copy fails when project param is invalid
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_project_param1),
    regexp = "Assertion on 'project' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_files_obj$create_folder, bad_project_param2),
    regexp = "Assertion on 'project' failed: Must inherit from class 'Project', but has classes 'App','Item','R6'.", # nolint
    fixed = TRUE
  )
})
