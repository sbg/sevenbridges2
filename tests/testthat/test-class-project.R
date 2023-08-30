test_that("Project initialization works", {
  # Item object creation works
  testthat::expect_no_error(Project$new(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_project_obj,
    classes = c("Item", "Project"),
    public = c(
      "category", "permissions", "modified_on", "created_on", "created_by",
      "root_folder", "settings", "tags", "type", "description",
      "billing_group", "name", "id", "create_task", "list_imports",
      "list_tasks", "create_app", "list_apps", "get_root_folder",
      "create_folder", "list_files", "modify_member_permissions", "get_member",
      "remove_member", "add_member", "list_members", "delete", "update",
      "detailed_print", "URL", "print"
    )
  )
})

test_that("Project print method works", {
  testthat::expect_snapshot(setup_project_obj$print())
})

test_that("Project detailed_print method works", {
  testthat::expect_snapshot(setup_project_obj$detailed_print())
})

test_that("Project update method throws error when expected", {
  bad_name_param <- list(name = 1)
  bad_description_param <- list(description = 1)
  bad_billing_param1 <- list(billing = 1)
  bad_billing_param2 <- list(billing = setup_file_obj)
  bad_tags_param <- list(tags = 1)
  bad_settings_param <- list(settings = 1)

  # Test with invalid name param
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_name_param),
    regexp = "Assertion on 'name' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Test with invalid description param
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_description_param),
    regexp = "Assertion on 'description' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Test with invalid billing param1
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_billing_param1),
    regexp = "Assertion on 'billing_group' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
  # Test with invalid billing param2
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_billing_param2),
    regexp = "Assertion on 'billing_group' failed: Must inherit from class 'Billing', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )
  # Test with invalid tags param
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_tags_param),
    regexp = "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')", # nolint
    fixed = TRUE
  )
  # Test with invalid settings param
  testthat::expect_error(
    do.call(setup_project_obj$update, bad_settings_param),
    regexp = "Settings must be provided as a list.", # nolint
    fixed = TRUE
  )
})

test_that("Project create_folder method throws error when expected", {
  missing_folder_name <- list(name = NULL)
  bad_format_folder_name <- list(name = 123)
  spaces_folder_name <- list(name = "name with spaces")
  invalid_folder_name <- list(name = "__start_with_underscores")

  # Test with missing_folder_name
  testthat::expect_error(
    do.call(setup_project_obj$create_folder, missing_folder_name),
    regexp = "Please, provide the folder's name.",
    fixed = TRUE
  )
  # Test with bad_format_folder_name
  testthat::expect_error(
    do.call(setup_project_obj$create_folder, bad_format_folder_name),
    regexp = "Assertion on 'name' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
  # Test with spaces_folder_name
  testthat::expect_error(
    do.call(setup_project_obj$create_folder, spaces_folder_name),
    regexp = "The folder name cannot contain spaces in the name.",
    fixed = TRUE
  )
  # Test with invalid_folder_name
  testthat::expect_error(
    do.call(setup_project_obj$create_folder, invalid_folder_name),
    regexp = "The folder name cannot start with \"__\"",
    fixed = TRUE
  )
})

test_that("Project list_apps method works", {
  test_query_terms_bad <-
    list(
      query_term = list(1, 2, 3)
    )
  test_id_bad <-
    list(
      id = 1,
      limit = 10,
      offset = 3
    )
  # Query fails when term is bad
  testthat::expect_error(
    do.call(setup_project_obj$list_apps, test_query_terms_bad),
    regexp = "Assertion on 'query_terms' failed: May only contain the following types: {character}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )
  # Query fails when bad id is provided
  testthat::expect_error(
    do.call(setup_project_obj$list_apps, test_id_bad),
    regexp = "Assertion on 'id' failed: Must be of type 'string' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})


test_that("Project create_app method works", {
  test_no_raw_no_path <- list(raw = NULL, from_path = NULL)
  test_both_raw_and_path <-
    list(raw = list("1", "2", "3"), from_path = "cwl_path")
  test_no_name <-
    list(
      raw = list("1", "2", "3"),
      name = NULL
    )
  # Create app fails when no raw cwl or no cwl path are provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_no_raw_no_path),
    regexp = "App raw body OR file path must be provided!",
    fixed = TRUE
  )
  # Create app fails when both raw cwl and cwl path are provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_both_raw_and_path),
    regexp = "Either raw body OR file path should be provided!",
    fixed = TRUE
  )
  # Create app fails when no app name is provided
  testthat::expect_error(
    do.call(setup_project_obj$create_app, test_no_name),
    regexp = "Name parameter must be provided!",
    fixed = TRUE
  )
})

test_that("Project add_member method throws error when expected", {
  # Both user and email are missing
  testthat::expect_error(
    setup_project_obj$add_member(),
    regexp = "Neither username nor email are provided. You must provide at least one of these parameters before you can add a user to a project.", # nolint
    fixed = TRUE
  )

  # Pass invalid user param
  testthat::expect_error(
    setup_project_obj$add_member(
      user = File$new(id = "file-id"),
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$add_member(
      user = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid email parameter
  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-user", email = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'email' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid permissions params
  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-username",
      permissions = 1234
    ),
    regexp = "Assertion on 'permissions' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-username",
      permissions = list()
    ),
    regexp = "Assertion on 'permissions' failed: Must have length >= 1, but has length 0.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-username",
      permissions = list(
        read = 123, copy = FALSE, execute = FALSE, write = 234,
        admin = FALSE
      )
    ),
    regexp = "Assertion on 'permissions' failed: May only contain the following types: {logical}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, execute = FALSE, admin = FALSE, write = FALSE,
        run = TRUE
      )
    ),
    regexp = "Assertion on 'permissions' failed: Must have length <= 5, but has length 6.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$add_member(
      user = "test-username",
      permissions = list(
        readme = TRUE, copyme = TRUE, admin = FALSE, write = FALSE,
        execute = TRUE
      )
    ),
    regexp = "Assertion on 'names(permissions)' failed: Must be a subset of {'read','copy','execute','write','admin'}, but has additional elements {'readme','copyme'}.", # nolint
    fixed = TRUE
  )
})

test_that("Project remove_member method throws error when expected", {
  # Pass invalid user param
  testthat::expect_error(
    setup_project_obj$remove_member(user = NULL),
    regexp = "Please provide a username for the user or project member you want to remove from the project.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$remove_member(
      user = File$new(id = "file-id")
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$remove_member(
      user = 1234
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Project get_member method throws error when expected", {
  # Pass invalid user param
  testthat::expect_error(
    setup_project_obj$get_member(user = NULL),
    regexp = "Please provide a username or Member object.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$get_member(
      user = File$new(id = "file-id")
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$get_member(
      user = 1234
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Project modify_member_permissions method throws error when expected", { # nolint
  # Pass invalid user param
  testthat::expect_error(
    setup_project_obj$modify_member_permissions(),
    regexp = "Please provide a username or Member object.",
    fixed = TRUE
  )
  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = File$new(id = "file-id"),
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid permissions params
  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = "test-username",
      permissions = 1234
    ),
    regexp = "Assertion on 'permissions' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = "test-username",
      permissions = list()
    ),
    regexp = "Assertion on 'permissions' failed: Must have length >= 1, but has length 0.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = "test-username",
      permissions = list(read = 123, copy = FALSE, admin = FALSE)
    ),
    regexp = "Assertion on 'permissions' failed: May only contain the following types: {logical}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE, execute = TRUE,
        run = TRUE
      )
    ),
    regexp = "Assertion on 'permissions' failed: Must have length <= 5, but has length 6.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_project_obj$modify_member_permissions(
      user = "test-username",
      permissions = list(readme = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'names(permissions)' failed: Must be a subset of {'read','copy','execute','write','admin'}, but has additional elements {'readme'}.", # nolint
    fixed = TRUE
  )
})
