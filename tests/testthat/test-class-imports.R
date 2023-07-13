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
