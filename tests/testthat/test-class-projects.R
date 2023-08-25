test_that("Projects initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Projects$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_projects_obj,
    classes = c("Resource", "Projects"),
    public = c("URL", "query", "get", "create")
  )
})

test_that("Projects query() throws error when expected", {
  # Setup test parameters for test
  test_bad_name <- list(name = 123)
  test_bad_owner <- list(owner = 123)
  test_bad_tags <- list(tags = 123)

  # Query fails when project param is not valid
  testthat::expect_error(do.call(setup_projects_obj$query, test_bad_name))

  # Query fails when owner param is not valid
  testthat::expect_error(do.call(setup_projects_obj$query, test_bad_owner))

  # Query fails when tags param is not valid
  testthat::expect_error(
    do.call(setup_projects_obj$query, test_bad_tags),
    regexp = "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')", # nolint
    fixed = TRUE
  )
})

test_that("Projects get() throws error when expected", {
  # Setup test parameters for test
  test_no_project <- list(project = NULL, project_owner = "owner")
  test_bad_project <- list(project = 1, project_owner = "owner")
  test_bad_project_owner <- list(project = "foo", project_owner = 123)

  # Get fails when no project is provided
  testthat::expect_error(do.call(setup_projects_obj$get, test_no_project))

  # Get fails when bad project is provided
  testthat::expect_error(do.call(setup_projects_obj$get, test_bad_project))

  # Get fails when bad project owner is provided
  testthat::expect_error(
    do.call(setup_projects_obj$get, test_bad_project_owner)
  )
})

test_that("Projects create() throws error when expected", {
  # Setup test parameters for test
  test_no_name <- list(name = NULL)
  test_bad_name <- list(name = 1)
  test_bad_billing1 <- list(name = "new-project", billing = 1)
  test_bad_billing2 <- list(name = "new-project", billing = setup_file_obj)
  test_bad_description <- list(name = "new-project", description = 1)
  test_bad_tags <- list(name = "new-project", tags = 1)
  test_bad_locked <- list(name = "new-project", locked = 1)
  test_bad_controlled <- list(name = "new-project", controlled = 1)
  test_bad_location <- list(name = "new-project", location = 1)
  test_bad_use_interr_instances <- list(
    name = "new-project",
    use_interruptible_instances = 1
  )
  test_bad_use_memoization <- list(name = "new-project", use_memoization = 1)
  test_bad_use_elastic_disk <- list(name = "new-project", use_elastic_disk = 1)
  test_bad_intermediate_files <- list(
    name = "new-project",
    intermediate_files = 1
  )

  # Get fails when no name is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_no_name),
    regexp = "You must provide at least a name for the project you want to create.", # nolint
    fixed = TRUE
  )

  # Get fails when bad name is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_name))

  # Get fails when bad billing is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_billing1))
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_billing2))

  # Get fails when bad description is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_description)
  )

  # Get fails when bad tags is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_tags),
    regexp = "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')", # nolint
    fixed = TRUE
  )

  # Get fails when bad locked is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_locked))

  # Get fails when bad controlled is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_controlled)
  )

  # Get fails when bad location is provided
  testthat::expect_error(do.call(setup_projects_obj$create, test_bad_location))

  # Get fails when bad use_interruptible_instances is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_use_interr_instances)
  )

  # Get fails when bad use_memoization is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_use_memoization)
  )

  # Get fails when bad use_elastic_disk is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_use_elastic_disk)
  )

  # Get fails when bad intermediate_files is provided
  testthat::expect_error(
    do.call(setup_projects_obj$create, test_bad_intermediate_files)
  )
})
