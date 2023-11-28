test_that("Tasks initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Tasks$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_tasks_obj,
    classes = c("Resource", "Tasks"),
    public = c("URL", "query", "get", "create")
  )
})

test_that("Tasks query() throws error when needed", {
  # Query fails when bad status is provided
  test_bad_status <- list(status = 1)
  testthat::expect_error(
    do.call(setup_tasks_obj$query, test_bad_status),
    regexp = "Status must be one of the following: QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, FAILED", # nolint
    fixed = TRUE
  )

  # Setup test parameters for test
  test_bad_parent <- list(parent = 1)
  test_bad_project <- list(project = 1)
  test_bad_created_from <- list(created_from = 1)
  test_bad_created_to <- list(created_to = 1)
  test_bad_started_from <- list(started_from = 1)
  test_bad_started_to <- list(started_to = 1)
  test_bad_ended_from <- list(ended_from = 1)
  test_bad_ended_to <- list(ended_to = 1)
  test_bad_order_by <- list(order_by = 1)
  test_bad_order <- list(order = 1)
  test_bad_origin_id <- list(origin_id = 1)

  # Test bad parent parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_parent))

  # Test bad project parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_project))

  # Test bad created_from parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_created_from))

  # Test bad created_to parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_created_to))

  # Test bad started_from parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_started_from))

  # Test bad started_to parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_started_to))

  # Test bad ended_from parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_ended_from))

  # Test bad ended_to parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_ended_to))

  # Test bad order_by parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_order_by))

  # Test bad order parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_order))

  # Test bad origin_id parameter
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_origin_id))
})

test_that("Tasks get() throws error when needed", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_tasks_obj$get, test_no_id),
    regexp = "Task ID must be provided!",
    fixed = TRUE
  )

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_tasks_obj$get, test_bad_id))
})

test_that("Tasks delete() throws error when needed", {
  # Setup test parameters for test
  test_no_task <- list(task = NULL)
  test_bad_task <- list(task = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_tasks_obj$delete, test_no_task))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_tasks_obj$delete, test_bad_task))
})

test_that("Tasks create() throws error when needed", {
  # Setup test parameters for test
  test_no_project <- list(project = NULL)
  test_bad_project <- list(project = 1)
  test_no_app <- list(project = setup_project_obj, app = NULL)
  test_bad_app <- list(project = setup_project_obj, app = 1)
  test_bad_revision <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      revision = "bad_revision"
    )
  test_bad_name <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      name = 1
    )
  test_bad_description <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      description = 1
    )
  test_bad_execution_settings <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      execution_settings = 1
    )
  test_bad_inputs <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      inputs = 1
    )
  test_bad_output_loc <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      output_location = 1
    )
  test_bad_batch <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      batch = 1
    )
  test_bad_batch_input <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      batch_input = 1
    )
  test_bad_batch_by <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      batch_by = 1
    )
  test_missing_batch_inputs <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      batch = TRUE
    )
  test_bad_use_inter_instance <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      use_interruptible_instance = 1
    )
  test_bad_action <-
    list(
      project = setup_project_obj,
      app = setup_app_obj,
      action = 1
    )

  # Test no project parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_no_project))

  # Test bad project parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_project))

  # Test no app parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_no_app))

  # Test bad app parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_app))

  # Test bad revision parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_revision))

  # Test bad name parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_name))

  # Test bad description parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_description))

  # Test bad execution_settings parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_execution_settings)) # nolint

  # Test bad inputs parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_inputs))

  # Test bad output_location parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_output_loc))

  # Test bad batch parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_batch))

  # Test bad batch_input parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_batch_input))

  # Test bad batch_by parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_batch_by))

  # Test missing batch input parameters
  testthat::expect_error(
    do.call(setup_tasks_obj$create, test_missing_batch_inputs),
    regexp = "Batch is set to TRUE, therefore, please, set batching criteria (batch_by) and batch inputs.", # nolint
    fixed = TRUE
  )

  # Test bad use_interruptible_instance parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_use_inter_instance)) # nolint

  # Test bad action parameter
  testthat::expect_error(do.call(setup_tasks_obj$create, test_bad_action))
})
