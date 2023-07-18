test_that("Tests initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Tasks$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_tasks_obj,
    classes = c("Resource", "Tasks"),
    public = c(
      "URL", "query", "get"
    )
  )
})

test_that("Tasks query() throws error when needed", {
  # Query fails when bad status is provided
  test_bad_status <- list(status = 1)
  testthat::expect_error(do.call(setup_tasks_obj$query, test_bad_status),
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
