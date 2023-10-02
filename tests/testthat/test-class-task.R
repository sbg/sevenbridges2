test_that("Task initialization works", {
  # Item object creation works
  testthat::expect_no_error(asTask(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_task_obj,
    classes = c("Item", "Task"),
    public = c(
      "output_location", "outputs", "inputs", "price", "warnings", "errors",
      "execution_status", "execution_settings", "batch_parent", "batch_input",
      "batch_group", "batch_by", "batch", "use_interruptable_instances",
      "origin", "end_time", "start_time", "created_on", "executed_by",
      "created_by", "app", "project", "description", "status", "name", "id",
      "URL", "auth", "clone", "rerun", "delete", "list_batch_children",
      "get_execution_details", "clone_task", "abort", "run", "print", "reload"
    )
  )
})

test_that("Task print method works", {
  testthat::expect_snapshot(setup_task_obj$print())
})

test_that("Task method run() throws error when expected", {
  bad_batch_param <- list(batch = 123)
  bad_use_interupt_inst_param <- list(use_interruptible_instances = 123)
  bad_in_place_param <- list(in_place = 123)

  testthat::expect_error(
    do.call(setup_task_obj$run, bad_batch_param),
    regexp = "Assertion on 'batch' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_task_obj$run, bad_use_interupt_inst_param),
    regexp = "Assertion on 'use_interruptible_instances' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_task_obj$run, bad_in_place_param),
    regexp = "Assertion on 'in_place' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Task method abort() throws error when expected", {
  bad_in_place_param <- list(in_place = 123)
  testthat::expect_error(
    do.call(setup_task_obj$abort, bad_in_place_param),
    regexp = "Assertion on 'in_place' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Task method clone_task() throws error when expected", {
  bad_run_param <- list(run = 123)
  testthat::expect_error(
    do.call(setup_task_obj$clone_task, bad_run_param),
    regexp = "Assertion on 'run' failed: Must be of type 'logical' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Task method list_batch_children() throws error when expected", {
  bad_batch_param <- list(batch = FALSE)
  testthat::expect_error(
    do.call(setup_task_obj$list_batch_children, bad_batch_param),
    regexp = "This task is not a batch task.",
    fixed = TRUE
  )
})

test_that("Task method update() throws error when expected", {
  test_bad_name <-
    list(
      name = 1
    )
  test_bad_description <-
    list(
      description = 1
    )
  test_bad_execution_settings <-
    list(
      execution_settings = 1
    )
  test_bad_inputs <-
    list(
      inputs = 1
    )
  test_bad_output_loc <-
    list(
      output_location = 1
    )
  test_bad_batch <-
    list(
      batch = 1
    )
  test_bad_batch_input <-
    list(
      batch_input = 1
    )
  test_bad_batch_by <-
    list(
      batch_by = 1
    )
  test_missing_batch_inputs <-
    list(
      batch = TRUE
    )

  # Test bad name parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_name))

  # Test bad description parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_description))

  # Test bad execution_settings parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_execution_settings)) # nolint

  # Test bad inputs parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_inputs))

  # Test bad output_location parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_output_loc))

  # Test bad batch parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_batch))

  # Test bad batch_input parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_batch_input))

  # Test bad batch_by parameter
  testthat::expect_error(do.call(setup_task_obj$update, test_bad_batch_by))

  # Test missing batch input parameters
  testthat::expect_error(
    do.call(setup_task_obj$update, test_missing_batch_inputs),
    regexp = "Batch is set to TRUE, therefore, please, set batching criteria (batch_by) and batch inputs.", # nolint
    fixed = TRUE
  )
})
