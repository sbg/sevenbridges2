# nolint start
#' @title R6 Class representing a Task
#'
#' @description
#' R6 Class representing a resource for managing tasks.
#'
#' @importFrom R6 R6Class
#' @export
Task <- R6::R6Class(
  # nolint end
  "Task",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "tasks/{id}",
      "run" = "tasks/{self$id}/actions/run",
      "abort" = "tasks/{self$id}/actions/abort",
      "clone" = "tasks/{self$id}/actions/clone",
      "execution_details" = "tasks/{self$id}/execution_details",
      "task" = "tasks/{self$id}"
    ),
    #' @field id The ID of the task.
    id = NULL,
    #' @field name The name of the task.
    name = NULL,
    #' @field status Task status (different from execution_status).
    #' Allowed values:
    #' * QUEUED
    #' * DRAFT
    #' * RUNNING
    #' * COMPLETED
    #' * ABORTED
    #' * FAILED
    status = NULL,
    #' @field description An optional description of a task.
    description = NULL,
    #' @field project Identifier of the project that
    #'  the task is located in.
    project = NULL,
    #' @field app The identifier of the app that was used for the task.
    app = NULL,
    #' @field created_by Username of the task creator.
    created_by = NULL,
    #' @field executed_by Username of the task executor.
    executed_by = NULL,
    #' @field created_on The time in form of string when the task was created.
    created_on = NULL,
    #' @field start_time Task start time in form of string.
    start_time = NULL,
    #' @field end_time Task end time in form of string .
    end_time = NULL,
    #' @field origin Id of the entity that created the task, e.g.
    #'  automation run, if task was created by an automation run.
    origin = NULL,
    #' @field use_interruptable_instances This field can be `TRUE` or
    #'  `FALSE`. Set this field to `TRUE` to allow the use of spot instances.
    use_interruptable_instances = NULL,
    #' @field batch `TRUE` for batch tasks, `FALSE` for regular and child
    #' tasks (batch this task; if `FALSE`, will not create a batch task).
    batch = NULL,
    #' @field batch_by Batching criteria (list).
    batch_by = NULL,
    #' @field batch_group Batch group for a batch task (list). Represents the
    #'  group that is assigned to the child task from the batching criteria that
    #'  was used when the task was started.
    batch_group = NULL,
    #' @field batch_input Input identifier on to which to apply batching.
    batch_input = NULL,
    #' @field batch_parent Parent task ID for a batch child. (batch task
    #'  which is the parent of this task).
    batch_parent = NULL,
    #' @field execution_settings Execution settings list for the task.
    execution_settings = NULL,
    #' @field execution_status Task execution status list - info about current
    #'  execution status.
    execution_status = NULL,
    #' @field errors Validations errors list stored as a high-level errors
    #' array property in the API response.
    errors = NULL,
    #' @field warnings Validation warnings list from API response.
    warnings = NULL,
    #' @field price Task cost (list) - contains amount and currency.
    price = NULL,
    #' @field inputs List of inputs that were submitted to the task.
    inputs = NULL,
    #' @field outputs List of generated outputs from the task.
    outputs = NULL,
    #' @field output_location List of locations where task outputs will be
    #'  stored.
    output_location = NULL,
    #'
    # Initialize Task object ----------------------------------------------
    #' @description Create new Task object.
    #'
    #' @param res Response containing Task object information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
      self$status <- res$status
      self$description <- res$description
      self$project <- res$project
      self$app <- res$app
      self$created_by <- res$created_by
      self$executed_by <- res$executed_by
      self$created_on <- res$created_on
      self$start_time <- res$start_time
      self$end_time <- res$end_time
      self$origin <- res$origin
      self$use_interruptable_instances <-
        res$use_interruptable_instances
      self$batch <- res$batch
      self$batch_by <- res$batch_by
      self$batch_group <- res$batch_group
      self$batch_input <- res$batch_input
      self$batch_parent <- res$batch_parent
      self$execution_settings <- res$execution_settings
      self$execution_status <- res$execution_status
      self$errors <- res$errors
      self$warnings <- res$warnings
      self$inputs <- private$map_input_output(res$inputs)
      self$outputs <- private$map_input_output(res$outputs)
      self$output_location <- res$output_location
    },

    # Print Task object ------------------------------------------------------
    #' @description Print method for Task class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    print = function() {
      x <- as.list(self)

      # Extract all except 'raw'
      x$raw <- NULL

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)

      # Extract name, description and status from service to print
      x <- append(x, x$service[c("name", "description", "status")])

      # Remove lists
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Task")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    # nocov start
    # Reload Task object ------------------------------------------------------
    #' @description Reload Task object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return \code{\link{Task}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Task object is refreshed!")
    }, # nocov end

    # Run task ---------------------------------------------------------------
    #' @description This call runs (executes) the task. Only tasks whose status
    #' is `DRAFT` can be run.
    #'
    #' @param batch Set this to `FALSE` to disable the default batching
    #'  for this task. Running a batch task is a recommended way to run multiple
    #'  tasks considering the API rate limit
    #' ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    #' @param use_interruptible_instances This field can be `TRUE` or
    #'  `FALSE`. Set this field to `TRUE` to allow the use of
    # nolint start
    #'  [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances).
    # nolint end
    #' @param in_place Default `TRUE`. Should the new object of
    #'  Task class be returned or the current to be reinitialized.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_logical
    #'
    #' @return \code{\link{Task}} object.
    run = function(batch = NULL,
                   use_interruptible_instances = NULL,
                   in_place = TRUE,
                   ...) {
      checkmate::assert_logical(batch, null.ok = TRUE)
      checkmate::assert_logical(use_interruptible_instances, null.ok = TRUE)
      checkmate::assert_logical(in_place, null.ok = FALSE)

      # nocov start
      path <- glue::glue(self$URL[["run"]])

      params <- list()
      params[["batch"]] <- batch
      params[["use_interruptible_instances"]] <-
        use_interruptible_instances

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        query = params,
        body = list(),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "Execution of task {green {self$name}} has started."
        )
      )

      if (in_place) {
        self$initialize(
          res = res,
          href = res$href,
          auth = self$auth,
          response = attr(res, "response")
        )
      } else {
        return(asTask(res, auth = self$auth))
      }
    }, # nocov end

    # Abort task -------------------------------------------------------------
    #' @description This call aborts the specified task. Only tasks whose
    #'  status is `RUNNING` or `QUEUED` may be aborted.
    #' @param in_place Default `TRUE`. Should the new object of
    #'  Task class be returned or the current to be reinitialized.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_logical
    #' @importFrom glue glue glue_col
    #' @importFrom rlang inform
    #'
    #' @return \code{\link{Task}} object.
    abort = function(in_place = TRUE, ...) {
      checkmate::assert_logical(in_place, null.ok = FALSE)

      # nocov start
      path <- glue::glue(self$URL[["abort"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "Task {green {self$name}} has been aborted."
        )
      )

      if (in_place) {
        self$initialize(
          res = res,
          href = res$href,
          auth = self$auth,
          response = attr(res, "response")
        )
      } else {
        return(asTask(res, auth = self$auth))
      }
    }, # nocov end

    # Clone task ------------------------------------------------------------
    #' @description This call clones the specified task. Once cloned, the task
    #'  can either be in `DRAFT` mode or immediately ran, by setting the `run`
    #'  parameter to `TRUE`.
    #' @param run Set this to `TRUE` in order to create a draft task
    #'  and execute it immediately. Default: `FALSE`.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_logical
    #' @importFrom glue glue glue_col
    #' @importFrom rlang inform
    #'
    #' @return \code{\link{Task}} object.
    clone_task = function(run = FALSE, ...) {
      # nocov start
      action <- NULL
      checkmate::assert_logical(run, null.ok = TRUE)
      if (!is_missing(run)) {
        if (run) {
          action <- "run"
        }
      }
      path <- glue::glue(self$URL[["clone"]])

      params <- list("action" = action)

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        query = params,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "New cloned draft task with id {green {res$id}} has been created."
        )
      )

      return(asTask(res, auth = self$auth))
    }, # nocov end

    # Get execution details of a task -----------------------------------------
    #' @description This call returns execution details of the specified task.
    #'  The task is referred to by its ID, which you can obtain by making the
    #'  call to list all tasks you can access. The call breaks down the
    #'  information into the task's distinct jobs. A job is a single subprocess
    #'  carried out in a task. The information returned by this call is broadly
    #'  similar to that which can be found in the task stats and logs provided
    #'  on the Platform.
    #'  The task execution details include the following information:
    #'  *  The name of the command line job that executed
    #'  *  The start time of the job
    #'  *  End time of the job (if it completed)
    #'  *  The status of the job (`DONE`, `FAILED`, or `RUNNING`)
    #'  *  Information on the computational instance that the job was run on,
    #'  including the provider ID, the type of instance used and the cloud
    #'  service provider
    #'  *  A link that can be used to download the standard error logs for the
    #'    job.
    #'  *  SHA hash of the Docker image ('checksum').
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #' @importFrom rlang inform
    #'
    #' @return List of execution details.
    get_execution_details = function(...) {
      # nocov start
      path <- glue::glue(self$URL[["execution_details"]])

      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      if (self$batch) {
        rlang::inform("Execution details can be seen on each child task.")
      }

      return(res)
    }, # nocov end

    # List batch child tasks -------------------------------------------------
    #' @description This call retrieves batch child tasks for this task if its
    #'  a batch task.
    #'
    #' @param status You can filter the returned tasks by their status.
    #'  Set the value of status to one of the following values:
    #'  * QUEUED
    #'  * DRAFT
    #'  * RUNNING
    #'  * COMPLETED
    #'  * ABORTED
    #'  * FAILED.
    #' @param project Provide the project ID or Project object you wish to list
    #'  the tasks from.
    #' @param created_from Enter the starting date string for querying tasks
    #'  created on the specified date and onwards.
    #' @param created_to Enter the ending date string for querying tasks
    #'  created until the specified date. You can use it in combination with
    #'  `created_from` to specify a time interval.
    #' @param started_from Enter the starting date string for querying tasks
    #'  started on the specified date and onwards.
    #' @param started_to Enter the ending date string for querying tasks
    #'  started until the specified date.
    #' @param ended_from Enter the starting date string for querying tasks
    #'  that ended on a specified date.
    #' @param ended_to Enter the ending date string for querying tasks that
    #'  ended until a specified date.
    #' @param order_by Order returned results by the specified field.
    #'  Allowed values: `created_time`, `start_time`, `name`, `end_time` and
    #'  `created_by`. Sort can be done only by one column. The default value is
    #'  `created_time`.
    #' @param order Sort results in ascending or descending order by
    #'  specifying `asc` or `desc`, respectively. Only taken into account if
    #'  `order_by` is explicitly specified. The default value is `asc`.
    #' @param origin_id Enter an automation run ID to list all tasks
    #'  created from the specified automation run.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom rlang abort
    #'
    #' @return \code{\link{Collection}} of \code{\link{Task}} objects.
    list_batch_children = function(status = NULL,
                                   project = NULL,
                                   created_from = NULL,
                                   created_to = NULL,
                                   started_from = NULL,
                                   started_to = NULL,
                                   ended_from = NULL,
                                   ended_to = NULL,
                                   order_by = NULL,
                                   order = NULL,
                                   origin_id = NULL,
                                   limit = getOption("sevenbridges2")$limit,
                                   offset = getOption("sevenbridges2")$offset,
                                   ...) {
      if (is_missing(self$batch) || !self$batch) {
        rlang::abort("This task is not a batch task.")
      }
      # nocov start
      parent <- self$id

      self$auth$tasks$query(
        status = status,
        parent = parent,
        project = project,
        created_from = created_from,
        created_to = created_to,
        started_from = started_from,
        started_to = started_to,
        ended_from = ended_from,
        ended_to = ended_to,
        order_by = order_by,
        order = order,
        origin_id = origin_id,
        offset = offset,
        limit = limit,
        ...
      )
    }, # nocov end

    # Delete task -----------------------------------------------------------
    #' @description This call deletes the specified task. The task is referred
    #'  to by its ID, which you can obtain by making the call to list all tasks
    #'  you can access.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue glue_col
    #' @importFrom rlang inform
    delete = function(...) {
      # nocov start
      path <- glue::glue(self$URL[["task"]])

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "The task with the following ID {green {id}} has been deleted."
        )
      )
    }, # nocov end

    # Rerun task -----------------------------------------------------------
    #' @description This call reruns (executes) the specified task.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #'
    #' @return \code{\link{Task}} object.
    rerun = function(...) {
      # nocov start
      path <- glue::glue(self$URL[["clone"]])

      self$clone_task(run = TRUE)
    }, # nocov end

    # Update task -----------------------------------------------------------
    #' @description Change the details of the specified task, including its
    #'  name, description, and inputs. Note that you can only modify tasks with
    #'  a task status of `DRAFT`. Tasks which are `RUNNING`, `QUEUED`,
    #'  `ABORTED`, `COMPLETED` or `FAILED` cannot be modified in order to
    #'  enable the reproducibility of analyses which have been queued for
    #'  execution or has initiated executing.
    #'  There are two things to note if you are editing a batch task:
    #'  \itemize{
    #'    \item `1` If you want to change the input on which to batch and
    #'    the batch criteria, you need to specify the batch_input and batch_by
    #'    parameters together in the same function call.
    #'    \item `2` If you want to disable batching on a task, set batch to
    #'    false. Or, you can also set the parameters batch_input and batch_by
    #'    to NULL.
    #'  }
    #' @param name The name of the task.
    #' @param description An optional description of the task.
    #' @param execution_settings Named list with detailed task execution
    #'  parameters. Detailed task execution parameters:
    #'  \itemize{
    #'    \item `instance_type`: Possible value is the specific instance type,
    #'      e.g. `"instance_type" = "c4.2xlarge;ebs-gp2;2000"`;
    #'    \item `max_parallel_instances`: Maximum number of instances
    #'      running at the same time. Takes any integer value equal to or
    #'      greater than 1, e.g. `"max_parallel_instances" = 2.`;
    #'    \item `use_memoization`: Set to `FALSE` by default. Set to `TRUE`
    #'      to enable
    #'      [memoization](https://docs.sevenbridges.com/docs/about-memoization);
    #'    \item `use_elastic_disk`: Set to `TRUE` to enable
    #'      [Elastic Disk](https://docs.sevenbridges.com/page/elastic-disk).
    #'  }
    #'
    #' Here is an example:
    #' ```{r}
    #' execution_settings <- list(
    #'   "instance_type" = "c4.2xlarge;ebs-gp2;2000",
    #'   "max_parallel_instances" = 2,
    #'   "use_memoization" = TRUE,
    #'   "use_elastic_disk" = TRUE
    #'   )
    #' ```
    #' @param inputs List of objects. See the section on
    # nolint start
    #' [specifying task inputs](https://docs.sevenbridges.com/docs/the-api#section-inputs)
    # nolint end
    #'  for information on creating task input objects. Here is an example with
    #'  various input types:
    #'  ```{r}
    #'  inputs <- list(
    #'    "input_file"= "<file_id/file_object>",
    #'    "input_directory" = "<folder_id/folder_object>",
    #'    "input_array_string" = list("<string_elem_1>", "<string_elem_2>"),
    #'    "input_boolean" = TRUE,
    #'    "input_double" = 54.6,
    #'    "input_enum" = "enum_1",
    #'    "input_float" = 11.2,
    #'    "input_integer" = "asdf",
    #'    "input_long" = 4212,
    #'    "input_string" = "test_string",
    #'    "input_record" = list(
    #'      "input_record_field_file" = "<file_id/file_object>",
    #'      "input_record_field_integer" = 42
    #'     )
    #'    )
    #'  ```
    #' @param output_location The output location list allows you to
    #'  define the exact location where your task outputs will be stored.
    #'  The location can either be defined for the entire project using the
    #'  main_location parameter, or individually per each output node, by
    #'  setting the nodes_override parameter to true and defining individual
    #'  output node locations within nodes_location.
    #'  See below for more details.
    #'  \itemize{
    #'    \item `main_location` - Defines the output location for all
    #'      output nodes in the task. Can be a string path within the project in
    #'      which the task is created, for example
    #'      `/Analysis/<task_id>_<task_name>/`
    #'      or a path on an attached volume, such as
    #'      `volumes://volume_name/<project_id>/html`.
    #'      Parts of the path enclosed in angle brackets <> are tokens that are
    #'      dynamically replaced with corresponding values during task
    #'      execution.
    #'    \item `main_location_alias`: The string location (path) in the
    #'      project that will point to the actual location where the outputs are
    #'      stored. Used if main_location is defined as a volume path (starting
    #'      with volumes://), to provide an easy way of accessing output data
    #'      directly from project files.
    #'    \item `nodes_override`: Enables defining of output locations
    #'      for output nodes individually through nodes_location (see below).
    #'      Set to `TRUE` to be able to define individual locations per output
    #'      node. Default: `FALSE`.
    #'      Even if nodes_override is set to `TRUE`, it is not necessary to
    #'      define output locations for each of the output nodes individually.
    #'      Data from those output nodes that don't have their locations
    #'      explicitly defined through nodes_location is either placed in
    #'      main_location (if defined) or at the project files root if a main
    #'      output location is not defined for the task.
    #'    \item `nodes_location`: List of output paths for individual
    #'      task output nodes in the following format for each output node:
    #'      <output-node-id> = list(
    #'        "output_location" = "<output-path>",
    #'        "output_location_alias" = "<alias-path>"
    #'      )
    #'      ```{r}
    #'      b64html = list(
    #'      "output_location" = "volumes://outputs/tasks/mar-19",
    #'      "output_location_alias" = "/rfranklin/tasks/picard"
    #'      )
    #'      ```
    #'      In the example above, b64html is the ID of the output node for which
    #'      you want to define the output location, while the parameters are
    #'      defined as follows:
    #'    \itemize{
    #'      \item `output_location` - Can be a path within the project in which
    #'        the task is created, for example
    #'        `/Analysis/<task_id>_<task_name>/`
    #'        or a path on an attached volume, such as
    #'        `volumes://volume_name/<project_id>/html`. Also accepts tokens.
    #'      \item `output_location_alias` - The location (path) in the project
    #'        that will point to the exact location where the output is stored.
    #'        Used if output_location is defined as a volume path
    #'        (starting with volumes://).
    #'      }
    #' }
    #' @param batch This is set to `FALSE` by default. Set to `TRUE` to
    #'  create a batch task and specify the `batch_input` and `batch-by`
    #'  criteria as described below.
    #' @param batch_input The ID of the input on which you wish to batch.
    #'  You would typically batch on the input consisting of a list of files.
    #'  If this parameter is omitted, the default batching criteria defined for
    #'  the app will be used.
    #' @param batch_by Batching criteria in form of list. For example:
    #'  ```{r}
    #'  batch_by = list(
    #'    type = "CRITERIA",
    #'    criteria = list("metadata.condition")
    #'  )
    #'  ```
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_string assert_list assert_logical
    #' @importFrom rlang abort
    #' @importFrom glue glue glue_col
    #'
    #' @return \code{\link{Task}} object.
    update = function(name = NULL,
                      description = NULL,
                      execution_settings = NULL,
                      inputs = NULL,
                      output_location = NULL,
                      batch = NULL,
                      batch_input = NULL,
                      batch_by = NULL,
                      ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_string(description, null.ok = TRUE)
      check_execution_settings(execution_settings)
      checkmate::assert_list(inputs, null.ok = TRUE)
      checkmate::assert_list(output_location, null.ok = TRUE)
      checkmate::assert_logical(batch, null.ok = TRUE)
      checkmate::assert_string(batch_input, null.ok = TRUE)
      checkmate::assert_list(batch_by, null.ok = TRUE)

      task_inputs <- list()
      task_data <- list()

      task_data <- list(
        "name" = name,
        "description" = description
      )

      if (!is_missing(batch) && batch) {
        if (!is_missing(batch_input) && !is_missing(batch_by)) {
          task_data[["batch_input"]] <- batch_input
          task_data[["batch_by"]] <- batch_by
        } else {
          rlang::abort("Batch is set to TRUE, therefore, please, set batching criteria (batch_by) and batch inputs.") # nolint
        }
      }
      # nocov start
      if (!is_missing(inputs)) {
        task_inputs <- self$auth$tasks$private$serialize_inputs(inputs)
        task_data[["inputs"]] <- task_inputs
      }
      if (!is_missing(output_location)) {
        task_data[["output_location"]] <- output_location
      }

      task_data[["execution_settings"]] <- execution_settings
      task_data[["batch"]] <- batch

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["task"]]),
        method = "PATCH",
        body = task_data,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "Task {green {self$name}} has been updated."
        )
      )

      self$initialize(
        res = res,
        href = res$href,
        auth = self$auth,
        response = attr(res, "response")
      )
    } # nocov end
  ),
  private = list(
    # Map input/output values -----------------------------------------------
    #' @importFrom checkmate test_list
    map_input_output = function(input) {
      return_value <- list()
      if (checkmate::test_list(input)) {
        if ("class" %in% names(input)) {
          if (tolower(input[["class"]]) %in% c("file", "folder")) {
            f_data <- map_fields_util(input)
            f_data <- append(f_data, input)

            if (!is.null(input[["secondaryFiles"]])) {
              secondary_files <- list()
              for (sf in input[["secondaryFiles"]]) {
                sf_data <- map_fields_util(sf)
                sf_data <- append(sf_data, sf)

                secondary_files <- append(secondary_files, list(sf_data))
              }
              f_data[["secondary_files"]] <- secondary_files
            }
            return_value <- asFile(f_data, auth = self$auth)
          }
        } else {
          for (i in seq_len(length(input))) {
            elem_name <- names(input[i])
            vals <- private$map_input_output(input[[i]])
            if (!is.null(elem_name)) {
              return_value[[elem_name]] <- vals
            } else {
              return_value <- c(return_value, list(vals))
            }
          }
        }
      } else {
        return_value <- input
      }
      return(return_value)
    },
    # Map fields -------------------------------------------------------------
    map_fields_util = function(input) {
      mapped_data <- list(
        id = input[["path"]],
        type = tolower(input[["class"]])
      )
      if ("basename" %in% names(input)) {
        mapped_data[["name"]] <- input[["basename"]]
      }
      if ("name" %in% names(input)) {
        mapped_data[["name"]] <- input[["name"]]
      }
      return(mapped_data)
    }
  )
)

# nocov start
# Helper functions for creating Task objects --------------------------------
asTask <- function(x = NULL, auth = NULL) {
  Task$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

asTaskList <- function(x, auth) {
  obj <- lapply(x$items, asTask, auth = auth)
  obj
}
# nocov end
