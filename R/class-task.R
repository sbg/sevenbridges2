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
    #' @field URL URL endpoint fields
    URL = list(
      "run" = "tasks/{id}/actions/run",
      "abort" = "tasks/{id}/actions/abort",
      "clone" = "tasks/{id}/actions/clone",
      "execution_details" = "tasks/{id}/execution_details",
      "task" = "tasks/{id}"
    ),
    #' @field id String. The ID of the task.
    id = NULL,
    #' @field name String. The name of the task.
    name = NULL,
    #' @field status String. Task status (different from execution_status).
    #' Allowed values:
    #' * QUEUED
    #' * DRAFT
    #' * RUNNING
    #' * COMPLETED
    #' * ABORTED
    #' * FAILED
    status = NULL,
    #' @field description String. An optional description of a task.
    description = NULL,
    #' @field project String. Identifier of the project that
    #' the task is located in.
    project = NULL,
    #' @field app String. The identifier of the app that was used for the task.
    app = NULL,
    #' @field created_by String. Username of the task creator.
    created_by = NULL,
    #' @field executed_by String. Username of the task executor.
    executed_by = NULL,
    #' @field created_on String. The time when the task was created.
    created_on = NULL,
    #' @field start_time String. Task start time.
    start_time = NULL,
    #' @field end_time String. Task end time.
    end_time = NULL,
    #' @field origin String. Id of the entity that created the task, e.g.
    #' automation run, if task was created by an automation run.
    origin = NULL,
    #' @field use_interruptable_instances Boolean. This field can be TRUE or
    #' FALSE. Set this field to TRUE to allow the use of spot instances.
    use_interruptable_instances = NULL,
    #' @field batch Boolean. TRUE for batch tasks, FALSE for regular & child
    #' tasks (batch this task; if FALSE, will not create a batch task).
    batch = NULL,
    #' @field batch_by List. Batching criteria.
    batch_by = NULL,
    #' @field batch_group List. Batch group for a batch task. Represents the
    #' group that is assigned to the child task from the batching criteria that
    #'  was used when the task was started.
    batch_group = NULL,
    #' @field batch_input String. Input identifier on to which to apply
    #' batching.
    batch_input = NULL,
    #' @field batch_parent String. Parent task for a batch child. (batch task
    #' which is the parent of this task).
    batch_parent = NULL,
    #' @field execution_settings List. Execution settings for the task.
    execution_settings = NULL,
    #' @field execution_status List. Task execution status. (info about current
    #' execution status)
    execution_status = NULL,
    #' @field errors List. Validations errors stored as a high-level errors
    #' array property in the API response.
    errors = NULL,
    #' @field warnings List. Validation warnings from API response.
    warnings = NULL,
    #' @field price List. Task cost. (contains amount and currency)
    price = NULL,
    #' @field inputs List. Inputs that were submitted to the task.
    inputs = NULL,
    #' @field outputs List. Generated outputs from the task.
    outputs = NULL,
    #' @field output_location List. Location where task outputs will be stored.
    output_location = NULL,
    #'
    #' @description Initialize Task class
    #' @param id String. The ID of the task.
    #' @param name String. The name of the task.
    #' @param status String. Task status (different from execution_status).
    #' Allowed values:
    #' * QUEUED
    #' * DRAFT
    #' * RUNNING
    #' * COMPLETED
    #' * ABORTED
    #' * FAILED
    #' @param description String. An optional description of a task.
    #' @param project String. Identifier of the project that
    #' the task is located in.
    #' @param app String. The identifier of the app that was used for the task.
    #' @param created_by String. Username of the task creator.
    #' @param executed_by String. Username of the task executor.
    #' @param created_on String. The time when the task was created.
    #' @param start_time String. Task start time.
    #' @param end_time String. Task end time.
    #' @param origin String. Id of the entity that created the task, e.g.
    #' automation run, if task was created by an automation run.
    #' @param use_interruptable_instances Boolean. This field can be TRUE or
    #' FALSE. Set this field to TRUE to allow the use of spot instances.
    #' @param batch Boolean. TRUE for batch tasks, FALSE for regular & child
    #' tasks (batch this task; if FALSE, will not create a batch task).
    #' @param batch_by List. Batching criteria.
    #' @param batch_group List. Batch group for a batch task. Represents the
    #' group that is assigned to the child task from the batching criteria that
    #'  was used when the task was started.
    #' @param batch_input String. Input identifier on to which to apply
    #' batching.
    #' @param batch_parent String. Parent task for a batch child. (batch task
    #' which is the parent of this task).
    #' @param execution_settings List. Execution settings for the task.
    #' @param execution_status List. Task execution status. (info about current
    #' execution status)
    #' @param errors List. Validations errors stored as a high-level errors
    #' array property in the API response.
    #' @param warnings List. Validation warnings from API response.
    #' @param price List. Task cost. (contains amount and currency)
    #' @param inputs List. Inputs that were submitted to the task.
    #' @param outputs List. Generated outputs from the task.
    #' @param output_location List. Location where task outputs will be stored.
    #' @param ... Other api parameters.
    initialize = function(id = NA,
                          name = NA,
                          status = NA,
                          description = NA,
                          project = NA,
                          app = NA,
                          created_by = NA,
                          executed_by = NA,
                          created_on = NA,
                          start_time = NA,
                          end_time = NA,
                          origin = NA,
                          use_interruptable_instances = NA,
                          batch = NA,
                          batch_by = NA,
                          batch_group = NA,
                          batch_input = NA,
                          batch_parent = NA,
                          execution_settings = NA,
                          execution_status = NA,
                          errors = NA,
                          warnings = NA,
                          price = NA,
                          inputs = NA,
                          outputs = NA,
                          output_location = NA,
                          ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$name <- name
      self$status <- status
      self$description <- description
      self$project <- project
      self$app <- app
      self$created_by <- created_by
      self$executed_by <- executed_by
      self$created_on <- created_on
      self$start_time <- start_time
      self$end_time <- end_time
      self$origin <- origin
      self$use_interruptable_instances <-
        use_interruptable_instances
      self$batch <- batch
      self$batch_by <- batch_by
      self$batch_group <- batch_group
      self$batch_input <- batch_input
      self$batch_parent <- batch_parent
      self$execution_settings <- execution_settings
      self$execution_status <- execution_status
      self$errors <- errors
      self$warnings <- warnings
      self$inputs <- private$map_input_output(inputs)
      self$outputs <- private$map_input_output(outputs)
      self$output_location <- output_location
    },

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
    #' @description This call runs (executes) the task. Only tasks whose status
    #' is "DRAFT" can be run.
    #'
    #' @param batch Boolean. Set this to FALSE to disable the default batching
    #' for this task. Running a batch task is a recommended way to run multiple
    #' tasks considering the API rate limit
    #' ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    #' @param use_interruptible_instances Boolean. This field can be TRUE or
    #' FALSE. Set this field to TRUE to allow the use of
    #' [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances)
    #' @param in_place Boolean. Default TRUE. Should the new object of
    #' Task class be returned or the current to be reinitialized.
    #' @param ... Other parameters that can be passed to api() function like
    #' fields etc.
    #' @importFrom checkmate assert_logical
    run = function(batch = NULL,
                   use_interruptible_instances = NULL,
                   in_place = TRUE,
                   ...) {
      checkmate::assert_logical(batch, null.ok = TRUE)
      checkmate::assert_logical(use_interruptible_instances, null.ok = TRUE)
      checkmate::assert_logical(in_place, null.ok = FALSE)

      # nocov start
      id <- self$id
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

      res <- status_check(res)

      if (in_place) {
        self$initialize(
          href = res$href,
          id = res$id,
          name = res$name,
          status = res$status,
          description = res$description,
          project = res$project,
          app = res$app,
          created_by = res$created_by,
          executed_by = res$executed_by,
          created_on = res$created_on,
          start_time = res$start_time,
          end_time = res$end_time,
          origin = res$origin,
          use_interruptable_instances =
            res$use_interruptable_instances,
          batch = res$batch,
          batch_by = res$batch_by,
          batch_group = res$batch_group,
          batch_input = res$batch_input,
          batch_parent = res$batch_parent,
          execution_settings = res$execution_settings,
          execution_status = res$execution_status,
          errors = res$errors,
          warnings = res$warnings,
          inputs = private$map_input_output(res$inputs),
          outputs = private$map_input_output(res$outputs),
          output_location = res$output_location,
          auth = self$auth,
          response = attr(res, "response")
        )
      } else {
        return(asTask(res, auth = self$auth))
      }
    },
    # nocov end
    #' @description This call aborts the specified task. Only tasks whose
    #' status is `RUNNING` or `QUEUED` may be aborted.
    #' @param in_place Boolean. Default TRUE. Should the new object of
    #' Task class be returned or the current to be reinitialized.
    #' @param ... Other parameters that can be passed to api() function like
    #' fields etc.
    abort = function(in_place = TRUE, ...) {
      # nocov start
      id <- self$id
      path <- glue::glue(self$URL[["abort"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      res <- status_check(res)

      if (in_place) {
        self$initialize(
          href = res$href,
          id = res$id,
          name = res$name,
          status = res$status,
          description = res$description,
          project = res$project,
          app = res$app,
          created_by = res$created_by,
          executed_by = res$executed_by,
          created_on = res$created_on,
          start_time = res$start_time,
          end_time = res$end_time,
          origin = res$origin,
          use_interruptable_instances =
            res$use_interruptable_instances,
          batch = res$batch,
          batch_by = res$batch_by,
          batch_group = res$batch_group,
          batch_input = res$batch_input,
          batch_parent = res$batch_parent,
          execution_settings = res$execution_settings,
          execution_status = res$execution_status,
          errors = res$errors,
          warnings = res$warnings,
          inputs = private$map_input_output(res$inputs),
          outputs = private$map_input_output(res$outputs),
          output_location = res$output_location,
          auth = self$auth,
          response = attr(res, "response")
        )
      } else {
        return(asTask(res, auth = self$auth))
      }
    },
    # nocov end
    #' @description This call clones the specified task. Once cloned, the task
    #' can either be in `draft` mode or immediately ran, by setting the `run`
    #' parameter to `TRUE`.
    #' @param run Boolean. Set this to `TRUE` in order to create a draft task
    #' and execute it immediately. Default: `FALSE`.
    #' @param ... Other parameters that can be passed to api() function like
    #' fields etc.
    #' @importFrom checkmate assert_logical
    clone_task = function(run = FALSE, ...) {
      # nocov start
      action <- NULL
      checkmate::assert_logical(run, null.ok = TRUE)
      if (!is_missing(run)) {
        if (run) {
          action <- "run"
        }
      }
      id <- self$id
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
      res <- status_check(res)

      return(asTask(res, auth = self$auth))
    }, # nocov end
    #' @description This call returns execution details of the specified task.
    #' The task is referred to by its ID, which you can obtain by making the
    #' call to list all tasks you can access. The call breaks down the
    #' information into the task's distinct jobs. A job is a single subprocess
    #' carried out in a task. The information returned by this call is broadly
    #' similar to that which can be found in the task stats and logs provided
    #' on the Platform.
    #' The task execution details include the following information:
    #' *  The name of the command line job that executed
    #' *  The start time of the job
    #' *  End time of the job (if it completed)
    #' *  The status of the job (`DONE`, `FAILED`, or `RUNNING`)
    #' *  Information on the computational instance that the job was run on,
    #' including the provider ID, the type of instance used and the cloud
    #' service provider
    #' *  A link that can be used to download the standard error logs for the
    #' job
    #' *  SHA hash of the Docker image ('checksum')
    #' @param ... Other parameters that can be passed to api() function like
    #' fields etc.
    get_execution_details = function(...) {
      # nocov start
      id <- self$id
      path <- glue::glue(self$URL[["execution_details"]])

      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      res <- status_check(res)

      return(res)
    }, # nocov end
    #' @description This call retrieves batch child tasks for this task if its
    #' a batch task.
    #' @param status String. You can filter the returned tasks by their status.
    #' Set the value of status to one of the following values: `QUEUED`,
    #' `DRAFT`, `RUNNING`, `COMPLETED`, `ABORTED`, `FAILED`.
    #' @param project Provide the project ID or project object you wish to list
    #'  the tasks from.
    #' @param created_from String. Enter the starting date for querying tasks
    #' created on the specified date and onwards.
    #' @param created_to String. Enter the ending date for querying tasks
    #' created until the specified date. You can use it in combination with
    #' `created_from` to specify a time interval.
    #' @param started_from String. Enter the starting date for querying tasks
    #' started on the specified date and onwards.
    #' @param started_to String. Enter the ending date for querying tasks
    #' started until the specified date.
    #' @param ended_from String. Enter the starting date for querying tasks
    #' that ended on a specified date.
    #' @param ended_to String. Enter the ending date for querying tasks that
    #' ended until a specified date.
    #' @param order_by String. Order returned results by the specified field.
    #' Allowed values: `created_time`, `start_time`, `name`, `end_time` and
    #' `created_by`. Sort can be done only by one column. The default value is
    #' `created_time`.
    #' @param order String. Sort results in ascending or descending order by
    #' specifying `asc` or `desc`, respectively. Only taken into account if
    #' `order_by` is explicitly specified. The default value is `asc`.
    #' @param origin_id String. Enter an automation run ID to list all tasks
    #' created from the specified automation run.
    #' @param limit The maximum number of collection items to return for a
    #' single request. Minimum value is 1. The maximum value is 100 and the
    #' default value is 50. This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0. This is a
    #' pagination-specific attribute.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
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
    #' @description This call deletes the specified task. The task is referred
    #' to by its ID, which you can obtain by making the call to list all tasks
    #' you can access.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    delete = function(...) {
      # nocov start
      id <- self$id
      path <- glue::glue(self$URL[["task"]])

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      res <- status_check(res)

      rlang::inform(
        glue::glue_col(
          "The task with the following ID {green {id}} has been deleted."
        )
      )
    }, # nocov end
    #' @description Change the details of the specified task, including its
    #' name, description, and inputs. Note that you can only modify tasks with
    #' a task status of DRAFT. Tasks which are RUNNING, QUEUED, ABORTED,
    #' COMPLETED or FAILED cannot be modified in order to enable the
    #' reproducibility of analyses which have been queued for execution or
    #' has initiated executing.
    #' There are two things to note if you are editing a batch task:
    #' \itemize{
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
    #' parameters. Detailed task execution parameters:
    #' * `instance_type`: String. Possible value is the specific instance type,
    #' e.g. `"instance_type" = "c4.2xlarge;ebs-gp2;2000"`.
    #' * `max_parallel_instances`: Integer. Maximum number of instances
    #' running at the same time. Takes any integer value equal to or greater
    #' than 1, e.g. `"max_parallel_instances" = 2.`
    #' * `use_memoization`: Boolean. Set to `FALSE` by default. Set to `TRUE`
    #' to enable
    #' [memoization](https://docs.sevenbridges.com/docs/about-memoization).
    #' * `use_elastic_disk`: Boolean. Set to `TRUE` to enable
    #' [Elastic Disk](https://docs.sevenbridges.com/page/elastic-disk).
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
    #'  ````
    #' @param batch Boolean. This is set to `FALSE` by default. Set to `TRUE` to
    #' create a batch task and specify the `batch_input` and `batch-by`
    #' criteria as described below.
    #' @param batch_input String. The ID of the input on which you wish to
    #' batch. You would typically batch on the input consisting of a list of
    #' files. If this parameter is omitted, the default batching criteria
    #' defined for the app will be used.
    #' @param batch_by String. Batching criteria.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    update = function(name = NULL,
                      description = NULL,
                      execution_settings = NULL,
                      inputs = NULL,
                      batch = FALSE,
                      batch_input = NULL,
                      batch_by = NULL,
                      ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_string(description, null.ok = TRUE)
      check_execution_settings(execution_settings)
      checkmate::assert_list(inputs, null.ok = TRUE)
      checkmate::assert_logical(batch, null.ok = FALSE)
      checkmate::assert_string(batch_input, null.ok = TRUE)
      checkmate::assert_string(batch_by, null.ok = TRUE)

      task_inputs <- list()
      task_data <- list()

      task_data <- list(
        "name" = name,
        "description" = description
      )

      if (batch) {
        if (!is_missing(batch_input) && !is_missing(batch_by)) {
          task_data[["batch_input"]] <- batch_input
          # task_data[["batch_by"]] <- batch_by
        } else {
          rlang::abort("Batch is set to TRUE, therefore, please, set batching criteria (batch_by) and batch inputs.") # nolint
        }
      }

      if (!is_missing(inputs)) {
        task_inputs <- self$auth$tasks$private$serialize_inputs(inputs)
        task_data[["inputs"]] <- task_inputs
      }

      task_data[["execution_settings"]] <- execution_settings
      task_data[["batch"]] <- batch

      id <- self$id

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["task"]]),
        method = "PATCH",
        body = task_data,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      self$initialize(
        href = res$href,
        id = res$id,
        name = res$name,
        status = res$status,
        description = res$description,
        project = res$project,
        app = res$app,
        created_by = res$created_by,
        executed_by = res$executed_by,
        created_on = res$created_on,
        start_time = res$start_time,
        end_time = res$end_time,
        origin = res$origin,
        use_interruptable_instances =
          res$use_interruptable_instances,
        batch = res$batch,
        batch_by = res$batch_by,
        batch_group = res$batch_group,
        batch_input = res$batch_input,
        batch_parent = res$batch_parent,
        execution_settings = res$execution_settings,
        execution_status = res$execution_status,
        errors = res$errors,
        warnings = res$warnings,
        inputs = private$map_input_output(res$inputs),
        outputs = private$map_input_output(res$outputs),
        output_location = res$output_location,
        auth = self$auth,
        response = attr(res, "response")
      )
    }
  ),
  private = list(
    # Map input/output values
    #' @importFrom checkmate test_list
    map_input_output = function(input) {
      return_value <- list()
      if (checkmate::test_list(input)) {
        if ("class" %in% names(input)) {
          if (tolower(input[["class"]]) %in% c("file", "folder")) {
            f_data <- list(
              id = input[["path"]],
              type = tolower(input[["class"]]),
              name = ifelse("basename" %in% names(input),
                input[["basename"]],
                input[["name"]]
              )
            )
            f_data <- append(f_data, input)
            if (!is.null(input[["secondaryFiles"]])) {
              secondary_files <- list()
              for (sf in input[["secondaryFiles"]]) {
                sf_data <- list(
                  id = sf[["path"]],
                  type = tolower(sf[["class"]]),
                  name = ifelse("basename" %in% names(sf),
                    sf[["basename"]],
                    sf[["name"]]
                  )
                )
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
    }
  )
)

# nocov start
# Helper function for creating Task objects
asTask <- function(x, auth = NULL) {
  Task$new(
    href = x$href,
    id = x$id,
    name = x$name,
    status = x$status,
    description = x$description,
    project = x$project,
    app = x$app,
    created_by = x$created_by,
    executed_by = x$executed_by,
    created_on = x$created_on,
    start_time = x$start_time,
    end_time = x$end_time,
    origin = x$origin,
    use_interruptable_instances = x$use_interruptable_instances,
    batch = x$batch,
    batch_by = x$batch_by,
    batch_group = x$batch_group,
    batch_input = x$batch_input,
    batch_parent = x$batch_parent,
    execution_settings = x$execution_settings,
    execution_status = x$execution_status,
    errors = x$errors,
    warnings = x$warnings,
    price = x$price,
    inputs = x$inputs,
    outputs = x$outputs,
    output_location = x$output_location,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of Task objects
asTaskList <- function(x, auth) {
  obj <- lapply(x$items, asTask, auth = auth)
  obj
}
# nocov end
