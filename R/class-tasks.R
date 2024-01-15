# nolint start
#' @title R6 Class representing tasks endpoints
#'
#' @description
#' R6 Class representing tasks resource endpoints.
#'
#' @importFrom R6 R6Class
#'
#' @export
Tasks <- R6::R6Class(
  "Tasks",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "tasks/",
      "get" = "tasks/{id}",
      "delete" = "tasks"
    ),

    #' @description Create new Tasks resource object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List tasks you can access ------------------------------------------
    #' @description This call lists all tasks you can access. \cr \cr
    #' Read more about how to use query parameters properly
    # nolint start
    #'  [here](https://docs.sevenbridges.com/reference/list-tasks-you-can-access).
    # nolint end
    #'
    #' @param status You can filter the returned tasks by their status.
    #'  Set the value of status to one of the following values: `QUEUED`,
    #'  `DRAFT`, `RUNNING`, `COMPLETED`, `ABORTED`, `FAILED`.
    #' @param parent Provide task ID or Task object of the parent task to return
    #'  all child tasks from that parent. A parent task is a task that specifies
    #'  criteria by which to batch its inputs into a series of further
    #'  sub-tasks, called child tasks. See the documentation on
    # nolint start
    #'  [batching tasks](https://docs.sevenbridges.com/docs/about-batch-analyses)
    # nolint end
    #'  for more details on how to run tasks in batches.
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
    #'  Allowed values: \cr `created_time`, `start_time`, `name`, `end_time` and
    #'  `created_by`. \cr Sort can be done only by one column. The default
    #'  value is `created_time`.
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
    #' @importFrom checkmate assert_string
    #'
    #' @return \code{\link{Collection}} of \code{\link{Task}} objects.
    query = function(status = NULL,
                     parent = NULL,
                     project = NULL,
                     created_from = NULL,
                     created_to = NULL,
                     started_from = NULL,
                     started_to = NULL,
                     ended_from = NULL,
                     ended_to = NULL,
                     order_by = c(
                       "created_time", "start_time", "name",
                       "end_time", "created_by"
                     ),
                     order = c("asc", "desc"),
                     origin_id = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      if (!is_missing(status)) {
        if (!(status %in% c(
          "QUEUED",
          "DRAFT",
          "RUNNING",
          "COMPLETED",
          "ABORTED",
          "FAILED"
        ))) {
          rlang::abort(
            "Status must be one of the following: QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, FAILED" # nolint
          )
        }
      }

      if (!is_missing(parent)) {
        parent <-
          check_and_transform_id(parent, class_name = "Task")
      }

      if (!is_missing(project)) {
        project <-
          check_and_transform_id(project, class_name = "Project")
      }

      if (!is_missing(created_from)) {
        created_from <- check_and_transform_datetime(created_from)
      }

      if (!is_missing(created_to)) {
        created_to <- check_and_transform_datetime(created_to)
      }

      if (!is_missing(started_from)) {
        started_from <- check_and_transform_datetime(started_from)
      }

      if (!is_missing(started_to)) {
        started_to <- check_and_transform_datetime(started_to)
      }

      if (!is_missing(ended_from)) {
        ended_from <- check_and_transform_datetime(ended_from)
      }

      if (!is_missing(ended_to)) {
        ended_to <- check_and_transform_datetime(ended_to)
      }

      order_by <- match.arg(order_by)
      order <- match.arg(order)

      checkmate::assert_string(origin_id, null.ok = TRUE)

      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
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

      res$items <- asTaskList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Get single task -------------------------------------------------------
    #' @description This call returns details of the specified task. The task
    #'  is referred to by its ID, which you can obtain by making the call to
    #'  list all tasks you can access. The task details include its creator, its
    #'  start and end time, the number of jobs completed in it, and its input
    #'  and output files. You can also see the status of the task.
    #'
    #' @param id The ID of the task you are querying.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #'
    #' @return \code{\link{Task}} object.
    get = function(id, ...) {
      if (is_missing(id)) {
        rlang::abort("Task ID must be provided!")
      }

      checkmate::assert_string(id)

      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        ...
      )

      return(asTask(res, auth = self$auth))
    }, # nocov end

    # Delete task  ----------------------------------------------------------
    #' @description This call deletes a task from the Seven Bridges Platform.
    #' Tasks are specified by their IDs, which you can obtain by using
    #' \code{Tasks$query()} to list tasks or by getting a single task
    #' using \code{Tasks$get()}.
    #'
    #' @param task \code{\link{Task}} object or task ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' as 'fields', etc.
    #'
    #' @importFrom glue glue
    delete = function(task, ...) {
      id <- check_and_transform_id(task, "Task")
      # nocov start
      res <- super$delete(
        id = id,
        ...
      )

      rlang::inform(
        message = glue::glue("Task {id} has been deleted.")
      )
    },
    # nocov end

    # Create a new draft task --------------------------------------------------
    #' @description This call creates a new task. You can create either a single
    #'  task or a batch task by using the app's default batching, override
    #'  batching, or disable batching completely. A parent task is a task that
    #'  specifies criteria by which to batch its inputs into a series of further
    #'  sub-tasks, called child tasks. the documentation on
    # nolint start
    #' [batching tasks](https://docs.sevenbridges.com/docs/about-batch-analyses)
    # nolint end
    #' for more details on batching criteria.
    #'
    #' @param project The ID of a project or a Project object where you
    #'  want to create the task in.
    #' @param app The ID of an app or an App object you want to run.
    #'  Recall that apps are specified by their projects, in the form
    #'  `<project_id>/<app_name>`.
    #' @param revision The app
    #' [revision (version)](https://docs.sevenbridges.com/docs/app-versions)
    #'  number.
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
    #'      task output nodes in the following format for each output node:\cr
    #'      \code{<output-node-id> = list(
    #'        "output_location" = "<output-path>",
    #'        "output_location_alias" = "<alias-path>"
    #'      )} \cr
    #'      ```{r}
    #'      b64html = list(
    #'      "output_location" = "volumes://outputs/tasks/mar-19",
    #'      "output_location_alias" = "/rfranklin/tasks/picard"
    #'      )
    #'      ```
    #'
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
    #' @param use_interruptible_instances This field can be `TRUE` or `FALSE`.
    #'  Set this field to `TRUE` to allow the use of
    # nolint start
    #' [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances).
    # nolint end
    #' @param action If set to `run`, the task will be run immediately upon
    #'  creation.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_string assert_list assert_logical
    #' @importFrom rlang abort
    #'
    #' @return \code{\link{Task}} object.
    create = function(project,
                      app,
                      revision = NULL,
                      name = NULL,
                      description = NULL,
                      execution_settings = NULL,
                      inputs = NULL,
                      output_location = NULL,
                      batch = NULL,
                      batch_input = NULL,
                      batch_by = NULL,
                      use_interruptible_instances = NULL,
                      action = NULL,
                      ...) {
      if (is_missing(project)) {
        rlang::abort("Project parameter must be provided!")
      }

      if (is_missing(app)) {
        rlang::abort("App parameter must be provided!")
      }

      project_id <-
        check_and_transform_id(project, class_name = "Project")
      app_id <- check_and_transform_id(app, class_name = "App")

      if (!is_missing(revision)) {
        checkmate::check_int(revision, null.ok = TRUE)
        app_id <- glue::glue("{app_id}/{revision}")
      } else if (checkmate::test_r6(app)) {
        app_id <- glue::glue("{app_id}/{app$revision}")
      }

      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_string(description, null.ok = TRUE)
      check_execution_settings(execution_settings)
      checkmate::assert_list(inputs, null.ok = TRUE)
      checkmate::assert_list(output_location, null.ok = TRUE)
      checkmate::assert_logical(batch, null.ok = TRUE)
      checkmate::assert_string(batch_input, null.ok = TRUE)
      checkmate::assert_list(batch_by, null.ok = TRUE)
      checkmate::assert_logical(use_interruptible_instances, null.ok = TRUE)
      checkmate::assert_string(action, null.ok = TRUE)

      task_inputs <- list()
      task_data <- list()
      params <- list()

      task_meta <- list(
        "name" = name,
        "project" = project_id,
        "app" = app_id,
        "description" = description
      )
      task_data <- c(task_data, task_meta)

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
        task_inputs <- private$serialize_inputs(inputs)
        task_data[["inputs"]] <- task_inputs
      }
      if (!is_missing(output_location)) {
        task_data[["output_location"]] <- output_location
      }
      task_data[["use_interruptible_instances"]] <-
        use_interruptible_instances

      task_data[["execution_settings"]] <- execution_settings

      params[["action"]] <- action
      params[["batch"]] <- batch

      res <- sevenbridges2::api(
        path = self$URL[["query"]],
        method = "POST",
        query = params,
        body = task_data,
        token = self$auth$get_token(),
        base_url = self$auth$url,
      )

      return(asTask(res, auth = self$auth))
    } # nocov end
  ),
  private = list(
    # Serialize input values  --------------------------------------------------
    #' @importFrom checkmate test_r6
    serialize_inputs = function(input_value) { # nocov start
      if (is.list(input_value)) {
        return_value <- list()
        if (is.null(names(input_value))) {
          for (x in input_value) {
            elem <- private$serialize_inputs(x)
            return_value <- c(return_value, list(elem))
          }
        } else {
          for (name in names(input_value)) {
            elem <- private$serialize_inputs(input_value[[name]])
            return_value[[name]] <- elem
          }
        }
      } else if (checkmate::test_r6(input_value, classes = "File")) {
        return_value <- private$to_api_file_format(input_value)
      } else {
        return_value <- input_value
      }

      return(return_value)
    },

    # Convert input value to File format  --------------------------------------
    #' @importFrom stringr str_to_title
    to_api_file_format = function(file) {
      return(list(
        "class" = stringr::str_to_title(file[["type"]]),
        "path" = file[["id"]]
      ))
    } # nocov end
  )
)
