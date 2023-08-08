# nolint start
#' @title R6 Class representing tasks endpoint
#'
#' @description
#' R6 Class representing tasks resource endpoint
#'
#' @importFrom R6 R6Class
#' @export
Tasks <- R6::R6Class(
  "Tasks",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "tasks/",
      "get" = "tasks/{id}"
    ),

    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List tasks you can access ------------------------------------------
    #' @description This call lists all tasks you can access.
    #'
    #' @param status String. You can filter the returned tasks by their status.
    #' Set the value of status to one of the following values: `QUEUED`,
    #' `DRAFT`, `RUNNING`, `COMPLETED`, `ABORTED`, `FAILED`.
    #' @param parent Provide task ID or task object of the parent task to return
    #'  all child tasks from that parent. A parent task is a task that specifies
    #' criteria by which to batch its inputs into a series of further sub-tasks,
    #'  calledchild tasks. See the documentation on
    #' [batching tasks](https://docs.sevenbridges.com/docs/about-batch-analyses)
    #'  for more details on how to run tasks in batches.
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
    #'
    #' @importFrom checkmate assert_string
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

      return(asTaskList(res, auth = self$auth)) # nocov end
    },

    # Get single task -------------------------------------------------------
    #' @description This call returns details of the specified task. The task
    #' is referred to by its ID, which you can obtain by making the call to
    #' list all tasks you can access. The task details include its creator, its
    #' start and end time, the number of jobs completed in it, and its input
    #' and output files. You can also see the status of the task.
    #'
    #' @param id The ID of the task you are querying.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
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

      return(asTask(res, auth = self$auth)) # nocov end
    },
    # Create a new draft task --------------------------------------------------
    #' @description This call creates a new task. You can create either a single
    #'  task or a batch task by using the app's default batching, override
    #'  batching, or disable batching completely. A parent task is a task that
    #'  specifies criteria by which to batch its inputs into a series of further
    #'  sub-tasks, called child tasks. the documentation on
    # nolint start
    #' [batching tasks] (https://docs.sevenbridges.com/docs/about-batch-analyses)
    # nolint end
    #' for more details on batching criteria.
    #'
    #' @param project The ID string of a project or a Project object where you
    #' want to create the task in.
    #' @param app The ID string of an app or an App object you want to run.
    #' Recall that apps are specified by their projects, in the form
    #' `{project_owner}/{project}/{app_name}`
    #' @param revision Numeric. The app
    #' [revision (version)] (https://docs.sevenbridges.com/docs/app-versions)
    #'  number.
    #' @param name String. The name of the task.
    #' @param description String. An optional description of the task.
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
    #' @param use_interruptible_instances Boolean. This field can be `TRUE` or
    #' `FALSE`. Set this field to `TRUE` to allow the use of
    # nolint start
    #' [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances).
    # nolint end
    #' @param action String. If set to `run`, the task will be run immediately
    #' upon creation.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    create = function(project,
                      app,
                      revision = NULL,
                      name = NULL,
                      description = NULL,
                      execution_settings = NULL,
                      inputs = NULL,
                      batch = NULL,
                      batch_input = NULL,
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
      checkmate::assert_logical(batch, null.ok = TRUE)
      checkmate::assert_string(batch_input, null.ok = TRUE)
      checkmate::assert_logical(use_interruptible_instances, null.ok = TRUE)
      checkmate::assert_string(action, null.ok = TRUE)

      task_inputs <- list()
      task_data <- list()
      params <- list()

      if (!is_missing(batch_input) && !is_missing(batch_by)) {
        task_data[["batch_input"]] <- batch_input
        task_data[["batch_by"]] <- batch_by
      }

      task_meta <- list(
        "name" = name,
        "project" = project_id,
        "app" = app_id,
        "description" = description
      )

      task_data <- c(task_data, task_meta)

      if (!is_missing(inputs)) {
        task_inputs <- private$serialize_inputs(inputs)
        task_data[["inputs"]] <- task_inputs
      }

      task_data[["use_interruptible_instances"]] <-
        use_interruptible_instances

      task_data[["execution_settings"]] <- execution_settings

      params[["action"]] <- action

      res <- sevenbridges2::api(
        path = self$URL[["query"]],
        method = "POST",
        query = params,
        body = task_data,
        token = self$auth$get_token(),
        base_url = self$auth$url,
      )

      res <- status_check(res)

      return(asTask(res, auth = self$auth))
    }
  ),
  private = list(
    # Serialize input values  --------------------------------------------------
    #' @importFrom checkmate test_r6
    serialize_inputs = function(input_value) {
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
    }
  )
)
