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

      if (!missing(project)) {
        project <-
          check_and_transform_id(project, class_name = "Project")
      }

      if (!missing(created_from)) {
        created_from <- check_and_transform_datetime(created_from)
      }

      if (!missing(created_to)) {
        created_to <- check_and_transform_datetime(created_to)
      }

      if (!missing(started_from)) {
        started_from <- check_and_transform_datetime(started_from)
      }

      if (!missing(started_to)) {
        started_to <- check_and_transform_datetime(started_to)
      }

      if (!missing(ended_from)) {
        ended_from <- check_and_transform_datetime(ended_from)
      }

      if (!missing(ended_to)) {
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

      return(res)
      # return(asTaskList(res, auth = self$auth)) # nocov end
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

      return(res)
      # return(asTask(res, auth = self$auth)) # nocov end
    }
  )
)
