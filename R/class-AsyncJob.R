# nolint start
#' @title R6 Class representing an AsyncJob
#'
#' @description
#' R6 Class representing a resource for managing asynchronous jobs.
#'
#' @importFrom R6 R6Class
#' @export
AsyncJob <- R6::R6Class(
  # nolint end
  "AsyncJob",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id Asynchronous job ID.
    id = NULL,
    #' @field type The type of job. Can be one of: COPY, DELETE, MOVE.
    type = NULL,
    #' @field state The following states are available: SUBMITTED, RESOLVING,
    #'  RUNNING and FINISHED.
    state = NULL,
    #' @field result The result of the job.
    result = NULL,
    #' @field total_files The total number of files that were processed for
    #' the job.
    total_files = NULL,
    #' @field completed_files The number of files that were successfully
    #' completed.
    completed_files = NULL,
    #' @field failed_files The number of files that failed.
    failed_files = NULL,
    #' @field started_on The time and date the job started.
    started_on = NULL,
    #' @field finished_on The time and date the job finished.
    finished_on = NULL,

    # Initialize AsyncJob object ----------------------------------------------
    #' @description Create a new AsyncJob object.
    #'
    #' @param res Response containing AsyncJob object information.
    #'
    #' @param ... Other response arguments.
    #'
    #' @return A new `AsyncJob` object.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$type <- res$type
      self$state <- res$state
      self$result <- res$result
      self$total_files <- res$total_files
      self$completed_files <- res$completed_files
      self$failed_files <- res$failed_files
      self$started_on <- res$started_on
      self$finished_on <- res$finished_on
    },

    # nocov start
    # Print AsyncJob object ---------------------------------------------------
    #' @description Print method for AsyncJob class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  asyncjob_object <- AsyncJob$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  asyncjob_object$print()
    #' }
    print = function() {
      x <- as.list(self)

      # Extract all except 'raw'
      x$raw <- NULL

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Asynchronous Job")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    # Reload AsyncJob object --------------------------------------------------
    #' @description Reloads AsyncJob object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when AsyncJob is requested
    #'  asyncjob_object <- AsyncJob$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  asyncjob_object$reload()
    #' }
    #' @return \code{\link{AsyncJob}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("AsyncJob object is refreshed!")
    }
  )
)

# Helper functions for creating AsyncJob objects ------------------------------
asAsyncJob <- function(x = NULL, auth = NULL) {
  AsyncJob$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asAsyncJobList <- function(x, auth) {
  obj <- lapply(x$items, asAsyncJob, auth = auth)
  obj
}
# nocov end
