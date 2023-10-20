# nolint start
#' @title R6 Class representing an Export
#'
#' @description
#' R6 Class representing a resource for managing volume export jobs.
#'
#' @importFrom R6 R6Class
#' @export
Export <- R6::R6Class(
  # nolint end
  "Export",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "storage/exports/{id}"
    ),
    #' @field id Export job string identifier.
    id = NULL,
    #' @field state The state of the export job. Possible values are:
    #'  \itemize{
    #'    \item `PENDING`: the export is queued;
    #'    \item `RUNNING`: the export is running;
    #'    \item `COMPLETED`: the export has completed successfully;
    #'    \item `FAILED`: the export has failed.
    #'  }
    state = NULL,
    #' @field source List containing source file id that is being exported to
    #'  the volume.
    source = NULL,
    #' @field destination List containing destination volume id and location
    #'  (file name) on the volume where the file is being exported.
    destination = NULL,
    #' @field overwrite Whether the exported file name was
    #'  overwritten or not, if another one with the same name had already
    #'  existed on the volume.
    overwrite = NULL,
    #' @field started_on Time when the export job started.
    started_on = NULL,
    #' @field finished_on Time when the export job ended.
    finished_on = NULL,
    #' @field properties List of volume properties set.
    properties = NULL,
    #' @field error In case of error in the export job, standard API error is
    #'  returned here.
    error = NULL,
    #' @field result File object that was exported.
    result = NULL,

    #' @description Create a new Export object.
    #' @param res Response containing Export job information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$state <- res$state
      self$source <- res$source
      self$destination <- res$destination
      self$overwrite <- res$overwrite
      self$started_on <- res$started_on
      self$finished_on <- res$finished_on
      self$properties <- res$properties
      self$error <- res$error
      if (!is_missing(res$result)) {
        self$result <- asFile(res$result, self$auth)
      }
    },

    # nocov start
    #' @description Print method for Export class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    print = function() {
      x <- as.list(self)

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)

      # Flatten the list keeping names
      x <- as.list(unlist(x))

      # Remove if any leftover lists
      x <- purrr::discard(x, .p = is.list)

      elements_subset <- c("id", "state", "started_on", "finished_on")
      x <- x[elements_subset]

      x <- purrr::discard(x, .p = is.null)
      # Remove it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Export job")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },
    #' @description Reload Export object information.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #' @return Export object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Export object is refreshed!")
    } # nocov end
  )
)
# nocov start
# Helper function for creating Export objects
asExport <- function(x = NULL, auth = NULL) {
  Export$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

# Helper function for creating a list of Export objects
asExportList <- function(x, auth) {
  obj <- lapply(x$items, asExport, auth = auth)
  obj
}
# nocov end
