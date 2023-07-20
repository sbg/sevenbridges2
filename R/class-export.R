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
    #' @field id String. Export job identifier.
    id = NULL,
    #' @field state String. The state of the export job. Possible values are:
    #' \itemize{
    #'    \item `PENDING`: the export is queued;
    #'    \item `RUNNING`: the export is running;
    #'    \item `COMPLETED`: the export has completed successfully;
    #'    \item `FAILED`: the export has failed.
    #' }
    state = NULL,
    #' @field source List containing source file id that is being exported to
    #' the volume.
    source = NULL,
    #' @field destination List containing destination volume id and location
    #' (file name) on the volume where the file is being exported.
    destination = NULL,
    #' @field overwrite Boolean. Whether the exported file name was
    #' overwritten or not, if another one with the same name had already
    #' existed on the volume.
    overwrite = NULL,
    #' @field started_on Time when the export job started.
    started_on = NULL,
    #' @field finished_on Time when the export job ended.
    finished_on = NULL,
    #' @field properties List of volume properties set.
    properties = NULL,
    #' @field error In case of error in the export job, standard API error is
    #' returned here.
    error = NULL,
    #' @field result File object that was exported.
    result = NULL,

    #' @description Create a new Export object.
    #' @param id String. Export job identifier.
    #' @param state String. The state of the export job.
    #' @param source List containing source file id that is being exported to
    #' the volume.
    #' @param destination List containing destination volume id and location
    #' (file name) on the volume where the file is being exported.
    #' @param overwrite Boolean. Whether the exported file name was
    #' overwritten or not, if another one with the same name had already
    #' existed on the volume.
    #' @param started_on Time when the export job started.
    #' @param finished_on Time when the export job ended.
    #' @param properties List of volume properties set.
    #' @param error In case of error in the export job, standard API error is
    #' returned here.
    #' @param result File object that was exported.
    #' @param ... Other arguments.
    initialize = function(id = NA, state = NA, source = NA, destination = NA,
                          overwrite = NA, started_on = NA, finished_on = NA,
                          properties = NA, error = NA, result = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$state <- state
      self$source <- source
      self$destination <- destination
      self$overwrite <- overwrite
      self$started_on <- started_on
      self$finished_on <- finished_on
      self$properties <- properties
      self$error <- error
      if (!is_missing(result)) {
        self$result <- asFile(result, self$auth)
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
    } # nocov end
  )
)
# nocov start
# Helper function for creating Export objects
asExport <- function(x, auth = NULL) {
  Export$new(
    href = x$href,
    id = x$id,
    state = x$state,
    source = x$source,
    destination = x$destination,
    overwrite = x$overwrite,
    started_on = x$started_on,
    finished_on = x$finished_on,
    properties = x$properties,
    error = x$error,
    result = x$result,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of Export objects
asExportList <- function(x, auth) {
  obj <- lapply(x$items, asExport, auth = auth)
  obj
}
# nocov end
