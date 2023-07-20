# nolint start
#' @title R6 Class representing an Import
#'
#' @description
#' R6 Class representing a resource for managing volume import jobs.
#'
#' @importFrom R6 R6Class
#' @export
Import <- R6::R6Class(
  # nolint end
  "Import",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id String. Import job identifier.
    id = NULL,
    #' @field state String. The state of the import job. Possible values are:
    #' \itemize{
    #'    \item `PENDING`: the import is queued;
    #'    \item `RUNNING`: the import is running;
    #'    \item `COMPLETED`: the import has completed successfully;
    #'    \item `FAILED`: the import has failed.
    #' }
    state = NULL,
    #' @field overwrite Boolean. Whether the imported file/folder name was
    #' overwritten or not, if another one with the same name had already
    #' existed.
    overwrite = NULL,
    #' @field autorename Boolean. Whether the imported file/folder name was
    #' automatically renamed (by prefixing its name with an underscore and
    #' number) if another one with the same name had already existed.
    autorename = NULL,
    #' @field preserve_folder_structure Boolean. Whether the imported folder
    #' structure was preserved or not.
    preserve_folder_structure = NULL,
    #' @field source List containing source volume id and source location of the
    #' file/folder is being imported to the platform.
    source = NULL,
    #' @field destination List containing destination project id or parent
    #' directory id where the file/folder is being imported, together with its
    #' name.
    destination = NULL,
    #' @field started_on Time when the import job started.
    started_on = NULL,
    #' @field finished_on Time when the import job ended.
    finished_on = NULL,
    #' @field error In case of error in the import job, standard API error is
    #' returned here.
    error = NULL,
    #' @field result File object that was imported.
    result = NULL,

    #' @description Create a new Import object.
    #' @param id String. Import job identifier.
    #' @param state String. The state of the import job.
    #' @param overwrite Boolean. Whether the imported file/folder name was
    #' overwritten or not if another one with the same name had already existed.
    #' @param autorename Boolean. Whether the imported file/folder name was
    #' automatically renamed (by prefixing its name with an underscore and
    #' number) if another one with the same name had already existed.
    #' @param preserve_folder_structure Boolean. Whether the imported folder
    #' structure was preserved or not.
    #' @param source List containing source volume id and source location of the
    #' file/folder is being imported to the platform.
    #' @param destination List containing destination project id or parent
    #' directory id where the file/folder is being imported, together with its
    #' name.
    #' @param started_on Time when the import job started.
    #' @param finished_on Time when the import job ended.
    #' @param error In case of error in the import job, standard API error is
    #' returned here.
    #' @param result File object that was imported.
    #' @param ... Other arguments.
    initialize = function(id = NA, state = NA, overwrite = NA, autorename = NA,
                          preserve_folder_structure = NA, source = NA,
                          destination = NA, started_on = NA, finished_on = NA,
                          error = NA, result = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$state <- state
      self$overwrite <- overwrite
      self$autorename <- autorename
      self$preserve_folder_structure <- preserve_folder_structure
      self$source <- source
      self$destination <- destination
      self$started_on <- started_on
      self$finished_on <- finished_on
      self$error <- error
      if (!is_missing(result)) {
        self$result <- asFile(result, self$auth)
      }
    },
    # nocov start
    #' @description Print method for Import class.
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

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Import job")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    } # nocov end
  )
)
# nocov start
# Helper function for creating Import objects
asImport <- function(x, auth = NULL) {
  Import$new(
    href = x$href,
    id = x$id,
    state = x$state,
    overwrite = x$overwrite,
    autorename = x$autorename,
    preserve_folder_structure = x$preserve_folder_structure,
    source = x$source,
    destination = x$destination,
    started_on = x$started_on,
    finished_on = x$finished_on,
    error = x$error,
    result = x$result,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of Import objects
asImportList <- function(x, auth) {
  obj <- lapply(x$items, asImport, auth = auth)
  obj
}
# nocov end
