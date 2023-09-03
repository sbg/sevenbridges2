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
    #' @field URL URL endpoint fields
    URL = list(
      "reload" = "storage/imports/{id}"
    ),
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
    #' @param res Response containing Import object information.
    #' @param ... Other arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$state <- res$state
      self$overwrite <- res$overwrite
      self$autorename <- res$autorename
      self$preserve_folder_structure <- res$preserve_folder_structure
      self$source <- res$source
      self$destination <- res$destination
      self$started_on <- res$started_on
      self$finished_on <- res$finished_on
      self$error <- res$error
      if (!is_missing(res$result)) {
        self$result <- asFile(res$result, self$auth)
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
    },
    #' @description
    #' Reload Import.
    #' @param ... Other query parameters.
    #' @return Import
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Import object is refreshed!")
    } # nocov end
  )
)
# nocov start
# Helper function for creating Import objects
asImport <- function(x = NULL, auth = NULL) {
  Import$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

# Helper function for creating a list of Import objects
asImportList <- function(x, auth) {
  obj <- lapply(x$items, asImport, auth = auth)
  obj
}
# nocov end
