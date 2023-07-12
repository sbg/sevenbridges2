# nolint start
#' @title R6 Class representing storage imports endpoints
#'
#' @description
#' R6 Class representing storage imports resource endpoints
#'
#' @importFrom R6 R6Class
#' @export
Imports <- R6::R6Class(
  "Imports",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "storage/imports",
      "get" = "storage/imports/{id}",
      "create" = "storage/imports"
    ),

    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },
    # List all import jobs --------------------------------------
    #' @description This call lists import jobs initiated by particular user.
    #' Note that when you import a file from your volume on your cloud storage
    #' provider (Amazon Web Services or Google Cloud Storage), you are
    #' creating an alias on the Platform which points to the file in your
    #' cloud storage bucket. Aliases appear as files on the Platform and can
    #' be copied, executed, and modified as such. They refer back to the
    #' respective file on the given volume.
    #'
    #' @param volume String volume id or Volume object. List all imports
    #' from this particular volume. Optional.
    #' @param project String project id or Project object. List all volume
    #' imports to this particular project. Optional.
    #' @param state The state of the import job. Possible values are:
    #' \itemize{
    #'    \item `PENDING`: the import is queued;
    #'    \item `RUNNING`: the import is running;
    #'    \item `COMPLETED`: the import has completed successfully;
    #'    \item `FAILED`: the import has failed.
    #' }
    #' Example: state = c("RUNNING", "FAILED")
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #' @importFrom checkmate assert_character assert_subset
    #' @return Collection of import jobs (Import class objects).
    query = function(volume = NULL, project = NULL, state = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      if (!is_missing(volume)) {
        volume <- check_and_transform_id(volume, "Volume")
      }
      if (!is_missing(project)) {
        project <- check_and_transform_id(project, "Project")
      }
      if (!is_missing(state)) {
        checkmate::assert_character(state, max.len = 4)
        checkmate::assert_subset(state,
          choices = c(
            "PENDING", "RUNNING",
            "COMPLETED", "FAILED"
          )
        )
      }
      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        volume = volume,
        project = project,
        state = state,
        limit = limit,
        offset = offset,
        ...
      )
      return(res)
      # return(asImportList(res, auth = self$auth))
    }, # nocov end

    # Get import job details -----------------------------------------------
    #' @description This call will return the details of an import job.
    #'
    #' @param id The import job identifier (id)
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @return Import job object.
    get = function(id, ...) {
      if (is_missing(id)) {
        rlang::abort("Import job ID must be provided!")
      }
      checkmate::assert_string(id)
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE,
        ...
      )
      return(res)
      # return(asImport(res, auth = self$auth))
    } # nocov end
  )
)
