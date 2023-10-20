# nolint start
#' @title R6 Class representing storage imports endpoints
#'
#' @description
#' R6 Class representing storage imports resource endpoints.
#'
#' @importFrom R6 R6Class
#' @export
Imports <- R6::R6Class(
  "Imports",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "storage/imports",
      "get" = "storage/imports/{id}",
      "create" = "storage/imports"
    ),

    #' @description Create a new Imports object.
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all import jobs --------------------------------------
    #' @description This call lists import jobs initiated by particular user.
    #'  Note that when you import a file from your volume on your cloud storage
    #'  provider (Amazon Web Services or Google Cloud Storage), you are
    #'  creating an alias on the Platform which points to the file in your
    #'  cloud storage bucket. Aliases appear as files on the Platform and can
    #'  be copied, executed, and modified as such. They refer back to the
    #'  respective file on the given volume.
    #'
    #' @param volume String volume id or Volume object. List all imports
    #'  from this particular volume. Optional.
    #' @param project String project id or Project object. List all volume
    #'  imports to this particular project. Optional.
    #' @param state The state of the import job. Possible values are:
    #'  \itemize{
    #'    \item `PENDING`: the import is queued;
    #'    \item `RUNNING`: the import is running;
    #'    \item `COMPLETED`: the import has completed successfully;
    #'    \item `FAILED`: the import has failed.
    #'  }
    #' Example:
    #' ```{r}
    #' state = c("RUNNING", "FAILED")
    #' ```
    #' @param limit The maximum number of collection items to return
    #' for a single request. Minimum value is `1`.
    #' The maximum value is `100` and the default value is `50`.
    #' This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #' of the first item to return. The default value is `0`.
    #' This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_character assert_subset
    #'
    #' @return Collection containing Import jobs (Import class objects).
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
      res$items <- asImportList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    }, # nocov end

    # Get import job details -----------------------------------------------
    #' @description This call will return the details of an import job.
    #'
    #' @param id The import job identifier (id).
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return Import object.
    get = function(id, ...) {
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE,
        ...
      )
      return(asImport(res, auth = self$auth))
    }, # nocov end

    # Start new import job -----------------------------------------------
    #' @description This call lets you queue a job to import a file or folder
    #'  from a volume into a project on the Platform.
    #'  Essentially, you are importing an item from your cloud storage provider
    #'  (Amazon Web Services, Google Cloud Storage, Azure or Ali Cloud) via the
    #'  volume onto the Platform. \cr
    #'  If successful, an alias will be created on the Platform. Aliases appear
    #'  on the Platform and can be copied, executed, and modified as such.
    #'  They refer back to the respective item on the given volume. \cr
    #'
    #'  If you want to import multiple files, the recommended way is to do it
    # nolint start
    #'  in bulk considering the API rate limit ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    # nolint end
    #'  (bulk operations will be implemented in next releases).
    #'
    #' @param source_volume String volume id or Volume object you want to import
    #'  files or folders from. Required if `source_location` parameter is
    #'  provided as a string.
    #' @param source_location String file/folder location name on the volume or
    #'  VolumeFile object you would like to import into some project/folder on
    #'  the platform.
    #' @param destination_project String destination project id or Project
    #'  object. Not required, but either `destination_project` or
    #'  `destination_parent` directory must be provided.
    #' @param destination_parent String folder id or File object
    #'  (with `type = 'FOLDER'`). Not required, but either `destination_project`
    #'  or `destination_parent` directory must be provided.
    #' @param name The name of the alias to create. This name should be unique
    #'  to the project. \cr
    #'  If the name is already in use in the project, you should
    #'  use the `overwrite` query parameter in this call to force any item with
    #'  that name to be deleted before the alias is created.
    #'  If name is omitted, the alias name will default to the last segment of
    #'  the complete location (including the prefix) on the volume. \cr
    #'
    #'  Segments are considered to be separated with forward slashes /.
    #'  Allowed characters in file names are all alphanumeric and special
    #'  characters except forward slash /, while folder names can contain
    #'  alphanumeric and special characters _, - and ..
    #'
    #' @param overwrite Set to `TRUE` if you want to overwrite the item if
    #'  another one with the same name already exists at the destination.
    #'  Bear in mind that if used with folders import, the folder's content
    #'  (files with the same name) will be overwritten, not the whole folder.
    #' @param autorename Set to `TRUE` if you want to automatically rename the
    #'  item (by prefixing its name with an underscore and number) if another
    #'  one with the same name already exists at the destination.
    #'  Bear in mind that if used with folders import, the folder content will
    #'  be renamed, not the whole folder.
    #' @param preserve_folder_structure Set to `TRUE` if you want to keep the
    #'  exact source folder structure. The default value is `TRUE` if the item
    #'  being imported is a folder. Should not be used if you are importing a
    #'  file. Bear in mind that if you use `preserve_folder_structure = FALSE`,
    #'  that the response will be the parent folder object containing imported
    #'  files alongside with other files if they exist.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate test_r6 assert_string assert_logical
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #'
    #' @return Import object.
    submit_import = function(source_volume = NULL, source_location,
                             destination_project = NULL,
                             destination_parent = NULL,
                             name = NULL, overwrite = FALSE,
                             autorename = FALSE,
                             preserve_folder_structure = NULL, ...) {
      if (is_missing(source_location)) {
        rlang::abort("Source file/folder location must be provided as a string or VolumeFile object!") # nolint
      }
      if (is_missing(source_volume)) {
        if (checkmate::test_r6(source_location, classes = "VolumeFile")) {
          volume <- check_and_transform_id(source_location,
            class_name = "VolumeFile",
            field_name = "volume"
          )
        } else {
          rlang::abort(
            "Volume id must be provided if source location is provided as string. \nSource file/folder location must be provided as a string or VolumeFile object." # nolint
          )
        }
      } else {
        volume <- check_and_transform_id(source_volume, class_name = "Volume")
      }
      location <- check_and_transform_id(source_location,
        class_name = "VolumeFile",
        field_name = "location"
      )
      if (is_missing(destination_project) &&
        is_missing(destination_parent)) {
        rlang::abort("Please, provide either destination project or parent parameter.") # nolint
      }
      if (!is_missing(destination_project) &&
        !is_missing(destination_parent)) {
        rlang::abort("Either destination project or parent parameter must be proveded, not both.") # nolint
      }
      if (!is_missing(destination_project)) {
        destination_project <- check_and_transform_id(
          destination_project,
          class_name = "Project"
        )
      }
      if (!is_missing(destination_parent)) {
        if (checkmate::test_r6(destination_parent, classes = "File") &&
          tolower(destination_parent$type) != "folder") {
          rlang::abort("Destination parent directory parameter must contain folder id or File object with type = 'folder'.") # nolint
        }
        destination_parent <- check_and_transform_id(
          x = destination_parent,
          class_name = "File"
        )
      }

      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_logical(overwrite, len = 1, null.ok = TRUE)
      checkmate::assert_logical(autorename, len = 1, null.ok = TRUE)
      checkmate::assert_logical(preserve_folder_structure, len = 1, null.ok = TRUE) # nolint

      # Build body
      # nocov start
      body <- list(
        source = list(
          volume = volume,
          location = location
        ),
        destination = list(
          project = destination_project,
          parent = destination_parent,
          name = name
        ),
        overwrite = overwrite,
        autorename = autorename,
        preserve_folder_structure = preserve_folder_structure
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      import <- asImport(res, auth = self$auth)

      rlang::inform(glue::glue_col("New import with id {green {import$id} } has started!")) # nolint

      return(import)
    },

    # Delete import job ----------------------------------------------------
    #' @description Deleting import jobs is not possible.
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting import jobs is not possible.")
    } # nocov end
  )
)
