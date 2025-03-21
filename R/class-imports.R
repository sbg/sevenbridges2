# nolint start
#' @title R6 Class representing storage imports endpoints
#'
#' @description
#' R6 Class for managing storage imports resource endpoints.
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
      "create" = "storage/imports",
      "bulk_get" = "bulk/storage/imports/get",
      "bulk_create" = "bulk/storage/imports/create"
    ),

    # Initialize Imports object -----------------------------------------------
    #' @description Create a new Imports object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List import jobs --------------------------------------------------------
    #' @description This call lists import jobs initiated by a particular user.
    #'  Note that when you import a file from your volume on your cloud storage
    #'  provider (Amazon Web Services or Google Cloud Storage), you are
    #'  creating an alias on the Platform which points to the file in your
    #'  cloud storage bucket. Aliases appear as files on the Platform and can
    #'  be copied, executed, and modified as such. They refer back to the
    #'  respective file on the given volume.
    #'
    #' @param volume Volume id or Volume object. List all imports
    #'  from this particular volume. Optional.
    #' @param project Project id or Project object. List all volume
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
    #' @examples
    #' \dontrun{
    #'  imports_object <- Imports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # List import job
    #'  imports_object$query()
    #' }
    #'
    #' @return \code{\link{Collection}} of \code{\link{Import}} objects.
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
    #' @examples
    #' \dontrun{
    #'  imports_object <- Imports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # List import job
    #'  imports_object$get(id = id)
    #' }
    #'
    #' @return \code{\link{Import}} object.
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
    #'  Bulk operations will be implemented in next releases.
    #'
    #' @param source_volume Volume id or Volume object you want to import
    #'  files or folders from.
    #' @param source_location File location name or folder prefix name on the
    #'  volume you would like to import into some project/folder
    #'  on the Platform.
    #' @param destination_project Destination project id or Project
    #'  object. Not required, but either \cr
    #'  `destination_project` or `destination_parent` directory must be
    #'  provided.
    #' @param destination_parent Folder id or File object
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
    #'  Segments are considered to be separated with forward slashes `/`.
    #'  Allowed characters in file names are all alphanumeric and special
    #'  characters except forward slash `/`, while folder names can contain
    #'  alphanumeric and special characters `_`, `-` and `.`.
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
    #'  the response will be the parent folder object containing imported files
    #'  alongside with other files if they exist.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate test_r6 assert_string assert_logical
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #'
    #' @examples
    #' \dontrun{
    #'  imports_object <- Imports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # Submit new import into a project
    #'  imports_object$submit_import(
    #'   source_location = volume_file_object,
    #'   destination_project = test_project_object,
    #'   autorename = TRUE
    #'  )
    #' }
    #'
    #' @return \code{\link{Import}} object.
    submit_import = function(source_volume, source_location,
                             destination_project = NULL,
                             destination_parent = NULL,
                             name = NULL, overwrite = FALSE,
                             autorename = FALSE,
                             preserve_folder_structure = NULL, ...) {
      if (is_missing(source_volume)) {
        rlang::abort(
          "Volume ID must be provided as string or Volume object."
        )
      } else {
        volume <- check_and_transform_id(source_volume, class_name = "Volume")
      }

      if (is_missing(source_location)) {
        rlang::abort("Source file/folder location/prefix must be provided as a string.") # nolint
      }
      checkmate::assert_string(source_location, na.ok = FALSE, null.ok = FALSE)

      if (is_missing(destination_project) &&
        is_missing(destination_parent)) {
        rlang::abort("Please provide either destination project or parent parameter.") # nolint
      }
      if (!is_missing(destination_project) &&
        !is_missing(destination_parent)) {
        rlang::abort("Either destination project or parent parameter must be provided, not both.") # nolint
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
          location = source_location
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

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body,
        advance_access = TRUE,
        ...
      )

      import <- asImport(res, auth = self$auth)

      rlang::inform(glue::glue_col("New import with id {green {import$id} } has started!")) # nolint

      return(import)
    },

    # Delete import job ----------------------------------------------------
    #' @description Import jobs cannot be deleted.
    #'
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting import jobs is not possible.")
    }, # nocov end

    # Get bulk import jobs ----------------------------------------------------
    #' @description This call returns the details of a bulk import job.
    #'  Note that when you import files from your volume on a cloud storage
    #'  provider (Amazon Web Services or Google Cloud Storage), you create
    #'  an alias on the Platform which points to the files in your cloud
    #'  storage bucket. Aliases appear as files on the Platform and can
    #'  be copied, executed, and modified.
    #'
    #' @param imports The list of the import job IDs as returned by the call
    #'  to start a bulk import job or list of \code{\link{Import}} objects.
    #'
    #' @importFrom checkmate assert_list
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  imports_object <- Imports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # List import job
    #'  imports_object$bulk_get(
    #'   imports = list("import-job-id-1", "import-job-id-2")
    #'   )
    #' }
    #'
    #' @return \code{\link{Collection}} with list of \code{\link{Import}}
    #'  objects.
    bulk_get = function(imports) {
      if (is_missing(imports)) {
        rlang::abort("Imports should be set as a list of import job IDs or list of Import objects.") # nolint
      }
      checkmate::assert_list(imports)
      unlisted_ids <- lapply(imports, check_and_transform_id, "Import")

      # Build body
      # nocov start
      body <- list(
        import_ids = unlisted_ids
      )

      path <- glue::glue(self$URL[["bulk_get"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body,
        advance_access = TRUE
      )

      res$items <- asImportList(res, auth = self$auth, bulk = TRUE)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Start bulk import job ---------------------------------------------------
    #' @description This call lets you perform a bulk import of files from
    #'  your volume (either Amazon Web Services or Google Cloud Storage)
    #'  into your project on the Platform.
    #'
    #'  You can use this call to either import files to a specific folder
    #'  or a project but you can also use it to import a folder and its files
    #'  into another destination folder while preserving folder structure.
    #'  One call can contain up to 100 items.
    # nolint start
    #'  Learn more about using the Volumes API for [Amazon S3](https://docs.sevenbridges.com/docs/aws-cloud-storage-tutorial) and
    #'  for [Google Cloud Storage](https://docs.sevenbridges.com/docs/google-cloud-storage-tutorial).
    # nolint end
    #'
    #' @param items Nested list of elements containing information about each
    #'  file/folder to be imported. For each element, users must provide:
    #'
    # nolint start
    #'  \itemize{
    #'      \item `source_volume` - Volume object or its ID to import
    #'        files/folders from,
    #'      \item `source_location` - Volume-specific location pointing to the
    #'        file or folder to import.
    #'        This location should be recognizable to the underlying cloud
    #'        service as a valid key or path to the item. If the item being
    #'        imported is a folder, its path should end with a `/`. \cr
    #'        Please note that if this volume was configured with a prefix
    #'        parameter when it was created, the value of prefix will be
    #'        prepended to the location before attempting to locate the item on
    #'        the volume.
    #'      \item `destination_project` - Project object or ID to import
    #'        files/folders into. Should not be used together with
    #'        destination_parent. If project is used, the items will be
    #'        imported to the root of the project's files.
    #'      \item `destination_parent` - File object of type 'folder' or its ID
    #'        to import files/folders into. Should not be used together with
    #'        destination_project. If parent is used, the import will take
    #'        place into the specified folder, within the project to which the
    #'        folder belongs.
    #'      \item `name` - The name of the alias to create.
    #'        This name should be unique to the project. If the name is already
    #'        in use in the project, you should use the `autorename` parameter
    #'        in this call to automatically rename the item (by prefixing its
    #'        name with an underscore and number). \cr
    #'        If name is omitted, the alias name will default to the last
    #'        segment of the complete location (including the prefix) on the
    #'        volume. Segments are considered to be separated with forward
    #'        slashes ('/').
    #'      \item `autorename` - Whether to automatically rename the item
    #'        (by prefixing its name with an underscore and number) if another
    #'        one with the same name already exists at the destination.
    #'      \item `preserve_folder_structure` - Whether to keep the exact
    #'        source folder structure. The default value is TRUE if the item
    #'        being imported is a folder. Should not be used if you are
    #'        importing a file.
    #'  }
    # nolint end
    #'  Example of the list:
    #'  ```{r}
    #'  items <- list(
    #'            list(
    #'              source_volume = 'rfranklin/my-volume',
    #'              source_location = 'chimeras.html.gz',
    #'              destination_project = 'rfranklin/my-project'
    #'            ),
    #'            list(
    #'              source_volume = 'rfranklin/my-volume',
    #'              source_location = 'my-folder/',
    #'              destination_project = 'rfranklin/my-project',
    #'              autorename = TRUE,
    #'              preserve_folder_structure = TRUE
    #'            ),
    #'            list(
    #'              source_volume = 'rfranklin/my-volume',
    #'              source_location = 'my-volume-folder/',
    #'              destination_parent = '567890abc1e5339df0414123',
    #'              name = 'new-folder-name',
    #'              autorename = TRUE,
    #'              preserve_folder_structure = TRUE
    #'            )
    #'          )
    #' ```
    # nolint start
    #'  Read more on how to [import folders from your volume into a project or a project folder](https://docs.sevenbridges.com/reference/start-a-bulk-import-job#import-a-volume-folder-into-a-specific-folder).
    # nolint end
    #'
    #'  Utility function \code{\link{prepare_items_for_bulk_import}}
    #'  can help you prepare the `items` parameter based on the provided
    #'  list of \code{\link{VolumeFile}} or \code{\link{VolumePrefix}} objects.
    #'
    #' @importFrom checkmate assert_list assert_string test_r6 assert_logical
    #' @importFrom rlang abort inform
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  imports_object <- Imports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # Submit new import into a project
    #'  imports_object$bulk_submit_import(items = list(
    #'    list(
    #'      source_volume = "rfranklin/my-volume",
    #'      source_location = "my-file.txt",
    #'      destination_project = test_project_object,
    #'      autorename = TRUE
    #'    ),
    #'    list(
    #'      source_volume = "rfranklin/my-volume",
    #'      source_location = "my-folder/",
    #'      destination_parent = "parent-folder-id",
    #'      autorename = FALSE,
    #'      preserve_folder_structure = TRUE
    #'    )
    #'   )
    #'  )
    #' }
    #'
    #' @return \code{\link{Collection}} with list of \code{\link{Import}}
    #'  objects.
    bulk_submit_import = function(items) {
      if (is_missing(items)) {
        rlang::abort("Items parameter should be set as a nested list of information on files/folders you want to import.") # nolint
      }
      checkmate::assert_list(items)

      body_elements <- list()

      for (i in seq_len(length(items))) {
        item <- items[[i]]
        checkmate::assert_list(item)
        body_element <- list()

        if (is_missing(item[["source_volume"]])) {
          rlang::abort(
            glue::glue("Volume ID must be provided as string or Volume object in element {i}."), # nolint
          )
        } else {
          volume <- check_and_transform_id(item[["source_volume"]],
            class_name = "Volume"
          )
        }

        if (is_missing(item[["source_location"]])) {
          rlang::abort(
            glue::glue("Source file/folder location/prefix must be provided as a string in element {i}.") # nolint
          )
        }
        checkmate::assert_string(
          item[["source_location"]],
          na.ok = FALSE, null.ok = FALSE
        )

        body_element$source <- list(
          volume = volume,
          location = item[["source_location"]]
        )

        if (is_missing(item[["destination_project"]]) &&
          is_missing(item[["destination_parent"]])) {
          rlang::abort(
            glue::glue("Please provide either destination project or parent parameter in element {i}.") # nolint
          )
        }
        if (!is_missing(item[["destination_project"]]) &&
          !is_missing(item[["destination_parent"]])) {
          rlang::abort(
            glue::glue("Either destination project or parent parameter must be provided in element {i}, not both.") # nolint
          )
        }
        if (!is_missing(item[["destination_project"]])) {
          destination_project <- check_and_transform_id(
            item[["destination_project"]],
            class_name = "Project"
          )
          body_element$destination <- list(
            project = destination_project
          )
        }
        if (!is_missing(item[["destination_parent"]])) {
          if (checkmate::test_r6(
            item[["destination_parent"]],
            classes = "File"
          ) &&
            tolower(item[["destination_parent"]]$type) != "folder") {
            rlang::abort(
              glue::glue("Destination parent directory parameter must contain folder id or File object with type = 'folder' in element {i}.") # nolint
            )
          }
          destination_parent <- check_and_transform_id(
            x = item[["destination_parent"]],
            class_name = "File"
          )
          body_element$destination <- list(
            parent = destination_parent
          )
        }
        if (!is_missing(item[["name"]])) {
          checkmate::assert_string(item[["name"]], null.ok = TRUE)
          body_element$destination$name <- item[["name"]]
        }
        checkmate::assert_logical(item[["autorename"]], len = 1, null.ok = TRUE)
        body_element$autorename <- item[["autorename"]]

        checkmate::assert_logical(item[["preserve_folder_structure"]], len = 1, null.ok = TRUE) # nolint
        body_element$preserve_folder_structure <- item[["preserve_folder_structure"]] # nolint
        body_elements <- append(body_elements, list(body_element))
      }

      # Build body
      # nocov start
      body <- list(
        items = body_elements
      )

      path <- glue::glue(self$URL[["bulk_create"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body,
        advance_access = TRUE
      )

      res$items <- asImportList(res, auth = self$auth, bulk = TRUE)

      rlang::inform(glue::glue("New import jobs have started."))

      return(asCollection(res, auth = self$auth))
      # nocov end
    }
  )
)
