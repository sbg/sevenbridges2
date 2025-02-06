# nolint start
#' @title R6 Class representing files endpoints.
#'
#' @description
#' R6 Class representing Files resource.
#'
#' @importFrom R6 R6Class
#' @export
Files <- R6::R6Class(
  # nolint end
  "Files",
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "files",
      "get" = "files/{id}",
      "copy" = "action/files/copy",
      "delete" = "files",
      "bulk_get" = "bulk/files/get",
      "bulk_update" = "bulk/files/update",
      "bulk_edit" = "bulk/files/edit",
      "bulk_delete" = "bulk/files/delete",
      "async_bulk_copy" = "async/files/copy",
      "async_bulk_delete" = "async/files/delete",
      "async_get_copy_job" = "async/files/copy/{job_id}",
      "async_get_delete_job" = "async/files/delete/{job_id}"
    ),

    # Initialize Files object -----------------------------------------------
    #' @description Create new Files resource object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List files --------------------------------------------------------------
    #' @description This call returns a list of files and subdirectories in a
    #'  specified project or directory within a project, with specified
    #'  properties that you can access. The project or directory whose contents
    #'  you want to list is specified as a query parameter in the call. Further
    #'  properties to filter by can also be specified as query parameters.
    #'  \cr \cr
    #'  Note that this call lists both files and subdirectories in the
    #'  specified project or directory within a project, but not the contents
    #'  of the subdirectories. \cr
    #'  To list the contents of a subdirectory, make a new call
    #'  and specify the subdirectory ID as the `parent` parameter. \cr
    #'  More information you can find in our
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/list-files-primary-method).
    # nolint end
    #'
    #' @param project Project identifier (ID) as string or a Project object.
    #'  Project should not be used together with parent.
    #'  If parent is used, the call will list the content of the specified
    #'  folder, within the project to which the folder belongs.
    #'  If project is used, the call will list the content at the root of
    #'  the project's files.
    #' @param parent The parent folder identifier as string or a File object
    #'  which must be of type `FOLDER`.
    #'  Should not be used together with project.
    #'  If parent is used, the call will list the content of the specified
    #'  folder, within the project to which the folder belongs.
    #'  If project is used, the call will list the content at the root of
    #'  the project's files.
    #' @param name Name of the file. List files with this name. Note that the
    #'  name must be an exact complete string for the results to match.
    #'  Multiple names can be represented as a vector.
    #' @param metadata List file with this metadata field values. List only
    #'  files that have the specified value in metadata field. Different
    #'  metadata fields are represented as a named list. You can also define
    #'  multiple instances of the same metadata field.
    #' @param origin Task object. List only files produced by task.
    #' @param tag Filters the files based on the specified tag(s). Each tag
    #'  must be an exact, complete match, for the results to match. Tags may
    #'  include spaces. Multiple tags should be provided as a vector of
    #'  strings. The method will return files that have any of the specified
    #'  tags.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  as 'fields', etc.
    #'
    #' @importFrom checkmate assert_character
    #' @importFrom rlang abort
    #'
    #' @examples
    #' \dontrun{
    #'  files_object <- Files$new(auth = auth)
    #'
    #'  # Query files in a project
    #'  files_object$query(project = project)
    #' }
    #'
    #' @return \code{\link{Collection}} of \code{\link{File}} objects.
    query = function(project = NULL,
                     parent = NULL,
                     name = NULL,
                     metadata = NULL,
                     origin = NULL,
                     tag = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      # Check input parameters
      if (!is_missing(name)) {
        checkmate::assert_character(name, null.ok = TRUE)
        # Transform into a list with name 'name'
        name_list <- list("name" = lapply(name, c))
        name <- transform_multiple_vals(name_list)
      }
      if (!is_missing(metadata)) {
        check_metadata(metadata)
        metadata_names <- paste0("metadata.", names(metadata))
        names(metadata) <- metadata_names
        metadata <- transform_multiple_vals(metadata)
      }
      if (!is_missing(origin)) {
        origin_task_id <-
          check_and_transform_id(origin, class_name = "Task")
      } else {
        origin_task_id <- NULL
      }
      if (!is_missing(tag)) {
        check_tags(tag)
        # Transform into a list with name 'tag'
        tag_list <- list("tag" = lapply(tag, c))
        tag <- transform_multiple_vals(tag_list)
      }

      # Check project and parent parameters
      if (is_missing(parent) && is_missing(project)) {
        rlang::abort("No project or parent directory was defined. You must provide one of the two!") # nolint
      }
      if (!is_missing(parent) && !is_missing(project)) {
        rlang::abort(
          "Project and parent parameters are mutually exclusive. You must provide one of the two, not both." # nolint
        )
      }

      if (!is_missing(project)) {
        project <- check_and_transform_id(project, "Project")
      }
      if (!is_missing(parent)) {
        parent <- check_and_transform_id(parent, "File")
      }
      # nocov start
      params_list <- c(
        list(
          project = project,
          parent = parent,
          origin.task = origin_task_id,
          limit = limit,
          offset = offset
        ),
        name,
        tag,
        metadata
      )

      # Run API call based on project/parent parameters
      res <- do.call(
        super$query,
        append(params_list, list(path = self$URL[["query"]]))
      )

      res$items <- asFileList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    },

    # Get file ---------------------------------------------------------------
    #' @description This call returns a single File object with its details.
    #' The call returns the file's name, its tags, and all of its metadata.
    #' Files are specified by their IDs, which you can obtain by making
    #' the API call to list all files in a project.
    #'
    #' @param id The file ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' as 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  files_object <- Files$new(auth = auth)
    #'
    #'  # Get file using id
    #'  files_object$get(id = id)
    #' }
    #'
    #' @return \code{\link{File}} object.
    get = function(id, ...) {
      res <- super$get(
        cls = self,
        id = id,
        ...
      )
      return(asFile(res, auth = self$auth))
    }, # nocov end

    # Delete file --------------------------------------------------------------
    #' @description This call removes a file from the Seven Bridges Platform.
    #' Files are specified by their IDs, which you can obtain by using
    #' \code{Files$query()} to list files or by getting a single file
    #' using \code{Files$get()}.
    #'
    #' @param file \code{\link{File}} object or file ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' as 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  files_object <- Files$new(auth = auth)
    #'
    #'  # Delete a file
    #'  files_object$delete(file = file)
    #' }
    #'
    delete = function(file, ...) {
      id <- check_and_transform_id(file, "File")
      # nocov start
      res <- super$delete(
        id = id,
        ...
      )

      rlang::inform(
        message = glue::glue("File {id} has been deleted.")
      )
    }, # nocov end

    # Copy files --------------------------------------------------------------
    #' @description  Copy file/files to the specified project. This call allows
    #'  you to copy files between projects. Unlike the call to copy a file
    #'  between projects, this call lets you batch the copy operation and copy
    #'  a list of files at a time. \cr
    #'  More information you may find
    # nolint start
    #'  [here](https://docs.sevenbridges.com/reference/copy-files-between-projects).
    # nolint end
    #'
    #' @param files The list of files' IDs or list of File object to copy.
    #' @param destination_project Project object or project ID.
    #'  where you want to copy files into.
    #'
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue_col
    #' @importFrom rlang inform
    #'
    #' @examples
    #' \dontrun{
    #'  files_object <- Files$new(auth = auth)
    #'
    #'  # Copy files to a project
    #'  files_object$copy(
    #'                file = file,
    #'                destination_project = project
    #'               )
    #' }
    #'
    copy = function(files, destination_project) {
      if (is_missing(files) || is_missing(destination_project)) {
        rlang::abort(
          "Parameter 'files' or 'destination_project' is missing. You need to provide both of them." # nolint
        )
      }
      checkmate::assert_list(files, types = "File")

      project_id <-
        check_and_transform_id(destination_project, "Project")
      file_ids <- lapply(files, check_and_transform_id, "File")
      # nocov start
      body <- list(
        "project" = project_id,
        "file_ids" = file_ids
      )

      res <- self$auth$api(
        path = glue::glue(self$URL[["copy"]]),
        method = "POST",
        body = body
      )

      result <- list()
      for (i in seq_len(length(res))) {
        element <- list(
          "Copied_file_id" = res[[i]]$new_file_id,
          "Copied_file_name" = res[[i]]$new_file_name
        )
        element <- setNames(list(element), names(res[i]))
        result <- append(result, element)
        rlang::inform(
          glue::glue_col(
            "{blue  Original file id:} {names(res[i])}", "\n",
            "{blue  Copied file id:} {res[[i]]$new_file_id}", "\n",
            "{blue  Copied file name:} {res[[i]]$new_file_name}", "\n"
          )
        )
      }
      invisible(result)
    }, # nocov end

    # Create folder -----------------------------------------------------------
    #' @description A method for creating a new folder. It allows you to create
    #'  a new folder on the Platform within the root folder of a specified
    #'  project or the provided parent folder. Remember that you should provide
    #'  either the destination project (as the `project` parameter) or the
    #'  destination folder (as the `parent` parameter), not both. \cr
    #'  More information you may find
    #'  [here](https://docs.sevenbridges.com/reference/create-a-folder).
    #'
    #' @param name The name of the folder you are about to create.
    #' @param parent The ID of the parent destination folder or a File
    #'  object which must be of type `FOLDER`.
    #' @param project The ID of the destination project, or a Project object.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'  files_object <- Files$new(auth = auth)
    #'
    #'  # Create folder in a project
    #'  files_object$create_folder(
    #'                 name = name,
    #'                 project = project
    #'                )
    #' }
    #'
    create_folder = function(name,
                             parent = NULL,
                             project = NULL) {
      check_folder_name(name)

      if (is_missing(parent) && is_missing(project)) {
        # nolint start
        rlang::abort("Both the project name and parent folder ID are missing. You need to provide one of them.")
        # nolint end
      } else if (!is_missing(parent) && !is_missing(project)) {
        # nolint start
        rlang::abort("You should specify a project or a parent folder, not both.")
        # nolint end
      }

      if (!is_missing(parent)) {
        if (inherits(parent, "File") && parent$type != "folder") {
          rlang::abort("The provided parent object is not a folder.")
        }
        parent_id <- check_and_transform_id(parent, "File")
        body <- list(
          "name" = name,
          "parent" = parent_id,
          "type" = "FOLDER"
        )
      } else if (!is_missing(project)) {
        project_id <- check_and_transform_id(project, "Project")
        body <- list(
          "name" = name,
          "project" = project_id,
          "type" = "FOLDER"
        )
      }
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["query"]]),
        body = body,
        method = "POST"
      )

      rlang::inform(glue::glue_col("New folder {green {name}} has been created.")) # nolint
      # nocov end
    },

    # Bulk deletion of files --------------------------------------------------
    #' @description This method facilitates bulk file deletion. It accepts
    #'  either a list of \code{\link{File}} objects or a list containing
    #'  files' IDs.
    #'
    #' @param files Either a list of \code{\link{File}} objects or a list
    #'  of strings (IDs) representing the files you intend to delete.
    #'
    #' @importFrom rlang abort inform format_error_bullets
    #' @importFrom checkmate assert_list
    #' @importFrom cli cli_text qty
    #' @importFrom glue glue
    #'
    #' @return None. The function only displays the IDs of the deleted files in
    #'  the console.
    #'
    #' @examples
    #' \dontrun{
    #'  # Delete two files by providing their IDs
    #'  a$files$bulk_delete(files = list("file_1_ID", "file_2_ID"))
    #' }
    #'
    #' \dontrun{
    #'  # Delete two files by providing a list of File objects
    #'  a$files$bulk_delete(files = list(File_Object_1, File_Object_2))
    #' }
    #'
    bulk_delete = function(files) {
      if (is_missing(files)) {
        rlang::abort(
          "Please provide the 'files' parameter."
        )
      }

      checkmate::assert_list(files)

      # nocov start
      files <- lapply(files, check_and_transform_id, "File")
      body <- list(
        "file_ids" = files
      )

      path <- glue::glue(self$URL[["bulk_delete"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      check_response_and_notify_user(files, res)
      # nocov end
    },

    # Get details of multiple files -------------------------------------------
    #' @description This call returns the details of multiple specified files,
    #'  including file names and file metadata. The maximum number of files you
    #'  can retrieve the details for per call is 100.
    #'
    #' @param files A list of \code{\link{File}} objects or list of strings
    #'  (IDs) of the files you are querying for details.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue
    #'
    #' @return \code{\link{Collection}} (list of \code{\link{File}} objects).
    #'
    #' @examples
    #' \dontrun{
    #'  # Get details of multiple files
    #'  a$files$bulk_get(
    #'                files = list("file_1_ID", "file_2_ID")
    #'               )
    #' }
    #'
    bulk_get = function(files) {
      if (is_missing(files)) {
        rlang::abort(
          "Please provide the 'files' parameter."
        )
      }

      checkmate::assert_list(files)

      # nocov start
      files <- lapply(files, check_and_transform_id, "File")
      body <- list(
        "file_ids" = files
      )

      path <- glue::glue(self$URL[["bulk_get"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      res$items <- asFileList(res, auth = self$auth, bulk = TRUE)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Update details of multiple files ----------------------------------------
    #' @description A method that sets new information for specified files,
    #'  replacing all existing information and erasing omitted parameters.
    #'
    #' @details For each of the specified files, the call sets a new `name`,
    #'  new `tags`, and `metadata`.
    #'
    #'  When editing fields in the \code{\link{File}} objects you wish to
    #'   update, keep the following in mind:
    #'
    # nolint start
    #'  \itemize{
    #'      \item The `name` field should be a string representing the new name of
    #'       the file.
    #'      \item The `metadata` field should be a named list of key-value pairs.
    #'       The keys and values should be strings.
    #'      \item The `tags` field should be an unnamed list of values.
    #'  }
    # nolint end
    #'
    #'  The maximum number of files you can update the details for per call is
    #'  100.
    #'
    #' @param files List of \code{\link{File}} objects.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_list
    #' @importFrom cli cli_text qty
    #' @importFrom glue glue
    #'
    #' @return \code{\link{Collection}} (list of \code{\link{File}} objects).
    #'
    #' @examples
    #' \dontrun{
    #'  # Update details of multiple files
    #'  a$files$bulk_update(
    #'                files = list(File_Object_1, File_Object_2)
    #'               )
    #' }
    #'
    bulk_update = function(files) {
      if (is_missing(files)) {
        rlang::abort(
          "Please provide the 'files' parameter."
        )
      }

      checkmate::assert_list(files, types = "File")

      # nocov start
      body <- list(
        items = lapply(files, function(file) {
          check_and_process_file_details(file)
        })
      )

      path <- glue::glue(self$URL[["bulk_update"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      rlang::inform(cli::cli_text("{cli::qty(length(files))} File{?s} {?has/have} been updated.")) # nolint

      res$items <- asFileList(res, auth = self$auth, bulk = TRUE)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Edit details of multiple files ------------------------------------------
    #' @description This method modifies the existing information for specified
    #'  files or adds new information while preserving omitted parameters.
    #'
    #' @details For each of the specified files, the call edits its `name`,
    #'  `tags`, and `metadata`.
    #'
    #'  When editing fields in the \code{\link{File}} objects you wish to
    #'  update, keep the following in mind:
    #'
    # nolint start
    #'  \itemize{
    #'      \item The `name` field should be a string representing the new name of
    #'       the file.
    #'      \item The `metadata` field should be a named list of key-value pairs.
    #'       The keys and values should be strings.
    #'      \item The `tags` field should be an unnamed list of values.
    #'  }
    # nolint end
    #'
    #'  The maximum number of files you can update the details for per call is
    #'  100.
    #'
    #' @param files List of \code{\link{File}} objects.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_list
    #' @importFrom cli cli_text qty
    #' @importFrom glue glue
    #'
    #' @return \code{\link{Collection}} (list of \code{\link{File}} objects).
    #'
    #' @examples
    #' \dontrun{
    #'  # Edit details of multiple files
    #'  a$files$bulk_edit(
    #'                files = list(File_Object_1, File_Object_2)
    #'               )
    #' }
    #'
    bulk_edit = function(files) {
      if (is_missing(files)) {
        rlang::abort(
          "Please provide the 'files' parameter."
        )
      }

      checkmate::assert_list(files, types = "File")

      # nocov start
      body <- list(
        items = lapply(files, function(file) {
          check_and_process_file_details(file)
        })
      )

      path <- glue::glue(self$URL[["bulk_edit"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      rlang::inform(cli::cli_text("{cli::qty(length(files))} File{?s} {?has/have} been updated.")) # nolint

      res$items <- asFileList(res, auth = self$auth, bulk = TRUE)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Asynchronous (bulk) action for coping multiple files --------------------
    #' @description This call lets you perform a bulk copy operation of files
    #'  and folders. Any underlying folder structure will be preserved.
    #'  You can copy:
    #'  \itemize{
    #'       \item to a folder within the same project,
    #'       \item to another project,
    #'       \item to a folder in another project.
    #'       }
    #'
    #' @param items Nested list of elements containing information about each
    #'  file/folder to be copied. For each element, users must provide:
    #'
    # nolint start
    #'  \itemize{
    #'      \item `file` - The ID of the file or folder you are copying.
    #'        Copying the project root folder is not allowed.
    #'        Use the API call for listing all files to obtain the ID.
    #'      \item `parent` - The ID of the folder you are copying files to.
    #'        Should not be used together with project. If project is used,
    #'        the items will be imported to the root of the project's files.
    #'        If parent is used, the import will take place into the specified
    #'        folder, within the project to which the folder belongs.
    #'      \item `project` - The project you are copying the file to.
    #'        Should not be used together with parent. If parent is used,
    #'        the import will take place into the specified folder, within the
    #'        project to which the folder belongs. If project is used, the
    #'        items will be imported to the root of the project's files.
    #'      \item `name` - Enter the new name for the file if you want to
    #'        rename it in the destination folder.
    #'  }
    # nolint end
    #'  Example of the list:
    #'  ```{r}
    #'  items <- list(
    #'            list(
    #'              file = '<file-id-1>',
    #'              parent = '<foolder-id>'
    #'            ),
    #'            list(
    #'              file = '<file-id-2>',
    #'              project = '<project-id-1>',
    #'              name = 'copied_file.txt'
    #'            ),
    #'            list(
    #'              file = '<file-id-3>',
    #'              parent = '<parent-id-2>',
    #'              name = 'copied_file2.txt'
    #'            )
    #'          )
    #' ```
    # nolint start
    #'  Read more on how to [perform async copy action on multiple files](https://docs.sevenbridges.com/reference/copy-multiple-files).
    # nolint end
    #'
    #'
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_list test_r6
    #' @importFrom glue glue
    #'
    #' @return \code{\link{AsyncJob}} object.
    #'
    #' @examples
    #' \dontrun{
    #'  # Copy multiple files
    #'  a$files$async_bulk_copy(
    #'    items = list(
    #'            list(
    #'              file = '<file-id-1>',
    #'              parent = '<foolder-id>'
    #'            ),
    #'            list(
    #'              file = '<file-id-2>',
    #'              project = '<project-id-1>',
    #'              name = 'copied_file.txt'
    #'            ),
    #'            list(
    #'              file = '<file-id-3>',
    #'              parent = '<parent-id-2>',
    #'              name = 'copied_file2.txt'
    #'            )
    #'          )
    #'   )
    #' }
    #'
    async_bulk_copy = function(items) {
      if (is_missing(items)) {
        rlang::abort("Items parameter should be set as a nested list of information on files/folders you want to copy.") # nolint
      }
      checkmate::assert_list(items)

      body_elements <- list()

      for (i in seq_len(length(items))) {
        item <- items[[i]]
        checkmate::assert_list(item)
        body_element <- list()

        if (is_missing(item[["file"]])) {
          rlang::abort(
            glue::glue("File ID must be provided as string or File object in element {i}."), # nolint
          )
        } else {
          body_element$file <- check_and_transform_id(item[["file"]],
            class_name = "File"
          )
        }

        if (is_missing(item[["project"]]) &&
          is_missing(item[["parent"]])) {
          rlang::abort(
            glue::glue("Please provide either destination project or parent parameter in element {i}.") # nolint
          )
        }
        if (!is_missing(item[["project"]]) &&
          !is_missing(item[["parent"]])) {
          rlang::abort(
            glue::glue("Either destination project or parent parameter must be proveded in element {i}, not both.") # nolint
          )
        }
        if (!is_missing(item[["project"]])) {
          body_element$project <- check_and_transform_id(
            item[["project"]],
            class_name = "Project"
          )
        }
        if (!is_missing(item[["parent"]])) {
          if (checkmate::test_r6(
            item[["parent"]],
            classes = "File"
          ) &&
            tolower(item[["parent"]]$type) != "folder") {
            rlang::abort(
              glue::glue("Destination parent directory parameter must contain folder id or File object with type = 'folder' in element {i}.") # nolint
            )
          }
          body_element$parent <- check_and_transform_id(
            x = item[["parent"]],
            class_name = "File"
          )
        }
        if (!is_missing(item[["name"]])) {
          checkmate::assert_string(item[["name"]], null.ok = TRUE)
          body_element$name <- item[["name"]]
        }
        body_elements <- append(body_elements, list(body_element))
      }

      # Build body
      # nocov start
      body <- list(
        items = body_elements
      )

      path <- glue::glue(self$URL[["async_bulk_copy"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      rlang::inform(glue::glue("New asynchronous job for coping files has started.")) # nolint

      return(asAsyncJob(res, auth = self$auth))
      # nocov end
    },

    # Asynchronous (bulk) action for deleting multiple files ------------------
    #' @description This call lets you perform perform an asynchronous bulk
    #' deletion of files or folders. Deleting folders which aren't empty is
    #' allowed.
    #'
    #' @param items List of File objects (both `file` or `folder` type) or list
    #'  of IDs of files/folders you want to delete.
    # nolint start
    #'  Read more on how to [perform async delete action on multiple files](https://docs.sevenbridges.com/reference/delete-multiple-files-and-folders).
    # nolint end
    #'
    #'
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue
    #'
    #' @return \code{\link{AsyncJob}} object.
    #'
    #' @examples
    #' \dontrun{
    #'  # Delete multiple files
    #'  a$files$async_bulk_delete(
    #'    items = list(file_obj1, file_obj2, "<folder-id-string>", "<file-id>")
    #'  )
    #' }
    #'
    async_bulk_delete = function(items) {
      if (is_missing(items)) {
        rlang::abort("Items parameter should be a list of files' or folders' IDs or `File` objects you want to delete.") # nolint
      }
      checkmate::assert_list(items)

      body_elements <- list()

      for (i in seq_len(length(items))) {
        item <- items[[i]]
        file_id <- check_and_transform_id(item, class_name = "File")
        element <- list("file" = file_id)
        body_elements <- append(body_elements, list(element))
      }

      # Build body
      # nocov start
      body <- list(
        items = body_elements
      )

      path <- glue::glue(self$URL[["async_bulk_delete"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body
      )

      rlang::inform(glue::glue("New asynchronous job for deleting files has started.")) # nolint

      return(asAsyncJob(res, auth = self$auth))
      # nocov end
    },

    # Get details of asynchronous job for coping multiple files ---------------
    #' @description This call gets the details of an asynchronous bulk copy job.
    #'  This information will be available for up to a month after the job has
    #'  been completed.
    #'
    #' @param job_id The ID of the copy job you are querying.
    #'  This ID can be found within the API response for the call for copying
    #'  files.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue
    #'
    #' @return \code{\link{AsyncJob}} object.
    #'
    #' @examples
    #' \dontrun{
    #'  # Get details of async copy job
    #'  a$files$async_get_copy_job(job_id = "job-id")
    #' }
    #'
    async_get_copy_job = function(job_id) {
      if (is_missing(job_id)) {
        rlang::abort(
          "Please provide the 'job_id' parameter."
        )
      }

      job_id <- check_and_transform_id(job_id, class_name = "AsyncJob")

      # nocov start
      path <- glue::glue(self$URL[["async_get_copy_job"]])

      res <- self$auth$api(
        path = path,
        method = "GET"
      )

      return(asAsyncJob(res, auth = self$auth))
      # nocov end
    },

    # Get details of asynchronous job for deleting multiple files -------------
    #' @description This call gets the details of an asynchronous bulk deletion
    #'  job. This information will be available for up to a month after the job
    #'  has been completed.
    #'
    #' @param job_id The ID of the copy job you are querying.
    #'  This ID can be found within the API response for the call for deleting
    #'  files.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue
    #'
    #' @return \code{\link{AsyncJob}} object.
    #'
    #' @examples
    #' \dontrun{
    #'  # Get details of async delete job
    #'  a$files$async_get_delete_job(job_id = "job-id")
    #' }
    #'
    async_get_delete_job = function(job_id) {
      if (is_missing(job_id)) {
        rlang::abort(
          "Please provide the 'job_id' parameter."
        )
      }

      job_id <- check_and_transform_id(job_id, class_name = "AsyncJob")

      # nocov start
      path <- glue::glue(self$URL[["async_get_delete_job"]])

      res <- self$auth$api(
        path = path,
        method = "GET"
      )

      return(asAsyncJob(res, auth = self$auth))
      # nocov end
    }
  )
)
