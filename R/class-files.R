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
      "copy" = "action/files/copy"
    ),
    #' @description Create new Files resource object.
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },
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
    #' @param name Name of the file. List file with this name. Note that the
    #'  name must be an exact complete string for the results to match.
    #'  Multiple names can be represented as a vector.
    #' @param metadata List file with this metadata field values. List only
    #'  files that have the specified value in metadata field. Different
    #'  metadata fields are represented as a named list. You can also define
    #'  multiple instances of the same metadata field.
    #' @param origin Task object. List only files produced by task.
    #' @param tag List files containing this tag. Note that the tag must be an
    #'  exact complete string for the results to match. Multiple tags can be
    #'  represented by vector of values.
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
    #' @importFrom checkmate assert_string assert_character
    #' @importFrom rlang abort
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
      checkmate::assert_string(name, null.ok = TRUE)
      if (!is_missing(metadata)) {
        check_metadata(metadata)
        metadata <- transform_metadata(metadata)
      }
      if (!is_missing(origin)) {
        origin_task_id <-
          check_and_transform_id(origin, class_name = "Task")
      } else {
        origin_task_id <- NULL
      }
      checkmate::assert_character(tag, null.ok = TRUE)

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
      params_list <- append(
        list(
          project = project,
          parent = parent,
          name = name,
          origin.task = origin_task_id,
          tag = tag,
          limit = limit,
          offset = offset
        ),
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
    #' @description This call returns a single File object with its details.
    #' The call returns the file's name, its tags, and all of its metadata.
    #' Files are specified by their IDs, which you can obtain by making
    #' the API call to list all files in a project.
    #'
    #' @param id The file ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' as 'fields', etc.
    get = function(id, ...) {
      res <- super$get(
        cls = self,
        id = id,
        ...
      )
      return(asFile(res, auth = self$auth))
    }, # nocov end
    #' @description  Copy file/files to the specified project. This call allows
    #'  you to copy files between projects. Unlike the call to copy a file
    #'  between projects, this call lets you batch the copy operation and copy
    #'  a list of files at a time. \cr
    #'  More information you may find in the
    # nolint start
    #'  \url{https://docs.sevenbridges.com/reference/copy-files-between-projects}.
    # nolint end
    #'
    #' @param files The list of files' IDs or list of File object to copy.
    #' @param destination_project Project object or project ID.
    #'  where you want to copy files into.
    #'
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue_col
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

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["copy"]]),
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url
      )

      result <- list()
      for (i in seq_len(length(res))) {
        element <- list(
          "Copied_file_id" = res[[i]]$new_file_id,
          "Copied_file_name" = res[[i]]$new_file_name
        )
        element <- setNames(list(element), names(res[i]))
        result <- append(result, element)
        cat(
          glue::glue_col("{blue  Original file id: }
                           {names(res[i])}"),
          "\n"
        )
        cat(
          glue::glue_col("{blue  Copied file id: }
                           {res[[i]]$new_file_id}"),
          "\n"
        )
        cat(
          glue::glue_col("{blue  Copied file name: }
                           {res[[i]]$new_file_name}"),
          "\n"
        )
        cat("\n")
      }
      invisible(result)
    }, # nocov end
    #' @description A method for creating a new folder. It allows you to create
    #'  a new folder on the Platform within the root folder of a specified
    #'  project or the provided parent folder. Remember that you should provide
    #'  either the destination project (as the `project` parameter) or the
    #'  destination folder (as the `parent` parameter), not both. \cr
    #'  More information you may find on
    #'  \url{https://docs.sevenbridges.com/reference/create-a-folder}.
    #'
    #' @param name The name of the folder you are about to create.
    #' @param parent The ID of the parent destination folder or a File
    #'  object which must be of type `FOLDER`.
    #' @param project The ID of the destination project, or a Project object.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue_col
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
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["query"]]),
        token = self$auth$get_token(),
        body = body,
        method = "POST",
        base_url = self$auth$url
      )

      rlang::inform(glue::glue_col("New folder {green {name}} has been created.")) # nolint
      # nocov end
    }
  )
)
