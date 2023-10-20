# nolint start
#' @title R6 Class representing a File
#'
#' @description
#' R6 Class representing a resource for managing files and folders.
#'
#' @importFrom R6 R6Class
#' @export
File <- R6::R6Class(
  # nolint end
  "File",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "files/{id}",
      "file" = "files/{self$id}",
      "add_tag" = "files/{self$id}/tags",
      "copy" = "files/{self$id}/actions/copy",
      "download_url" = "files/{self$id}/download_info",
      "metadata" = "files/{self$id}/metadata",
      "move" = "files/{self$id}/actions/move",
      "content" = "files/{self$id}/list"
    ),
    #' @field id String used as a file ID.
    id = NULL,
    #' @field name String used as a file name.
    name = NULL,
    #' @field size File size.
    size = NULL,
    #' @field project Project ID if any, where file/folder is located.
    project = NULL,
    #' @field created_on Date file/folder was created on.
    created_on = NULL,
    #' @field modified_on Date file/folder was modified on.
    modified_on = NULL,
    #' @field storage File/folder's storage type.
    storage = NULL,
    #' @field origin Task ID if file/folder is produced by some task execution.
    origin = NULL,
    #' @field tags List of tags associated with the file.
    tags = NULL,
    #' @field metadata List for metadata associated with the file.
    metadata = NULL,
    #' @field url File download URL.
    url = NULL,
    #' @field parent Parent folder ID.
    parent = NULL,
    #' @field type This can be of type `file` or `folder`.
    type = NULL,
    #' @field secondary_files Secondary files linked to the file if exist.
    secondary_files = NULL,

    #' @description Create a new File object.
    #'
    #' @param res Response containing File object information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
      self$size <- res$size
      self$project <- res$project
      self$parent <- res$parent
      self$type <- res$type
      self$created_on <- res$created_on
      self$modified_on <- res$modified_on
      self$storage <- res$storage
      self$origin <- res$origin
      self$tags <- res$tags
      self$metadata <- res$metadata
      self$url <- res$url
      self$secondary_files <-
        private$get_secondary_files(res$secondary_files)
    },

    # nocov start
    #' @description Print method for File class.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end cli_bullets
    print = function() {
      x <- as.list(self)

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      string <-
        glue::glue("{names(x)}: {ifelse(names(x) == 'size', paste0(x, ' bytes'), x)}") # nolint

      cli::cli_h1("File")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    #' @description Detailed print method for File class.
    #'
    #' @details  The call returns the file's name, its tags, and all of its
    #' metadata. Apart from regular file fields there are some additional
    #' fields:
    #' \itemize{
    #'   \item `storage` field denotes the type of storage for the file
    #'   which can be either PLATFORM or VOLUME depending on where the file is
    #'   stored.
    #'   \item `origin` field denotes the task that produced the file, if it
    #'   was created by a task on the Seven Bridges Platform.
    #'   \item `metadata` field lists the metadata fields and values for the
    #'   file.
    #'   \item `tags` field lists the tags for the file. Learn more about
    #'   [tagging your files](https://docs.sevenbridges.com/docs/tag-your-files)
    #'    on the Platform.
    #' }
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    detailed_print = function() {
      x <- as.list(self)

      if (!is.null(x$tags) && length(x$tags) != 0) {
        file_tags <- x$tags
        names(file_tags) <- paste0("tag_", seq_along(file_tags))
        string_file_tags <-
          glue::glue("{names(file_tags)}: {file_tags}")
      }

      if (!is.null(x$metadata) && length(x$metadata) != 0) {
        file_metadata <- x$metadata
        string_file_metadata <-
          glue::glue("{names(file_metadata)}: {file_metadata}")
      }

      if (!is.null(x$origin) && length(x$origin) != 0) {
        file_origin <- x$origin
        string_file_origin <-
          glue::glue("{names(file_origin)}: {file_origin}")
      }

      if (!is.null(x$storage) && length(x$storage) != 0) {
        file_storage <- x$storage
        string_file_storage <-
          glue::glue("{names(file_storage)}: {file_storage}")
      }

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      string <-
        glue::glue("{names(x)}: {ifelse(names(x) == 'size', paste0(x, ' bytes'), x)}") # nolint
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("File")
      cli::cli_li(string)

      ifelse(exists("file_tags") && !is.null(file_tags),
        {
          cli::cli_li("tags")
          cli::cli_ul(string_file_tags)
        },
        ""
      )
      ifelse(exists("file_metadata") && !is.null(file_metadata),
        {
          cli::cli_li("metadata")
          cli::cli_ul(string_file_metadata)
        },
        ""
      )
      ifelse(exists("file_origin") && !is.null(file_origin),
        {
          cli::cli_li("origin")
          cli::cli_ul(string_file_origin)
        },
        ""
      )
      ifelse(exists("file_storage") && !is.null(file_storage),
        {
          cli::cli_li("storage")
          cli::cli_ul(string_file_storage)
        },
        ""
      )
      # Close container elements
      cli::cli_end()
    },
    #' @description Reload File object information.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #' @return File
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("File object is refreshed!")
    }, # nocov end
    #' @description Updates the name, the full set metadata, and tags
    #' for a specified file.
    #' .
    #' @param name The new name of the file.
    #' @param metadata The metadata fields and their values that you want to
    #'  update. This is a named list of key-value pairs. The keys and values are
    #'  strings.
    #' @param tags The tags you want to update, represented as unnamed list of
    #'  values to add as tags.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'limit', 'offset', 'fields', etc.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @return Updated File object.
    update = function(name = NULL,
                      metadata = NULL,
                      tags = NULL,
                      ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      check_tags(tags)
      check_metadata(metadata)
      # nocov start
      body <- list(
        "name" = name,
        "tags" = tags,
        "metadata" = metadata
      )

      body <- body[!sapply(body, is.null)]
      if (length(body) == 0) {
        rlang::abort("Please provide updated information.")
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["file"]]),
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      # Reload object
      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
      rlang::inform("File has been updated!")
    }, # nocov end
    #' @description This method allows you to tag files on the Platform.
    #'  You can tag your files on the Platform with keywords to make it easier
    #'  to identify and organize files you’ve imported from public datasets
    #'  or copied between projects. \cr
    #'  More details on how to use this call can be found
    #'  [here](https://docs.sevenbridges.com/reference/add-tags-to-a-file).
    #'
    #' @param tags The tags you want to update, represented as unnamed list of
    #'  values to add as tags.
    #' @param overwrite Set to `TRUE` if you want to overwrite existing tags.
    #'  Default: `FALSE`.
    #' @param ... Additional parameters that can be passed to the method.
    #'
    #' @importFrom checkmate assert_logical
    #' @importFrom glue glue
    #' @importFrom rlang abort
    add_tag = function(tags, overwrite = FALSE, ...) {
      if (is_missing(tags)) {
        # nolint start
        rlang::abort("Tags parameter is missing. You need to provide at least one.")
        # nolint end
      }

      check_tags(tags)
      checkmate::assert_logical(overwrite)
      # nocov start
      if (overwrite) {
        body <- tags
      } else {
        body <- unique(c(self$tags, tags))
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["add_tag"]]),
        method = "PUT",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      # Add tags to object
      if (overwrite) {
        self$tags <- tags
      } else {
        self$tags <- unique(c(self$tags, tags))
      }
    }, # nocov end
    #' @description This call copies the specified file to a new project.
    #'  Files retain their metadata when copied, but may be assigned new names
    #'  in their target project. To make this call, you should have
    #'  [copy permission](https://docs.sevenbridges.com/docs/set-permissions)
    #'  within the project you are copying from. \cr Note: If you want to copy
    #'  multiple files, the recommended way is to do it in bulk considering the
    #'  API rate limit
    #'  ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    #'  You can do that using `Auth$copy_files()` operation.
    #'
    #' @param project The ID of the project or a Project object where you want
    #'   to copy the file to.
    #' @param name The new name the file will have in the target project.
    #'  If its name will not change, omit this key.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @return Copied File object.
    copy_to = function(project, name = NULL, ...) {
      if (is_missing(project)) {
        rlang::abort("Project parameter is missing. You need to provide one.")
      }
      project_id <- check_and_transform_id(project, "Project")
      checkmate::assert_string(name, null.ok = TRUE)
      # nocov start
      body <- list(
        project = project_id,
        name = name
      )

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["copy"]]),
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      # Return newly created file
      return(asFile(res, auth = self$auth))
    },
    #' @description This method returns a URL that you can use to download
    #'  the specified file.
    #' @importFrom glue glue
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    get_download_url = function(...) {
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["download_url"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      # Set url field
      self$url <- res$url

      # Return download url
      return(self$url)
    },
    #' @description This call returns the metadata values for the specified
    #'  file.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom DescTools StripAttr
    #' @importFrom glue glue
    get_metadata = function(...) {
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["metadata"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      # Set url field
      self$metadata <-
        DescTools::StripAttr(res, attr_names = "response")

      return(self$metadata)
    }, # nocov end
    #' @description This call changes the metadata values for the specified
    #'  file. \cr
    #'  More details about how to modify metadata, you can find in the
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/modify-a-files-metadata).
    # nolint end
    #' @param metadata_fields Enter a list of key-value pairs of metadata fields
    #'  and metadata values.
    #' @param overwrite Set to `TRUE` if you want to overwrite existing tags.
    #' Default: `FALSE`.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom DescTools StripAttr
    #' @importFrom checkmate assert_logical
    #' @importFrom rlang abort
    #' @importFrom glue glue
    set_metadata = function(metadata_fields, overwrite = FALSE, ...) {
      if (is_missing(metadata_fields)) {
        # nolint start
        rlang::abort("Metadata fields are missing. You need to provide at least one.")
        # nolint end
      }

      check_metadata(metadata_fields)
      checkmate::assert_logical(overwrite)
      # nocov start
      body <- metadata_fields

      if (overwrite) {
        method <- "PUT"
      } else {
        method <- "PATCH"
      }
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["metadata"]]),
        method = method,
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      # Set new metadata fields
      self$metadata <-
        DescTools::StripAttr(res, attr_names = "response")

      return(self$metadata)
    }, # nocov end
    #' @description This call moves a file from one folder to another.
    #'  Moving of files is only allowed within the same project.
    #'
    #' @param parent The ID string of target folder or a File object which must
    #'  be of type `FOLDER`.
    #' @param name Specify a new name for a file in case you want to rename it.
    #'  If you want to use the same name, omit this key.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #' @return Moved File object.
    move_to_folder = function(parent, name = NULL) {
      if (is_missing(parent)) {
        # nolint start
        rlang::abort("Parent folder is missing. You need to provide one.")
        # nolint end
      }
      if (inherits(parent, "File") && parent$type != "folder") {
        rlang::abort("Parent must be a folder, not a file!")
      }
      parent_id <- check_and_transform_id(parent, "File")

      checkmate::assert_string(name, null.ok = TRUE)
      # nocov start
      body <- list(
        parent = parent_id,
        name = name
      )

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["move"]]),
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url
      )

      # Return newly created file
      return(asFile(res, auth = self$auth))
    },
    #' @description List folder contents.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    list_contents = function(limit = getOption("sevenbridges2")$"limit",
                             offset = getOption("sevenbridges2")$"offset",
                             ...) {
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["content"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        offset = offset,
        ...
      )

      res$items <- asFileList(res, auth = self$auth)

      # Return folder contents as Collection
      return(asCollection(res, auth = self$auth))
    },
    #' @description Delete method for File objects.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end cli_bullets
    #' @importFrom rlang inform abort
    #' @importFrom glue glue
    delete = function() {
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["file"]]),
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url
      )

      rlang::inform(message = glue::glue("File {self$id} has been deleted."))
    }, # nocov end
    #' @description Download method for File objects. It allows download a
    #'  platform file to your local computer. To specify the destination for
    #'  your download, you should provide the path to the destination directory
    #'  as `directory_path` parameter.
    #' @param directory_path Path to the destination directory of a new file.
    #' @param filename Full name for the new file, including its extension. By
    #'  default, the name field of File object will be used.
    #' @param method Method to be used for downloading files. By default, this
    #'  parameter is set to `curl`.
    #' @param retry_count Number of retries if error occurs during download.
    #' @param retry_timeout Number of seconds between two retries.
    #' @importFrom rlang inform warn abort
    #' @importFrom glue glue_col
    download = function(directory_path = getwd(),
                        filename = self$name,
                        method = "curl",
                        # nolint start
                        retry_count = getOption("sevenbridges2")$default_retry_count,
                        retry_timeout = getOption("sevenbridges2")$default_retry_timeout) {
      # nolint end
      # get download url for the file if it was not generated previously
      if (is_missing(self$url)) {
        self$url <- self$get_download_url()
      }

      # check if directory exists
      check_download_path(directory_path, filename)

      # check retry parameters
      check_retry_params(retry_count, parameter_to_validate = "count")
      check_retry_params(retry_timeout, parameter_to_validate = "timeout")

      # nocov start
      # create full destination path for download
      destfile <- file.path(directory_path, filename)

      # Retry mechanism
      for (i in 1:retry_count) {
        tryCatch(
          {
            download.file(self$url, destfile, method = method)
            # successful download
            # nolint start
            rlang::inform(
              glue::glue_col(
                "File {green {filename}} has been downloaded to the {green {directory_path}} directory."
              )
            )
            # nolint end
            break
          },
          error = function(e) {
            # failed download
            # nolint start
            rlang::warn(
              glue::glue_col(
                "Download attempt {green {i}} failed. Error message: {red {e$message}}"
              )
            )
            # nolint end
            # failed download after last attempt
            if (i == retry_count) {
              # nolint start
              rlang::abort(
                glue::glue_col(
                  "Download failed after maximum allowed number of attempts ({red {retry_count}})."
                )
              )
              # nolint end
            }
            # wait for 5 seconds before new attemt - print the countdown message
            for (seconds_left in retry_timeout:1) {
              cat(glue::glue_col("Retrying in {green {seconds_left}} seconds...", "\r")) # nolint
              Sys.sleep(1)
            }

            # print a blank line to clear the countdown message
            cat("\r", "                              ", "\r")
          }
        )
      }
    },
    #' @description This call lets you queue a job to export this file from a
    #' project on the Platform into a volume. The file selected for export must
    #' not be a public file or an alias. Aliases are objects stored in your
    #' cloud storage bucket which have been made available on the Platform.
    #' The volume you are exporting to must be configured for read-write access.
    #' To do this, set the `access_mode` parameter to `RW` when creating or
    #' modifying a volume.
    #'
    #' Essentially, the call writes to your cloud storage bucket via the volume.
    #' If this call is successful, the original project file will become an
    #' alias to the newly exported object on the volume. The source file will
    #' be deleted from the Platform and, if no more copies of this file exist,
    #' it will no longer count towards your total storage price on the Platform.
    #' In summary, once you export a file from the Platform to a volume, it is
    #' no longer part of the storage on the Platform and cannot be exported
    #' again. \cr
    #'
    #' Read more about this operation in our documentation
    #' [here](https://docs.sevenbridges.com/reference/start-an-export-job-v2).
    #'
    #' If you want to export multiple files, the recommended way is to do it
    #' in bulk considering the API rate limit
    #' ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    #'
    #' @param destination_volume String volume id or Volume object you want to
    #'  export files into. Required.
    #' @param destination_location String volume-specific location to which the
    #'  file will be exported.
    #'  This location should be recognizable to the underlying cloud service as
    #'  a valid key or path to a new file. Please note that if this volume has
    #'  been configured with a prefix parameter, the value of prefix will be
    #'  prepended to location before attempting to create the file on the
    #'  volume.
    #'
    #'  If you would like to export the file into some folder on the volume,
    #'  please add folder name as prefix before file name in form
    #'  `<folder-name>/<file-name>`.
    #' @param overwrite Set to `TRUE` of you want to overwrite the item that
    #'  already exists at the destination. Default: `FALSE`.
    #' @param copy_only If `TRUE`, file will be copied to a volume but
    #'  source file will remain on the Platform.
    #' @param properties Named list of additional volume properties, like:
    #' \itemize{
    #'    \item `sse_algorithm` - S3 server-side encryption to use when
    #'    exporting to this bucket. Supported values:
    #'    `AES256` (SSE-S3 encryption), `aws:kms`, `null`
    #'    (no server-side encryption). Default: `AES256`.
    #'    \item `sse_aws_kms_key_Id`: Applies to type: `s3`.
    #'    If AWS KMS encryption is used, this should be set to the required KMS
    #'    key. If not set and `aws:kms` is set as `sse_algorithm`,
    #'    default KMS key is used.
    #'    \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'    on during export. Supported values: any one of S3 canned ACLs;
    #'    null (do not apply canned ACLs). Default: null.
    #' }
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return Export job object.
    submit_export = function(destination_volume,
                             destination_location,
                             overwrite = FALSE,
                             copy_only = FALSE,
                             properties = NULL,
                             ...) {
      self$auth$exports$submit_export(
        source_file = self,
        destination_volume = destination_volume,
        destination_location = destination_location,
        overwrite = overwrite,
        copy_only = copy_only,
        properties = properties,
        ...
      )
      # nolint start
      rlang::inform(
        glue::glue_col(
          "File {green {self$name}} has been exported to the {green {destination_volume}} volume.
                                   Please, reload file object to fetch updated information."
        )
      )
      # nolint end
    }
  ),
  private = list(
    # Handle secondary files parameter to return list of File objects
    get_secondary_files = function(secondary_files) {
      if (!is_missing(secondary_files)) {
        sf_list <- list()
        for (ind in seq_len(length(secondary_files))) {
          fl <- asFile(secondary_files[[ind]], auth = self$auth)
          sf_list <- append(sf_list, fl)
        }
        return(sf_list)
      }
    }
  )
)

# Helper function for creating File objects
asFile <- function(x = NULL, auth = NULL) {
  File$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

# Helper function for creating a list of File objects
asFileList <- function(x, auth) {
  obj <- lapply(x$items, asFile, auth = auth)
  obj
}

# nocov end
