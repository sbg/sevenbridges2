# nolint start
#' @title R6 Class representing a file
#'
#' @description
#' R6 Class representing a resource for managing files.
#'
#' @importFrom R6 R6Class
#' @export
File <- R6::R6Class(
  # nolint end
  "File",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "file" = "files/{self$id}",
      "add_tag" = "files/{self$id}/tags",
      "copy" = "files/{self$id}/actions/copy",
      "download_url" = "files/{self$id}/download_info",
      "metadata" = "files/{self$id}/metadata",
      "move" = "files/{self$id}/actions/move",
      "content" = "files/{self$id}/list"
    ),
    #' @field id Character used as a file ID.
    id = NULL,
    #' @field name String used as a file name.
    name = NULL,
    #' @field size File size.
    size = NULL,
    #' @field project Project ID if any, when returned by an API call.
    project = NULL,
    #' @field created_on Date created on.
    created_on = NULL,
    #' @field modified_on Date modified on.
    modified_on = NULL,
    #' @field storage List as storage type.
    storage = NULL,
    #' @field origin List as origin.
    origin = NULL,
    #' @field tags List as tags.
    tags = NULL,
    #' @field metadata List for metadata associated with the file.
    metadata = NULL,
    #' @field url File download url.
    url = NULL,
    #' @field parent Parent folder ID.
    parent = NULL,
    #' @field type This can be of type `File` or `Folder`.
    type = NULL,
    #' @field secondary_files Secondary files
    secondary_files = NULL,

    #' @description Create a new File object.
    #' @param id Character used as file ID.
    #' @param name String used as file name.
    #' @param size File size.
    #' @param project Project ID if any, when returned by an API call.
    #' @param parent Parent folder ID.
    #' @param type `File` or `"Folder"`.
    #' @param created_on Date created on.
    #' @param modified_on Date modified on.
    #' @param storage List as storage type.
    #' @param origin List as origin.
    #' @param tags List as tags.
    #' @param metadata  List for metadata associated with the file.
    #' @param url  File download url.
    #' @param secondary_files Secondary files
    #' @param ... Other arguments.
    initialize = function(id = NA, name = NA, size = NA, project = NA,
                          parent = NA, type = NA, created_on = NA,
                          modified_on = NA, storage = NA, origin = NA,
                          tags = NA, metadata = NA, url = NA,
                          secondary_files = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$name <- name
      self$size <- size
      self$project <- project
      self$parent <- parent
      self$type <- type
      self$created_on <- created_on
      self$modified_on <- modified_on
      self$storage <- storage
      self$origin <- origin
      self$tags <- tags
      self$metadata <- metadata
      self$url <- url
      self$secondary_files <- private$get_secondary_files(secondary_files)
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

      string <- glue::glue("{names(x)}: {ifelse(names(x) == 'size', paste0(x, ' bytes'), x)}") # nolint

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
    #'   \item **`storage`** field denotes the type of storage for the file
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
        string_file_tags <- glue::glue(
          "{names(file_tags)}: {file_tags}"
        )
      }

      if (!is.null(x$metadata) && length(x$metadata) != 0) {
        file_metadata <- x$metadata
        string_file_metadata <- glue::glue(
          "{names(file_metadata)}: {file_metadata}"
        )
      }

      if (!is.null(x$origin) && length(x$origin) != 0) {
        file_origin <- x$origin
        string_file_origin <- glue::glue(
          "{names(file_origin)}: {file_origin}"
        )
      }

      if (!is.null(x$storage) && length(x$storage) != 0) {
        file_storage <- x$storage
        string_file_storage <- glue::glue(
          "{names(file_storage)}: {file_storage}"
        )
      }

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      string <- glue::glue("{names(x)}: {ifelse(names(x) == 'size', paste0(x, ' bytes'), x)}") # nolint
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

    #' @description
    #' Updates the name, the full set metadata, and tags
    #' for a specified file.
    #' .
    #' @param name The new name of the file.
    #' @param metadata The metadata fields and their values that you want to
    #' update. This is a named list of key-value pairs. The keys and values are
    #' strings.
    #' @param tags The tags you want to update, represented as unnamed list of
    #' values to add as tags.
    #' @param ... Additional parameters that can be passed to the method.
    #' @return `File` or `Folder`
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    update = function(name = NULL,
                      metadata = NULL,
                      tags = NULL, ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      check_tags(tags)
      check_metadata(metadata)

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

      res <- status_check(res)

      rlang::inform("File has been updated!")

      # Reload object
      self$initialize(
        href = res$href,
        id = res$id,
        name = res$name,
        size = res$size,
        project = res$project,
        parent = res$parent,
        type = res$type,
        created_on = res$created_on,
        modified_on = res$modified_on,
        storage = res$storage,
        origin = res$origin,
        metadata = res$metadata,
        tags = res$tags,
        auth = auth,
        response = attr(res, "response")
      )
    },

    #' @description
    #' This method allows you to tag files on the Platform. You can tag your
    #' files on the Platform with keywords to make it easier to identify and
    #' organize files you’ve imported from public datasets or copied between
    #' projects.
    #' .
    #' @param tags The tags you want to update, represented as unnamed list of
    #' values to add as tags.
    #' @param overwrite Set to TRUE if you want to ovewrite existing tags.
    #' Default: FALSE.
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

      res <- status_check(res)

      # Add tags to object
      if (overwrite) {
        self$tags <- tags
      } else {
        self$tags <- unique(c(self$tags, tags))
      }
    },

    #' @description
    #' This call copies the specified file to a new project. Files retain their
    #' metadata when copied, but may be assigned new names in their target
    #' project. To make this call, you should have
    #' [copy permission](https://docs.sevenbridges.com/docs/set-permissions)
    #' within the project you are copying from. Note: If you want to copy
    #' multiple files, the recommended way is to do it in bulk considering the
    #' API rate limit
    #' ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).You can
    #' do that using `Auth$copy_files()` operation.
    #'
    #' @param project The ID of the project or a Project object where you want
    #'   to copy the file to. Project name should be specified in the
    #'   `<username>/<project-name>` format, e.g. `rfranklin/my-project`.
    #' @param name The new name the file will have in the target project.
    #' If its name will not change, omit this key.
    #' @param ... Additional parameters that can be passed to the method.
    #'
    #' @importFrom checkmate assert_r6 assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @return `File` or `Folder`
    copy_to = function(project, name = NULL, ...) {
      if (is_missing(project)) {
        rlang::abort("Project parameter is missing. You need to provide one.")
      }
      project_id <- check_and_transform_id(project, "Project")
      checkmate::assert_string(name, null.ok = TRUE)

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

      res <- status_check(res)

      # Return newly created file
      return(asFile(res, auth = self$auth))
    },

    #' @description
    #' This method returns a URL that you can use to download the specified
    #' file.
    #' @importFrom glue glue
    #' @param ... Additional parameters that can be passed to the method.
    get_download_url = function(...) {
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["download_url"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      # Set url field
      self$url <- res$url

      # Return download url
      return(self$url)
    },

    #' @description
    #' This call returns the metadata values for the specified file.
    #' @param ... Additional parameters that can be passed to the method.
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

      res <- status_check(res)

      # Set url field
      self$metadata <- DescTools::StripAttr(res, attr_names = "response")

      return(self$metadata)
    },

    #' @description
    #' This call changes the metadata values for the specified file.
    #'
    #' @param metadata_fields Enter a list of key-value pairs of metadata fields
    #' and metadata values
    #' @param overwrite Set to TRUE if you want to ovewrite existing tags.
    #' Default: FALSE.
    #' @param ... Additional parameters that can be passed to the method.
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

      res <- status_check(res)

      # Set new metadata fields
      self$metadata <- DescTools::StripAttr(res, attr_names = "response")

      return(self$metadata)
    },

    #' @description
    #' This call moves a file from one folder to another. Moving of files is
    #' only allowed within the same project.
    #'
    #' @param parent The ID string of target folder or a File object which must
    #'   be of type `FOLDER`.
    #' @param name Specifies a new name for a file in case you want to rename it
    #' . If you want to use the same name, omit this key.
    #' @param ... Additional parameters that can be passed to the method.
    #'
    #' @importFrom checkmate assert_r6 assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    move_to_folder = function(parent, name = NULL, ...) {
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

      body <- list(
        parent = parent_id,
        name = name
      )

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["move"]]),
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      # Return newly created file
      return(asFile(res, auth = self$auth))
    },

    #' @description
    #' List folder contents.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param ... Additional parameters that can be passed to the method.
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

      res <- status_check(res)

      res$items <- asFileList(res, auth = self$auth)

      # Return folder contents as Collection
      return(asCollection(res, auth = self$auth))
    },

    #' @description Delete method for File class.
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

      if (res$status_code == 204) {
        rlang::inform(message = glue::glue(
          "File {self$id} has been deleted."
        ))
      } else if (res$status_code %in% c("401", "403", "404", "503")) {
        msg <- httr::content(res, as = "parsed")$message
        rlang::abort(glue::glue("HTTP Status {res$status_code} : {msg}"))
      }
    },
    #' @description Download method for File class. It allows download a
    #' platform file to your local computer. To specify the destination for
    #' your download, you should provide the path to the destination directory
    #' as `directory_path` parameter.
    #' @param directory_path Path to the destination directory of a new file.
    #' @param filename Full name for the new file, including its extension. By
    #' default, the name field of File object will be used.
    #' @param method Method to be used for downloading files. By default, this
    #' parameter is set to `curl`.
    #' @param retry_count Number of retries if error occurs during download.
    #' @param retry_timeout Number of seconds between two retries.
    #' @importFrom rlang inform warn abort
    #' @importFrom glue glue_col
    download = function(directory_path = getwd(),
                        filename = self$name,
                        method = "curl",
                        retry_count = getOption("sevenbridges2")$default_retry_count, # nolint
                        retry_timeout = getOption("sevenbridges2")$default_retry_timeout) { # nolint


      # get download url for the file if it was not generated previously
      if (is_missing(self$url)) {
        self$url <- self$get_download_url()
      }

      # check if directory exists
      check_download_path(directory_path, filename)

      # check retry parameters
      check_retry_params(retry_count, parameter_to_validate = "count")
      check_retry_params(retry_timeout, parameter_to_validate = "timeout")

      # create full destination path for download
      destfile <- file.path(directory_path, filename)

      # Retry mechanism
      for (i in 1:retry_count) {
        tryCatch(
          {
            download.file(self$url, destfile, method = method)
            # successful download
            # nolint start
            rlang::inform(glue::glue_col("File {green {filename}} has been downloaded to the {green {directory_path}} directory."))
            # nolint end
            break
          },
          error = function(e) {
            # failed download
            # nolint start
            rlang::warn(glue::glue_col("Download attempt {green {i}} failed. Error message: {red {e$message}}"))
            # nolint end
            # failed download after last attempt
            if (i == retry_count) {
              # nolint start
              rlang::abort(glue::glue_col("Download failed after maximum allowed number of attempts ({red {retry_count}})."))
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
    #' again.
    #'
    #' If you want to export multiple files, the recommended way is to do it
    #' in bulk considering the API rate limit ([learn more]
    #' (https://docs.sevenbridges.com/docs/api-rate-limit)).
    #'
    #' @param destination_volume String volume id or Volume object you want to
    #' export files into. Required.
    #' @param destination_location String volume-specific location to which the
    #' file will be exported.
    #' This location should be recognizable to the underlying cloud service as
    #' a valid key or path to a new file. Please note that if this volume has
    #' been configured with a prefix parameter, the value of prefix will be
    #' prepended to location before attempting to create the file on the volume.
    #'
    #' If you would like to export the file into some folder on the volume,
    #' please add folder name as prefix before file name in form
    #' `<folder-name>/<file-name>`.
    #' @param overwrite Boolean. Whether to overwrite the item if another one
    #' with the same name already exists at the destination.
    #' @param copy_only Boolean. If true, file will be copied to a volume but
    #' source file will remain on the Platform.
    #' @param properties Named list of additional volume properties, like:
    #' \itemize{
    #'    \item `sse_algorithm` - String. S3 server-side encryption to use when
    #'    exporting to this bucket. Supported values:
    #'    AES256 (SSE-S3 encryption), aws:kms, null (no server-side encryption).
    #'    Default: AES256.
    #'    \item `sse_aws_kms_key_Id`: String. Applies to type: s3.
    #'    If AWS KMS encryption is used, this should be set to the required KMS
    #'    key. If not set and aws:kms is set as sse_algorithm, default KMS key
    #'    is used.
    #'    \item `aws_canned_acl`: String. S3 canned ACL to apply on the object
    #'    on during export. Supported values: any one of S3 canned ACLs;
    #'    null (do not apply canned ACLs). Default: null.
    #' }
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #'
    #' @return Export job object.
    submit_export = function(destination_volume, destination_location,
                             overwrite = FALSE, copy_only = FALSE,
                             properties = NULL, ...) {
      self$auth$exports$submit_export(
        source_file = self,
        destination_volume = destination_volume,
        destination_location = destination_location,
        overwrite = overwrite,
        copy_only = copy_only,
        properties = properties,
        ...
      )
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
asFile <- function(x, auth = NULL) {
  File$new(
    href = x$href,
    id = x$id,
    name = x$name,
    size = x$size,
    project = x$project,
    parent = x$parent,
    type = x$type,
    created_on = x$created_on,
    modified_on = x$modified_on,
    storage = x$storage,
    origin = x$origin,
    metadata = x$metadata,
    tags = x$tags,
    secondary_files = x$secondary_files,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of File objects
asFileList <- function(x, auth) {
  obj <- lapply(x$items, asFile, auth = auth)
  obj
}

# nocov end
