# nolint start
#' @title R6 Class representing an upload job
#'
#' @description
#' R6 Class representing a resource for managing files' uploads.
#'
#' @importFrom R6 R6Class
#' @export
Upload <- R6::R6Class(
  # nolint end
  "Upload",
  # inherit = Item,
  portable = FALSE,
  public = list(
    #' @field upload_id Upload ID received after upload initialisation.
    upload_id = NULL,
    #' @field path Relative or absolute path to the file on the local disc.
    path = NULL,
    #' @field project Project's identifier (character).
    project = NULL,
    #' @field parent The ID of the folder to which the item is being uploaded.
    #' Should not be used together with 'project'.
    parent = NULL,
    #' @field file_name New file name. Optional.
    file_name = NULL,
    #' @field overwrite If true will overwrite file on the server.
    overwrite = NULL,
    #' @field file_size File size.
    file_size = NULL,
    #' @field base_part_size Size of part in bytes.
    base_part_size = NULL,
    #' @field part_length Number of parts to upload.
    part_length = NULL,
    #' @field parts List of parts to be uploaded (class Part).
    parts = NULL,
    #' @field auth Authentication object.
    auth = NULL,

    #' @description Create a new Upload object.
    #' @param path Path to the file on the local disc.
    #' @param project Project's identifier (character).
    #' @param parent The ID of the folder to which the item is being uploaded.
    #' @param file_name New file name. Optional.
    #' @param overwrite If true will overwrite file on the server.
    #' @param file_size File size.
    #' @param part_size Size of a single part in bytes.
    #' @param auth Authentication object.
    #' @param ... Other arguments.
    initialize = function(path = NA, project = NA, parent = NA,
                          file_name = NA, overwrite = NA, file_size = NA,
                          part_size = NA, auth = NA, ...) {
      # Initialize Item class
      # super$initialize(...)

      self$upload_id <- NULL
      self$path <- normalizePath(path)
      self$project <- project
      self$parent <- parent

      if (is_missing(file_name)) {
        file_name <- basename(path)
      }
      if (grepl("\\s", file_name) || grepl("\\/", file_name)) {
        # nolint start
        rlang::abort("The file name cannot contain spaces or backslashes.")
        # nolint end
      }

      self$file_name <- file_name
      self$overwrite <- overwrite
      self$base_part_size <- part_size
      self$file_size <- file_size
      self$part_length <- as.integer(
        ceiling(self$file_size / self$base_part_size)
      )
      self$parts <- list()
      self$auth <- auth
    },

    #' @description Initialize new upload job.
    #' @param ... Other arguments.
    upload_init = function(...) {
      body <- list(
        "name" = self$file_name,
        "size" = self$file_size,
        "part_size" = self$base_part_size
      )
      if (!is.null(self$project)) {
        body[["project"]] <- self$project$id
      } else if (!is.null(self$parent)) {
        body[["parent"]] <- self$parent$id
      }

      res <- sevenbridges2::api(
        path = "upload/multipart",
        method = "POST",
        query = list(overwrite = self$overwrite),
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      self$upload_id <- res$upload_id

      self$base_part_size <- as.integer(res$part_size)
      self$part_length <- as.integer(
        ceiling(self$file_size / self$base_part_size)
      )

      # nolint start
      rlang::inform(glue::glue("New upload job is initialized with upload_id: {self$upload_id}."))
      # nolint end
      return(self)
    },
    #' @description Get the details of an active multipart upload.
    #' @param list_parts If TRUE, also return a list of parts
    #' that have been reported as completed for this multipart upload.
    #' @param ... Other arguments.
    #' @importFrom checkmate assert_logical
    upload_info = function(list_parts = TRUE, ...) {
      if (is.null(upload_id)) {
        rlang::abort("Upload is not initialized yet.")
      }
      checkmate::assert_logical(list_parts)

      res <- sevenbridges2::api(
        path = paste0("upload/multipart/", self$upload_id),
        method = "GET",
        query = list(list_parts = list_parts),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)
      res # prettier print TODO
    }
  )
)


# nolint start
#' @title R6 Class representing a part of the uploading file
#'
#' @description
#' R6 Class representing a resource for managing parts of the files' uploads.
#'
#' @importFrom R6 R6Class
#' @export
Part <- R6::R6Class(
  # nolint end
  "Part",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field part_number Part number.
    part_number = NULL,
    #' @field part_size Part size.
    part_size = NULL,
    #' @field url The URL to which to make the HTTP part upload request.
    url = NULL,
    #' @field expires ISO 8601 combined date and time representation
    #' in Coordinated Universal Time (UTC) by when the HTTP
    #' part upload request should be made.
    expires = NULL,
    #' @field headers A map of headers and values that should be
    #' set when making the HTTP part upload request.
    headers = NULL,
    #' @field success_codes A list of status codes returned by
    #' the HTTP part upload request that should be recognized as success.
    #' A successful part upload request should be reported back
    #' to the API in a call to report an uploaded file part by
    #' passing the information collected from the report object.
    success_codes = NULL,
    #' @field report Report object.
    report = NULL,
    #' @field etag ETag received after starting a part upload.
    etag = NULL,

    #' @description Create a new Part object.
    #' @param part_number Part number.
    #' @param part_size Part size.
    #' @param url The URL to which to make the HTTP part upload request.
    #' @param expires Combined date and time representation
    #' in UTC by when the HTTP part upload request should be made.
    #' @param headers A map of headers and values that should be
    #' set when making the HTTP part upload request.
    #' @param success_codes A list of status codes returned by
    #' the HTTP part upload request that should be recognized as success.
    #' @param report Report object.
    #' @param etag ETag received after starting a part upload.
    #' @param ... Other arguments.
    initialize = function(part_number = NA, part_size = NA,
                          url = NA, expires = NA, headers = NA,
                          success_codes = NA, report = NA,
                          etag = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$part_number <- part_number
      self$part_size <- part_size
      self$url <- url
      self$expires <- expires # handle this
      self$headers <- headers
      self$success_codes <- success_codes
      self$report <- report
      self$etag <- etag
    }
  )
)
