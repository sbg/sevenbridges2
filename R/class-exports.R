# nolint start
#' @title R6 Class representing storage exports endpoints
#'
#' @description
#' R6 Class representing storage exports resource endpoints.
#'
#' @importFrom R6 R6Class
#' @export
Exports <- R6::R6Class(
  "Exports",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "storage/exports",
      "get" = "storage/exports/{id}",
      "create" = "storage/exports"
    ),

    #' @description Create a new Exports object.
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all export jobs --------------------------------------
    #' @description This call lists export jobs initiated by particular user.
    #'  Note that when you export a file from a project on the Platform into a
    #'  volume, you write to your cloud storage bucket.
    #'
    #' @param volume String volume id or Volume object. List all exports
    #'  into this particular volume. Optional.
    #' @param state The state of the export job. Possible values are:
    #'  \itemize{
    #'    \item `PENDING`: the export is queued;
    #'    \item `RUNNING`: the export is running;
    #'    \item `COMPLETED`: the export has completed successfully;
    #'    \item `FAILED`: the export has failed.
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
    #' like 'fields', etc.
    #'
    #' @importFrom checkmate assert_character assert_subset
    #'
    #' @return Collection of Export jobs (Export class objects).
    query = function(volume = NULL, state = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      if (!is_missing(volume)) {
        volume <- check_and_transform_id(volume, "Volume")
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
        state = state,
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asExportList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    }, # nocov end

    # Get export job details -----------------------------------------------
    #' @description This call will return the details of an export job.
    #'
    #' @param id The export job identifier (id).
    #' @param ... Other arguments that can be passed to core `api()` function
    #' like 'fields', etc.
    #'
    #' @return Export object.
    get = function(id, ...) {
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE,
        ...
      )
      return(asExport(res, auth = self$auth))
    }, # nocov end

    # Start new export job -----------------------------------------------
    #' @description This call lets you queue a job to export a file from a
    #'  project on the Platform into a volume. The file selected for export must
    #'  not be a public file or an alias. Aliases are objects stored in your
    #'  cloud storage bucket which have been made available on the Platform.
    #'  The volume you are exporting to must be configured for
    #'  read-write access. To do this, set the `access_mode` parameter to
    #'  `RW` when creating or modifying a volume. \cr
    #'
    #'  Essentially, the call writes to your cloud storage bucket via the
    #'  volume. If this call is successful, the original project file will
    #'  become an alias to the newly exported object on the volume.
    #'  The source file will be deleted from the Platform and, if no more
    #'  copies of this file exist, it will no longer count towards your total
    #'  storage price on the Platform. \cr
    #'  In summary, once you export a file from the Platform to a volume, it is
    #'  no longer part of the storage on the Platform and cannot be exported
    #'  again. \cr
    #'
    #'  If you want to export multiple files, the recommended way is to do it
    # nolint start
    #' in bulk considering the API rate limit ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    # nolint end
    #'
    #' @param source_file String file id or File object you want to export to
    #'  the volume.
    #' @param destination_volume String volume id or Volume object you want to
    #'  export files into.
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
    #' @param overwrite Set to `TRUE` if you want to overwrite the item
    #'  if another one with the same name already exists at the destination.
    #' @param copy_only If `TRUE`, file will be copied to a volume but
    #'  source file will remain on the Platform.
    #' @param properties Named list of additional volume properties, like:
    #'  \itemize{
    #'    \item `sse_algorithm` - S3 server-side encryption to use when
    #'      exporting to this bucket. Supported values:
    #'      `AES256` (SSE-S3 encryption), `aws:kms`, `null`
    #'      (no server-side encryption). Default: `AES256`.
    #'    \item `sse_aws_kms_key_Id`: Applies to type: `s3`.
    #'      If AWS KMS encryption is used, this should be set to the required
    #'      KMS key. If not set and aws:kms is set as sse_algorithm,
    #'      default KMS key is used.
    #'    \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'      on during export. Supported values: `any one of S3 canned ACLs`;
    #'      `null` (do not apply canned ACLs). Default: `null`.
    #'  }
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate test_r6 assert_string assert_logical assert_list
    #' @importFrom glue glue glue_col
    #' @importFrom rlang abort
    #'
    #' @return Export object.
    submit_export = function(source_file, destination_volume,
                             destination_location, overwrite = FALSE,
                             copy_only = FALSE, properties = NULL, ...) {
      if (is_missing(source_file)) {
        rlang::abort("Source file must be provided as a string or File object!")
      }
      if (checkmate::test_r6(source_file, classes = "File") &&
        tolower(source_file$type) == "folder") {
        rlang::abort("Folders cannot be exported. Please, provide single file id or File object with type = 'file'.") # nolint
      }
      file <- check_and_transform_id(source_file, class_name = "File")

      if (is_missing(destination_volume)) {
        rlang::abort("Destination volume must be provided as a string or Volume object!") # nolint
      }
      destination_volume <- check_and_transform_id(destination_volume,
        class_name = "Volume"
      )
      if (is_missing(destination_location)) {
        rlang::abort("Destination location name must be provided as a string!")
      }
      checkmate::assert_string(
        destination_location,
        null.ok = FALSE
      )
      checkmate::assert_logical(overwrite, len = 1, null.ok = TRUE)
      checkmate::assert_logical(copy_only, len = 1, null.ok = TRUE)
      checkmate::assert_list(properties, null.ok = TRUE)

      # Build body
      # nocov start
      body <- list(
        destination = list(
          volume = destination_volume,
          location = destination_location
        ),
        source = list(
          file = file
        ),
        overwrite = overwrite,
        properties = properties
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        query = list(copy_only = copy_only),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      export <- asExport(res, auth = self$auth)

      rlang::inform(glue::glue_col("New export with id {green {export$id} } has started!")) # nolint

      return(export)
    },

    # Delete export job ----------------------------------------------------
    #' @description Deleting export jobs is not possible.
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting export jobs is not possible.")
    } # nocov end
  )
)
