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
      "create" = "storage/exports",
      "bulk_get" = "bulk/storage/exports/get",
      "bulk_create" = "bulk/storage/exports/create"
    ),

    # Initialize Exports object -----------------------------------------------
    #' @description Create a new Exports object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List export jobs --------------------------------------------------------
    #' @description This call lists export jobs initiated by a particular user.
    #'  Note that when you export a file from a project on the Platform into a
    #'  volume, you write to your cloud storage bucket.
    #'
    #' @param volume Volume id or Volume object. List all exports
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
    #' @examples
    #' \dontrun{
    #'  exports_object <- Exports$new(
    #'    auth = auth
    #'  )
    #'
    #'  # List all your running or failed export jobs on the volume
    #'  exports_object$query(volume = volume, state = c("RUNNING", "FAILED"))
    #' }
    #'
    #' @return \code{\link{Collection}} of \code{\link{Export}} objects.
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

    # Get export job details --------------------------------------------------
    #' @description This call will return the details of an export job.
    #'
    #' @param id The export job identifier (id).
    #' @param ... Other arguments that can be passed to core `api()` function
    #' like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  exports_object <- Exports$new(
    #'    auth = auth
    #'  )
    #'
    #'  # Get export job by ID
    #'  exports_object$get(id = id)
    #' }
    #'
    #' @return \code{\link{Export}} object.
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

    # Start new export job ----------------------------------------------------
    #' @description This call lets you queue a job to export a file from a
    #'  project on the Platform into a volume. The file selected for export
    #'  must not be a public file or an alias. Aliases are objects stored in
    #'  your cloud storage bucket which have been made available on the
    #'  Platform.
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
    #'  Read more about this operation in our documentation
    #'  [here](https://docs.sevenbridges.com/reference/start-an-export-job-v2).
    #'  \cr
    #'
    #'  If you want to export multiple files, the recommended way is to do it
    # nolint start
    #'  in bulk considering the API rate limit ([learn more](https://docs.sevenbridges.com/docs/api-rate-limit)).
    # nolint end
    #'  Bulk operations will be implemented in next releases.
    #'
    #' @param source_file File id or File object you want to export to
    #'  the volume.
    #' @param destination_volume Volume id or Volume object you want to
    #'  export files into.
    #' @param destination_location Volume-specific location to which the
    #'  file will be exported.
    #'  This location should be recognizable to the underlying cloud service as
    #'  a valid key or path to a new file. Please note that if this volume has
    #'  been configured with a prefix parameter, the value of prefix will be
    #'  prepended to location before attempting to create the file on the
    #'  volume.
    #'
    #'  If you would like to export the file into a folder on the volume,
    #'  please add the folder name as a prefix before the file name in the form
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
    #'      KMS key. If not set and `aws:kms` is set as `sse_algorithm`,
    #'      default KMS key is used.
    #'    \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'      on during export. Supported values: any one of
    # nolint start
    #'      [S3 canned ACLs](https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl);
    # nolint end
    #'      `null` (do not apply canned ACLs). Default: `null`.
    #'  }
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate test_r6 assert_string assert_logical assert_list
    #' @importFrom glue glue glue_col
    #' @importFrom rlang abort
    #'
    #' @examples
    #' \dontrun{
    #'  exports_object <- Exports$new(
    #'    auth = auth
    #'  )
    #'
    #'  # Submit export job
    #'  exp_job1 <- exports_object$submit_export(
    #'                           source_file = test_file,
    #'                           destination_volume = vol1,
    #'                           destination_location = "new_volume_file.txt"
    #'                 )
    #' }
    #'
    #' @return \code{\link{Export}} object.
    submit_export = function(source_file, destination_volume,
                             destination_location, overwrite = FALSE,
                             copy_only = FALSE, properties = NULL, ...) {
      if (is_missing(source_file)) {
        rlang::abort("Source file must be provided as a string or File object!")
      }
      if (checkmate::test_r6(source_file, classes = "File") &&
        tolower(source_file$type) == "folder") {
        rlang::abort("Folders cannot be exported. Please provide a single file ID or File object with type = 'file'.") # nolint
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

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body,
        query = list(copy_only = copy_only),
        advance_access = TRUE,
        ...
      )

      export <- asExport(res, auth = self$auth)

      rlang::inform(glue::glue_col("New export with id {green {export$id} } has started!")) # nolint

      return(export)
    },

    # Delete export job -------------------------------------------------------
    #' @description Export jobs cannot be deleted.
    #'
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting export jobs is not possible.")
    }, # nocov end

    # Get bulk export jobs ----------------------------------------------------
    #' @description This call returns the details of a bulk export job.
    #'  When you export files from a project on the Platform into a volume,
    #'  you write to your cloud storage bucket. This call obtains the details
    #'  of that job.
    #'
    #' @param exports The list of the export job IDs as returned by the call
    #'  to start a bulk export job or list of \code{\link{Export}} objects.
    #'
    #' @importFrom checkmate assert_list
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  exports_object <- Exports$new(
    #'                     auth = auth,
    #'                    )
    #'
    #'  # List export jobs
    #'  exports_object$bulk_get(
    #'   exports = list("export-job-id-1", "export-job-id-2")
    #'   )
    #' }
    #'
    #' @return \code{\link{Collection}} with list of \code{\link{Export}}
    #'  objects.
    bulk_get = function(exports) {
      if (is_missing(exports)) {
        rlang::abort("Exports should be set as a list of export job IDs or list of Export objects.") # nolint
      }

      checkmate::assert_list(exports)
      unlisted_ids <- lapply(exports, check_and_transform_id, "Export")

      # Build body
      # nocov start
      body <- list(
        export_ids = unlisted_ids
      )

      path <- glue::glue(self$URL[["bulk_get"]])

      res <- self$auth$api(
        path = path,
        method = "POST",
        body = body,
        advance_access = TRUE
      )

      res$items <- asExportList(res, auth = self$auth, bulk = TRUE)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Start bulk export job ---------------------------------------------------
    #' @description Bulk export files from your project on the Seven Bridges
    #'  Platform into your volume. One call can contain up to 100 items.
    #'  Files selected for export must not be public files or aliases.
    #'  Aliases are objects stored in your cloud storage bucket which have
    #'  been made available on the Platform. The volume you are exporting to
    #'  must be configured for read-write access. To do this, set the
    #'  `access_mode` parameter to RW when creating or modifying a volume.
    #'
    #'  Essentially, the call writes to your cloud storage bucket via the
    #'  volume. If this call is successful, the original project files will
    #'  become aliases to the newly exported objects on the volume.
    #'  Source files will be deleted from the Platform and, if no more copies
    #'  of the files exist, they will no longer count towards your total
    #'  storage price on the Platform. In summary, once you export files from
    #'  the Platform to a volume, they are no longer part of the storage on
    #'  the Platform and cannot be exported again.
    #'
    # nolint start
    #'  Learn more about using the Volumes API for [Amazon S3](https://docs.sevenbridges.com/docs/aws-cloud-storage-tutorial) and
    #'  for [Google Cloud Storage](https://docs.sevenbridges.com/docs/google-cloud-storage-tutorial).
    # nolint end
    #'
    #' @param items Nested list of elements containing information about each
    #'  file to be exported. For each element, users must provide:
    #'  \itemize{
    #'      \item `source_file` - File ID or File object you want to export to
    #'        the volume,
    #'      \item `destination_volume` - Volume ID or Volume object you want to
    #'        export files into.
    #'      \item `destination_location` - Volume-specific location to which
    #'        the file will be exported. This location should be recognizable
    #'        to the underlying cloud service as a valid key or path to a
    #'        new file. Please note that if this volume has been configured
    #'        with a `prefix` parameter, the value of `prefix` will be
    #'        prepended to the location before attempting to create the file on
    #'        the volume. \cr
    #'        If you would like to export the file into a folder on
    #'        the volume, please add folder name as a prefix before the file
    #'        name in the `<folder-name>/<file-name>` form.
    #'      \item `overwrite` - Set to `TRUE` if you want to overwrite the
    #'        item with the same name if it already exists at the
    #'        destination.
    #'      \item `properties` - Named list of additional volume properties,
    #'        like:
    #'        \itemize{
    #'          \item `sse_algorithm` - S3 server-side encryption to use when
    #'            exporting to this bucket. Supported values:
    #'            `AES256` (SSE-S3 encryption), `aws:kms`, `null`
    #'            (no server-side encryption). Default: `AES256`.
    #'          \item `sse_aws_kms_key_Id`: Applies to type: `s3`.
    #'            If AWS KMS encryption is used, this should be set to the
    #'            required KMS key. If not set and `aws:kms` is set as
    #'            `sse_algorithm`, default KMS key is used.
    #'          \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'            during export. Supported values: any one of
    # nolint start
    #'      [S3 canned ACLs](https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl);
    #'      `null` (do not apply canned ACLs). Default: `null`.
    #'      }
    #'  }
    # nolint end
    #'
    #'
    #'  Example of the list:
    #'  ```{r}
    #'  items <- list(
    #'            list(
    #'              source_file = "test_file-id",
    #'              destination_volume = "volume-id",
    #'              destination_location = "new_volume_file.txt"
    #'            ),
    #'            list(
    #'              source_file = "test_file_obj",
    #'              destination_volume = "test_volume_obj",
    #'              destination_location = "/volume_folder/exported_file.txt",
    #'              overwrite = TRUE
    #'            ),
    #'            list(
    #'              source_file = "project_file_3_id",
    #'              destination_volume = "volume-id",
    #'              destination_location = "project_file_3.txt",
    #'              properties = list(
    #'                sse_algorithm = "AES256"
    #'              )
    #'            )
    #'          )
    #' ```
    #'
    # nolint start
    #'  Read more on how to [export files from your project to a volume or a volume folder](https://docs.sevenbridges.com/reference/start-a-bulk-export-job).
    # nolint end
    #'
    #'  Utility function \code{\link{prepare_items_for_bulk_export}} can help
    #'  you prepare the `items` parameter for the `bulk_submit_export()`
    #'  method.
    #'
    #' @param copy_only If set to true, the files will be copied to a volume
    #'  but the source files will remain on the Platform.
    #'
    #' @importFrom checkmate assert_list assert_string test_r6 assert_logical
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  exports_object <- Exports$new(
    #'                     auth = auth
    #'                    )
    #'
    #'  # Submit new bulk export into a volume
    #'  exports_object$bulk_submit_export(items = list(
    #'    list(
    #'      source_file = "test_file-id",
    #'      destination_volume = "volume-id",
    #'      destination_location = "new_volume_file.txt"
    #'    ),
    #'    list(
    #'      source_file = test_file_obj,
    #'      destination_volume = test_volume_obj,
    #'      destination_location = "/volume_folder/exported_file.txt",
    #'      overwrite = TRUE
    #'    ),
    #'    list(
    #'      source_file = "project_file_3_id",
    #'      destination_volume = "volume-id",
    #'      destination_location = "project_file_3.txt",
    #'      properties = list(
    #'       sse_algorithm = "AES256"
    #'      )
    #'    )
    #'   ), copy_only = TRUE
    #'  )
    #' }
    #'
    #' @return \code{\link{Collection}} with list of \code{\link{Export}}
    #'  objects.
    bulk_submit_export = function(items, copy_only = FALSE) {
      if (is_missing(items)) {
        rlang::abort("Items parameter should be set as a nested list of information on files you want to export.") # nolint
      }
      checkmate::assert_list(items)

      checkmate::assert_logical(copy_only, len = 1, null.ok = TRUE)


      body_elements <- list()

      for (i in seq_len(length(items))) {
        item <- items[[i]]
        checkmate::assert_list(item)
        body_element <- list()

        if (is_missing(item[["source_file"]])) {
          rlang::abort(glue::glue("Source file must be provided as a string or File object in element {i}.")) # nolint
        }
        if (checkmate::test_r6(item[["source_file"]], classes = "File") &&
          tolower(item[["source_file"]]$type) == "folder") {
          rlang::abort(glue::glue("Folders cannot be exported. Please provide a single file ID or File object with type = 'file' in element {i}.")) # nolint
        }
        body_element$source <- list(
          file = check_and_transform_id(item[["source_file"]],
            class_name = "File"
          )
        )

        if (is_missing(item[["destination_volume"]])) {
          rlang::abort(glue::glue("Destination volume must be provided as a string or Volume object in element {i}.")) # nolint
        }
        destination_volume <- check_and_transform_id(
          item[["destination_volume"]],
          class_name = "Volume"
        )
        if (is_missing(item[["destination_location"]])) {
          rlang::abort(glue::glue("Destination location name must be provided as a string in element {i}.")) # nolint
        }
        checkmate::assert_string(
          item[["destination_location"]],
          null.ok = FALSE
        )
        body_element$destination <- list(
          volume = destination_volume,
          location = item[["destination_location"]]
        )
        checkmate::assert_logical(item[["overwrite"]], len = 1, null.ok = TRUE)
        body_element$overwrite <- item[["overwrite"]]

        checkmate::assert_list(item[["properties"]], null.ok = TRUE)
        body_element$properties <- item[["properties"]]

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
        query = list(copy_only = copy_only),
        method = "POST",
        body = body,
        advance_access = TRUE
      )

      failed_export_tries <- list()
      # Process the response items when returned errors
      for (i in seq_along(res$items)) {
        item <- res$items[[i]]
        if (!is.null(item$error)) {
          res$items[[i]]$resource$error <- item$error
          failed_export_tries <- append(failed_export_tries, list(item))
        }
      }

      if (length(failed_export_tries) == length(res$items)) {
        rlang::abort("None of the files could be exported. Please check file export limitations in the API documentation.") # nolint
      }

      res$items <- asExportList(res, auth = self$auth, bulk = TRUE)
      rlang::inform(glue::glue("New export jobs have started."))

      if (length(failed_export_tries) > 0) {
        rlang::inform(glue::glue("However, some files cannot be exported.
                                 Please check file export limitations in the API documentation.")) # nolint
      }

      return(asCollection(res, auth = self$auth))
      # nocov end
    }
  )
)
