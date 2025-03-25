#' @title Prepare items for bulk export
#'
#' @description Utility function to prepare the `items` parameter, a list of
#'  elements containing information about each file to be exported using the
#'  `bulk_submit_export()` method.
#'
#' @details Based on the provided list of \code{\link{File}} objects or
#'  file IDs, this function allows you to set the following fields for each
#'  item:
#'  \itemize{
#'      \item `source_file`
#'      \item `destination_volume`
#'      \item `destination_location`
#'      \item `overwrite`
#'      \item `properties`
#'  }
#'
#'  However, keep in mind that there are certain constraints:
#'  \itemize{
#'      \item The same `destination_volume` applies to all items in the
#'       resulting list.
#'      \item The same applies to `overwrite` and `properties`
#'       parameters.
#'      \item By default, the `destination_location` field is populated with
#'       the source file name. Upon retrieval of the list of items for bulk
#'       export, you can manually update the \cr
#'       `destination_location` field for each element of the list as needed.
#'       Additionally, you have the flexibility to manually modify any other
#'       fields in the list if required.
#'  }
#'
#' @param files A list of \code{\link{File}} objects or list of strings
#'  (IDs) of the files you are about to export to a volume.
#' @param destination_volume Either a \code{\link{Volume}} object or the ID of
#'  the volume to which the file will be exported.
#' @param destination_location_prefix Character. If the volume has been
#'  configured with a prefix parameter, \cr
#'  `destination_location_prefix` value will be prepended to location before
#'  attempting to create the file on the volume. This parameter can be treated
#'  as a path to a new file on the volume. The default value is `NULL`.
#'
#'  If you would like to export the file into a folder on the volume,
#'  please add folder name as the prefix before the file name in the
#'  `"<folder-name>/"` form. Remember to put a slash character ("/") at the end
#'  of the string.
#'
#'  Keep in mind that the same prefix will be added to all items (files) in the
#'  resulting list.
#' @param overwrite Logical. If this is set to `TRUE` and a named file exists
#'  in the project where the alias is about to be created, the existing file
#'  will be deleted. `FALSE` by default.
#'
#'  Keep in mind that the same overwrite option will be applied to all items
#'  (files) in the resulting list.
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
#'      during export. Supported values: any one of
# nolint start
#'      [S3 canned ACLs](https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl);
# nolint end
#'      `null` (do not apply canned ACLs). Default: `null`.
#'  }
#'
#'  Keep in mind that the same properties will be applied to all items (files)
#'  in the resulting list.
#'
#' @seealso \code{\link{Exports}}, \code{\link{File}}, \code{\link{Volume}}
#'
#' @importFrom rlang abort
# nolint start
#' @importFrom checkmate assert_character assert_logical assert_list assert_subset
# nolint end
#' @importFrom purrr map
#'
#' @return List of body params items for starting an export job.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Prepare 3 items for bulk export action
#' file_object_1 <- a$files$get(id = "file_1_ID")
#' file_object_2 <- a$files$get(id = "file_2_ID")
#' file_object_3 <- a$files$get(id = "file_3_ID")
#'
#' files_to_export <- list(file_object_1, file_object_2, file_object_3)
#'
#' prepare_items_for_bulk_export(files_to_export,
#'   destination_volume = "aws_example_volume"
#' )
#' }
#' \dontrun{
#' # Example 2: Prepare 3 items for bulk export action into some folder
#' # on the volume - use folder name as prefix before file names
#' file_object_1 <- a$files$get(id = "file_1_ID")
#' file_object_2 <- a$files$get(id = "file_2_ID")
#' file_object_3 <- a$files$get(id = "file_3_ID")
#'
#' files_to_export <- list(file_object_1, file_object_2, file_object_3)
#'
#' prepare_items_for_bulk_export(files_to_export,
#'   destination_volume = "aws_example_volume",
#'   destination_location_prefix = "example_folder/"
#' )
#' }
#'
prepare_items_for_bulk_export <- function(files, destination_volume, destination_location_prefix = NULL, overwrite = TRUE, properties = NULL) { # nolint
  # Check 'files' parameter
  if (is_missing(files)) {
    rlang::abort("Parameter 'files' is missing. Please provide either a list of File objects, or a list of file IDs.") # nolint
  }
  checkmate::assert_list(files, types = c("character", "File"), min.len = 1)

  for (file in files) {
    if (checkmate::test_r6(file, classes = "File") &&
      tolower(file$type) == "folder") {
      rlang::abort("Provided list contains folder objects, which cannot be exported. Please make sure to remove all folder objects (type = 'folder') and try again.") # nolint
    }
  }

  # Check 'destination_volume' parameter
  if (is_missing(destination_volume)) {
    rlang::abort("Missing 'destination_volume' parameter. Destination volume must be provided either as a string or a Volume object.") # nolint
  }
  destination_volume <- check_and_transform_id(destination_volume,
    class_name = "Volume"
  )

  # Check 'destination_location_prefix' parameter
  checkmate::assert_string(destination_location_prefix, null.ok = TRUE)

  # Ensure the destination_location_prefix string ends with a slash character
  # nocov start
  if (!is_missing(destination_location_prefix)) {
    if (!endsWith(destination_location_prefix, "/")) {
      destination_location_prefix <- paste(destination_location_prefix, "/", sep = "") # nolint
    }
  }
  # nocov end


  # Check 'overwrite' parameter
  checkmate::assert_logical(overwrite, len = 1, null.ok = TRUE)

  # Check 'properties' parameter
  checkmate::assert_list(properties,
    types = c("character", "null"),
    max.len = 3,
    null.ok = TRUE,
    names = "named"
  )
  checkmate::assert_subset(names(properties), c(
    "sse_algorithm",
    "sse_aws_kms_key_Id",
    "aws_canned_acl"
  ))

  # Create list of items
  resulting_list_of_items <- purrr::map(files, ~ list(
    "source_file" = .x,
    "destination_volume" = destination_volume,
    "destination_location" = ifelse(!is.null(destination_location_prefix), paste0(destination_location_prefix, .x$name), .x$name), # nolint
    "overwrite" = overwrite,
    "properties" = properties
  ))

  return(resulting_list_of_items)
}
