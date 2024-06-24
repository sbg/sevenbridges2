#' @title Prepare items for bulk import
#'
#' @description Utility function to prepare the `items` parameter, a list of
#'  elements containing information about each file or folder to be imported
#'  using the `bulk_submit_import()` method.
#'
#' @details Based on the provided list of \code{\link{VolumeFile}} or
#'  \code{\link{VolumePrefix}} objects, this function allows you to set the
#'  following fields for each item:
#'  \itemize{
#'      \item `source_volume`
#'      \item `source_location`
#'      \item `destination_project` or `destination_parent`
#'      \item `autorename`
#'      \item `preserve_folder_structure`
#'  }
#'
#'  However, keep in mind that there are certain constraints:
#'  \itemize{
#'      \item The same `destination_project`/`destination_parent` selection
#'       applies to all items in the resulting list.
#'      \item The same applies to `autorename` and `preserve_folder_structure`
#'       parameters.
#'      \item This function doesn't allow specification of the `name` of
#'       aliases to create. This is something that should be specified per
#'       item, therefore it cannot be applied to the entire list. However, once
#'       you have the output of the `prepare_items_for_bulk_import` function
#'       you can manually add the `name` field to certain items if necessary.
#'  }
#'
#' @param volumes A list of \code{\link{VolumeFile}} or
#'  \code{\link{VolumePrefix}} objects to be imported.
#' @param destination_project Destination project ID or \code{\link{Project}}
#'  object. Not required, but either `destination_project` or
#'  `destination_parent` directory must be provided.
#' @param destination_parent Folder ID or \code{\link{Project}} object
#'  (with `type = 'FOLDER'`). Not required, but either `destination_project` or
#'  `destination_parent` directory must be provided.
#' @param autorename Logical indicating whether to autorename conflicting
#'  files (default is `FALSE`). Set to `TRUE` if you want to automatically
#'  rename the item (by prefixing its name with an underscore and number) if
#'  another one with the same name already exists at the destination.
#'  Bear in mind that if used with folders import, the folder content will
#'  be renamed, not the whole folder. Keep in mind that the same `autorename`
#'  option will be applied to all items.
#' @param preserve_folder_structure Logical indicating whether to preserve
#'  folder structure. Set to `TRUE` if you want to keep the
#'  exact source folder structure. The default value is `TRUE` if the item
#'  being imported is a folder. Should not be used if you are importing a
#'  file. Bear in mind that if you use `preserve_folder_structure = FALSE`,
#'  that the response will be the parent folder object containing imported
#'  files alongside with other files if they exist. Keep in mind that the same
#'  `preserve_folder_structure` option will be applied to all folders.
#'
#' @importFrom rlang abort
#' @importFrom checkmate assert_list assert_logical test_r6
#'
#' @seealso \code{\link{Imports}}, \code{\link{VolumeFile}},
#'  \code{\link{VolumePrefix}}
#'
#' @return A list of elements containing information about each file/folder to
#'  be imported.
#'
#' @examples
#' \dontrun{
#' # Example 1: Prepare 2 items for bulk import action - provide destination
#' # project
#' volume_obj_1 <- a$volumes$get
#' volume_obj_2 <- a$volumes$get
#'
#' volumes_to_import <- list(volume_obj_1, volume_obj_2)
#'
#' destination_project <- a$projects$get(id = "project_id")
#'
#' prepare_items_for_bulk_import(
#'   volumes = volumes_to_import,
#'   destination_project = destination_project
#' )
#' }
#' \dontrun{
#' # Example 2: Prepare 2 items for bulk import action - provide destination
#' # parent
#' volume_obj_1 <- a$volumes$get
#' volume_obj_2 <- a$volumes$get
#'
#' volumes_to_import <- list(volume_obj_1, volume_obj_2)
#'
#' destination_parent <- a$files$get(id = "folder_id")
#'
#' prepare_items_for_bulk_import(
#'   volumes = volumes_to_import,
#'   destination_parent = destination_parent
#' )
#' }
#'
prepare_items_for_bulk_import <- function(volumes, destination_project = NULL, destination_parent = NULL, autorename = FALSE, preserve_folder_structure = TRUE) { # nolint
  # Check 'volumes' parameter
  if (is_missing(volumes)) {
    rlang::abort("Parameter 'volumes' is missing. Please provide a list of VolumeFile or VolumePrefix objects.") # nolint
  }

  checkmate::assert_list(volumes,
    types = c("VolumeFile", "VolumePrefix"),
    min.len = 1,
    any.missing = FALSE
  )

  # Check 'destination_project' and 'destination_parent' parameters
  if (is_missing(destination_project) &&
    is_missing(destination_parent)) {
    rlang::abort("Please specify either the 'destination_project' or 'destination_parent' parameter.") # nolint
  }
  if (!is_missing(destination_project) &&
    !is_missing(destination_parent)) {
    rlang::abort("Either destination project or parent parameter must be proveded, not both.") # nolint
  }
  if (!is_missing(destination_project)) {
    destination <- check_and_transform_id(
      destination_project,
      class_name = "Project"
    )
    destination_field_name <- "destination_project"
  }
  if (!is_missing(destination_parent)) {
    if (checkmate::test_r6(destination_parent, classes = "File") &&
      tolower(destination_parent$type) != "folder") {
      rlang::abort("Destination parent directory parameter must contain folder id or File object with type = 'folder'.") # nolint
    }
    destination <- check_and_transform_id(
      x = destination_parent,
      class_name = "File"
    )
    destination_field_name <- "destination_parent"
  }

  # Check 'autorename' parameter
  checkmate::assert_logical(autorename, len = 1)
  # Check 'preserve_folder_structure' parameter
  checkmate::assert_logical(preserve_folder_structure, len = 1) # nolint


  result <- lapply(volumes,
    prepare_volume_import_item,
    destination_field_name = destination_field_name,
    destination = destination,
    autorename = autorename,
    preserve_folder_structure = preserve_folder_structure
  )

  return(result)
}

# Prepare volume item for bulk import action
#'
#' @description The function constructs a list representing a single body item
#'  for `bulk_submit_import()` method.
#'
#' @param volume_object Either a \code{\link{VolumeFile}} object or a
#'  \code{\link{VolumePrefix}} object.
#' @param destination_field_name String. Specifies the destination-specific
#'  field to be added to the imported item. This parameter must be either
#'  `destination_project` or `destination_parent`.
#' @param destination String. Specifies the destination for the imported
#'  file. The value depends on the `destination_field_name` parameter:
#'  - If `destination_field_name` is `destination_project`, `destination`
#'    should be the ID of the project where the item will be imported. The item
#'    will be imported to the root of the project's files.
#'  - If `destination_field_name` is `destination_parent`, `destination` should
#'    be the ID of the target folder within the project where the item should
#'    be imported.
#' @param autorename Logical indicating whether to autorename conflicting
#'  files (default is `FALSE`). Set to `TRUE` if you want to automatically
#'  rename the item (by prefixing its name with an underscore and number) if
#'  another one with the same name already exists at the destination.
#'  Bear in mind that if used with folders import, the folder content will
#'  be renamed, not the whole folder.
#' @param preserve_folder_structure Logical indicating whether to preserve
#'  folder structure. Set to `TRUE` if you want to keep the
#'  exact source folder structure. The default value is `TRUE` if the item
#'  being imported is a folder. Should not be used if you are importing a
#'  file. Bear in mind that if you use `preserve_folder_structure = FALSE`,
#'  that the response will be the parent folder object containing imported
#'  files alongside with other files if they exist.
#'
#' @return List of key-value pairs forming an item of request body for staring
#'  an import job.
#'
#' @seealso \code{\link{VolumeFile}}, \code{\link{VolumePrefix}}
#'
#' @importFrom rlang abort
# nolint start
#' @importFrom checkmate test_r6 assert_logical test_class assert_string assert_subset
# nolint end
#'
#' @examples
#' \dontrun{
#' # Example 1: Prepare volume item with destination project
#' prepare_volume_import_item(
#'   volume_object = VolumeFileObject,
#'   destination_field_name = "destination_project",
#'   destination = "project_id",
#'   autorename = TRUE
#' )
#'
#' # Example 2: Prepare volume item with destination parent directory
#' prepare_volume_import_item(
#'   volume_object = VolumePrefixObject,
#'   destination_parent = "destination_parent",
#'   destination = "parent_folder_id",
#'   autorename = TRUE,
#'   preserve_folder_structure = TRUE
#' )
#' }
#'
#' @noRd
prepare_volume_import_item <- function(volume_object, destination_field_name, destination, autorename = FALSE, preserve_folder_structure = TRUE) { # nolint
  if (is_missing(volume_object)) {
    rlang::abort("Parameter 'volume_object' is missing. Please provide either a VolumeFile or a VolumePrefix object.") # nolint
  }

  if (!checkmate::test_r6(volume_object, classes = "VolumeFile") && !checkmate::test_r6(volume_object, classes = "VolumePrefix")) { # nolint
    rlang::abort("Please make sure that the provided 'volume_object' parameter is either a VolumeFile or a VolumePrefix object.") # nolint
  }

  # Check destination_field_name parameter
  if (is_missing(destination_field_name)) {
    rlang::abort("Parameter 'destination_field_name' is missing. It should be one of the following two strings: 'destination_project' or 'destination_parrent'") # nolint
  }
  checkmate::assert_string(destination_field_name, null.ok = FALSE)
  checkmate::assert_subset(destination_field_name, c("destination_project", "destination_parent")) # nolint

  # Check destination parameter
  if (is_missing(destination)) {
    rlang::abort("Parameter 'destination' is missing.") # nolint
  }
  checkmate::assert_string(destination, null.ok = FALSE)

  # Check autorename parameter
  checkmate::assert_logical(autorename, len = 1)
  # Check preserve_folder_structure parameter
  checkmate::assert_logical(preserve_folder_structure, len = 1)


  # ---------------------------------------------------------------------------
  if (checkmate::test_class(volume_object, "VolumeFile")) {
    item <- list(
      "source_volume" = volume_object$volume,
      "source_location" = volume_object$location,
      autorename = autorename
    )
    # Add and populate destination field
    item[[destination_field_name]] <- destination
  } else if (checkmate::test_class(volume_object, "VolumePrefix")) {
    item <- list(
      "source_volume" = volume_object$volume,
      "source_location" = volume_object$prefix,
      autorename = autorename,
      preserve_folder_structure = preserve_folder_structure
    )
    # Add and populate destination field
    item[[destination_field_name]] <- destination
  }
  # ---------------------------------------------------------------------------
  return(item)
}
