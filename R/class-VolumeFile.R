# nolint start
#' @title R6 Class representing a VolumeFile
#'
#' @description
#' R6 Class representing a resource for managing VolumeFile objects.
#'
#' @importFrom R6 R6Class
#' @export
VolumeFile <- R6::R6Class(
  # nolint end
  "VolumeFile",
  portable = FALSE,
  public = list(
    #' @field href URL to the resource.
    href = NULL,
    #' @field location File/prefix location on the volume.
    location = NULL,
    #' @field type Type of resource - can be either FILE or PREFIX.
    type = NULL,
    #' @field storage_type Type of storage (cloud provider). Can be one of:
    #' 's3', 'gcs', 'azure', 'OSS'.
    storage_type = NULL,
    #' @field volume Volume id.
    volume = NULL,
    #' @field metadata File's metadata if exists.
    metadata = NULL,
    #' @field auth SevenBridges authentication object.
    auth = NULL,
    #' @description Create a new VolumeFile object.
    #' @param href URL to the resource.
    #' @param location File/prefix location on the volume.
    #' @param type Type of resource - can be either FILE or PREFIX.
    #' @param storage_type Type of storage (cloud provider). Can be one of:
    #' 's3', 'gcs', 'azure', 'OSS'.
    #' @param volume Volume id.
    #' @param metadata File's metadata if exists.
    #' @param auth SevenBridges authentication object.
    initialize = function(href = NA, location = NA, type = NA,
                          storage_type = NA, volume = NA, metadata = NA,
                          auth = NA) {
      self$href <- href
      self$location <- location
      self$type <- type
      self$storage_type <- storage_type
      self$volume <- volume
      self$metadata <- metadata
      self$auth <- auth
    },
    # nocov start
    #' @description Print method for VolumeFile class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    print = function() {
      x <- as.list(self)

      # Extract all except 'raw'
      x$raw <- NULL

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)

      # Remove lists
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("VolumeFile")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    }, # nocov end
    # Start new import job -----------------------------------------------
    #' @description This call lets you queue a job to import this file or folder
    #' from a volume into a project on the Platform.
    #' Essentially, you are importing an item from your cloud storage provider
    #' (Amazon Web Services, Google Cloud Storage, Azure or Ali Cloud) via the
    #' volume onto the Platform.
    #' If successful, an alias will be created on the Platform. Aliases appear
    #' on the Platform and can be copied, executed, and modified as such.
    #' They refer back to the respective item on the given volume.
    #'
    #' @param destination_project String destination project id or Project
    #' object. Not required, but either `destination_project` or
    #' `destination_parent` directory must be provided.
    #' @param destination_parent String folder id or File object
    #' (with type = 'FOLDER'). Not required, but either `destination_project`
    #' or `destination_parent` directory must be provided.
    #' @param name The name of the alias to create. This name should be unique
    #' to the project.
    #' If the name is already in use in the project, you should
    #' use the `overwrite` query parameter in this call to force any item with
    #' that name to be deleted before the alias is created.
    #' If name is omitted, the alias name will default to the last segment of
    #' the complete location (including the prefix) on the volume.
    #'
    #' Segments are considered to be separated with forward slashes /.
    #' Allowed characters in file names are all alphanumeric and special
    #' characters except forward slash /, while folder names can contain
    #' alphanumeric and special characters _, - and ..
    #'
    #' @param overwrite Boolean. Whether to overwrite the item if another one
    #' with the same name already exists at the destination.
    #' Bear in mind that if used with folders import, the folder's content
    #' (files with the same name) will be overwritten, not the whole folder.
    #' @param autorename Boolean. Whether to automatically rename the item
    #' (by prefixing its name with an underscore and number) if another one
    #' with the same name already exists at the destination.
    #' Bear in mind that if used with folders import, the folder content will
    #' be renamed, not the whole folder.
    #' @param preserve_folder_structure Boolean. Whether to keep the exact
    #' source folder structure. The default value is true if the item being
    #' imported is a folder. Should not be used if you are importing a file.
    #' Bear in mind that if you use preserve_folder_structure = FALSE, that the
    #' response will be the parent folder object containing imported files
    #' alongside with other files if they exist.
    #'
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #'
    #' @return Import job object.
    import = function(destination_project = NULL, destination_parent = NULL,
                      name = NULL, overwrite = FALSE, autorename = FALSE,
                      preserve_folder_structure = NULL, ...) {
      self$auth$imports$submit_import(
        source_volume = self$volume,
        source_location = self$location,
        destination_project = destination_project,
        destination_parent = destination_parent,
        name = name,
        overwrite = overwrite,
        autorename = autorename,
        preserve_folder_structure = preserve_folder_structure,
        ...
      )
    }
  )
)

# Helper function for creating VolumeFile objects
asVolumeFile <- function(x, auth = NULL) {
  VolumeFile$new(
    href = x$href,
    location = ifelse(length(x$prefix) > 0, paste0(x$prefix, "/"), x$location),
    type = ifelse(length(x$prefix) > 0, "PREFIX", "FILE"),
    storage_type = x$type,
    volume = x$volume,
    metadata = x$metadata,
    auth = auth
  )
}

# Helper function for creating a list of VolumeFile objects
asVolumeFileList <- function(x, auth) {
  obj <- lapply(x, asVolumeFile, auth = auth)
  obj
}
