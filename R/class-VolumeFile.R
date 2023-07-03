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
    } # nocov end
  )
)

# nocov start
# Helper function for creating VolumeFile objects
asVolumeFile <- function(x, auth = NULL) {
  VolumeFile$new(
    href = x$href,
    location = paste0(x$prefix, x$location),
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
# nocov end
