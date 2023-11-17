# nolint start
#' @title R6 Class representing a VolumeFile
#'
#' @description
#' R6 Class representing a resource for managing VolumeFile objects.
#'
#' @importFrom R6 R6Class
#'
#' @export
VolumeFile <- R6::R6Class(
  # nolint end
  "VolumeFile",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "storage/volumes/{self$volume}/object"
    ),
    #' @field location File location on the volume.
    location = NULL,
    #' @field type Type of storage (cloud provider). Can be one of:
    #'  `s3`, `gcs`, `azure`, `OSS`.
    type = NULL,
    #' @field volume Volume id.
    volume = NULL,
    #' @field metadata File's metadata if exists.
    metadata = NULL,

    # Initialize VolumeFile object -------------------------------------------
    #' @description Create a new VolumeFile object.
    #'
    #' @param res Response containing VolumeFile object info.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$location <- res$location
      self$type <- res$type
      self$volume <- res$volume
      self$metadata <- res$metadata
    },

    # nocov start
    # Print VolumeFile object -------------------------------------------------
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
    },

    # Reload VolumeFile object ------------------------------------------------
    #' @description Reload VolumeFile object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return \code{\link{VolumeFile}} object.
    reload = function(...) {
      reload_url <- ""
      if (!is_missing(self$href)) {
        reload_url <- self$href
      }

      res <- sevenbridges2::api(
        url = reload_url,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        path = glue::glue(self$URL[["get"]]),
        query = list(
          location = self$location
        ),
        advance_access = TRUE,
        ...
      )
      if (is.null(res$volume)) {
        res$volume <- self$volume
      }

      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
      rlang::inform("VolumeFile object is refreshed!")
    }, # nocov end

    # Start new file import job -----------------------------------------------
    #' @description This call lets you queue a job to import this file or folder
    #'  from a volume into a project on the Platform. \cr
    #'  Essentially, you are importing an item from your cloud storage provider
    #'  (Amazon Web Services, Google Cloud Storage, Azure or Ali Cloud) via the
    #'  volume onto the Platform. \cr
    #'  If successful, an alias will be created on the Platform. Aliases appear
    #'  on the Platform and can be copied, executed, and modified as such.
    #'  They refer back to the respective item on the given volume.
    #'
    #' @param destination_project String destination project id or Project
    #'  object. Not required, but either `destination_project` or
    #'  `destination_parent` directory must be provided.
    #' @param destination_parent String folder id or File object
    #'  (with `type = 'FOLDER'`). Not required, but either `destination_project`
    #'  or `destination_parent` directory must be provided.
    #' @param name The name of the alias to create. This name should be unique
    #'  to the project. \cr
    #'  If the name is already in use in the project, you should
    #'  use the `overwrite` query parameter in this call to force any item with
    #'  that name to be deleted before the alias is created.
    #'  If name is omitted, the alias name will default to the last segment of
    #'  the complete location (including the prefix) on the volume. \cr
    #'
    #'  Segments are considered to be separated with forward slashes /.
    #'  Allowed characters in file names are all alphanumeric and special
    #'  characters except forward slash /, while folder names can contain
    #'  alphanumeric and special characters _, - and ..
    #' @param overwrite Set to `TRUE` if you want to overwrite the item if
    #'  another one with the same name already exists at the destination.
    #'  Bear in mind that if used with folders import, the folder's content
    #'  (files with the same name) will be overwritten, not the whole folder.
    #' @param autorename Set to `TRUE` if you want to automatically rename the
    #'  item (by prefixing its name with an underscore and number) if another
    #'  one with the same name already exists at the destination.
    #'  Bear in mind that if used with folders import, the folder content will
    #'  be renamed, not the whole folder.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return \code{\link{Import}} object.
    import = function(destination_project = NULL, destination_parent = NULL,
                      name = NULL, overwrite = FALSE, autorename = FALSE,
                      ...) {
      # nocov start
      self$auth$imports$submit_import(
        source_volume = self$volume,
        source_location = self$location,
        destination_project = destination_project,
        destination_parent = destination_parent,
        name = name,
        overwrite = overwrite,
        autorename = autorename,
        ...
      ) # nocov end
    }
  )
)
# nocov start
# Helper functions for creating VolumeFile objects ---------------------------
asVolumeFile <- function(x = NULL, auth = NULL) {
  VolumeFile$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

asVolumeFileList <- function(x, auth) {
  obj <- lapply(x, asVolumeFile, auth = auth)
  obj
}
# nocov end
