# nolint start
#' @title R6 Class representing a VolumePrefix
#'
#' @description
#' R6 Class representing a resource for managing VolumePrefix objects.
#'
#' @importFrom R6 R6Class
#' @export
VolumePrefix <- R6::R6Class(
  # nolint end
  "VolumePrefix",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "list" = "storage/volumes/{self$volume}/list"
    ),
    #' @field prefix File/prefix name on the volume.
    prefix = NULL,
    #' @field volume Volume id.
    volume = NULL,

    #' @description Create a new VolumePrefix object.
    #' @param res Response containing VolumePrefix object info.
    #' @param ... Other arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)
      if (!is.null(res$prefix) && !endsWith(res$prefix, "/")) {
        self$prefix <- paste0(res$prefix, "/")
      } else {
        self$prefix <- res$prefix
      }
      self$volume <- res$volume
    },
    # nocov start
    #' @description Print method for VolumePrefix class.
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

      cli::cli_h1("VolumePrefix")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },
    #' @description
    #' Reload VolumePrefix object.
    reload = function() {
      rlang::inform(
        "Reload operation is not available for VolumePrefix objects."
      )
    }, # nocov end
    #' @description List volume folder contents
    #' This call lists the contents of a specific volume folder.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param continuation_token Continuation token received to use for next
    #' chunk of results. Behaves similarly like offset parameter.
    #' @param ... Other parameters that can be passed to api() function, like
    #' fields for example. With fields parameter you can specify a subset of
    #' fields to include in the response. You can use: `href`, `location`,
    #' `volume`, `type`, `metadata`, `_all`. Default: `_all`.
    #' @return VolumeContentCollection object containing list of VolumeFile
    #' and VolumePrefix objects.
    list_files = function(limit = getOption("sevenbridges2")$limit,
                          continuation_token = NULL,
                          ...) {
      checkmate::assert_character(continuation_token,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
      # nocov start
      path <- glue::glue(self$URL[["list"]])

      res <- sevenbridges2::api(
        path = path,
        query = list(
          prefix = self$prefix,
          continuation_token = continuation_token
        ),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        limit = limit,
        ...
      )

      return(asVolumeContentCollection(res, auth = self$auth))
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
      # nocov start
      self$auth$imports$submit_import(
        source_volume = self$volume,
        source_location = self$prefix,
        destination_project = destination_project,
        destination_parent = destination_parent,
        name = name,
        overwrite = overwrite,
        autorename = autorename,
        preserve_folder_structure = preserve_folder_structure,
        ...
      ) # nocov end
    }
  )
)

# Helper function for creating VolumePrefix objects
asVolumePrefix <- function(x = NULL, auth = NULL) {
  VolumePrefix$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

# Helper function for creating a list of VolumePrefix objects
asVolumePrefixList <- function(x, auth) {
  obj <- lapply(x, asVolumePrefix, auth = auth)
  obj
}
