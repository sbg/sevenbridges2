# nolint start
#' @title R6 Class representing a Volume
#'
#' @description
#' R6 Class representing a resource for managing volumes.
#'
#' @importFrom R6 R6Class
#' @export
Volume <- R6::R6Class(
  # nolint end
  "Volume",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "list" = "storage/volumes/{id}/list",
      "volume_file_details" = "storage/volumes/{id}",
      "update" = "storage/volumes/{id}",
      "deactivate" = "storage/volumes/{id}",
      "delete" = "storage/volumes/{id}",
      "list_members" = "storage/volumes/{id}/members",
      "add_members" = "storage/volumes/{id}/members",
      "remove_members" = "storage/volumes/{id}/members/{username}",
      "member_permissions" = "storage/volumes/{id}/members/{username}",
      "overwrite_permissions" = "storage/volumes/{id}/members/{username}/permissions", # nolint
      "modify_permissions" = "storage/volumes/{id}/members/{username}/permissions" # nolint
    ),
    #' @field id String. Volume ID, constructed from {division}/{volume_name}
    #' or {volume_owner}/{volume_name}
    id = NULL,
    #' @field name String. The name of the volume. It must be unique from all
    #' other volumes for this user. Required if from_path parameter
    #' is not provided.
    name = NULL,
    #' @field description String. The description of the volume.
    description = NULL,
    #' @field access_mode String. Signifies whether this volume should be used
    #' for read-write (RW) or read-only (RO) operations. The access mode is
    #' consulted independently of the credentials granted to Seven Bridges
    #' when the volume was created, so it is possible to use a read-write
    #' credentials to register both read-write and read-only volumes using it.
    #' Default: `"RW"`.
    access_mode = NULL,
    #' @field service String. This object more closely describes the mapping of
    #' the volume to the cloud service where the data is stored.
    service = NULL,
    #' @field created_on The date and time this volume was created.
    created_on = NULL,
    #' @field modified_on The date and time this volume was last modified.
    modified_on = NULL,
    #' @field active Boolean. If a volume is deactivated, this field will be
    #' set to FALSE.
    active = NULL,
    #' @field href Link to the current volume resource.
    href = NULL,
    #' @description Create a new Volume object.
    #' @param id String. Volume ID, constructed from {division}/{volume_name}
    #' or {volume_owner}/{volume_name}
    #' @param name String. The name of the volume. It must be unique from all
    #' other volumes for this user.
    #' @param description String. The description of the volume.
    #' @param access_mode String. Signifies whether this volume should be used
    #' for read-write (RW) or read-only (RO) operations. The access mode is
    #' consulted independently of the credentials granted to Seven Bridges
    #' when the volume was created, so it is possible to use a read-write
    #' credentials to register both read-write and read-only volumes using it.
    #' Default: `"RW"`.
    #' @param service String. This object more closely describes the mapping of
    #' the volume to the cloud service where the data is stored.
    #' @param created_on The date and time this volume was created.
    #' @param modified_on The date and time this volume was last modified.
    #' @param active Boolean. If a volume is deactivated, this field will be
    #' set to FALSE.
    #' @param href Link to the current volume resource.
    #' @param ... Other arguments.
    initialize = function(id = NA, name = NA, description = NA,
                          access_mode = NA, service = NA, created_on = NA,
                          modified_on = NA, active = NA, href = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$name <- name
      self$description <- description
      self$access_mode <- access_mode
      self$service <- service
      self$created_on <- created_on
      self$modified_on <- modified_on
      self$active <- active
      self$href <- href
    },
    # nocov start PREPAKUJ!
    #' @description Print method for Volume class.
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

      # Extract type, bucket and prefix from service to print
      x <- append(x, x$service[c("type", "bucket", "prefix")])

      # Remove lists
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Volume")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    }, # nocov end
    #' @description Update a volume.
    #' This function updates the details of a specific volume.
    #' @param description String. The description of the volume.
    #' @param access_mode String. Signifies whether this volume should be used
    #' for read-write (RW) or read-only (RO) operations. The access mode is
    #' consulted independently of the credentials granted to Seven Bridges
    #' when the volume was created, so it is possible to use a read-write
    #' credentials to register both read-write and read-only volumes using it.
    #' Default: `"RW"`.
    #' @param service String. This object more closely describes the mapping of
    #' the volume to the cloud service where the data is stored.
    #' @importFrom checkmate assert_character assert_list
    update = function(description = NULL, access_mode = NULL,
                      service = NULL) {
      checkmate::assert_character(description, null.ok = TRUE)
      checkmate::assert_character(access_mode, null.ok = TRUE)
      if (!is_missing(access_mode) &&
        !(access_mode %in% c("RW", "RO"))) {
        rlang::abort("Access mode must be RW or RO.")
      }
      checkmate::assert_list(service,
        any.missing = FALSE, all.missing = FALSE,
        null.ok = TRUE
      )
      # nocov start
      body <- list(
        description = description,
        service = service,
        access_mode = access_mode
      )
      body <- body[!sapply(body, is.null)]

      path <- glue::glue(self$URL[["update"]])

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      res <- status_check(res)

      self$initialize(
        href = res$href,
        id = res$id,
        name = res$name,
        description = res$description,
        access_mode = res$access_mode,
        service = res$service,
        created_on = res$created_on,
        modified_on = res$modified_on,
        active = res$active,
        auth = auth,
        response = attr(res, "response")
      )
    }, # nocov end
    #' @description Deactivate volume
    #' Once deactivated, you cannot import from, export to, or browse within a
    #' volume. As such, the content of the files imported from this volume will
    #' no longer be accessible on the Platform. However, you can update the
    #' volume and manage members.
    #' Note that you cannot deactivate the volume if you have running imports
    #' or exports unless you force the operation using the query parameter
    #' force=TRUE.
    #' Note that to delete a volume, first you must deactivate it and delete
    #' all files which have been imported from the volume to the Platform.
    #'
    #' @param ... Other query parameters like 'force'.
    deactivate = function(...) {
      if (!self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} is already deactivated.")
        )
      }
      path <- glue::glue(self$URL[["update"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = list("active" = FALSE),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      res <- status_check(res)
      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{red deactivated}."))) # nolint
      self$active <- FALSE
      self
    }, # nocov end
    #' @description Reactivate volume
    #' This function reactivates the previously deactivated volume by updating
    #' the 'active' field of the volume to TRUE.
    #' @param ... Other query parameters like 'force'.
    reactivate = function(...) {
      if (self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} is already active.")
        )
      }
      path <- glue::glue(self$URL[["update"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = list("active" = TRUE),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      res <- status_check(res)
      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{green reactivated}."))) # nolint
      self$active <- TRUE
      self
    }, # nocov end
    #' @description Delete volume
    #' This call deletes a volume you've created to refer to storage on
    #' Amazon Web Services, Google Cloud Storage, Azure or Ali cloud.
    #' To be able to delete a volume, you first need to deactivate it and then
    #' delete all files on the Platform that were previously imported from
    #' the volume.
    delete = function() {
      if (self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} must be deactivated first in order to be able to delete it.") # nolint
        )
      }
      path <- glue::glue(self$URL[["delete"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )
      res <- status_check(res)

      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{red deleted}."))) # nolint
      self$initialize(
        href = NULL,
        id = NULL,
        name = NULL,
        description = NULL,
        access_mode = NULL,
        service = NULL,
        created_on = NULL,
        modified_on = NULL,
        active = NULL,
        auth = NULL,
        response = attr(res, "response")
      )
    }, # nocov end
    #' @description List volume contents
    #' This call lists the contents of a specific volume.
    #' @param parent This is prefix parameter in volume context. If specified,
    #' the content of the parent directory on the current volume is listed.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response. You can use: `href`, `location`, `volume`, `type`,
    #' `metadata`, `_all`. Default: `_all`.
    #' @param link Link to use in the next chunk of results. Contains limit and
    #' continuation_token. If provided it will overwrite other arguments'
    #' values passed.
    #' @param continuation_token Continuation token received to use for next
    #' chunk of results. Behaves similarly like offset parameter.
    #' @return VolumeFileCollection object containing list of VolumeFile
    #' objects.
    list_files = function(parent = NULL,
                          limit = getOption("sevenbridges2")$"limit",
                          fields = "_all",
                          link = NULL,
                          continuation_token = NULL) {
      checkmate::assert_character(parent,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
      checkmate::assert_character(fields, null.ok = TRUE, typed.missing = TRUE)
      if (!all(fields %in% c(
        "href", "location", "volume", "type",
        "metadata", "_all", NULL
      ))) {
        rlang::abort("Fields parameter can contain subset of values: 'href', 'location', 'volume', 'type', 'metadata', '_all'") # nolint
      }
      checkmate::assert_character(link,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
      checkmate::assert_character(continuation_token,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )

      path <- glue::glue(self$URL[["list"]]) # nocov start

      res <- sevenbridges2::api(
        url = link,
        path = path,
        query = list(prefix = parent, continuation_token = continuation_token),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        limit = limit,
        fields = fields
      )
      res <- status_check(res)

      return(asVolumeFileCollection(res, auth = self$auth))
    }, # nocov end
    #' @description Get volume file information
    #' This function returns the specific Volume File.
    #' @param location Volume file id, which is represented as file
    #' location.
    #' @param link Link to the file resource received from listing volume's
    #' contents. Cannot be used together with location.
    #' @importFrom checkmate assert_character
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #' @return VolumeFile object.
    get_file = function(location = NULL, link = NULL) {
      checkmate::assert_character(location,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
      checkmate::assert_character(link,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
      if (!is_missing(location) && !is_missing(link)) {
        rlang::abort("Please, provide either location or link, not both.")
      }
      if (is_missing(location) && is_missing(link)) {
        rlang::abort("Empty arguments are not allowed. Please, provide either location or link.") # nolint
      }
      if (!is_missing(link)) {
        link <- glue::glue(link, "&fields=_all")
      }
      # nocov start
      path <- glue::glue(self$URL[["volume_file_details"]], "/object")

      res <- sevenbridges2::api(
        url = link,
        path = path,
        query = list(location = location),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )
      res <- status_check(res)

      return(asVolumeFile(res, auth = self$auth))
    } # nocov end
  )
)

# nocov start
# Helper function for creating Volume objects
asVolume <- function(x, auth = NULL) {
  Volume$new(
    href = x$href,
    id = x$id,
    name = x$name,
    description = x$description,
    access_mode = x$access_mode,
    service = x$service,
    created_on = x$created_on,
    modified_on = x$modified_on,
    active = x$active,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of Volume objects
asVolumeList <- function(x, auth) {
  obj <- lapply(x$items, asVolume, auth = auth)
  obj
}
# nocov end
