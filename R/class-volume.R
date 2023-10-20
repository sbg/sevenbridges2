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
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "storage/volumes/{id}",
      "volume" = "storage/volumes/{self$id}",
      "list" = "storage/volumes/{self$id}/list",
      "volume_file" = "storage/volumes/{self$id}/object",
      "members" = "storage/volumes/{self$id}/members",
      "member_username" = "storage/volumes/{self$id}/members/{username}",
      "member_permissions" = "storage/volumes/{self$id}/members/{username}/permissions" # nolint
    ),
    #' @field id Volume ID, constructed from {division}/{volume_name}
    #'  or {volume_owner}/{volume_name}.
    id = NULL,
    #' @field name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if `from_path` parameter
    #'  is not provided.
    name = NULL,
    #' @field description The description of the volume.
    description = NULL,
    #' @field access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    access_mode = NULL,
    #' @field service This object in form of string more closely describes the
    #'  mapping of the volume to the cloud service where the data is stored.
    service = NULL,
    #' @field created_on The date and time this volume was created.
    created_on = NULL,
    #' @field modified_on The date and time this volume was last modified.
    modified_on = NULL,
    #' @field active If a volume is deactivated, this field will be
    #'  set to `FALSE`.
    active = NULL,

    #' @description Create a new Volume object.
    #' @param res Response containing Volume object info.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
      self$description <- res$description
      self$access_mode <- res$access_mode
      self$service <- res$service
      self$created_on <- res$created_on
      self$modified_on <- res$modified_on
      self$active <- res$active
    },
    # nocov start
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
    },

    #' @description Reload Volume object information.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #' @return Volume object.
    reload = function(...) {
      super$reload(
        cls = self,
        advance_access = TRUE,
        ...
      )
      rlang::inform("Volume object is refreshed!")
    },

    # nocov end
    #' @description Update a volume.
    #'  This function updates the details of a specific volume.
    #' @param description The new description of the volume.
    #' @param access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    #' @param service This object in form of string more closely describes the
    #'  mapping of the volume to the cloud service where the data is stored.
    #'
    #' @importFrom checkmate assert_character assert_list
    #'
    #' @return Volume object.
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

      path <- glue::glue(self$URL[["volume"]])

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      self$initialize(
        res = res,
        href = res$href,
        auth = self$auth,
        response = attr(res, "response")
      )
    }, # nocov end

    #' @description Deactivate volume.
    #'  Once deactivated, you cannot import from, export to, or browse within a
    #'  volume. As such, the content of the files imported from this volume will
    #'  no longer be accessible on the Platform. However, you can update the
    #'  volume and manage members. \cr
    #'  Note that you cannot deactivate the volume if you have running imports
    #'  or exports unless you force the operation using the query parameter
    #'  force=TRUE.
    #'  Note that to delete a volume, first you must deactivate it and delete
    #'  all files which have been imported from the volume to the Platform.
    #'
    #' @param ... Other query parameters or arguments that can be passed to
    #'  core `api()` function like 'force'.
    #'  Use it within query parameter, like `query = list(force = TRUE)`.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    deactivate = function(...) {
      if (!self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} is already deactivated.")
        )
      }
      path <- glue::glue(self$URL[["volume"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = list("active" = FALSE),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{red deactivated}."))) # nolint

      self$active <- FALSE

      return(self)
    }, # nocov end

    #' @description Reactivate volume.
    #'  This function reactivates the previously deactivated volume by updating
    #'  the `active` field of the volume to `TRUE`.
    #' @param ... Other query parameters or arguments that can be passed to
    #'  core `api()` function like 'force'.
    #'  Use it within query parameter, like `query = list(force = TRUE)`.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    reactivate = function(...) {
      if (self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} is already active.")
        )
      }
      path <- glue::glue(self$URL[["volume"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = list("active" = TRUE),
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{green reactivated}."))) # nolint

      self$active <- TRUE

      return(self)
    }, # nocov end

    #' @description Delete volume.
    #'  This call deletes a volume you've created to refer to storage on
    #'  Amazon Web Services, Google Cloud Storage, Azure or Ali cloud.
    #'  To be able to delete a volume, you first need to deactivate it and then
    #'  delete all files on the Platform that were previously imported from
    #'  the volume.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    delete = function() {
      if (self$active) {
        rlang::abort(
          glue::glue("The volume {self$name} must be deactivated first in order to be able to delete it.") # nolint
        )
      }
      path <- glue::glue(self$URL[["volume"]]) # nocov start

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )


      rlang::inform(glue::glue("The volume {self$name} has been ", glue::glue_col("{red deleted}."))) # nolint

      self$initialize(
        res = NULL,
        href = NULL,
        auth = NULL,
        response = attr(res, "response")
      )
    }, # nocov end

    #' @description List volume contents.
    #'  This call lists the contents of a specific volume.
    #' @param prefix This is parent parameter in volume context. If specified,
    #'  the content of the parent directory on the current volume is listed.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param link Link to use in the next chunk of results. Contains limit and
    #'  continuation_token. If provided it will overwrite other arguments'
    #'  values passed.
    #' @param continuation_token Continuation token received to use for next
    #'  chunk of results. Behaves similarly like offset parameter.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields' for example. With fields parameter you can specify a
    #'  subset of fields to include in the response.
    #'  You can use: `href`, `location`, `volume`, `type`, `metadata`, `_all`.
    #'  Default: `_all`.
    #'
    #' @importFrom checkmate assert_character
    #' @importFrom glue glue
    #'
    #' @return VolumeContentCollection object containing list of VolumeFile
    #' and VolumePrefix objects.
    list_contents = function(prefix = NULL,
                             limit = getOption("sevenbridges2")$limit,
                             link = NULL,
                             continuation_token = NULL,
                             ...) {
      checkmate::assert_character(prefix,
        len = 1, null.ok = TRUE,
        typed.missing = TRUE
      )
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
        query = list(prefix = prefix, continuation_token = continuation_token),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        limit = limit,
        ...
      )

      return(asVolumeContentCollection(res, auth = self$auth))
    }, # nocov end

    #' @description Get volume file information.
    #'  This function returns the specific Volume File.
    #'
    #' @param location Volume file id, which is represented as file
    #'  location.
    #' @param link Link to the file resource received from listing volume's
    #'  contents. Cannot be used together with location.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_character
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #'
    #' @return VolumeFile object.
    get_file = function(location = NULL, link = NULL, ...) {
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
      # nocov start
      if (!is_missing(link)) {
        link <- glue::glue(link, "&fields=_all")
      }

      path <- glue::glue(self$URL[["volume_file"]])

      res <- sevenbridges2::api(
        url = link,
        path = path,
        query = list(location = location),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      return(asVolumeFile(res, auth = self$auth))
      # nocov end
    },

    #' @description List members of a volume.
    #'  This function returns the members of a specific volume.
    #'
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other parameters that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #'
    #' @return Collection containing list of Member objects.
    list_members = function(limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      # nocov start
      path <- glue::glue(self$URL[["members"]])

      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        limit = limit,
        offset = offset,
        ...
      )

      res$items <- asMemberList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    #' @description Add member to a volume.
    #'  This function adds members to the specified volume.
    #'
    #' @param user The Seven Bridges Platform username of the person
    #'  you want to add to the volume or object of class Member containing
    #'  user's username.
    #' @param permissions List of permissions granted to the user being added.
    #'  Permissions include listing the contents of a volume, importing files
    #'  from the volume to the Platform, exporting files from the Platform to
    #'  the volume, and admin privileges. \cr
    #'  It can contain fields: 'read', 'copy', 'write' and 'admin' with
    #'  logical fields - TRUE if certain permission is allowed to the user, or
    #'  FALSE if it's not.
    #'  Example:
    #'  ```{r}
    #'    permissions = list(read = TRUE, copy = TRUE, write = FALSE,
    #'    admin = FALSE)
    #'  ```
    #' @importFrom checkmate assert_list assert_subset
    #' @importFrom glue glue
    #'
    #' @return Member object.
    add_member = function(user, permissions = list(
                            read = TRUE,
                            copy = FALSE,
                            write = FALSE,
                            admin = FALSE
                          )) {
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      checkmate::assert_list(permissions,
        null.ok = FALSE, len = 4,
        types = "logical"
      )
      checkmate::assert_subset(names(permissions),
        empty.ok = FALSE,
        choices = c("read", "copy", "write", "admin")
      )
      # nocov start
      path <- glue::glue(self$URL[["members"]])

      body <- list(
        username = username,
        permissions = permissions
      )
      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asMember(res, auth = self$auth))
      # nocov end
    },

    #' @description Remove member from a volume.
    #'  This function removes members from the specified volume.
    #'
    #' @param user The Seven Bridges Platform username of the person
    #'  you want to remove from the volume or object of class Member containing
    #'  user's username.
    #'
    #' @importFrom glue glue glue_col
    remove_member = function(user) {
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      # nocov start
      path <- glue::glue(self$URL[["member_username"]])

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      rlang::inform(glue_col("Member {green {username}} was successfully removed from the {green {id}} volume.")) # nolint
      # nocov end
    },

    #' @description Get member's details.
    #'  This function returns member's information.
    #'
    #' @param user The Seven Bridges Platform username of the person
    #'  you want to get information about or object of class Member containing
    #'  user's username.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #'
    #' @return Member object.
    get_member = function(user, ...) {
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      # nocov start
      path <- glue::glue(self$URL[["member_username"]])

      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE,
        ...
      )

      return(asMember(res, auth = self$auth))
      # nocov end
    },

    #' @description Modify a volume member's permission.
    #'  This function modifies the permissions for a member of a specific
    #'  volume. Note that this does not overwrite all previously set permissions
    #'  for the member.
    #' @param user The Seven Bridges Platform username of the person
    #'  you want to modify permissions for or object of class Member containing
    #'  user's username.
    #' @param permissions List of specific (or all) permissions you want to
    #'  update for the member of the volume.
    #'  Permissions include listing the contents of a volume, importing files
    #'  from the volume to the Platform, exporting files from the Platform to
    #'  the volume, and admin privileges.
    #'  It can contain fields: 'read', 'copy', 'write' and 'admin' with
    #'  logical fields - TRUE if certain permission is allowed to the user, or
    #'  FALSE if it's not.
    #'  Example:
    #'  ```{r}
    #'    permissions = list(read = TRUE, copy = TRUE, write = FALSE,
    #'    admin = FALSE)
    #'  ```
    #'
    #' @importFrom checkmate assert_list
    #' @importFrom glue glue glue_col
    #'
    #' @return Permission object.
    modify_member_permissions = function(user, permissions = list(
                                           read = TRUE,
                                           copy = FALSE,
                                           write = FALSE,
                                           admin = FALSE
                                         )) {
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      checkmate::assert_list(permissions,
        null.ok = FALSE, max.len = 4,
        types = "logical"
      )
      checkmate::assert_subset(names(permissions),
        empty.ok = FALSE,
        choices = c("read", "copy", "write", "admin")
      )
      # nocov start
      body <- flatten_query(permissions)

      path <- glue::glue(self$URL[["member_permissions"]])

      res <- sevenbridges2::api(
        path = path,
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      rlang::inform(glue::glue_col("Member {green {username}}'s permissions have been {green updated} to:")) # nolint

      return(asPermission(res, auth = self$auth))
      # nocov end
    },
    # nocov start
    #' @description This call lists import jobs initiated by particular user
    #'  from this volume.
    #'
    #' @param project String project id or Project object. List all volume
    #'  imports to this project. Optional.
    #' @param state String. The state of the import job. Possible values are:
    #'  \itemize{
    #'    \item `PENDING`: the import is queued;
    #'    \item `RUNNING`: the import is running;
    #'    \item `COMPLETED`: the import has completed successfully;
    #'    \item `FAILED`: the import has failed.
    #'  }
    #'  Example: `state = c("RUNNING", "FAILED")`
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return Collection containing list of Import jobs (Import class objects).
    list_imports = function(project = NULL, state = NULL,
                            limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      self$auth$imports$query(
        volume = self,
        project = project,
        state = state,
        limit = limit,
        offset = offset,
        ...
      )
    },

    #' @description This call lists export jobs initiated by a user into this
    #'  volume.
    #'  Note that when you export a file from a project on the Platform into a
    #'  volume, you write to your cloud storage bucket.
    #'
    #' @param state The state of the export job. Possible values are:
    #'  \itemize{
    #'    \item `PENDING`: the export is queued;
    #'    \item `RUNNING`: the export is running;
    #'    \item `COMPLETED`: the export has completed successfully;
    #'    \item `FAILED`: the export has failed.
    #'  }
    #'  Example: `state = c("RUNNING", "FAILED")`
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return Collection containing list of export jobs (Export class objects).
    list_exports = function(state = NULL,
                            limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      self$auth$exports$query(
        volume = self,
        state = state,
        limit = limit,
        offset = offset,
        ...
      )
    } # nocov end
  )
)

# nocov start
# Helper function for creating Volume objects
asVolume <- function(x = NULL, auth = NULL) {
  Volume$new(
    res = x,
    href = x$href,
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
