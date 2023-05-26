# nolint start
#' @title R6 Class representing an app
#'
#' @description
#' R6 Class representing a resource for managing apps.
#'
#' @importFrom R6 R6Class
#' @export
App <- R6::R6Class(
  # nolint end
  "App",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id Character used as an app ID.
    id = NULL,
    #' @field project Project ID if any, when returned by an API call.
    project = NULL,
    #' @field name String used as an app name.
    name = NULL,
    #' @field revision Integer representing app's revision.
    revision = NULL,
    #' @field copy_of The original application of which this is a copy.
    copy_of = NULL,
    #' @field latest_revision Integer representing app's latest revision.
    latest_revision = NULL,
    #' @field raw App's raw CWL (JSON or YAML).
    raw = NULL,
    #' @description Create a new App object.
    #' @param id Character used as an app ID.
    #' @param project Project ID if any, when returned by an API call.
    #' @param name String used as a file name.
    #' @param revision Integer representing app's revision.
    #' @param raw App's raw CWL (JSON or YAML).
    #' @param copy_of The original application of which this is a copy.
    #' @param latest_revision Integer representing app's latest revision.
    #' @param ... Other arguments.
    initialize = function(id = NA, project = NA, name = NA, revision = NA,
                          raw = NA, copy_of = NA, latest_revision = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$project <- project
      self$name <- name
      self$revision <- revision
      self$copy_of <- copy_of
      self$latest_revision <- latest_revision
      self$raw <- raw
    },
    # nocov start
    #' @description Print method for App class.
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
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("App")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    }, # nocov end
    #' @description A method that copies the current app.
    #'
    #' @details
    #' The method copies the current app to the specified project.
    #'
    #' @param project Project object or project ID. If you opt for the latter,
    #' remember that the project ID should be specified in
    #' `<username>/<project-name>` format, e.g. `rfranklin/my-project`.
    #' @param name The new name the app will have in the target project.
    #' Optional.
    #' @param strategy The method for copying the app. Supported strategies:
    #' \itemize{
    #'    \item `clone` - copy all revisions; get updates from the same app as
    #'    the copied app (default)
    #'    \item `direct`: copy latest revision; get updates from the copied app
    #'    \item `clone_direct`: copy all revisions; get updates from the copied
    #'    app
    #'    \item `transient`: copy latest revision; get updates from the same
    #'    app as the copied app
    #' }
    #' @param use_revision Parameter specifying which app's revision should be
    #' copied. If set to `FALSE` (default), the latest revision of the app will
    #' be copied.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_string assert_logical
    copy = function(project, name = NULL, strategy = "clone", use_revision = FALSE, ...) { # nolint
      if (is_missing(project)) {
        rlang::abort("You need to specify the destination project.")
      }
      # Check (and transform) project argument
      project <- check_and_transform_id(project,
        class_name = "Project",
        field_name = "id"
      )

      # Check name argument
      checkmate::assert_string(name, null.ok = TRUE)

      # Check strategy argument
      check_app_copy_strategy(strategy)

      # Check use_revision argument
      checkmate::assert_logical(use_revision, len = 1)

      # nocov start
      # Create body
      body <- list(
        project = project,
        name = name,
        strategy = strategy
      )

      # Use full app ID (with revision number) or omit revision number (copy
      # the latest version of the app)
      app_id <- ifelse(use_revision,
        paste0(self$id, self$revision, collapse = "/"),
        self$id
      )

      res <- sevenbridges2::api(
        path = paste0("apps/", app_id, "/", "actions/copy"),
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      rlang::inform(glue::glue_col("App {green {self$name}} has been copied to {green {project}} project.")) # nolint

      # Return newly created app
      asApp(res, auth = self$auth)
      # nocov end
    },
    #' @description Get app's revision
    #'
    #' @details
    #' This call allows you to obtain a particular revision of an
    #' app, which is not necessarily the most recent version.
    #'
    #' @param revision Integer denoting the revision of the app.
    #' @param in_place If TRUE, replace current app object with new for
    #' specified app revision.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom checkmate assert_numeric
    get_revision = function(revision = self$revision, in_place = FALSE, ...) {
      # Check if revision is positive number and convert it to integer
      checkmate::assert_numeric(revision, lower = 0, len = 1)
      revision <- as.integer(revision)

      # Check in_place parameter to be logical
      if (is_missing(in_place)) {
        rlang::abort("You need to specify the in_place parameter.")
      } else {
        checkmate::assert_logical(in_place, len = 1, any.missing = FALSE, null.ok = FALSE) # nolint
      }

      # nocov start
      res <- sevenbridges2::api(
        path = paste0(c("apps", self$id, revision), collapse = "/"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      if (in_place) {
        self$initialize(
          href = res$href,
          id = res$id,
          project = res$project,
          name = res$name,
          revision = res$revision,
          raw = res$raw,
          copy_of = ifelse(!is.null(res$raw$`sbg:copyOf`), res$raw$`sbg:copyOf`, NA), # nolint
          latest_revision = ifelse(!is.null(res$raw$`sbg:latestRevision`), res$raw$`sbg:latestRevision`, NA), # nolint
          auth = auth,
          response = attr(res, "response")
        )
      } else {
        # Return new object
        return(asApp(res, self$auth))
      } # nocov end
    },
    #' @description Create a new app revision.
    #'
    #' @details
    #' This call creates a new revision for an existing app. It adds a new CWL
    #' app description, and stores it as the named revision for the specified
    #' app. The revision number must not already exist and should follow the
    #' sequence of previously created revisions.
    #' @param raw A list containing a raw CWL for the app revision you are
    #' about to create. To generate such a list, you might want to load some
    #' existing JSON / YAML file. In case that your CWL file is in JSON format,
    #' please use the `fromJSON` function from the `jsonlite` package to
    #' minimize potential problems with parsing the JSON file. If you want to
    #' load a CWL file in YAML format, it is highly recommended to use the
    #' `read_yaml` function from the `yaml` package. Keep in mind that this
    #' parameter should not be used together with the `file_path` parameter.
    #' @param from_path A path to a file containing the raw CWL for the app
    #' (JSON or YAML). This parameter should not be used together with the
    #' `raw` parameter.
    #' @param raw_format The type of format used (JSON or YAML).
    #' @param in_place If TRUE, replace current app object with
    #' newly created revision.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom rlang abort
    #' @importFrom glue glue_col
    #' @importFrom checkmate assert_list assert_character
    #' @importFrom stringr str_extract
    #' @importFrom tools file_ext
    #' @importFrom jsonlite fromJSON
    #' @importFrom yaml yaml.load
    #' @importFrom readr read_file
    create_revision = function(raw = NULL, from_path = NULL, raw_format = c("JSON", "YAML"), in_place = FALSE, ...) { # nolint
      if (is_missing(raw) && is_missing(from_path)) {
        rlang::abort(glue::glue_col("Both parameters {magenta raw} and {magenta from_path} are missing. Please provide one of them.")) # nolint
      }

      if (!is_missing(raw) && !is_missing(from_path)) {
        rlang::abort(glue::glue_col("Both parameters {magenta raw} and {magenta from_path} are provided. Please use only one of them.")) # nolint
      }

      raw_format <- match.arg(raw_format)

      # Check in_place parameter to be logical
      if (is_missing(in_place)) {
        rlang::abort("You need to specify the in_place parameter.")
      } else {
        checkmate::assert_logical(in_place, len = 1, any.missing = FALSE, null.ok = FALSE) # nolint
      }

      if (!is_missing(raw)) {
        # Check if raw parameter is a list
        checkmate::assert_list(raw)
        raw_cwl <- raw
      }

      if (!is_missing(from_path)) {
        # Check if the provided file path is a string
        checkmate::assert_character(from_path, len = 1)

        # Check if the file with the provided path really exists on local disk
        check_file_path(from_path)

        raw_body <- readr::read_file(file = from_path)

        # Check raw_format and read the file with the appropriate function
        if (raw_format == "JSON") {
          raw_cwl <- jsonlite::fromJSON(raw_body, simplifyDataFrame = FALSE)
        }

        if (raw_format == "YAML") {
          raw_cwl <- yaml::yaml.load(raw_body)
        }
      }

      # nocov start
      res <- sevenbridges2::api(
        path = paste0(c("apps", self$id, self$latest_revision + 1, "raw"), collapse = "/"), # nolint
        method = "POST",
        body = raw_cwl,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      rlang::inform(glue::glue_col("New {green {self$name}} app revision with number {green {self$latest_revision + 1}} has been created.")) # nolint

      # Return new or reload current object with newly created revision
      return(self$get_revision(revision = self$latest_revision + 1, in_place = in_place)) # nolint

      # Update current object's latest_revision
      # self$latest_revision <- res$`sbg:latestRevision`
      #
      # if (in_place) {
      #   # Update the rest of the current object's fields
      #   self$revision <- res$`sbg:latestRevision`
      #   self$raw <- res
      #   return(self)
      # } else {
      #   return(self$get_revision(revision = self$latest_revision, in_place = FALSE)) # nolint
      # }

      # Update app's details since res object doesn't contain all the
      # information
      # self <- self$get_revision(revision = self$revision)
      # ------------------- CHECK THIS ----------------------
      # ALTERNATIVELY we can manually update all fields
      # to avoid making one more API call
      # -----------------------------------------------------

      # Print new object
      # return(self)
    },
    #' @description Synchronize a copied app with its parent app
    #'
    #' @details
    #' This call synchronizes a copied app with the source app from which it
    #' has been copied.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    sync = function(...) {
      res <- sevenbridges2::api(
        path = paste0("apps/", self$id, "/actions/sync"),
        method = "POST",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      rlang::inform(glue::glue_col("App {green {self$name}} has been updated.")) # nolint

      # Reload object
      self$initialize(
        href = res$href,
        id = res$id,
        project = res$project,
        name = res$name,
        revision = res$revision,
        raw = res$raw,
        copy_of = ifelse(!is.null(res$raw$`sbg:copyOf`), res$raw$`sbg:copyOf`, NA), # nolint
        latest_revision = ifelse(!is.null(res$raw$`sbg:latestRevision`), res$raw$`sbg:latestRevision`, NA), # nolint
        auth = auth,
        response = attr(res, "response")
      )
    } # nocov end
  )
)
# nocov start
# Helper function for creating App objects
asApp <- function(x, auth = NULL) {
  App$new(
    href = x$href,
    id = sub("/[^/]*$", "", x$id),
    project = x$project,
    name = x$name,
    revision = x$revision,
    raw = x$raw,
    copy_of = ifelse(!is.null(x$raw$`sbg:copyOf`), x$raw$`sbg:copyOf`, NA),
    latest_revision = ifelse(!is.null(x$raw$`sbg:latestRevision`), x$raw$`sbg:latestRevision`, NA), # nolint
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of App objects
asAppList <- function(x, auth) {
  obj <- lapply(x$items, asApp, auth = auth)
  obj
}
# nocov end
