# nolint start
#' @title R6 Class Representing Authentication Object
#'
#' @description Authentication object with methods to access API endpoints.
#'  Every object could be requested from this Auth object and any action
#'  could start from this object using cascading style. Please check
#'  `vignette("api")` for more information.
#'
#' @importFrom R6 R6Class
#' @importFrom  rlang abort warn inform
#'
#' @details
#'  This is the main object for authentication to platforms powered by
#'  Seven Bridges.
#'
#' @export
Auth <- R6::R6Class(
  # nolint end
  "Auth",
  public = list(
    #' @field from Authentication method.
    from = NULL,

    #' @field platform The platform to use.
    platform = NULL,

    #' @field url Base URL for API.
    url = NULL,

    #' @field sysenv_url Name of the system environment variable storing
    #'  the API base URL.
    sysenv_url = NULL,

    #' @field sysenv_token Name of the system environment variable storing
    #'  the auth token.
    sysenv_token = NULL,

    #' @field config_file Location of the user configuration file.
    config_file = NULL,

    #' @field profile_name Profile name in the user configuration file.
    profile_name = NULL,

    #' @field fs FS (FileSystem) object, for mount and unmount file system.
    fs = NULL,

    #' @field authorization Logical. Is the `token` an API
    #'  authentication token (`FALSE`) or an access token from the
    #'  Seven Bridges single sign-on (`TRUE`)?
    authorization = NULL,

    #' @field projects Projects object, for accessing projects resources on the
    #'  platform.
    projects = NULL,

    #' @field files Files object, for accessing files resources on the
    #'  platform.
    files = NULL,

    #' @field apps Apps object, for accessing apps resources on the platform.
    apps = NULL,

    #' @field volumes Volumes object, for accessing volumes resources on the
    #'  platform.
    volumes = NULL,

    #' @field tasks Tasks object, for accessing volumes resources on the
    #'  platform.
    tasks = NULL,

    #' @field imports Storage imports object, for accessing volume imports
    #'  resources on the platform.
    imports = NULL,

    #' @field exports Storage exports object, for accessing volume exports
    #'  resources on the platform.
    exports = NULL,

    #' @field invoices Invoices object, for accessing invoice resources on the
    #'  platform.
    invoices = NULL,

    #' @field billing_groups Billing_groups object, for accessing billing groups
    #'  resources on the platform.
    billing_groups = NULL,

    #' @description
    #'  Create a new Seven Bridges API Authentication object.
    #'  All methods can be accessed through this object.
    #'
    #' @param from Authentication method. Could be:
    #'  \itemize{
    #'    \item `"direct"` - pass the credential information to the arguments
    #'      directly,
    #'    \item `"env"` - read from pre-set system environment variables, or
    #'    \item `"file"` - read configurations from a credentials file.
    #'    }
    #'  Default is `"direct"`.
    #'
    #' @param platform The platform to use.
    #'  If `platform` and `url` are both not specified,
    #'  the default is `"aws-us"` (Seven Bridges Platform - US).
    #'  Other possible values include:
    #'  \itemize{
    #'    \item `"aws-eu"` - Seven Bridges Platform - EU,
    #'    \item`"cgc"` - Cancer Genomics Cloud,
    #'    \item`"ali-cn"` - Seven Bridges Platform - China,
    #'    \item`"cavatica"` - Cavatica, and
    #'    \item`"f4c"` - BioData Catalyst Powered by Seven Bridges.
    #'  }
    #' @param url Base URL for API. Please only use this when you
    #'  want to specify a platform that is not in the `platform` list
    #'  above, and also leaving `platform` unspecified.
    #'
    #' @param token Your authentication token.
    #'
    #' @param sysenv_url Name of the system environment variable storing
    #'  the API base URL. By default: `"SB_API_ENDPOINT"`.
    #'
    #' @param sysenv_token Name of the system environment variable storing
    #'  the auth token. By default: `"SB_AUTH_TOKEN"`.
    #'
    #' @param config_file Location of the user configuration file.
    #'  By default: `"~/.sevenbridges/credentials"`.
    #'
    #' @param profile_name Profile name in the user configuration file.
    #'  The default value is `"default"`.
    #'
    #' @param fs FS (FileSystem) object, for mount and unmount file system.
    #'
    #' @param authorization Logical. Is the `token` an API
    #'  authentication token (`FALSE`) or an access token from the
    #'  Seven Bridges single sign-on (`TRUE`)?
    #'
    #' @param ... Other arguments passed to methods.
    #' @return `Auth` class object.
    initialize = function(from = c("direct", "env", "file"),
                          platform = NA,
                          url = NA,
                          token = NA,
                          sysenv_url = NA,
                          sysenv_token = NA,
                          config_file = NA,
                          profile_name = NA,
                          fs = NA,
                          authorization = FALSE,
                          ...) {
      self$from <- match.arg(from)
      self$fs <- fs
      self$authorization <- authorization

      # There are three options for authentication
      #   - direct
      #   - env
      #   - file

      if (self$from == "direct") {
        # In this case, `sysenv_url`, `sysenv_token`,
        # `config_file`, and `profile_name`
        # should all be `NULL` even if they
        # are assigned values
        self$sysenv_url <- NULL
        self$sysenv_token <- NULL
        self$config_file <- NULL
        self$profile_name <- NULL

        # Four cases depending on `platform` and `url`

        # Case 1: platform and url are both provided
        if (!is_missing(platform) & !is_missing(url)) {
          rlang::abort("`platform` and `url` cannot be set simultaneously")
        }

        # Case 2: platform and url are both *not* provided
        if (is_missing(platform) & is_missing(url)) {
          # nolint start
          rlang::abort("`platform` and `url` are not set, please, set one of them.")
          # nolint end
        }

        # Case 3: platform is provided, url is not provided
        if (!is_missing(platform) & is_missing(url)) {
          # platform name sanity check
          self$platform <- platform
          if (self$platform %in% names(sbg_baseurl)) {
            self$url <- sbg_baseurl[[self$platform]]
          } else {
            # nolint start
            rlang::abort("Platform does not exist, please check its spelling (case-sensitive)")
            # nolint end
          }
          message("Using platform: ", self$platform)
        }

        # Case 4: platform is not provided, url is provided
        if (is_missing(platform) & !is_missing(url)) {
          self$url <- normalize_url(url) # nocov
          # look up an accurate platform name
          self$platform <- sbg_platform_lookup(self$url) # nocov
        }

        if (is_missing(token)) {
          rlang::abort('`token` must be set when `from = "direct"`')
        } else {
          sbg_set_env(url = self$url, token = token)
        }
      }

      if (self$from == "env") {
        # In this case, `config_file` and `profile_name`
        # should be `NULL` even if they
        # are assigned values
        self$config_file <- NULL
        self$profile_name <- NULL

        # get system environment variables
        if (is_missing(sysenv_url)) {
          self$sysenv_url <- sbg_default_sysenv_url # nocov
        } else {
          self$sysenv_url <- sysenv_url
        }
        if (is_missing(sysenv_token)) {
          self$sysenv_token <- sbg_default_sysenv_token # nocov
        } else {
          self$sysenv_token <- sysenv_token
        }
        rlang::inform(
          paste0(
            "Authenticating with system environment variables: ",
            self$sysenv_url,
            " and ",
            self$sysenv_token
          )
        )

        # extract url and normalize it if necessary
        self$url <- normalize_url(sbg_get_env(self$sysenv_url))

        .sysenv_url_name <- self$sysenv_url

        Sys.setenv(.sysenv_url_name = self$url)

        # lookup an accurate platform name instead of simply `NULL`
        self$platform <- sbg_platform_lookup(self$url)
      }

      if (self$from == "file") {
        # In this case, `sysenv_url`, `sysenv_token`,
        # should be `NULL` even if they
        # are assigned values
        self$sysenv_url <- NULL
        self$sysenv_token <- NULL

        # parse user config file
        if (is_missing(config_file)) {
          self$config_file <- sbg_default_config_file # nocov
        } else {
          self$config_file <- config_file
        }
        config_list <- sbg_parse_config(self$config_file)
        rlang::inform(paste0(
          "Authenticating with user configuration file: ",
          self$config_file
        ))

        # locate user profile with url + token
        if (is_missing(profile_name)) {
          self$profile_name <- sbg_default_profile_name # nocov
        } else {
          self$profile_name <- profile_name
        }
        # extract url + token from profile
        self$url <-
          normalize_url(config_list[[self$profile_name]][["api_endpoint"]])
        .token <- config_list[[self$profile_name]][["auth_token"]]
        if (is_missing(self$url) || is_missing(.token)) {
          # nocov start
          rlang::abort(
            "`The field api_endpoint` or `auth_token` is missing in profile:",
            self$profile_name
          )
          # nocov end
        } else {
          sbg_set_env(
            url = self$url,
            token = .token,
            sysenv_url_name = paste0(self$profile_name, "_url"),
            sysenv_token_name = paste0(self$profile_name, "_token")
          )
        }
        rlang::inform(paste0(
          "Authenticating with user profile: ",
          self$profile_name
        ))

        self$sysenv_token <- paste0(self$profile_name, "_token")

        # look up an accurate platform name instead of simply `NULL`
        self$platform <- sbg_platform_lookup(self$url)
      }
      # Projects resource
      self$projects <- Projects$new(self)

      # Files resource
      self$files <- Files$new(self)

      # Apps resource
      self$apps <- Apps$new(self)

      # Volumes resource
      self$volumes <- Volumes$new(self)

      # Tasks resource
      self$tasks <- Tasks$new(self)

      # Imports resource
      self$imports <- Imports$new(self)

      # Exports resource
      self$exports <- Exports$new(self)

      # Invoices resource
      self$invoices <- Invoices$new(self)

      # Billng_groups resousrce
      self$billing_groups <- Billing_groups$new(self)
    },
    #' @description
    #'  Returns the authentication token read from system environment variable.
    #' @return An API authentication token in form of a string.
    get_token = function() {
      if (self$from == "env" || self$from == "file") {
        Sys.getenv(self$sysenv_token)
      } else {
        Sys.getenv("SB_AUTH_TOKEN")
      }
    },
    #' @description
    #'  This method returns all API paths and pass arguments to core `api()`
    #'  function.
    #' @param limit The maximum number of collection items to return for a
    #'  single request. Minimum value is `1`. The maximum value is `100` and the
    #'  default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection of
    #'  the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param fields Selector specifying a subset of fields to include in the
    #'  response when listing resources (e.g., all your projects) or getting
    #'  details of a specific resource (e.g., a given project).
    #' @param ... Other arguments passed to core `api()` function.
    api = function(...,
                   limit = getOption("sevenbridges2")$"limit",
                   offset = getOption("sevenbridges2")$"offset",
                   fields = NULL) {
      # nocov start
      res <- sevenbridges2::api(
        self$get_token(),
        base_url = self$url,
        limit = limit,
        offset = offset,
        fields = fields,
        authorization = self$authorization,
        ...
      )
      return(res)
    },
    #' @description Get details about the authenticated user
    #' @param username The username of a user for whom you want to get basic
    #'  account information. If not provided, information about the currently
    #'  authenticated user will be returned.
    #' @return `User` class object
    user = function(username = NULL) {
      if (is.null(username)) {
        res <- sevenbridges2::api(
          token = self$get_token(),
          path = "user/",
          method = "GET",
          base_url = self$url
        )
        # nolint start
        rlang::inform("Username not provided, showing the currently authenticated user information.")
        # nolint end
      } else {
        res <- sevenbridges2::api(
          token = self$get_token(),
          path = paste0("users/", username),
          method = "GET",
          base_url = self$url
        )
      }

      # Create User object
      return(asUser(res, auth = self))
    },
    #' @description Get information about current rate limit \cr \cr
    #'  This call returns information about your current rate limit. This is the
    #'  number of API calls you can make in one hour. This call also returns
    #'  information about your current instance limit.
    rate_limit = function() {
      res <- sevenbridges2::api(
        path = "rate_limit",
        method = "GET",
        token = self$get_token(),
        base_url = self$url
      )

      return(asRate(res, auth = self))
    }, # nocov end
    #' @description This method allows you to upload a single file from your
    #'  local computer to the Platform.
    #'
    #' @param path File path on local disk.
    #' @param project `Project` object or its ID. Project should not be used
    #'  together with parent. If parent is used, the call will upload the file
    #'  to the specified Platform folder, within the project to which the
    #'  folder belongs. If project is used, the call will upload the file to
    #'  the root of the project's files.
    #' @param parent Parent folder object (of `File` class) or its ID.
    #'  Should not be used together with project. If parent is used, the call
    #'  will upload the file to the
    #'  specified Platform folder, within the project to which the folder
    #'  belongs. If project is used, the call will upload the file to the root
    #'  of the project's files.
    #' @param filename Optional new file name. By default the uploaded file will
    #'  have the same name as the original file provided with the `path`
    #'  parameter. If its name will not change, omit this key.
    #' @param overwrite In case there is already a file with the same name in
    #'  the selected platform project/folder, this option allows you to control
    #'  whether that file will be overwritten or not. If overwrite is set to
    #'  `TRUE` and a file already exists under the name specified in the
    #'  request, the existing file will be deleted and a new one created in its
    #'  place.
    #' @param part_size The preferred size for upload parts in bytes. If omitted
    #'  or set to a value that is incompatible with the cloud storage provider,
    #'  a default value will be used.
    #' @param init If `TRUE`, the method will initialize and return the Upload
    #'  object and stop. If `FALSE`, the method will return the Upload object
    #'  and start the upload process immediately.
    #' @importFrom checkmate test_r6 test_class assert_logical
    #' @importFrom rlang abort
    upload = function(path,
                      project = NULL,
                      parent = NULL,
                      filename = NULL,
                      overwrite = FALSE,
                      part_size = getOption("sevenbridges2")$RECOMMENDED_PART_SIZE, # nolint
                      init = FALSE) {
      # Check if the provided path is valid, i.e. if the file exists
      # and get its size.
      if (file.exists(path)) {
        file_size <- file.size(path)
      } else {
        rlang::abort("There is no file at the specified path.")
      }

      # Check project and parent parameters
      if (is_missing(parent) && is_missing(project)) {
        rlang::abort("Both the project name and parent folder ID are missing. You need to provide one of them.") # nolint
      } else if (!is_missing(parent) && !is_missing(project)) {
        rlang::abort("You should specify a project or a parent folder, not both.") # nolint
      }

      if (!is_missing(parent)) {
        if (inherits(parent, "File") && parent$type != "folder") {
          rlang::abort("The provided parent object is not a folder.")
        }
        parent <- check_and_transform_id(parent, "File")
      } else if (!is_missing(project)) {
        project <- check_and_transform_id(project, "Project")
      }

      # Check filename
      if (is_missing(filename)) {
        filename <- basename(path)
      }

      if (grepl("\\s", filename) || grepl("\\/", filename)) {
        rlang::abort("The file name cannot contain spaces or backslashes.") # nolint
      }

      # Check init param
      checkmate::assert_logical(init)

      # Check overwrite param
      checkmate::assert_logical(overwrite)

      # nocov start
      # Check size and part_size params
      check_upload_params(size = file_size, part_size = part_size)

      # Create Upload object
      u <- Upload$new(
        path = path,
        project = project,
        parent = parent,
        filename = filename,
        overwrite = overwrite,
        file_size = file_size,
        part_size = part_size,
        initialized = FALSE,
        auth = self
      )

      if (init) {
        # Initialize multipart upload only
        u$init()
      } else {
        # Initialize and start multipart upload
        u$init()$start()
      }
    },
    #' @description This method returns the list of all ongoing uploads.
    #' @importFrom cli cli_h1 cli_li cli_end
    #' @importFrom glue glue
    list_ongoing_uploads = function() {
      # Run API call based on id parameter
      res <- sevenbridges2::api(
        path = "upload/multipart",
        method = "GET",
        token = self$get_token(),
        base_url = self$url
      )

      # Print information about ongoing uploads
      cli::cli_h1("Ongoing uploads")
      for (item in res$items) {
        fields_to_show <- c(
          "href",
          "project",
          "parent",
          "name",
          "initiated",
          "upload_id"
        )
        string <-
          glue::glue("{fields_to_show}: {item[fields_to_show]}")

        cli::cli_h1("Upload info")
        cli::cli_li(string)
      }
      # Close container elements
      cli::cli_end()

      invisible(res)
    },
    #' @description This call aborts an ongoing multipart upload.
    #' @param upload_id Upload object or ID of the upload process that you want
    #'  to abort.
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_character
    #' @importFrom glue glue_col
    upload_abort = function(upload_id) {
      upload_id <- check_and_transform_id(upload_id, "Upload")

      res <- sevenbridges2::api(
        path = glue::glue("upload/multipart/{upload_id}"),
        method = "DELETE",
        token = self$get_token(),
        base_url = self$url
      )

      rlang::inform(
        glue::glue_col(
          "The upload process with the following ID {green {upload_id}} has been aborted." # nolint
        )
      )
    }, # nocov end
    #' @description Send feedback to Seven Bridges. \cr \cr
    #'  Send feedback on ideas, thoughts, and problems via the sevenbridges2 API
    #'  package with three available types: `idea`, `thought`, and `problem`.
    #'  You can send one feedback item per minute.
    #' @param text Specifies the content for the feedback i.e. feedback text.
    #' @param type Specifies the type of feedback. The following are available:
    #'  `idea`, `thought` and `problem`.
    #' @param referrer The name of the person submitting the feedback.
    #' @param ... Additional query parameters if applicable.
    #' @importFrom rlang inform
    #' @importFrom checkmate assert_string
    send_feedback = function(text,
                             type = c("idea", "thought", "problem"),
                             referrer = NULL,
                             ...) {
      checkmate::assert_string(text, null.ok = FALSE)
      type <- match.arg(type)
      checkmate::assert_string(referrer, null.ok = TRUE)
      # nocov start
      if (is.null(referrer)) {
        referrer <- suppressMessages(self$user()[["username"]])
      }

      body <- list(
        text = text,
        type = type,
        referrer = referrer
      )

      res <- sevenbridges2::api(
        path = "action/notifications/feedback",
        method = "POST",
        body = body,
        token = self$get_token(),
        base_url = self$url,
        ...
      )

      rlang::inform(
        "Thank you for your feedback!"
      )
    } # nocov end
  )
)
