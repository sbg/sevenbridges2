# nolint start
#' @title R6 Class Representing Authentication Object
#'
#' @description Authentication object with methods to access API endpoints.
#' Every object could be requested from this Auth object and any action
#' could start from this object using cascading style. Please check
#' \code{vignette("api")} for more information.
#'
#' @importFrom R6 R6Class
#' @importFrom  rlang abort warn inform
#'
#' @details
#' This is the main object for authentication to platforms powered by
#' Seven Bridges.
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
    #' the API base URL.
    sysenv_url = NULL,

    #' @field sysenv_token Name of the system environment variable storing
    #' the auth token.
    sysenv_token = NULL,

    #' @field config_file Location of the user configuration file.
    config_file = NULL,

    #' @field profile_name Profile name in the user configuration file.
    profile_name = NULL,

    #' @field fs FS object, for mount and unmount file system.
    fs = NULL,

    #' @field authorization Logical. Is the \code{token} an API
    #' auth token (\code{FALSE}) or an access token from the
    #' Seven Bridges single sign-on (\code{TRUE})?
    authorization = NULL,

    #' @description
    #' Create a new Auth object. All methods can be accessed through this
    #' object.
    #'
    #' @param from Authentication method. Could be \code{"direct"}
    #' (pass the credential information to the arguments directly),
    #' \code{"env"} (read from pre-set system environment variables),
    #' or \code{"file"} (read configurations from a credentials file).
    #' Default is \code{"direct"}.
    #'
    #' @param platform The platform to use.
    #' If \code{platform} and \code{url} are both not specified,
    #' the default is \code{"aws-us"} (Seven Bridges Platform - US).
    #' Other possible values include
    #' \code{"aws-eu"} (Seven Bridges Platform - EU),
    #' \code{"cgc"} (Cancer Genomics Cloud),
    #' \code{"ali-cn"} (Seven Bridges Platform - China),
    #' \code{"cavatica"} (Cavatica), and
    #' \code{"f4c"} (BioData Catalyst Powered by Seven Bridges).
    #'
    #' @param url Base URL for API. Please only use this when you
    #' want to specify a platform that is not in the \code{platform} list
    #' above, and also leaving \code{platform} unspecified.
    #'
    #' @param token Your authentication token.
    #'
    #' @param sysenv_url Name of the system environment variable storing
    #' the API base URL. By default: \code{"SB_API_ENDPOINT"}.
    #'
    #' @param sysenv_token Name of the system environment variable storing
    #' the auth token. By default: \code{"SB_AUTH_TOKEN"}.
    #'
    #' @param config_file Location of the user configuration file.
    #' By default: \code{"~/.sevenbridges/credentials"}.
    #'
    #' @param profile_name Profile name in the user configuration file.
    #' The default value is \code{"default"}.
    #'
    #' @param fs FS object, for mount and unmount file system.
    #'
    #' @param authorization Logical. Is the \code{token} an API
    #' auth token (\code{FALSE}) or an access token from the
    #' Seven Bridges single sign-on (\code{TRUE})?
    #'
    #' @param ... Other arguments passed to methods.
    #' @return A new `Auth` object.
    initialize = function(from = c("direct", "env", "file"), platform = NA,
                          url = NA, token = NA, sysenv_url = NA,
                          sysenv_token = NA, config_file = NA,
                          profile_name = NA, fs = NA,
                          authorization = FALSE, ...) {
      self$fs <- fs
      self$from <- match.arg(from)
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
            self$sysenv_url, " and ", self$sysenv_token
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
          rlang::abort(
            "`The field api_endpoint` or `auth_token` is missing in profile:",
            self$profile_name
          )
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
    },
    #' @description
    #' Returns the authentication token read from system environment variable.
    #' @return An API authentication token in form of a string.
    get_token = function() {
      if (self$from == "env" || self$from == "file") {
        Sys.getenv(self$sysenv_token)
      } else {
        Sys.getenv("SB_AUTH_TOKEN")
      }
    },
    #' @description
    #' This method returns all API paths and pass arguments to core `api()`
    #'  function.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param fields All API calls take the optional query parameter fields.
    #' This parameter enables you to specify the fields you want to be returned
    #' when listing resources (e.g., all your projects) or getting details of a
    #' specific resource (e.g., a given project).
    #' @param ... Other arguments passed to core api function.
    #' @param complete Parameter for API request that allows you to search and
    #'  list all items. By default, it is set to `FALSE`.
    #' @importFrom httr headers
    api = function(..., limit = getOption("sevenbridges2")$"limit",
                   offset = getOption("sevenbridges2")$"offset",
                   fields = NULL, complete = FALSE) {
      req <- sevenbridges2::api(
        self$get_token(),
        base_url = self$url,
        limit = limit,
        offset = offset,
        fields = fields,
        authorization = self$authorization,
        ...
      )
      req <- status_check(req)

      if (complete) { # nocov start
        N <- as.numeric(httr::headers(sbg_get_response(req))
        [["x-total-matching-query"]])
        if (length(N)) .item <- length(req$items)
        if (.item < N) {
          pb <- txtProgressBar(min = 1, max = N %/% 100 + 1, style = 3)
          res <- NULL

          for (i in 1:(N %/% 100 + 1)) {
            .limit <- 100
            .offset <- (i - 1) * 100
            req <- sevenbridges2::api(
              self$get_token(),
              base_url = self$url,
              limit = .limit, offset = .offset,
              fields = fields, authorization = self$authorization, ...
            )
            req <- status_check(req)
            res$items <- c(res$items, req$items)
            setTxtProgressBar(pb, i)
          }
          cat("\n")
          res$href <- NULL
        } else {
          return(req)
        }
        return(res)
      } else {
        return(req)
      } # nocov end
    },
    # nocov start
    # user ---------------------------------------------------------------------
    #' @description Get details about the authenticated user
    #' @param username The user name of a user for whom you want to get basic
    #' account information.
    user = function(username = NULL) {
      if (is.null(username)) {
        req <- sevenbridges2::api(
          token = self$get_token(),
          path = "user/",
          method = "GET",
          base_url = self$url
        )
        # nolint start
        rlang::inform("username not provided, showing the currently authenticated user information")
        # nolint end
      } else {
        req <- sevenbridges2::api(
          token = self$get_token(),
          path = paste0("users/", username),
          method = "GET",
          base_url = self$url
        )
      }

      # Extract parsed contents of a request
      req <- status_check(req)

      # Create User object
      asUser(req, self)
    },
    # rate limit --------------------------------------------------------------
    #' @description Get information about current rate limit \cr \cr
    #' This call returns information about your current rate limit. This is the
    #' number of API calls you can make in one hour. This call also returns
    #' information about your current instance limit.
    rate_limit = function() {
      req <- sevenbridges2::api(
        path = "rate_limit",
        method = "GET",
        token = self$get_token(),
        base_url = self$url
      )

      # Extract parsed contents of a request
      req <- status_check(req)

      asRate(req)
    },
    # billing -----------------------------------------------------------------
    #' @description Get list of paths used to access billing information via
    #' the API
    #' @param ... Other arguments passed to methods.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li
    billing = function(...) {
      # list billing API paths
      req <- sevenbridges2::api(
        path = "billing",
        method = "GET",
        token = self$get_token(),
        base_url = self$url,
        ...
      )

      req <- status_check(req)

      print_billing_api_paths <- function(x) {
        # x <- as.list(self)
        # x <- purrr::discard(x, .p = is.list)
        x <- purrr::discard(x, .p = is.function)
        x <- purrr::discard(x, .p = is.environment)
        x <- purrr::discard(x, .p = is.null)
        x <- purrr::discard(x, .p = ~ .x == "")
        string <- glue::glue("{names(x)}: {x}")
        names(string) <- rep("*", times = length(string))

        cli::cli_h1("Billing API paths")
        cli::cli_li(string)
      }
      print_billing_api_paths(req)
      invisible(req)
    },
    # billing groups ----------------------------------------------------------
    #' @description Get billing group information
    #' @param id Billing group identifier.
    #' @param ... Other arguments passed to methods.
    billing_groups = function(id = NULL, ...) {
      if (is.null(id)) {
        # list billing API paths
        req <- sevenbridges2::api(
          path = "billing/groups",
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...
        )

        req <- status_check(req)

        billing_list <- asBillingList(req, self)
        billing_list
      } else {
        req <- sevenbridges2::api(
          path = paste0("billing/groups/", id),
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...
        )

        req <- status_check(req)

        asBilling(req, auth = self)
      }
    },
    # invoices ---------------------------------------------------------------
    #' @description Get invoice information \cr \cr
    #' If no id provided, This call returns a list of invoices, with
    #' information about each, including whether or not the invoice is pending
    #' the billing period it covers. The call returns information about all
    #' your available invoices, unless you use the query parameter bg_id to
    #' specify the ID of a particular billing group, in which case it will
    #' return the invoice incurred by that billing group only. If id was
    #' provided, This call retrieves information about a selected invoice,
    #' including the costs for analysis and storage, and the invoice period.
    #'
    #' @param id Invoice identifier.
    #' @param billing_group_id ID of a particular billing group. If provided,
    #' the method will return the invoice incurred by that billing group only.
    #' @param ... Other arguments passed to methods.
    invoice = function(id = NULL, billing_group_id = NULL, ...) {
      if (is.null(id)) {
        if (is.null(billing_group_id)) {
          req <- sevenbridges2::api(
            path = "billing/invoices",
            method = "GET",
            token = self$get_token(),
            base_url = self$url,
            ...
          )
          req <- status_check(req)
          req
        } else {
          req <- sevenbridges2::api(
            path = "billing/invoices",
            method = "GET",
            token = self$get_token(),
            query = list(billing_group = billing_group_id),
            base_url = self$url,
            ...
          )
          req <- status_check(req)
          req
        }
      } else {
        req <- sevenbridges2::api(
          path = paste0("billing/invoices/", id),
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...
        )
        req <- status_check(req)

        asInvoice(req, auth = self)
      }
    },
    # list all projects -------------------------------------------------------
    #' @description A method to list all projects available to particular user.
    #' if the username is not provided, all projects available to the currently
    #' authenticated user will be listed. Otherwise, projects will be listed
    #' for the user whose username is provided. Please keep in mind that this
    #' way you will only be able to list projects you are a member of.
    #' @param name Project's name.
    #' @param owner The username of the owner whose projects you want to query.
    #' @param tags The list of project tags.
    #' @param ... Other arguments that can be passed to this method.
    #' Such as query parameters.
    projects = function(name = NULL, owner = NULL, tags = NULL, ...) {
      check_tags(tags)
      if (is.null(owner)) {
        res <- sevenbridges2::api(
          path = "projects",
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          query = list(name = name, tags = tags),
          ...
        )
      } else {
        res <- sevenbridges2::api(
          path = paste0("projects", "/", owner),
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          query = list(name = name, tags = tags),
          ...
        )
      }

      res <- status_check(res)

      res <- asProjectList(res, auth = self)

      res
    },
    # get specific project ----------------------------------------------------
    #' @description This call creates an object containing the details
    #' of a specified project.
    #' @param project_owner Project owner's Platform username.
    #' @param project The short name of the project you are querying.
    #' @details
    #' Note that project_owner is always case-sensitive, and that project is
    #' not the project's name but its short name. For full details of
    #' identifying objects using the API, please see the API overview.
    #' @param ... Other arguments.
    #' @return Project object.
    project_get = function(project_owner = suppressMessages(
                             self$user()$username
                           ), project, ...) {
      res <- sevenbridges2::api(
        path = paste0("projects/", project_owner, "/", project),
        method = "GET",
        token = self$get_token(),
        base_url = self$url,
        ...
      )

      res <- status_check(res)

      asProject(res, auth = self)
    },
    # create new project
    #' @description A method for creating a new project.
    #'
    #' @param name The name of the project you are creating.
    #' @param billing_group The ID of the billing group for the project.
    #' @param description Description of the project.
    #' @param tags The list of project tags.
    #' @param type Project's type. All projects have type v2.
    #' @param locked (boolean) Set this field to true to lock down a project.
    #'    Locking down a project prevents any Seven Bridges team member from
    #'    viewing any information about the task.
    #' @param controlled Set this field to true to define this project as
    #' controlled i.e. one which will contain controlled data. Set false to
    #' define the project as open i.e. one which will contain open data.
    #' @param location Specify the location for this project:
    #' aws:us-east-1 or aws:us-west-2
    #' @param use_interruptible_instances Defines the use of spot instances.
    #' @param use_memoization Set to false by default. Set to true to enable
    #' memoization.
    #' @param use_elastic_disk Set to true to enable Elastic disk.
    #' @param intermediate_files A list defining the retention period for
    #' intermediate files. Expected elements:
    #' \itemize{
    #' \item retention - Specifies that intermediate files should be retained
    #' for a limited amount of time. The value is always LIMITED.
    #' \item duration - Specifies intermediate files retention period in hours.
    #' The minimum value is 1. The maximum value is 120 and the default value
    #' is 24.
    #' }
    #' @param ... Other arguments.
    #' @importFrom rlang inform
    #' @importFrom glue glue
    project_new = function(name = NULL,
                           billing_group = NULL,
                           description = name,
                           tags = NULL,
                           type = "v2",
                           locked = FALSE,
                           controlled = FALSE,
                           location = NULL,
                           use_interruptible_instances = TRUE,
                           use_memoization = FALSE,
                           use_elastic_disk = FALSE,
                           intermediate_files = list(
                             "retention" = "LIMITED",
                             "duration" = 24
                           ),
                           ...) {
      if (is.null(name)) {
        rlang::abort("You must provide at least a name for the project you want
                     to create.")
      }

      # check tags
      if (!is.null(tags) && is.character(tags)) tags <- as.list(tags)

      body <- list(
        "name" = name,
        "type" = type,
        "description" = description,
        "tags" = tags,
        "billing_group" = billing_group,
        "settings" = list(
          "locked" = locked,
          "controlled" = controlled,
          "location" = location,
          "use_interruptible_instances" = use_interruptible_instances,
          "use_memoization" = use_memoization,
          "use_elastic_disk" = use_elastic_disk,
          "intermediate_files" = intermediate_files
        )
      )

      res <- sevenbridges2::api(
        path = "projects",
        token = self$get_token(),
        body = body,
        method = "POST",
        base_url = self$url,
        ...
      )

      res <- status_check(res)

      rlang::inform(glue::glue("New project has been created on the
                               {self$platform} platform."))
      asProject(res, auth = self)
    },
    # list all files -------------------------------------------------------
    #' @description This call returns a list of files and subdirectories in a
    #' specified project or directory within a project, with specified
    #' properties that you can access. The project or directory whose contents
    #' you want to list is specified as a query parameter in the call. Further
    #'  properties to filter by can also be specified as query parameters.
    #' Note that this call lists both files and subdirectories in the specified
    #' project or directory within a project, but not the contents of the
    #' subdirectories. To list the contents of a subdirectory, make a new call
    #' and specify the subdirectory ID as the parent parameter.
    #' @param project Project object. Project should not be used together
    #' with parent. If parent is used, the call will list the content of the
    #' specified folder, within the project to which the folder belongs.
    #' If project is used, the call will list the content at the root of the
    #' project's files.
    #' @param parent Parent folder object. Should not be used together with
    #' project. If parent is used, the call will list the content of the
    #' specified folder, within the project to which the folder belongs.
    #' If project is used, the call will list the content at the root of the
    #' project's files.
    #' @param name Name of the file. List file with this name. Note that the
    #' name must be an exact complete string for the results to match. Multiple
    #' names can be represented as a vector.
    #' @param metadata List file with this metadata field values. List only
    #' files that have the specified value in metadata field. Different metadata
    #' fields are represented as a named list. You can also define multiple
    #' instances of the same metadata field.
    #' @param origin Task object. List only files produced by task.
    #' @param tag List files containing this tag. Note that the tag must be an
    #'  exact complete string for the results to match. Multiple tags can be
    #'  represented by vector of values.
    #' @param ... Other arguments that can be passed to this method.
    #' Such as query parameters.
    files = function(project = NULL, parent = NULL, name = NULL,
                     metadata = NULL, origin = NULL, tag = NULL, ...) {
      # Check input parameters
      checkmate::assert_r6(project, classes = "Project", null.ok = TRUE)
      checkmate::assert_r6(parent, classes = "File", null.ok = TRUE)
      checkmate::assert_vector(name, null.ok = TRUE)
      checkmate::assert_list(metadata,
        types = "string", names = TRUE,
        null.ok = TRUE
      )
      checkmate::assert_r6(origin, classes = "Task", null.ok = TRUE)
      checkmate::assert_vector(tag, null.ok = TRUE)

      # Run API call based on project/parent parameters
      if (!is.null(project)) {
        res <- sevenbridges2::api(
          path = "files",
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          query = list(
            project = project$id,
            name = name,
            metadata = metadata,
            origin = origin,
            tag = tag
          ),
          ...
        )
      } else if (!is.null(parent)) {
        res <- sevenbridges2::api(
          path = "files",
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          query = list(
            parent = parent$id,
            name = name,
            metadata = metadata,
            origin = origin,
            tag = tag
          ),
          ...
        )
      }

      res <- status_check(res)

      res <- asFileList(res, auth = self)

      res
    },
    # Get single file -------------------------------------------------------
    #' @description This call returns a single file object with
    #' specified id.
    #' @param id File id.
    #' @param ... Other arguments that can be passed to this method.
    get_file = function(id, ...) {
      checkmate::assert_character(id)
      if (is_missing(id)) rlang::abort("File id must be non-empty string.")

      # Run API call based on id parameter
      res <- sevenbridges2::api(
        path = paste0("files/", id),
        method = "GET",
        token = self$get_token(),
        base_url = self$url,
        ...
      )
      res <- status_check(res)
      asFile(res)
    },
    #' @description  Copy file/files to the specified project.
    #'
    #' @param files List of File class objects to copy.
    #' @param destination_project Project object or project ID in form of
    #' <project_owner>/<project-name> where you want to copy files into
    copy_files = function(files, destination_project) {
      checkmate::assert_list(files, types = "File")
      if (is.atomic(destination_project) && !is_missing(destination_project)) {
        checkmate::assert_character(destination_project)
        project_id <- destination_project
      } else {
        checkmate::assert_r6(destination_project, classes = "Project")
        project_id <- destination_project$id
      }

      file_ids <- lapply(files, "[[", "id")

      body <- list(
        "project" = project_id,
        "file_ids" = file_ids
      )

      req <- sevenbridges2::api(
        path = "action/files/copy",
        method = "POST",
        body = body,
        token = self$get_token(),
        base_url = self$url
      )

      res <- status_check(req)
      result <- list()
      for (i in seq_len(length(res))) {
        element <- list(
          "Copied_file_id" = res[[i]]$new_file_id,
          "Copied_file_name" = res[[i]]$new_file_name
        )
        element <- setNames(list(element), names(res[i]))
        result <- append(result, element)
        cat(glue::glue_col("{blue  Original file id: }
                           {names(res[i])}"), "\n")
        cat(glue::glue_col("{blue  Copied file id: }
                           {res[[i]]$new_file_id}"), "\n")
        cat(glue::glue_col("{blue  Copied file name: }
                           {res[[i]]$new_file_name}"), "\n")
        cat("\n")
      }
      result
    },
    # create new folder
    #' @description A method for creating a new folder. It allows you to create
    #' a new folder on the Platform within the root folder of a specified
    #' project or the provided parent folder. Remember that you should provide
    #' either the destination project (as the `project` parameter) or the
    #' destination folder (as the `parent` parameter), not both.
    #'
    #' @param name The name of the folder you are about to create.
    #' @param parent The ID of the parent destination folder or a File object
    #' (which must be of type `FOLDER`).
    #' @param project The ID of the destination project, or a Project object.
    #' @importFrom rlang abort inform
    #' @importFrom glue glue_col
    #' @importFrom checkmate test_r6 test_class
    create_folder = function(name, parent = NULL, project = NULL) {
      check_folder_name(name)

      if (is_missing(parent) && is_missing(project)) {
        # nolint start
        rlang::abort("Both the project name and parent folder ID are missing. You need to provide one of them.")
        # nolint end
      } else if (!is_missing(parent) && !is_missing(project)) {
        # nolint start
        rlang::abort("You should specify a project name or a parent folder ID, not both.")
        # nolint end
      }

      if (!is_missing(parent)) {
        if (checkmate::test_r6(parent, classes = "File")) {
          if (parent$type == "folder") {
            parent <- parent$id
          } else {
            rlang::abort("The provided parent object is not a folder.")
          }
        } else if (checkmate::test_class(parent, classes = "character")) {
          parent <- parent
        } else {
          # nolint start
          rlang::abort("The provided `parent` argument is neither a File object nor an ID of a folder.")
          # nolint end
        }
        body <- list(
          "name" = name,
          "parent" = parent,
          "type" = "FOLDER"
        )
      } else if (!is_missing(project)) {
        if (checkmate::test_r6(project, classes = "Project")) {
          project <- project$id
        } else if (checkmate::test_class(project, classes = "character")) {
          project <- project
        } else {
          # nolint start
          rlang::abort("The provided `project` argument is neither a Project object nor an ID of a project.")
          # nolint end
        }
        body <- list(
          "name" = name,
          "project" = project,
          "type" = "FOLDER"
        )
      }

      res <- sevenbridges2::api(
        path = "files",
        token = self$get_token(),
        body = body,
        method = "POST",
        base_url = self$url
      )

      res <- status_check(res)

      if (attr(res, "response")$status_code == 201) {
        # nolint start
        rlang::inform(glue::glue_col("New folder {green {name}} has been created."))
        # nolint end
        asFile(res, self$auth)
      }
    }
    # nocov end
  )
)
