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
    #' Create a new Auth object. All methods can be accessed through this object.
    #'
    #' @param from Authentication method. Could be \code{"direct"}
    #' (pass the credential information to the arguments directly),
    #' \code{"env"} (read from pre-set system environment variables),
    #' or \code{"file"} (read configurations from a credentials file).
    #' Default is \code{"direct"}.
    #'
    #' @param platform The platform to use.
    #' If \code{platform} and \code{url} are both not specified,
    #' the default is \code{"cgc"} (Cancer Genomics Cloud).
    #' Other possible values include
    #' \code{"aws-us"} (Seven Bridges Platform - US),
    #' \code{"aws-eu"} (Seven Bridges Platform - EU),
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
    initialize = function(from = c("direct", "env", "file"), platform = NULL,
                          url = NULL, token = NULL, sysenv_url = NULL,
                          sysenv_token = NULL, config_file = NULL,
                          profile_name = NULL, fs = NULL,
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
        if (!is.null(platform) & !is.null(url)) {
          rlang::abort("`platform` and `url` cannot be set simultaneously")
        }

        # Case 2: platform and url are both *not* provided
        if (is.null(platform) & is.null(url)) {
          rlang::warn(paste0(
            "`platform` and `url` are not set, will use the default platform: ",
            sbg_default_platform
          ))
          self$platform <- sbg_default_platform
          self$url <- sbg_baseurl[[sbg_default_platform]]
        }

        # Case 3: platform is provided, url is not provided
        if (!is.null(platform) & is.null(url)) {

          # platform name sanity check
          self$platform <- platform
          if (self$platform %in% names(sbg_baseurl)) {
            self$url <- sbg_baseurl[[self$platform]]
          } else {
            rlang::abort("Platform does not exist, please check its spelling (case-sensitive)")
          }
          message("Using platform: ", self$platform)
        }

        # Case 4: platform is not provided, url is provided
        if (is.null(platform) & !is.null(url)) {
          self$url <- normalize_url(url)
          # look up an accurate platform name
          self$platform <- sbg_platform_lookup(self$url)
        }

        if (is.null(token)) {
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
        if (is.null(sysenv_url)) {
          self$sysenv_url <- sbg_default_sysenv_url
        } else {
          self$sysenv_url <- sysenv_url
        }
        if (is.null(sysenv_token)) {
          self$sysenv_token <- sbg_default_sysenv_token
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
        if (is.null(config_file)) {
          self$config_file <- sbg_default_config_file
        } else {
          self$config_file <- config_file
        }
        config_list <- sbg_parse_config(self$config_file)
        rlang::inform(paste0(
          "Authenticating with user configuration file: ",
          self$config_file
        ))

        # locate user profile with url + token
        if (is.null(profile_name)) {
          self$profile_name <- sbg_default_profile_name
        } else {
          self$profile_name <- profile_name
        }
        # extract url + token from profile
        self$url <- normalize_url(config_list[[self$profile_name]][["api_endpoint"]])
        .token <- config_list[[self$profile_name]][["auth_token"]]
        if (is.null(self$url) || is.null(.token)) {
          rlang::abort(
            "`The field api_endpoint` or `auth_token` is missing in profile:",
            self$profile_name
          )
        } else {
          sbg_set_env(url = self$url,
                      token = .token,
                      sysenv_url_name = paste0(self$profile_name, "_url"),
                      sysenv_token_name = paste0(self$profile_name, "_token"))
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
    #' API paths
    #' @description
    #' This method returns all API paths and pass arguments to core `api()`
    #'  function.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `100`.
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
      "This call returns all API paths, and pass arguments to api() function with input token and url automatically"

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

      if (complete) {
        N <- as.numeric(httr::headers(sbg_get_response(req))[["x-total-matching-query"]])
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
      }
    },
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
          base_url = self$url,
        )
        rlang::inform("username not provided, showing the currently authenticated user information")
      } else {
        req <- sevenbridges2::api(
          token = self$get_token(),
          path = paste0("users/", username),
          method = "GET",
          base_url = self$url,
        )
      }

      # Extract parsed contents of a request
      req <- status_check(req)

      # Create User object
      asUser(req, self)
    },
    # rate limit --------------------------------------------------------------
    #' @description Get information about current rate limit
    rate_limit = function() {
      "This call returns information about your current rate limit. This is the number of API calls you can make in one hour."
      req <- sevenbridges2::api(
        path = "rate_limit",
        method = "GET",
        token = self$get_token(),
        base_url = self$url)

      # Extract parsed contents of a request
      req <- status_check(req)

      asRate(req)
    },
    # billing -----------------------------------------------------------------
    #' @description Get list of paths used to access billing information via the API
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
        ...)

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
          ...)

        req <- status_check(req)

        billing_list <- asBillingList(req, self$auth)
        billing_list
      } else {
        req <- sevenbridges2::api(
          path = paste0("billing/groups/", id),
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...)

        req <- status_check(req)

        asBilling(req, auth = self)
      }
    },
    # invoices ---------------------------------------------------------------
    #' @description Get invoice information
    #' @param id Invoice identifier.
    #' @param billing_group_id ID of a particular billing group. If provided,
    #' the method will return the invoice incurred by that billing group only.
    #' @param ... Other arguments passed to methods.
    invoice = function(id = NULL, billing_group_id = NULL, ...) {
      "If no id provided, This call returns a list of invoices, with information about each, including whether or not the invoice is pending and the billing period it covers. The call returns information about all your available invoices, unless you use the query parameter bg_id to specify the ID of a particular billing group, in which case it will return the invoice incurred by that billing group only. If id was provided, This call retrieves information about a selected invoice, including the costs for analysis and storage, and the invoice period."

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
    #' @param id Project's ID.
    #' @param name Project's name.
    #' @param owner The username of the owner whose projects you want to query.
    #' @param ... Other arguments that can be passed to this method.
    #' Such as query parameters.
    project_query = function(id = NULL, name = NULL, owner = NULL, ...) {
      if (is.null(owner)) {
        res <- sevenbridges2::api(
          path = "projects",
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...
        )
      } else {
        res <- sevenbridges2::api(
          path = paste0("projects", "/", owner),
          method = "GET",
          token = self$get_token(),
          base_url = self$url,
          ...
        )
      }

      res <- status_check(res)

      res <- asProjectList(res, auth = self)

      res <- m.match(res, id = id, name = name,
                     exact = FALSE,
                     ignore.case = TRUE
      )

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
    project_get = function(project_owner = suppressMessages(self$user()$username), project = NULL, ...) {
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
    #' @param elastic_disk Set to true to enable Elastic disk.
    #' @param intermediate_files A list defining the retention period for
    #' intermediate files. Expected elements:
    #' \itemize{
    #' \item retention - Specifies that intermediate files should be retained for a
    #  limited amount of time. The value is always LIMITED.
    #' \item duration - Specifies intermediate files retention period in hours.
    #' The minimum value is 1. The maximum value is 120 and the default value
    #' is 24.
    #' }
    #' @param ... Other arguments.
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
                           elastic_disk = FALSE,
                           intermediate_files = list("retention" = "LIMITED", "duration" = 24),
                           ...) {
      "Create new projects, required parameters: name, billing_group_id, optional parameteres: tags, description, type, and settings."

      if (is.null(name)) {
        rlang::abort("You must provide at least a name for the project you want to create.")
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
          "use_memoization" = use_memoization # ,
          # "elastic_disk" = elastic_disk,
          # "intermediate_files" = intermediate_files)
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

      asProject(res, auth = self)
    }
  )
)
