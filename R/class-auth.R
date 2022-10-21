#' @title R6 Class Representing Authentication Object
#'
#' @description
#' Authentication object with methods to access API endpoints.
#' Every object could be requested from this Auth object and any action
#' could start from this object using cascading style. Please check
#' \code{vignette("api")} for more information.
#'
#' @field from [character] Authentication method. Could be \code{"direct"}
#' (pass the credential information to the arguments directly),
#' \code{"env"} (read from pre-set system environment variables),
#' or \code{"file"} (read configurations from a credentials file).
#' Default is \code{"direct"}.
#' @field platform [character] The platform to use.
#' If \code{platform} and \code{url} are both not specified,
#' the default is \code{"cgc"} (Cancer Genomics Cloud).
#' Other possible values include
#' \code{"aws-us"} (Seven Bridges Platform - US),
#' \code{"aws-eu"} (Seven Bridges Platform - EU),
#' \code{"ali-cn"} (Seven Bridges Platform - China),
#' \code{"cavatica"} (Cavatica), and
#' \code{"f4c"} (BioData Catalyst Powered by Seven Bridges).
#' @field url [character] Base URL for API. Please only use this when you
#' want to specify a platform that is not in the \code{platform} list
#' above, and also leaving \code{platform} unspecified.
#' @field sysenv_url Name of the system environment variable storing
#' the API base URL. By default: \code{"SB_API_ENDPOINT"}.
#' @field sysenv_token Name of the system environment variable storing
#' the auth token. By default: \code{"SB_AUTH_TOKEN"}.
#' @field config_file [character] Location of the user configuration file.
#' By default: \code{"~/.sevenbridges/credentials"}.
#' @field profile_name [character] Profile name in the user configuration file.
#' The default value is \code{"default"}.
#' @field fs FS object, for mount and unmount file system.
#' @field authorization Logical. Is the \code{token} an API
#' auth token (\code{FALSE}) or an access token from the
#' Seven Bridges single sign-on (\code{TRUE})?
#'
#' @importFrom R6 R6Class
#' @import cli
#'
#' @details
#' This is the main object for authentication to platforms powered by
#' Seven Bridges.
#'
#' @export
Auth <- R6::R6Class(
  "Auth",
  public = list(
    from = NULL,
    platform = NULL,
    url = NULL,
    sysenv_url = NULL,
    sysenv_token = NULL,
    config_file = NULL,
    profile_name = NULL,
    fs = NULL,
    authorization = NULL,
    #' @description
    #' Create a new Auth object.
    #' All methods can be accessed through this object.
    #' @return A new `Auth` object.
    initialize = function(from = c("direct", "env", "file"), platform = NULL, url = NULL,
                          token = NULL, sysenv_url = NULL, sysenv_token = NULL,
                          config_file = NULL, profile_name = NULL, fs = NULL,
                          authorization = FALSE, ...) {

      self$fs <- fs
      self$from <- match.arg(from)
      self$authorization <- authorization

      if (from == "direct") {
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
          rlang::warn(paste0("`platform` and `url` are not set, will use the default platform: ",
                  .sbg_default_platform
          ))
          self$platform <-  .sbg_default_platform
          self$url <- .sbg_baseurl[[.sbg_default_platform]]
        }

        # Case 3: platform is provided, url is not provided
        if (!is.null(platform) & is.null(url)) {

          # platform name sanity check
          self$platform <- platform
          if (self$platform %in% names(.sbg_baseurl)) {
            self$url <- .sbg_baseurl[[self$platform]]
          } else {
            rlang::abort("Platform does not exist, please check its spelling (case-sensitive)")
          }
          message("Using platform: ", self$platform)
        }

        # Case 4: platform is not provided, url is provided
        if (is.null(platform) & !is.null(url)) {
          self$url <- normalize_url(url)
          # lookup an accurate platform name
          self$platform <- sbg_platform_lookup(self$url)
        }

        if (is.null(token)) {
          rlang::abort('`token` must be set when `from = "direct"`')
        } else {
          Sys.setenv(.sbg_default_sysenv_token = token)
        }

      }

      if (from == "env") {

        # In this case, `config_file` and `profile_name`
        # should be `NULL` even if they
        # are assigned values
        self$config_file <- NULL
        self$profile_name <- NULL

        # get system environment variables
        if (is.null(sysenv_url)) {
          self$sysenv_url <- .sbg_default_sysenv_url
        } else {
          self$sysenv_url <- sysenv_url
        }
        if (is.null(sysenv_token)) {
          self$sysenv_token <- .sbg_default_sysenv_token
        } else {
          self$sysenv_token <- sysenv_token
        }
        rlang::inform(
          paste0("Authenticating with system environment variables: ",
          self$sysenv_url, " and ", self$sysenv_token)
        )

        # extract url
        self$url <- normalize_url(sbg_get_env(self$sysenv_url))

        # lookup an accurate platform name instead of simply `NULL`
        self$platform <- sbg_platform_lookup(self$url)
      }


      if (from == "file") {

        # In this case, `sysenv_url`, `sysenv_token`,
        # should be `NULL` even if they
        # are assigned values
        self$sysenv_url <- NULL
        self$sysenv_token <- NULL

        # parse user config file
        if (is.null(config_file)) {
          self$config_file <- .sbg_default_config_file
        } else {
          self$config_file <- config_file
        }
        config_list <- sbg_parse_config(self$config_file)
        rlang::inform(paste0("Authenticating with user configuration file: ", self$config_file))

        # locate user profile with url + token
        if (is.null(profile_name)) {
          self$profile_name <- .sbg_default_profile_name
        } else {
          self$profile_name <- profile_name
        }
        # extract url + token from profile
        self$url <- normalize_url(config_list[[self$profile_name]][["api_endpoint"]])
        .token <- config_list[[self$profile_name]][["auth_token"]]
        if (is.null(self$url) | is.null(.token)) {
          rlang::abort("`The field api_endpoint` or `auth_token` is missing in profile:",
               self$profile_name
          )
        } else {
          Sys.setenv("SB_AUTH_TOKEN" = .token)
        }
        rlang::inform(paste0("Authenticating with user profile: ", self$profile_name))

        # lookup an accurate platform name instead of simply `NULL`
        self$platform <- sbg_platform_lookup(self$url)
      }
    },
    get_token = function() {
      Sys.getenv("SB_AUTH_TOKEN")
    },
    # user ---------------------------------------------------------------------
    #' @description Get details about the authenticated user
    user = function(username = NULL) {
      if (is.null(username)) {
        req <- api(
          token = self$get_token(),
          path = "user/",
          method = "GET"
        )
        rlang::inform("username not provided, showing the currently authenticated user information")
      } else {
        req <- api(
          token = self$get_token(),
          path = paste0("users/", username),
          method = "GET"
        )
      }

      req <- status_check(req)

      asUser(req)
    }
  )
)
