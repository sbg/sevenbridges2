#' @title R6 Class Representing a Resource
#'
#' @description
#' Base class for describing a resource on the platform:
#' Projects, Tasks, Pipelines, Filess, Apps etc.
#'
#' @details
#' This is a base class for describing a resource on the platform:
#' Projects, Tasks, Pipelines, Filess, Apps etc.
#'
#' @importFrom R6 R6Class
Resource <- R6::R6Class(
  "Resource",
  portable = FALSE,
  public = list(
    #' @field auth Seven Bridges Auth object
    auth = NULL,

    #' @description
    #' Create a new Resource object.
    #' @param auth Seven Bridges Auth object.
    initialize = function(auth = NA) {
      self$auth <- auth
    },

    #' @description
    #' Generic query implementation that is used by the resources.
    #' @param ... Query parameters
    query = function(...) {
      args <- list(...)

      # Get auth, url and path from args and remove them from query parameters
      auth <- args$auth
      path <- args$path
      args[["auth"]] <- NULL
      args[["path"]] <- NULL

      # Check for valid limit value
      if (!args$limit | args$limit <= 0) {
        args$limit <- getOption("sevenbridges2")$limit
      }


      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = auth$get_token(),
        base_url = auth$url,
        query = args
      )

      res <- status_check(res)

      return(res)
    },

    #' @description
    #' Generic query implementation that fetches the resource from the server.
    #' @param cls Resource Class object.
    #' @param id Object id
    #' @importFrom rlang abort
    #' @importFrom glue glue
    get = function(cls, id) {
      auth <- cls$auth
      if (is.null(cls$URL[["get"]])) {
        rlang::abort("Unable to retrieve resource!")
      }

      url <- cls$URL$get
      path <- glue::glue(url)

      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = auth$get_token(),
        base_url = auth$url
      )

      res <- status_check(res)

      res
    },

    #' @description
    #' Generic query implementation that deletes the resource from the server.
    #' @param cls Resource Class object.
    #' @param id Object id
    #' @importFrom rlang abort
    #' @importFrom glue glue
    delete = function(cls, id) {
      auth <- cls$auth
      if (!cls$url[["delete"]]) {
        rlang::abort("Resource can not be deleted!")
      }

      url <- cls$url[["delete"]]
      path <- glue::glue("{url}/{id}")

      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = auth$get_token(),
        base_url = url
      )

      res <- status_check(res)

      res
    }
  ),
  private = list()
)
