#' @title R6 Class Representing a Resource
#'
#' @description
#' Base class for describing a resource.
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

      if (is_missing(args$path)) {
        rlang::abort("Please provide path parameter!")
      }
      path <- args$path

      # Remove path, they are not needed for query parameters
      args[["path"]] <- NULL

      # Remove advance_access, they are not needed for query parameters
      adv_access <- extract_common_query_params(args, "advance_access")
      limit <- extract_common_query_params(args, "limit")
      offset <- extract_common_query_params(args, "offset")
      fields <- extract_common_query_params(args, "fields")
      args[c("advance_access", "limit", "offset", "fields")] <- NULL

      # nocov start
      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        query = args,
        advance_access = adv_access,
        limit = limit,
        offset = offset,
        fields = fields
      )

      res <- status_check(res)

      return(res)
      # nocov end
    },

    #' @description
    #' Generic query implementation that fetches the resource from the server.
    #' @param cls Resource Class object.
    #' @param id Object id
    #' @param ... Additional parameters for api(), like fields,
    #' advance_access etc.
    #'
    #' @importFrom rlang abort
    #' @importFrom glue glue
    get = function(cls, id, ...) {
      if (is_missing(cls)) {
        rlang::abort("Please provide cls parameter!")
      }
      if (is_missing(id)) {
        rlang::abort("Please provide id parameter!")
      }

      if (is.null(cls[["auth"]])) {
        rlang::abort("Your cls parameter doesn't have field auth!")
      }
      auth <- cls$auth

      if (is.null(cls$URL[["get"]])) {
        rlang::abort("Unable to retrieve resource!")
      }

      url <- cls$URL$get
      path <- glue::glue(url)

      # nocov start
      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = auth$get_token(),
        base_url = auth$url,
        ...
      )

      res <- status_check(res)

      return(res)
      # nocov end
    },

    #' @description
    #' Generic query implementation that deletes the resource from the server.
    #' @param cls Resource Class object.
    #' @param id Object id
    #' @param ... Additional parameters for api(), like fields,
    #' advance_access etc.
    #'
    #' @importFrom rlang abort
    #' @importFrom glue glue
    delete = function(cls, id, ...) {
      if (is_missing(cls)) {
        rlang::abort("Please provide cls parameter!")
      }
      if (is_missing(id)) {
        rlang::abort("Please provide id parameter!")
      }

      if (is.null(cls[["auth"]])) {
        rlang::abort("Your cls parameter doesn't have field auth!")
      }

      if (is.null(cls$url[["delete"]])) {
        rlang::abort("Resource can not be deleted!")
      }

      url <- cls$url[["delete"]]
      path <- glue::glue("{url}/{id}")

      # nocov start
      res <- sevenbridges2::api(
        path = path,
        method = "DELETE",
        token = auth$get_token(),
        base_url = url,
        ...
      )

      res <- status_check(res)

      return(res)
      # nocov end
    }
  )
)
