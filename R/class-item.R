#' @title R6 Class Representing an Item
#'
#' @description
#' Base class for describing a set of objects:
#' Project, Task, Pipeline, Files, etc.
#'
#' @details
#' This is a base class for describing a set of objects:
#' User, Project, Task, etc.
#'
#' @field response save the raw response from a request.
#' @field href API href.
#' @field auth Seven Bridges Auth object.
#'
#' @importFrom R6 R6Class
Item <- R6::R6Class(
  "Item",
  portable = FALSE,
  public = list(
    response = NULL,
    auth = NULL,
    href = NULL,
    #' @description
    #' Create a new Item object.
    #' @param href API request URL.
    #' @param response API response.
    #' @param auth Seven Bridges Auth object.
    initialize = function(href = NA, response = NA, auth = NA) {
      self$href <- href
      self$response <- response
      self$auth <- auth
    },
    # nocov start
    #' @description
    #' Reload the Item (resource).
    #' @param cls Item class object.
    #' @param ... Other query parameters.
    #' @importFrom rlang abort
    reload = function(cls, ...) {
      if (is_missing(cls)) {
        rlang::abort("Please provide cls parameter!")
      }
      if (!is_missing(self$href)) {
        reload_url <- self$href
      } else {
        reload_url <- ""
      }
      id <- cls$id
      if (inherits(cls, "App")) {
        revision <- cls$revision
      }
      res <- sevenbridges2::api(
        url = reload_url,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        path = glue::glue(cls$URL[["get"]]),
        ...
      )


      cls$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
    } # nocov end
  )
)
