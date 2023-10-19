#' @title R6 Class Representing an Item
#'
#' @description
#' Base class for describing objects:
#' Project, Task, App, File, etc.
#'
#' @field response Raw response from the request.
#' @field href Item's API request URL.
#' @field auth Seven Bridges Authentication object.
#'
#' @importFrom R6 R6Class
Item <- R6::R6Class(
  "Item",
  portable = FALSE,
  public = list(
    response = NULL,
    auth = NULL,
    href = NULL,
    #' @description Create a new Item object.
    #' @param href Item's API request URL.
    #' @param response Raw API response.
    #' @param auth Seven Bridges Authentication object.
    initialize = function(href = NA, response = NA, auth = NA) {
      self$href <- href
      self$response <- response
      self$auth <- auth
    },
    # nocov start
    #' @description Reload the Item (resource).
    #' @param cls Item class object.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'limit', 'offset', 'fields', etc.
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
