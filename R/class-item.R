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
#' @field href API href
#' @importFrom R6 R6Class
#' @export
Item <- R6::R6Class(
  "Item",
  portable = FALSE,
  public = list(
    response = NULL,
    href = NULL,
    initialize = function(href = NULL, response = NULL) {
      self$href <- href
      self$response <- response
    },
    get_token = function() {
      Sys.getenv("SB_AUTH_TOKEN")
    }
  )
)
