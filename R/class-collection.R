# nolint start
#' @title R6 Class representing a Collection
#'
#' @description
#' R6 Class representing a resource for managing collections.
#' Wrapper for SevenBridges pageable resources.
#' Among the actual collection items it contains information regarding
#' the total number of entries available in on the server and resource href.
#'
#' @field href API request URL.
#' @field auth Seven Bridges Auth object.
#' @field response Save raw API response.
#' @field continuation_token Continuation token to be used for pagination.
#' @field links List of links (hrefs) for next page resources.
#' @field limit Defines the number of items you want to get from your API
#' request. By default, `limit` is set to `50`. Maximum is `100`.
#'
#' @importFrom R6 R6Class
#' @export
Collection <- R6::R6Class(
  # nolint end
  "Collection",
  portable = FALSE,
  public = list(
    href = NULL,
    auth = NULL,
    response = NULL,
    continuation_token = NULL,
    links = NULL,
    limit = NULL,
    #' @description Create a new Collection object.
    #' @param href API request URL.
    #' @param auth Seven Bridges Auth object.
    #' @param response Raw API response.
    #' @param continuation_token Continuation token to be used for pagination.
    #' @param links List of links (hrefs) for next page resources.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    initialize = function(href = NA, auth = NA, response = NA,
                          continuation_token = NA, links = NA, limit = NA) {
      self$href <- href
      self$auth <- auth
      self$response <- response
      self$continuation_token <- continuation_token
      self$links <- links
      self$limit <- limit
    }
  )
)

# nocov start
# Helper function for creating Collection objects
asCollection <- function(x, auth = NULL) {
  Collection$new(
    href = x$href,
    continuation_token = x$continuation_token,
    links = x$links,
    limit = x$limit,
    auth = auth,
    response = attr(x, "response")
  )
}
# nocov end
