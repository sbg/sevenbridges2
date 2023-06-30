# nolint start
#' @title R6 Class representing a Collection
#'
#' @description
#' R6 Class representing a resource for managing collections.
#' Wrapper for SevenBridges pageable resources.
#' Among the actual collection items it contains information regarding
#' the total number of entries available in on the server and resource href.
#'
#' @importFrom R6 R6Class
#' @export
Collection <- R6::R6Class(
  # nolint end
  "Collection",
  portable = FALSE,
  public = list(
    #' @field href API request URL
    href = NULL,
    #' @field items Items in response.
    items = NULL,
    #' @field continuation_token Continuation token to be used for pagination.
    continuation_token = NULL,
    #' @field links List of links (hrefs) for next page resources.
    links = NULL,
    #' @field limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    limit = NULL,
    #' @field auth Authentication object
    auth = NULL,
    #' @param href API request URL
    #' @param items List of items returned in API response.
    #' @param continuation_token Continuation token to be used for pagination.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param links List of links (hrefs) for next page resources.
    #' @param auth Seven Bridges Auth object.
    initialize = function(href = NA, items = NA, continuation_token = NA,
                          links = NA, limit = NA, auth = NA) {
      self$href <- href
      self$items <- items
      self$continuation_token <- continuation_token
      self$links <- links
      self$limit <- limit
      self$auth <- auth
    },
    #' @description Return results.
    results = function() {
      return(self$items)
    },
    #' @description Return next page/bulk of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    `next` = function(...) {
      checkmate::assert_list(links, null.ok = TRUE)
      if (length(links) > 0) {
        next_page_link <- links[[1]]
      } else {
        rlang::abort("There are no links for the next page of results to be returned.") # nolint
      }
      res <- sevenbridges2::api(
        url = next_page_link,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )
      res <- status_check(res)

      return(asCollection(res))
    }
  )
)

# nocov start
# Helper function for creating Collection objects
asCollection <- function(x, auth = NULL) {
  Collection$new(
    href = x$href,
    items = x$items,
    continuation_token = x$continuation_token,
    links = x$links,
    limit = x$limit,
    auth = auth
  )
}
# nocov end
