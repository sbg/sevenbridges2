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
    #' @field href API request URL.
    href = NULL,
    #' @field items Items returned in API response.
    items = NULL,
    #' @field links List of links (hrefs) for next page resources.
    links = NULL,
    #' @field response Save raw API response.
    response = NULL,
    #' @field auth Seven Bridges Auth object.
    auth = NULL,

    #' @description Create a new Collection object.
    #' @param href API request URL.
    #' @param items Items returned in API response.
    #' @param links List of links (hrefs) for next page resources.
    #' @param response Raw API response.
    #' @param auth Seven Bridges Auth object.
    initialize = function(href = NA, items = NA, links = NA, response = NA,
                          auth = NA) {
      self$href <- href
      self$items <- items
      self$links <- links
      self$response <- response
      self$auth <- auth
    },
    #' @description Return next page of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
    next_page = function(...) {
      checkmate::assert_list(self$links, null.ok = TRUE)
      if (length(self$links) == 0) {
        rlang::abort("No more entries to be returned.")
      }
      for (link in self$links) {
        if (tolower(link$rel) == "next") {
          res <- sevenbridges2::api(
            url = link$href,
            method = link$method,
            token = self$auth$get_token(),
            base_url = self$auth$url,
            ...
          )
          res <- status_check(res)
          private$load(res, auth = self$auth)
        }
      }
    },
    #' @description Return previous page of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
    prev_page = function(...) {
      checkmate::assert_list(self$links, null.ok = TRUE)
      if (length(self$links) == 0) {
        rlang::abort("No more entries to be returned.") # nolint
      }
      for (link in self$links) {
        if (tolower(link$rel) == "prev") {
          res <- sevenbridges2::api(
            url = link$href,
            method = link$method,
            token = self$auth$get_token(),
            base_url = self$auth$url,
            ...
          )
          res <- status_check(res)
          private$load(res, auth = self$auth)
        }
      }
    }
  ),
  private = list(
    # Reload object to get new results
    load = function(res, auth) {
      self$initialize(
        href = res$href,
        items = res$items,
        links = res$links,
        auth = auth,
        response = attr(res, "response")
      )
      return(self)
    }
  )
)

# nocov start
# Helper function for creating Collection objects
asCollection <- function(x, auth = NULL) {
  Collection$new(
    href = x$href,
    items = x$items,
    links = x$links,
    auth = auth,
    response = attr(x, "response")
  )
}
# nocov end
