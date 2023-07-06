# nolint start
#' @title R6 Class representing a VolumeFileCollection
#'
#' @description
#' R6 Class representing a resource for managing volume files collections.
#'
#' @importFrom R6 R6Class
#' @export
VolumeFileCollection <- R6::R6Class(
  # nolint end
  "VolumeFileCollection",
  inherit = Collection,
  portable = FALSE,
  public = list(
    #' @description Create new VolumeFileCollection object.
    #' @importFrom checkmate assert_list
    #' @param items Items representing volume files.
    #' @param prefixes Prefixes (folders) in volume content response.
    #' @param ... Other arguments.
    initialize = function(items = NA, prefixes = NA, ...) {
      # Initialize Collection class
      super$initialize(...)

      checkmate::assert_list(items, null.ok = FALSE)
      checkmate::assert_list(prefixes, null.ok = FALSE)

      self$items <- asVolumeFileList(
        x = append(items, prefixes),
        auth = self$auth
      )
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
        if (length(link[["next"]]) > 0) {
          res <- sevenbridges2::api(
            url = link[["next"]],
            method = "GET",
            token = self$auth$get_token(),
            base_url = self$auth$url,
            advance_access = TRUE,
            ...
          )
          res <- status_check(res)
          private$load(res, auth = self$auth)
        }
      }
    },
    #' @description Return previous page of results.
    #' @importFrom rlang abort
    prev_page = function() {
      rlang::abort("Cannot paginate backwards.")
    }
  ),
  private = list(
    # Reload object to get new results
    load = function(res, auth) {
      self$initialize(
        href = res$href,
        items = res$items,
        prefixes = res$prefixes,
        links = res$links,
        auth = auth,
        response = attr(res, "response")
      )
      return(self)
    }
  )
)

# nocov start
# Helper function for creating VolumeFileCollection objects
asVolumeFileCollection <- function(x, auth = NULL) {
  VolumeFileCollection$new(
    href = x$href,
    items = x$items,
    prefixes = x$prefixes,
    links = x$links,
    auth = auth,
    response = attr(x, "response")
  )
}
# nocov end
