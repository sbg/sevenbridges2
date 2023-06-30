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
    #' @field prefixes Prefixes (folders) in volume content response.
    prefixes = NULL,
    #' @param prefixes Prefixes (folders) in volume content response.
    initialize = function(prefixes = NA) {
      # Initialize Collection class
      super$initialize(...)

      self$prefixes <- prefixes
    },
    #' @description Return results as a list of VolumeFile objects.
    results = function() {
      return(asVolumeFilesList(self$items, self$prefixes, self$auth))
    },
    #' @description Return next page of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
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

      return(VolumeFileCollection(res))
    }
  )
)

# nocov start
# Helper function for creating VolumeFileCollection objects
asVolumeFileCollection <- function(x, auth = NULL) {
  VolumeFileCollection$new(
    prefixes = x$prefixes,
    auth = auth
  )
}
# nocov end
