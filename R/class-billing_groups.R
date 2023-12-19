# nolint start
#' @title R6 Class representing billing groups endpoints
#'
#' @description
#' R6 Class representing billing groups resource endpoints.
#'
#' @importFrom R6 R6Class
#'
#' @export
Billing_groups <- R6::R6Class(
  "Billing_groups",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "billing/groups",
      "get" = "billing/groups/{id}"
    ),

    # Initialize Billing_groups object ----------------------------------------
    #' @description Create a new Billing_groups object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all billing groups -------------------------------------------------
    #' @description List all your billing groups, including groups
    #'  that are pending or have been disabled.
    #'
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like query parameters, 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  billing_groups_object <- Billing_groups$new(
    #'    auth = auth
    #'  )
    #'
    #'  # List all your billing groups
    #'  billing_groups_object$query()
    #' }
    #'
    #' @return \code{\link{Collection}} of \code{\link{Billing}} groups.
    query = function(limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asBillingList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    }, # nocov end

    # Get billing group -------------------------------------------------------
    #' @description Retrieve a single billing group, specified by its id.
    #'  To find the `billing_group`, use the call `Billing_groups$query()`
    #'  to list all your billing groups. The information returned
    #'  includes the billing group owner, the total balance, and the status of
    #'  the billing group (pending or confirmed).
    #'
    #' @param id The ID of the billing group you are querying.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  billing_groups_object <- Billing_groups$new(
    #'    auth = auth
    #'  )
    #'
    #'  # Get single billing group
    #'  billing_groups_object$get(id = id)
    #' }
    #'
    #' @return \code{\link{Billing}} object.
    get = function(id, ...) {
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE,
        ...
      )
      return(asBilling(res, auth = self$auth))
    } # nocov end
  )
)
