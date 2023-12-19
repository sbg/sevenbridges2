# nolint start
#' @title R6 Class representing invoices endpoints
#'
#' @description
#' R6 Class representing invoice resource endpoints
#'
#' @importFrom R6 R6Class
#'
#' @export
Invoices <- R6::R6Class(
  "Invoices",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "billing/invoices",
      "get" = "billing/invoices/{id}"
    ),

    # Initialize Invoices object ---------------------------------------------
    #' @description Create a new Invoices object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all invoices ------------------------------------------------------
    #' @description The call returns information about all your available
    #'  invoices, unless you use the `billing_group` query parameter to specify
    #'  the ID of a particular billing group, in which case it will return the
    #'  invoice incurred by that billing group only.
    #'
    #' @param billing_group ID of a billing group or billing group object
    #'  you want to list invoices for. Optional.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like query parameters or 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  invoices_object <- Invoices$new(
    #'    auth = auth
    #'  )
    #'
    #'  # List all your invoices
    #'  invoices_object$query(billing_group = billing_group)
    #' }
    #'
    #' @return \code{\link{Collection}} of \code{\link{Invoice}} objects.
    query = function(billing_group = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      if (!is_missing(billing_group)) {
        billing_group <- check_and_transform_id(billing_group, "Billing")
      }

      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        billing_group = billing_group,
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asInvoiceList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    }, # nocov end

    # Get invoice -------------------------------------------------------------
    #' @description This call retrieves information about a selected invoice,
    #'  including the costs for analysis and storage, and the invoice period.
    #'  Use the call to list invoices to retrieve the `invoice_id`s for a
    #'  specified billing group.
    #'
    #' @param id The ID of the invoice you are querying.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  invoices_object <- Invoices$new(
    #'    auth = auth
    #'  )
    #'
    #'  # Get single invoice by id
    #'  invoices_object$get(id = id)
    #' }
    #'
    #' @return \code{\link{Invoice}} object.
    get = function(id, ...) {
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE,
        ...
      )
      return(asInvoice(res, auth = self$auth))
    } # nocov end
  )
)
