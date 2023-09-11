# nolint start
#' @title R6 Class representing invoices endpoints
#'
#' @description
#' R6 Class representing invoice resource endpoints
#'
#' @importFrom R6 R6Class
#' @export
Invoices <- R6::R6Class(
  "Invoices",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "billing/invoices",
      "get" = "billing/invoices/{id}"
    ),
    #' @description Create a new Invoices object.
    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },
    # List all invoices --------------------------------------
    #' @description The call returns information about all your available
    #' invoices, unless you use the `billing_group` query parameter to specify
    #' the ID of a particular billing group, in which case it will return the
    #' invoice incurred by that billing group only.
    #'
    #' @param billing_group ID of a billing group or billing group object for
    #' which you want to list invoices. Optional.
    #' @return Collection of invoices (Invoice class objects).
    query = function(billing_group = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      if (!is_missing(billing_group)) {
        billing <- check_and_transform_id(billing_group, "Billing")
      }

      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        billing_group = billing,
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asInvoiceList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # nocov end

    # Get invoice details -----------------------------------------------
    #' @description This call retrieves information about a selected invoice,
    #' including the costs for analysis and storage, and the invoice period. Use
    #'  the call to list invoices to retrieve the `invoice_id`s for a specified
    #'  billing group.
    #'
    #' @param id The ID of the invoice you are querying.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #'
    #' @return Invoice job object.
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
