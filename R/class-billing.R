#' @title R6 Class representing billing information.
#'
#' @description
#' R6 Class representing a central resource for managing billing groups.
#'
#' @importFrom R6 R6Class
#'
#' @details
#' This is main object for Billing
# nolint start
Billing <- R6::R6Class(
  # nolint end
  "Billing",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "billing/groups/{id}",
      "breakdown_analysis" = "billing/groups/{self$id}/breakdown/analysis",
      "storage_breakdown" = "billing/groups/{self$id}/breakdown/storage",
      "egress_breakdown" = "billing/groups/{self$id}/breakdown/egress"
    ),
    #' @field id Billing group identifier.
    id = NULL,
    #' @field owner Username of the user that owns the billing group.
    owner = NULL,
    #' @field name Billing group name.
    name = NULL,
    #' @field type Billing group type
    type = NULL,
    #' @field pending Billing group approval status.
    pending = NULL,
    #' @field disabled Indicator of whether the billing group is disabled.
    disabled = NULL,
    #' @field balance Billing group balance.
    balance = NULL,

    #' @description Create a new Billing object.
    #'
    #' @param res Response containing Billing object information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$owner <- res$owner
      self$name <- res$name
      self$type <- res$type
      self$pending <- res$pending
      self$disabled <- res$disabled
      self$balance <- res$balance
    },
    # nocov start
    #' @description Print billing group information as a bullet list.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul
    print = function() {
      x <- as.list(self)
      if (!is.null(x$balance)) {
        balance <- x$balance
      }
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = ~ .x == "")


      string <- glue::glue("{names(x)}: {x}")
      string_balance <- glue::glue("{names(balance)}: {balance}")

      cli::cli_h1("Billing group info")
      cli::cli_ul()
      cli::cli_li(string)
      ifelse(!is.null(balance),
        {
          cli::cli_li("balance")
          cli::cli_ul(string_balance)
        },
        ""
      )
      cli::cli_end()
    },
    #' @description Reload Billing group object.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'limit', 'offset', 'fields', etc.
    #'
    #' @return \code{\link{Billing}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Billing group object is refreshed!")
    },
    #' @description Method for getting a analysis breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param date_from A string representing the starting date for retrieving
    #'  transactions analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #'  transactions analysis in the following format: mm-dd-yyyy.
    #' @param invoice A string representing invoice ID or Invoice object to
    #'  show a breakdown for the specific invoice. If omitted, the current
    #'  spending breakdown is returned.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param fields Selector specifying a subset of fields to include in the
    #'  response.
    #' @param ... Other arguments that can be passed to core `api()` function.
    analysis_breakdown = function(date_from = NULL,
                                  date_to = NULL,
                                  invoice = NULL,
                                  fields = NULL,
                                  limit = getOption("sevenbridges2")$limit,
                                  offset = getOption("sevenbridges2")$offset,
                                  ...) {
      if (!is_missing(invoice)) {
        invoice <- check_and_transform_id(invoice, "Invoice")
      }

      if (!is_missing(date_from)) {
        date_from <- check_and_transform_datetime(date_from)
      }

      if (!is_missing(date_to)) {
        date_to <- check_and_transform_datetime(date_to)
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["breakdown_analysis"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        query = list(
          invoice_id = invoice,
          date_from = date_from,
          date_to = date_to
        ),
        limit = limit,
        offset = offset,
        fields = fields,
        ...
      )

      return(res)
    },
    #' @description Method for getting a storage breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param date_from A string representing the starting date for retrieving
    #'  storage analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #'  storage analysis in the following format: mm-dd-yyyy.
    #' @param invoice A string representing invoice ID or Invoice object to
    #'  show a breakdown for the specific invoice. If omitted, the current
    #'  spending breakdown is returned.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param fields Selector specifying a subset of fields to include in the
    #'  response.
    #' @param ... Other arguments that can be passed to core `api()` function.
    storage_breakdown = function(date_from = NULL,
                                 date_to = NULL,
                                 invoice = NULL,
                                 fields = NULL,
                                 limit = getOption("sevenbridges2")$limit,
                                 offset = getOption("sevenbridges2")$offset,
                                 ...) {
      if (!is_missing(invoice)) {
        invoice <- check_and_transform_id(invoice, "Invoice")
      }

      if (!is_missing(date_from)) {
        date_from <- check_and_transform_datetime(date_from)
      }

      if (!is_missing(date_to)) {
        date_to <- check_and_transform_datetime(date_to)
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["storage_breakdown"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        query = list(
          invoice_id = invoice,
          date_from = date_from,
          date_to = date_to
        ),
        limit = limit,
        offset = offset,
        fields = fields,
        ...
      )

      return(res)
    },
    #' @description Method for getting a egress breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param date_from A string representing the starting date for retrieving
    #'  egress analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #'  egress analysis in the following format: mm-dd-yyyy.
    #' @param invoice A string representing invoice ID or Invoice object to
    #'  show a breakdown for the specific invoice. If omitted, the current
    #'  spending breakdown is returned.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param fields Selector specifying a subset of fields to include in the
    #'  response.
    #' @param ... Other arguments that can be passed to core `api()` function.
    egress_breakdown = function(date_from = NULL,
                                date_to = NULL,
                                invoice = NULL,
                                fields = NULL,
                                limit = getOption("sevenbridges2")$limit,
                                offset = getOption("sevenbridges2")$offset,
                                ...) {
      if (!is_missing(invoice)) {
        invoice <- check_and_transform_id(invoice, "Invoice")
      }

      if (!is_missing(date_from)) {
        date_from <- check_and_transform_datetime(date_from)
      }

      if (!is_missing(date_to)) {
        date_to <- check_and_transform_datetime(date_to)
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["egress_breakdown"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        query = list(
          invoice_id = invoice,
          date_from = date_from,
          date_to = date_to
        ),
        limit = limit,
        offset = offset,
        fields = fields,
        ...
      )

      return(res)
    } # nocov end
  )
)

asBilling <- function(x = NULL, auth = NULL) {
  Billing$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asBillingList <- function(x, auth) {
  obj <- lapply(x$items, asBilling, auth = auth)
  obj
}
