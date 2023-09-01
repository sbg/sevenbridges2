#' @title R6 Class representing billing information.
#'
#' @description
#' R6 Class representing a central resource for managing billing groups.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Billing
# nolint start
Billing <- R6::R6Class(
  # nolint end
  "Billing",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "billing_group" = "billing/groups/{self$id}",
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

    #' @description
    #' Create a new Billing object.
    #' @param res Response containing File object information.
    #' @param ... Other arguments.
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
    #' @description
    #' Print billing group information as a bullet list.
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
    #' @description
    #' Reload Billing group object.
    #' @param ... Other query parameters.
    #' @return Billing
    reload = function(...) {
      path <- glue::glue(self$URL[["billing_group"]])
      res <- super$reload(
        path = path,
        ...
      )
      rlang::inform("Billing group object is refreshed!")
      # Reload object
      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
    },
    #' @description Method for getting a analysis breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0.
    #' @param date_from A string representing the starting date for retrieving
    #' transactions analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #' transactions analysis in the following format: mm-dd-yyyy.
    #' @param invoice_id A string representing invoice ID or Invoice object to
    #'   show a breakdown for the specific invoice. If omitted, the current
    #'   spending breakdown is returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    #' @param ... Other arguments.
    analysis_breakdown = function(date_from = NULL,
                                  date_to = NULL,
                                  invoice_id = NULL,
                                  fields = NULL,
                                  limit = getOption("sevenbridges2")$limit,
                                  offset = getOption("sevenbridges2")$offset,
                                  ...) {
      invoice_id <- check_and_transform_id(invoice_id, "Invoice")

      req <- sevenbridges2::api(
        path = glue::glue(self$URL[["breakdown_analysis"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to,
        ...
      )
      req <- status_check(req)
      # asAnalysisBreakdownList(req)
      # req_as_json <- jsonlite::toJSON(req, pretty = TRUE)
      # cat(req_as_json)
      return(req)
    },
    #' @description Method for getting a storage breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0.
    #' @param date_from A string representing the starting date for retrieving
    #' storage analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #' storage analysis in the following format: mm-dd-yyyy.
    #' @param invoice_id A string representing invoice ID or Invoice object to
    #'   show a breakdown for the specific invoice. If omitted, the current
    #'   spending breakdown is returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    #' @param ... Other arguments.
    storage_breakdown = function(date_from = NULL,
                                 date_to = NULL,
                                 invoice_id = NULL,
                                 fields = NULL,
                                 limit = getOption("sevenbridges2")$limit,
                                 offset = getOption("sevenbridges2")$offset,
                                 ...) {
      invoice_id <- check_and_transform_id(invoice_id, "Invoice")

      req <- sevenbridges2::api(
        path = glue::glue(self$URL[["storage_breakdown"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to,
        ...
      )
      req <- status_check(req)
      return(req)
    },
    #' @description Method for getting a egress breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0.
    #' @param date_from A string representing the starting date for retrieving
    #' egress analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #' egress analysis in the following format: mm-dd-yyyy.
    #' @param invoice_id A string representing invoice ID or Invoice object to
    #'   show a breakdown for the specific invoice. If omitted, the current
    #'   spending breakdown is returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    #' @param ... Other arguments.
    egress_breakdown = function(date_from = NULL,
                                date_to = NULL,
                                invoice_id = NULL,
                                fields = NULL,
                                limit = getOption("sevenbridges2")$limit,
                                offset = getOption("sevenbridges2")$offset,
                                ...) {
      invoice_id <- check_and_transform_id(invoice_id, "Invoice")

      req <- sevenbridges2::api(
        path = glue::glue(self$URL[["egress_breakdown"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to,
        ...
      )
      req <- status_check(req)
      return(req)
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
