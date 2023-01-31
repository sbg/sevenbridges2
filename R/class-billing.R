#' @title R6 Class representing billing information.
#'
#' @description
#' R6 Class representing a central resource for managing billing groups.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Billing
#' @export
# nolint start
Billing <- R6::R6Class(
  # nolint end
  "Billing",
  inherit = Item,
  portable = FALSE,
  public = list(
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
    #' @param id  Billing group identifier.
    #' @param owner Username of the user that owns the billing group.
    #' @param name Billing group name.
    #' @param type Billing group type (free or regular)
    #' @param pending Billing group approval status. TRUE if billing group is
    #' not yet approved, FALSE if the billing group has been approved.
    #' @param disabled Indicator of whether the billing group is disabled.
    #' TRUE if billing group is disabled, FALSE if its enabled.
    #' @param balance Billing group balance.
    #'
    #' @param ... Additional arguments.
    #'
    #' @return A new Billing object.
    initialize = function(id = NULL,
                          owner = NULL,
                          name = NULL,
                          type = NULL,
                          pending = FALSE,
                          disabled = FALSE,
                          balance = NULL, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$owner <- owner
      self$name <- name
      self$type <- type
      self$pending <- pending
      self$disabled <- disabled
      self$balance <- balance
    },
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
    },
    #' @description Method for getting a analysis breakdown for a billing group.
    #'
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0.
    #' @param date_from A string representing the starting date for retrieving
    #' transactions analysis in the following format: mm-dd-yyyy.
    #' @param date_to A string representing the ending date for retrieving
    #' transactions analysis in the following format: mm-dd-yyyy.
    #' @param invoice_id A string representing invoice ID to show a breakdown
    #' for the specific invoice. If omitted, the current spending breakdown is
    #' returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    analysis_breakdown = function(date_from = NULL,
                                  date_to = NULL,
                                  invoice_id = NULL,
                                  fields = NULL,
                                  limit = getOption("sevenbridges2")$limit,
                                  offset = getOption("sevenbridges2")$offset) {
      req <- sevenbridges2::api(
        path = paste0("billing/groups/", id, "/breakdown/analysis"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to
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
    #' @param invoice_id A string representing invoice ID to show a breakdown
    #' for the specific invoice. If omitted, the current spending breakdown is
    #' returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    storage_breakdown = function(date_from = NULL,
                                 date_to = NULL,
                                 invoice_id = NULL,
                                 fields = NULL,
                                 limit = getOption("sevenbridges2")$limit,
                                 offset = getOption("sevenbridges2")$offset) {
      req <- sevenbridges2::api(
        path = paste0("billing/groups/", id, "/breakdown/storage"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to
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
    #' @param invoice_id A string representing invoice ID to show a breakdown
    #' for the specific invoice. If omitted, the current spending breakdown is
    #' returned.
    #' @param limit An integer representing the maximum number of collection
    #' items to return for a single request. The default value is 50, while
    #' maximum is 100.
    #' @param fields Selector specifying a subset of fields to include in the
    #' response.
    egress_breakdown = function(date_from = NULL,
                                date_to = NULL,
                                invoice_id = NULL,
                                fields = NULL,
                                limit = getOption("sevenbridges2")$limit,
                                offset = getOption("sevenbridges2")$offset) {
      req <- sevenbridges2::api(
        path = paste0("billing/groups/", id, "/breakdown/egress"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        invoice_id = invoice_id,
        offset = offset,
        date_from = date_from,
        date_to = date_to
      )
      req <- status_check(req)
      return(req)
    }
  )
)

asBilling <- function(x, auth = NULL) {
  Billing$new(
    id = x$id,
    owner = x$owner,
    name = x$name,
    type = x$type,
    pending = x$pending,
    disabled = x$disabled,
    balance = x$balance,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asBillingList <- function(x, auth) {
  obj <- lapply(x$items, asBilling, auth = auth)
  obj
}
