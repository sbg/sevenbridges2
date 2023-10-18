#' @title R6 Class representing invoice information.
#'
#' @description
#' R6 Class representing invoice information.
#'
#' @importFrom R6 R6Class
#' @details
#'  This object contains information about a selected invoice,
#'  including the costs for analysis and storage, and the invoice period.
#'
Invoice <- R6::R6Class(
  "Invoice",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "get" = "billing/invoices/{id}"
    ),
    #' @field id Invoice identifier.
    id = NULL,
    #' @field pending Invoice approval status.
    pending = NULL,
    #' @field approval_date Invoice approval date.
    approval_date = NULL,
    #' @field invoice_period Invoicing period (from-to).
    invoice_period = NULL,
    #' @field analysis_costs Costs of your analysis.
    analysis_costs = NULL,
    #' @field storage_costs Storage costs.
    storage_costs = NULL,
    #' @field total Total costs.
    total = NULL,

    #' @description Create new Invoice object.
    #' @param res Response containing Invoice object information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$pending <- res$pending
      self$approval_date <- res$approval_date
      self$invoice_period <- res$invoice_period
      self$analysis_costs <- res$analysis_costs
      self$storage_costs <- res$storage_costs
      self$total <- res$total
    },
    # nocov start
    #' @description Print invoice information as a bullet list.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_ul cli_li
    print = function() {
      x <- as.list(self)
      if (!is.null(x$invoice_period)) {
        invoice_period <- x$invoice_period
      }
      if (!is.null(x$analysis_costs)) {
        analysis_costs <- x$analysis_costs
      }
      if (!is.null(x$storage_costs)) {
        storage_costs <- x$storage_costs
      }
      if (!is.null(x$total)) {
        total <- x$total
      }
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = ~ .x == "")
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue("{names(x)}: {x}")
      string_invoice_period <- glue::glue(
        "{names(invoice_period)}: {invoice_period}"
      )
      string_analysis_costs <- glue::glue(
        "{names(analysis_costs)}: {analysis_costs}"
      )
      string_storage_costs <- glue::glue(
        "{names(storage_costs)}: {storage_costs}"
      )
      string_total <- glue::glue("{names(total)}: {total}")

      cli::cli_h1("Invoice info")
      cli::cli_ul()
      cli::cli_li(string)

      ifelse(!is.null(invoice_period),
        {
          cli::cli_li("invoice_period")
          cli::cli_ul(string_invoice_period)
        },
        ""
      )
      ifelse(!is.null(analysis_costs),
        {
          cli::cli_li("analysis_costs")
          cli::cli_ul(string_analysis_costs)
        },
        ""
      )
      ifelse(!is.null(storage_costs),
        {
          cli::cli_li("storage_costs")
          cli::cli_ul(string_storage_costs)
        },
        ""
      )
      ifelse(!is.null(total),
        {
          cli::cli_li("total")
          cli::cli_ul(string_total)
        },
        ""
      )

      # Close container elements
      cli::cli_end()
    },
    #' @description Reload Invoice.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #' @return Invoice
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Invoice object is refreshed!")
    } # nocov end
  )
)

asInvoice <- function(x = NULL, auth = NULL) {
  Invoice$new(
    res = x,
    href = x$href,
    response = attr(x, "response"),
    auth = auth
  )
}

asInvoiceList <- function(x, auth) {
  obj <- lapply(x$items, asInvoice, auth = auth)
  obj
}
