#' @title R6 Class representing a Division
#'
#' @description
#' R6 Class representing a central resource for managing divisions.
#'
#' @importFrom R6 R6Class
#'
#' @export
Division <- R6::R6Class(
  "Division",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id The ID of the division.
    id = NULL,
    #' @field name Division's name.
    name = NULL,

    # Initialize Division object
    #' @description Create a new Division object
    #'
    #' @param res Response containing the Division object information.
    #'
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
    },

    # nocov start
    # Print Division object
    #' @description Print method for Division class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    #'
    #' @examples
    #' \dontrun{
    #'  division_object <- Division$new(
    #'  	res = x,
    #' 		href = x$href,
    #' 		auth = auth,
    #' 		response = attr(x, "response")
    #'  )
    #'  division_object$print()
    #' }
    print = function() {
      x <- as.list(self)

      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = ~ .x == "")

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Division")
      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    # Reload Division object
    #' @description Reload Division object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #'  @importFrom rlang inform
    #'
    #' @examples
    #' \dontrun{
    #'  division_object <- Division$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  division_object$reload()
    #' }
    #'
    #' @return \code{\link{Division}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Division object is refreshed!")
    }
    # nocov end
  )
)

# Helper functions for creating Division objects ------------------------------
asDivision <- function(x = NULL, auth = NULL) {
  Division$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asDivisionList <- function(x, auth) {
  obj <- lapply(x$items, asDivision, auth = auth)
  obj
}
