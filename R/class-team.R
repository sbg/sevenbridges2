#' @title R6 Class representing a Team
#'
#' @description
#' R6 Class representing a central resource for managing teams.
#'
#' @importFrom R6 R6Class
#'
#' @export
Team <- R6::R6Class(
  "Team",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id The ID of the team.
    id = NULL,
    #' @field name Team's name.
    name = NULL,

    # Initialize Team object
    #' @description Create a new Team object
    #'
    #' @param res Response containing the Team object information.
    #'
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
    },

    # nocov start
    # Print Team object -------------------------------------------------------
    #' @description Print method for Team class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    #'
    #' @examples
    #' \dontrun{
    #'  team_object <- Team$new(
    #'  	res = x,
    #' 		href = x$href,
    #' 		auth = auth,
    #' 		response = attr(x, "response")
    #'  )
    #'  team_object$print()
    #' }
    print = function() {
      x <- as.list(self)

      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = ~ .x == "")

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("Team")
      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    # Reload Team object ------------------------------------------------------
    #' @description Reload Team object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #'  @importFrom rlang inform
    #'
    #' @examples
    #' \dontrun{
    #'  team_object <- Team$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  team_object$reload()
    #' }
    #'
    #' @return \code{\link{Team}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Team object is refreshed!")
    }
    # nocov end
  )
)

# Helper functions for creating Team objects ----------------------------------
asTeam <- function(x = NULL, auth = NULL) {
  Team$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asTeamList <- function(x, auth) {
  obj <- lapply(x$items, asTeam, auth = auth)
  obj
}
