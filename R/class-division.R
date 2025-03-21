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
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "list_teams" = "divisions/{id}/teams",
      "list_members" = "users",
      "remove_member" = "users/{username}"
    ),
    #' @field id The ID of the division.
    id = NULL,
    #' @field name Division's name.
    name = NULL,

    # Initialize Division object ----------------------------------------------
    #' @description Create a new Division object.
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
    # Print Division object ---------------------------------------------------
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

    # Reload Division object --------------------------------------------------
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
    },
    # nocov end

    # List all teams in a division --------------------------------------------
    #' @description This call retrieves a list of all teams in a division that
    #'  you are a member of. Each team's ID and name will be returned.
    #'
    #' @param list_all Boolean. Set this field to `TRUE` if you want to list
    #'  all teams within the division (regardless of whether you are a member
    #'  of a team or not). Default value is `FALSE`.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_logical
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'   # Get details of a specific division
    #'   division_obj <- a$divisions$get(id = "division-id")
    #'
    #'   # Retrieve a list of division teams you are a member of
    #'   division_obj$list_teams()
    #'
    #'   # Retrieve a list of all teams within the division regardless of
    #'   # whether you are a member of a team or not
    #'   division_obj$list_teams(list_all = TRUE)
    #' }
    #'
    #' @return A \code{\link{Collection}} of \code{\link{Team}} objects.
    list_teams = function(list_all = FALSE, ...) {
      checkmate::assert_logical(list_all)

      list_all <- ifelse(isTRUE(list_all), "true", "false")
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["list_teams"]]),
        method = "GET",
        query = list("_all" = list_all),
        ...
      )

      res$items <- asTeamList(res, self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # nocov end

    # List division members ---------------------------------------------------
    #' @description This call retrieves a list of all members of a division. In
    #'  addition, you can list members with a specific role, e.g. all
    #'  administrators within a division.
    #'
    #' @param role Filter members by role. Supported roles are `ADMIN`,
    #'  `MEMBER`, and `EXTERNAL_COLLABORATOR`. If `NULL` (default), members of
    #'  all roles will be retrieved.
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like other query parameters or 'fields', etc.
    #'
    #' @importFrom checkmate assert_choice
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'   # Get details of a specific division
    #'   division_obj <- a$divisions$get(id = "division-id")
    #'
    #'   # Retrieve a list of all division members
    #'   division_obj$list_members()
    #'
    #'   # Or filter members by role. The following roles are supported:
    #'   # "MEMBER", "ADMIN", and "EXTERNAL_COLLABORATOR"
    #'   division_obj$list_members(role = "ADMIN")
    #' }
    #'
    #' @return A \code{\link{Collection}} of \code{\link{User}} objects.
    list_members = function(role = NULL,
                            limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      checkmate::assert_choice(role,
        c("MEMBER", "ADMIN", "EXTERNAL_COLLABORATOR"),
        null.ok = TRUE
      )
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["list_members"]]),
        method = "GET",
        query = list(division = self$id, role = role),
        limit = limit,
        offset = offset,
        ...
      )

      res$items <- asUserList(res, self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # nocov end

    # Remove a member from a division -----------------------------------------
    #' @description Removes a specified user from a division. This action
    #'  revokes the user's membership in the division but does not delete their
    #'  Platform account. Note that only users with the `ADMIN` role in the
    #'  division can perform this action.
    #'
    #' @param user The Seven Bridges Platform username of the user to be
    #'  removed, specified in the format `division-name/username`, or an object
    #'  of class `User` that contains the username.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specific division
    #'   division_obj <- a$divisions$get(id = "division-id")
    #'
    #'   # Remove a member using their username
    #'   division_obj$remove_member(user = "division-name/username")
    #'
    #'   # Remove a member using a User object
    #'   members <- division_obj$list_members(role = "MEMBER")
    #'   member_to_remove <- members$items[[1]]
    #'   division_obj$remove_member(user = member_to_remove)
    #' }
    remove_member = function(user) {
      if (self$auth$user()$role != "ADMIN") {
        rlang::abort(
          "You don't have the required permissions to remove members from this division. Only users with the 'ADMIN' role can perform this action." # nolint
        )
      }

      if (is_missing(user)) {
        rlang::abort(
          "Please provide a username or a User object to remove the member from the division." # nolint
        )
      }

      username <- check_and_transform_id(user,
        class_name = "User",
        field_name = "username"
      )
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["remove_member"]]),
        method = "DELETE"
      )

      rlang::inform(
        message = glue::glue_col(
          "User {green {username}} has been removed
          from the {green {self$id}} division.",
          .literal = TRUE
        )
      )
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
