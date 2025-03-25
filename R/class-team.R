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
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "members" = "teams/{self$id}/members",
      "member" = "teams/{self$id}/members/{username}",
      "team" = "teams/{self$id}"
    ),
    #' @field id The ID of the team.
    id = NULL,
    #' @field name Team's name.
    name = NULL,

    # Initialize Team object
    #' @description Create a new Team object.
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
    #' @importFrom rlang inform
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
    },
    # nocov end

    # List team members -------------------------------------------------------
    #' @description This call retrieves a list of all team members within a
    #'  specified team. Each member's username will be returned.
    #'
    #' @param limit The maximum number of collection items to return
    #'  for a single request. Minimum value is `1`.
    #'  The maximum value is `100` and the default value is `50`.
    #'  This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection
    #'  of the first item to return. The default value is `0`.
    #'  This is a pagination-specific attribute.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified team
    #'   my_team <- a$teams$get(id = "team-id")
    #'
    #'   # Retrieve a list of all team members
    #'   my_team$list_members()
    #' }
    #'
    #' @return A \code{\link{Collection}} of \code{\link{User}} objects.
    list_members = function(limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["members"]]),
        method = "GET",
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asUserList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Add a team member -------------------------------------------------------
    #' @description This call adds a division member to the specified team.
    #'  This action requires `ADMIN` privileges.
    #'
    #' @param user User ID of the division member you are adding to the team
    #'  using the following format: `division_id/username`. Alternatively,
    #'  a `User` object can be provided.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified team
    #'   my_team <- a$teams$get(id = "team-id")
    #'
    #'   # Add new member to the team
    #'   my_team$add_member(user = "user-id")
    #' }
    add_member = function(user) {
      if (is_missing(user)) {
        rlang::abort(
          "Please provide a username or a User object to add the member to the team." # nolint
        )
      }

      username <- check_and_transform_id(user,
        class_name = "User",
        field_name = "username"
      )

      # nocov start
      body <- list(
        "id" = username
      )

      res <- self$auth$api(
        path = glue::glue(self$URL[["members"]]),
        method = "POST",
        body = body
      )

      rlang::inform(
        message = glue::glue_col(
          "User {green {username}} has been added
          to the {green {self$name}} team",
          .literal = TRUE
        )
      )
      # nocov end
    },

    # Remove a team member ----------------------------------------------------
    #' @description This call removes a member from a team. By removing a
    #'  member, you remove the user's membership to the team, but do not
    #'  remove their account from the division.
    #'  This action requires `ADMIN` privileges.
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
    #'   # Retrieve details of a specified team
    #'   my_team <- a$teams$get(id = "team-id")
    #'
    #'   # Remove a member from the team
    #'   my_team$remove_member(user = "user-id")
    #' }
    remove_member = function(user) {
      if (is_missing(user)) {
        rlang::abort(
          "Please provide a username or a User object to remove the member from the team." # nolint
        )
      }

      username <- check_and_transform_id(user,
        class_name = "User",
        field_name = "username"
      )

      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["member"]]),
        method = "DELETE"
      )

      rlang::inform(
        message = glue::glue_col(
          "User {green {username}} has been removed
          from the {green {self$id}} team",
          .literal = TRUE
        )
      )
      # nocov end
    },

    # Rename a team -----------------------------------------------------------
    #' @description This call renames the specified team. This action requires
    #'  `ADMIN` privileges.
    #'
    #' @param name The new name for the team.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified team
    #'   my_team <- a$teams$get(id = "team-id")
    #'
    #'   # Rename the team
    #'   my_team$rename(name = "new-team-name")
    #' }
    rename = function(name = NULL) {
      if (is_missing(name)) {
        rlang::abort(
          "Please provide the new name for the team." # nolint
        )
      }

      checkmate::assert_string(name)

      # nocov start
      body <- list(
        "name" = name
      )

      res <- self$auth$api(
        path = glue::glue(self$URL[["team"]]),
        method = "PATCH",
        body = body
      )

      # Reload object
      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )

      rlang::inform(
        glue::glue_col("The team has been renamed.") # nolint
      )
      # nocov end
    },

    # Delete a team -----------------------------------------------------------
    #' @description This call deletes a team. By deleting a team, you remove
    #'  the users' membership to the team, but do not remove their accounts
    #'  from the division.
    #'  This action requires `ADMIN` privileges.
    #'
    #' @importFrom rlang inform
    #' @importFrom glue glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified team
    #'   my_team <- a$teams$get(id = "team-id")
    #'
    #'   # Delete a team
    #'   my_team$delete()
    #' }
    delete = function() {
      # nocov start
      res <- self$auth$api(
        path = glue::glue(self$URL[["team"]]),
        method = "DELETE"
      )

      rlang::inform(
        glue::glue_col("The team {green {self$name}} has been deleted.") # nolint
      )
      # nocov end
    }
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
