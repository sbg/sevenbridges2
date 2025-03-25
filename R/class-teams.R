# nolint start
#' @title R6 Class representing teams endpoints
#'
#' @description
#' R6 Class representing teams resource endpoints.
#'
#' @importFrom R6 R6Class
#'
#' @export
Teams <- R6::R6Class(
  "Teams",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "divisions/{division_id}/teams",
      "get" = "teams/{id}",
      "create" = "teams",
      "delete" = "teams"
    ),

    #' @description Create new Teams resource object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all teams in a division --------------------------------------------
    #' @description This call retrieves a list of all teams in a division that
    #'  you are a member of. Each team's ID and name will be returned.
    #'
    #' @param division The string ID of the division or Division object
    #'  you are querying.
    #' @param list_all Boolean. Set this field to `TRUE` if you want to list
    #'  all teams within the division (regardless of whether you are a member
    #'  of a team or not). Default value is `FALSE`.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_logical
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve a list of all teams within the division regardless of
    #'   # whether you are a member of a team or not
    #'   a$teams$query(division = "division-id", list_all = TRUE)
    #' }
    #'
    #' @return A \code{\link{Collection}} of \code{\link{Team}} objects.
    query = function(division, list_all = FALSE, ...) {
      if (is_missing(division)) {
        rlang::abort("Please provide the division ID or Division object you're querying.") # nolint
      }
      division_id <- check_and_transform_id(division,
        class_name = "Division",
        field_name = "id"
      )

      checkmate::assert_logical(list_all)

      # nocov start
      list_all <- ifelse(isTRUE(list_all), "true", "false")

      params_list <- list(
        path = glue::glue(self$URL[["query"]]),
        "_all" = list_all,
        ...
      )

      res <- do.call(
        super$query,
        params_list
      )

      res$items <- asTeamList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Get details of a team -----------------------------------------------
    #' @description This call returns the details of a specified team.
    #'  You can only get details of a team you are a member of.
    #'
    #' @param id The ID of the team you are querying. The function
    #'  also accepts a Team object and extracts the ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom rlang abort
    #'
    #' @return \code{\link{Team}} object.
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified team
    #'   a$teams$get(id = "team-id")
    #' }
    get = function(id, ...) {
      if (is_missing(id)) {
        rlang::abort(
          "Please provide the 'id' parameter."
        )
      }

      id <- check_and_transform_id(id,
        class_name = "Team"
      )

      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        ...
      )

      return(asTeam(res, auth = self$auth))
      # nocov end
    },

    # Create a team --------------------------------------------
    #' @description This call creates a new team within a specified division.
    #'
    #' @param division The string ID of the division or Division object
    #'  where you want to create a team.
    #' @param name Enter the name for the new team.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_string
    #'
    #' @examples
    #' \dontrun{
    #'   # Create new team
    #'   a$teams$create(division = "division-id", name = "my-new-team")
    #' }
    #'
    #' @return A \code{\link{Team}} object.
    create = function(division, name) {
      if (is_missing(division) || is_missing(name)) {
        rlang::abort("Division or new team name is missing. Please provide both parameters.") # nolint
      }
      division_id <- check_and_transform_id(division,
        class_name = "Division",
        field_name = "id"
      )
      checkmate::assert_string(name, null.ok = FALSE)

      # nocov start
      body <- list(
        division = division_id,
        name = name
      )

      res <- self$auth$api(
        path = self$URL[["create"]],
        method = "POST",
        body = body
      )

      return(asTeam(res, auth = self$auth))
    }, # nocov end

    # Delete a team -----------------------------------------------
    #' @description This call deletes a team. By deleting a team, you remove
    #'  the users' membership to the team, but do not remove their accounts
    #'  from the division.
    #'
    #' @param team The team ID or Team object that you want to delete.
    #' @param ... Other arguments that can be passed to core `api()` function.
    #'
    #' @importFrom rlang abort inform
    #' @importFrom glue glue_col
    #'
    #' @examples
    #' \dontrun{
    #'   # Delete a team
    #'   a$teams$delete(team = "team-id")
    #' }
    delete = function(team, ...) {
      if (is_missing(team)) {
        rlang::abort(
          "Please provide the team ID as string or as Team object."
        )
      }

      id <- check_and_transform_id(team,
        class_name = "Team"
      )

      # nocov start
      res <- super$delete(
        id = id,
        ...
      )

      rlang::inform(
        glue::glue_col("The team {green {id} } has been deleted successfully.")
      ) # nocov end
    }
  )
)
