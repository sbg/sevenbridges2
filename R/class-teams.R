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
      "delete" = "teams/{id}"
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
    #'  you are a member of. Each team's ID, name as well as your role in the
    #'  team will be returned.
    #'
    #' @param division The string ID of the division or Division object
    #'  you are querying.
    #' @param list_all Boolean. Set this field to `TRUE` if you want to list
    #'  all teams within the division (regardless of whether you are a member
    #'  of a team or not). Default value is `FALSE`.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve a list of all teams within the division regardless of
    #'   # whether you are a member of a team or not
    #'   auth$teams$query(division_id = "division-id", list_all = TRUE)
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
      list_all <- ifelse(isTRUE(list_all), "true", "false")

      # nocov start
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
    }
  )
)
