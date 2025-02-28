# nolint start
#' @title R6 Class representing divisions endpoints.
#'
#' @description
#' R6 Class representing Divisions resource.
#'
#' @importFrom R6 R6Class
#' @export
Divisions <- R6::R6Class(
  # nolint end
  "Divisions",
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "list_all_divisions" = "divisions",
      "get_division_details" = "divisions/{division_id}"
    ),

    # Initialize Divisions object ---------------------------------------------
    #' @description Create new Divisions resource object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List divisions ----------------------------------------------------------
    #' @description This call retrieves a list of all divisions you are a
    #'  member of. Each division's ID, name and URL on platform will be
    #'  returned.
    #'
    #' @importFrom glue glue
    #'
    #' @return A \code{\link{Collection}} of \code{\link{Division}} objects.
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve a list of all divisions you are a member of
    #'   a$Divisions$list_all_divisions()
    #' }
    list_all_divisions = function() {
      # nocov start
      params_list <- list(
        path = glue::glue(self$URL[["list_all_divisions"]])
      )

      res <- do.call(
        super$query,
        params_list
      )

      res$items <- asDivisionList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
      # nocov end
    },

    # Get details of a division -----------------------------------------------
    #' @description This call returns the details of a specified division.
    #'
    #' @param division_id The ID of the division you are querying. The function
    #'  also accepts a Division object and extracts the ID.
    #'
    #' @importFrom glue glue
    #'
    #' @return \code{\link{Division}} object.
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified division
    #'   a$Divisions$get_division_details(division_id = "division-id")
    #' }
    get_division_details = function(division_id) {
      if (is_missing(division_id)) {
        rlang::abort(
          "Please provide the 'division_id' parameter."
        )
      }

      division_id <- check_and_transform_id(division_id,
        class_name = "Division"
      )

      # nocov start
      path <- glue::glue(self$URL[["get_division_details"]])

      res <- self$auth$api(
        path = path,
        method = "GET"
      )

      return(asDivision(res, auth = self$auth))
      # nocov end
    }
  )
)
