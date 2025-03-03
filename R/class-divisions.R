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
      "query" = "divisions",
      "get" = "divisions/{id}"
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
    #'   a$Divisions$query()
    #' }
    query = function() {
      # nocov start
      params_list <- list(
        path = glue::glue(self$URL[["query"]])
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
    #' @param id The ID of the division you are querying. The function
    #'  also accepts a Division object and extracts the ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return \code{\link{Division}} object.
    #'
    #' @examples
    #' \dontrun{
    #'   # Retrieve details of a specified division
    #'   a$Divisions$get(id = "division-id")
    #' }
    get = function(id, ...) {
      if (is_missing(id)) {
        rlang::abort(
          "Please provide the 'id' parameter."
        )
      }

      id <- check_and_transform_id(id,
        class_name = "Division"
      )

      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        ...
      )

      return(asDivision(res, auth = self$auth))
      # nocov end
    }
  )
)
