# nolint start
#' @title R6 Class representing volumes endpoint
#'
#' @description
#' R6 Class representing volumes resource endpoint
#'
#' @importFrom R6 R6Class
#' @export
Volumes <- R6::R6Class(
  "Volumes",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "storage/volumes",
      "get" = "storage/volumes/{id}",
      "create" = "storage/volumes"
    ),

    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all volumes you've registered --------------------------------------
    #' @description This call lists all the volumes you've registered.
    #'
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    #' @importFrom checkmate assert_list
    query = function(...) {
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        ...
      )
      return(res)
      # return(asVolumeList(res, auth = self$auth))
    },

    # Get single volume -------------------------------------------------------
    #' @description This call returns details of the specified volume.
    #' The volume is referred to by its ID, which you can obtain by
    #' making the call to list all the volumes you've registered.
    #' @param id The Volume ID consists of volume owner's name (for enterprise
    #' users) and volume name in form {volume_owner}/{volume_name},
    #' or division name (if user belongs to some division) and volume
    #' name in form {division}/{volume_name}. You can also get the Volume ID
    #' for a volume by making the call to list all volumes you've registered.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    get = function(id) {
      if (is_missing(id)) {
        rlang::abort("Volume ID must be provided!")
      }
      checkmate::assert_string(id)

      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE
      )
      return(res)
      # return(asVolume(res, auth = self$auth))
    },
    # Delete volume -------------------------------------------------------
    #' @description Please, use delete() operation on the exact Volume object.
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting volumes is possible to perform on the specific instance of class Volume.") # nolint
    }
  )
)
