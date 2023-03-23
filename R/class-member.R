#' @title R6 Class representing a project member
#'
#' @description
#' R6 Class representing a resource for managing project members.
#'
#' @importFrom R6 R6Class
#' @export
Member <- R6::R6Class(
  "Member",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id Member's id.
    id = NULL,
    #' @field username Member's username.
    username = NULL,
    #' @field email Member's email.
    email = NULL,
    #' @field type Member's type.
    type = NULL,
    #' @field permissions Member's permissions.
    permissions = NULL,
    #' @description Create a new Member object.
    #' @param id Member's id - same as username.
    #' @param username Member's username - same as id.
    #' @param email Member's email.
    #' @param type Type of membership. Default value is \code{USER}.
    #' @param permissions Member's permissions within a project.
    #' Should be an object of class Permission.
    #' @param ... Other arguments.
    initialize = function(id = NA, username = NA, email = NA, type = NA,
                          permissions = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$username <- username
      self$email <- email
      self$type <- type
      self$permissions <- permissions
    },
    #' @description Print method for Member class.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    print = function() {
      x <- as.list(self)

      if (!is.null(x$permissions) && length(x$permissions) != 0) {
        permissions <- x$permissions
        # Convert permissions env to a list and keep only those elements that
        # are logical
        permissions <- as.list(permissions)
        permissions <- purrr::keep(permissions, .p = is.logical)
        string_permissions <- glue::glue("{names(permissions)}: {permissions}")
      }

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = ~ .x == "")

      string <- glue::glue("{names(x)}: {x}")

      cli::cli_h1("Member")

      cli::cli_li(string)

      ifelse(exists("permissions") && !is.null(permissions),
        {
          cli::cli_li("permissions:")
          cli::cli_ul(string_permissions)
        },
        ""
      )

      # Close container elements
      cli::cli_end()
    }
  )
)


# Helper function for creating Member objects
asMember <- function(x, auth = NULL) {
  Member$new(
    id = x$id,
    username = x$username,
    email = x$email,
    type = x$type,
    permissions = asPermission(x$permissions, auth = auth),
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of Member objects
asMemberList <- function(x, auth) {
  obj <- lapply(x$items, asMember, auth = auth)
  obj
}
