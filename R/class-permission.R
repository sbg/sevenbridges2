#' @title R6 Class representing member's permissions
#'
#' @description
#' R6 Class representing member's permissions.
#'
#' @importFrom R6 R6Class
#' @export
Permission <- R6::R6Class(
  "Permission",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field write Write permission.
    write = NULL,
    #' @field read Read permission.
    read = NULL,
    #' @field copy Copy permission.
    copy = NULL,
    #' @field execute Execute permission.
    execute = NULL,
    #' @field admin Admin permission.
    admin = NULL,
    #' @description Create a new Permission object.
    #' @param write User can add, modify, and remove files and workflows in a
    #' project. Set value to \code{TRUE} to assign the user write permission.
    #' Set to \code{FALSE} to remove write permission.
    #' @param read User can view file names, metadata, and workflows.
    #' They cannot view file contents. All members of a project have read
    #' permissions by default. Even if you try setting read permissions to
    #' \code{FALSE}, they will still default to \code{TRUE}
    #' @param copy User can view file content, copy, and download files from a
    #' project. Set value to \code{TRUE} to assign the user copy permission.
    #' Set to \code{FALSE} to remove copy permission.
    #' @param execute User can execute workflows and abort tasks in a project.
    #' Set value to \code{TRUE} to assign the user execute permission.
    #' Set to \code{FALSE} to remove execute permission.
    #' @param admin User can modify another user's permissions on a project,
    #' add or remove people from the project and manage funding sources.
    #' They also have all of the above permissions. Set value to \code{TRUE} to
    #' assign the user admin permission. Set to \code{FALSE} to remove admin
    #' permission.
    #' @param ... Other arguments.
    initialize = function(write = TRUE, read = TRUE, copy = TRUE,
                          execute = TRUE, admin = FALSE, ...) {
      # Initialize Item class
      super$initialize(...)

      self$write <- write
      self$read <- read
      self$copy <- copy
      self$execute <- execute
      self$admin <- admin
    },
    #' @description Print method for Permission class.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_end
    print = function() {
      x <- as.list(self)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = ~ .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("Permisions")
      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    }
  )
)
# nolint start
# Helper function for creating Permission objects
asPermission <- function(x, auth = NULL) {
  Permission$new(
    write = x$write,
    read = x$read,
    copy = x$copy,
    execute = x$execute,
    admin = x$admin,
    href = x$href,
    auth = auth
  )
}
# nolint end
