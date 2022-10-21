#' @title R6 Class Representing a platform User
#'
#' @description
#' User object containing user information.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Users.
#' @export
User <- R6::R6Class(
  "User",
  inherit = Item,
  portable = FALSE,
  public = list(
    username = NULL,
    email = NULL,
    first_name = NULL,
    last_name = NULL,
    affiliation = NULL,
    phone = NULL,
    address = NULL,
    city = NULL,
    state = NULL,
    country = NULL,
    zip_code = NULL,
    projects = NULL,
    billing_groups = NULL,
    tasks = NULL,
    #' @description
    #' Create a new User object.
    initialize = function(username = NULL,
                          email = NULL,
                          first_name = NULL,
                          last_name = NULL,
                          affiliation = NULL,
                          phone = NULL,
                          address = NULL,
                          city = NULL,
                          state = NULL,
                          country = NULL,
                          zip_code = NULL,
                          projects = NULL,
                          billing_groups = NULL,
                          tasks = NULL, ...) {

      self$username <- username
      self$email <-email
      self$first_name <- first_name
      self$last_name <- last_name
      self$affiliation <- affiliation
      self$phone <- phone
      self$address <- address
      self$city <- city
      self$state <- state
      self$country <- country
      self$zip_code <- zip_code
      self$projects <- projects
      self$billing_groups <- billing_groups
      self$tasks <- tasks

      # Initialize Item class
      super$initialize(...)

    },
    #' @description
    #' Print user information as billeted list
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_bullets
    #' @noRd
    print = function(...) {
      x <- as.list(self)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = ~ .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("User")
      cli::cli_bullets(string)
    }
  )

)

asUser <- function(x) {
  User$new(
    username = x$username,
    email = x$email,
    first_name = x$first_name,
    last_name = x$last_name,
    affiliation = x$affiliation,
    phone = x$phone,
    address = x$address,
    city = x$city,
    state = x$state,
    country = x$country,
    zip_code = x$zip_code,
    projects = x$projects,
    billing_groups = x$billing_groups,
    tasks = x$tasks,
    href = x$href,
    response = attr(x, 'response')
  )
}
