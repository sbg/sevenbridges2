#' @title R6 Class Representing a platform User
#'
#' @description
#' User object containing user information.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Users.
User <- R6::R6Class(
  "User",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field username User name.
    username = NULL,
    #' @field email User's email address.
    email = NULL,
    #' @field first_name User's first name.
    first_name = NULL,
    #' @field last_name User's last name.
    last_name = NULL,
    #' @field affiliation The company or the institute the user is affiliated
    #' with.
    affiliation = NULL,
    #' @field phone User's phone number.
    phone = NULL,
    #' @field address User's residential address.
    address = NULL,
    #' @field city User's city of residence.
    city = NULL,
    #' @field  state User's state of residence.
    state = NULL,
    #' @field  country User's country of residence.
    country = NULL,
    #' @field zip_code Zip code for the user's residence.
    zip_code = NULL,
    #' @field role User's role.
    role = NULL,
    #' @field tags Platform tags associated with the user.
    tags = NULL,

    #' @description
    #' Create a new User object.
    #' @param username User name.
    #' @param email User's email address.
    #' @param first_name User's first name.
    #' @param last_name User's last name.
    #' @param affiliation The company or the institute the user is affiliated
    #' with.
    #' @param phone User's phone number.
    #' @param address User's residential address.
    #' @param city User's city of residence.
    #' @param state User's state of residence.
    #' @param country User's country of residence.
    #' @param zip_code Zip code for the user's residence.
    #' @param role User's role.
    #' @param tags Platform tags associated with the user.
    #'
    #' @param ... Additional arguments.
    #'
    #' @return A new User object.
    initialize = function(username = NA,
                          email = NA,
                          first_name = NA,
                          last_name = NA,
                          affiliation = NA,
                          phone = NA,
                          address = NA,
                          city = NA,
                          state = NA,
                          country = NA,
                          zip_code = NA,
                          role = NA,
                          tags = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$username <- username
      self$email <- email
      self$first_name <- first_name
      self$last_name <- last_name
      self$affiliation <- affiliation
      self$phone <- phone
      self$address <- address
      self$city <- city
      self$state <- state
      self$country <- country
      self$zip_code <- zip_code
      self$role <- role
      self$tags <- tags
    },
    #' @description
    #' Print user information as bullet list
    #' @importFrom purrr discard
    #' @importFrom glue glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    print = function() {
      x <- as.list(self)
      tags <- x$tags

      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = ~ .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("User")
      cli::cli_li(string)

      if (!is_missing(tags)) {
        tag_names <- sapply(tags, "[[", "tag")
        tag_expirations <- sapply(tags, "[[", "expires_at")
        tags_list <- as.list(as.character(parse_time(tag_expirations, use_milliseconds = TRUE)))
        names(tags_list) <- tag_names

        string_tags <- glue::glue_col("{blue  tag name:} {names(tags_list)};
                                      {blue expires at:} {tags_list}")
      }

      ifelse(!is_missing(tags),
        {
          cli::cli_li("tags")
          cli::cli_ul(string_tags)
        },
        ""
      )

      # Close container elements
      cli::cli_end()
    }
  )
)

# Helper function for creating User objects
asUser <- function(x, auth = NULL) {
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
    role = x$role,
    tags = x$tags,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}
