#' @title R6 Class Representing a platform User
#'
#' @description
#' User object containing user information.
#'
#' @importFrom R6 R6Class
#' @details This is main object for Users.
User <- R6::R6Class(
  "User",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "user"
    ),
    #' @field username User name.
    username = NULL,
    #' @field email User's email address.
    email = NULL,
    #' @field first_name User's first name.
    first_name = NULL,
    #' @field last_name User's last name.
    last_name = NULL,
    #' @field affiliation The company or the institute the user is affiliated
    #'  with.
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

    # Initialize User object --------------------------------------------------
    #' @description Create a new User object.
    #'
    #' @param res Response containing User object information.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$username <- res$username
      self$email <- res$email
      self$first_name <- res$first_name
      self$last_name <- res$last_name
      self$affiliation <- res$affiliation
      self$phone <- res$phone
      self$address <- res$address
      self$city <- res$city
      self$state <- res$state
      self$country <- res$country
      self$zip_code <- res$zip_code
      self$role <- res$role
      self$tags <- res$tags
    },

    # nocov start
    # Print User object ------------------------------------------------------
    #' @description Print user information as bullet list.
    #'
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
        tags_list <- as.list(as.character(parse_time(tag_expirations,
          use_milliseconds = TRUE
        )))
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
    },

    # Reload User object ------------------------------------------------------
    #' @description Reload User object information.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @return \code{\link{User}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("User object is refreshed!")
    }
  ) # nocov end
)

# Helper function for creating User objects ----------------------------------
asUser <- function(x = NULL, auth = NULL) {
  User$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}
