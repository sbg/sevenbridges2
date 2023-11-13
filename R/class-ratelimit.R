#' @title R6 Class Representing a Rate Limit for a user
#'
#' @description
#' Rate object containing information about user's rate limit.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Rate Limit.
Rate <- R6::R6Class(
  "Rate",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field rate A list containing the information about user's current rate
    #'  limit. It consists of the following fields:
    #'  \itemize{
    #'    \item `limit` Indicates how many requests can be made in five
    #'      minutes.
    #'    \item `remaining` Indicates how many requests remain.
    #'    \item `reset` Indicates the time when the request rate limit will
    #'      be reset.
    #'  }
    rate = NULL,
    #' @field instance A list containing the information about user's current
    #'  instance limit. It consists of the following fields:
    #'  \itemize{
    #'    \item `limit` Indicates the total number of instances available
    #'      to the user. For the first few months, instance limits are
    #'      unlimited. This is indicated by a special limit of -1.
    #'      Correspondingly, the remaining value is high.
    #'    \item `remaining` Indicates the number of the instances that are
    #'      available at the moment. For the first few months, instance limits
    #'      are unlimited. This is indicated by a high remaining value.
    #'      Correspondingly, the limit is set to a special value of -1.
    #'  }
    instance = NULL,

    #' @description Create a new Rate limit object.
    #'
    #' @param res Response containing Rate limit object info.
    #' @param ... Other response arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      # Rate
      self$rate$limit <- res$rate$limit
      self$rate$remaining <- res$rate$remaining
      self$rate$reset <- parse_time(reset_time_as_unix_epoch = res$rate$reset)

      # Instance
      self$instance$limit <- res$instance$limit
      self$instance$remaining <- res$instance$remaining
    },

    # nocov start
    #' @description Print rate limit information as a bullet list.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    print = function() {
      x <- as.list(self)

      if (!is.null(x$rate) && length(x$rate) != 0) {
        rate_info <- x$rate
        string_rate_info <- glue::glue("{names(rate_info)}: {rate_info}")
      }

      if (!is.null(x$instance) && length(x$instance) != 0) {
        instance_info <- x$instance
        string_instance_info <- glue::glue("{names(instance_info)}:
                                           {instance_info}")
      }

      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.na)
      x <- purrr::discard(x, .p = ~ .x == "")

      cli::cli_h1("Rate limit")
      ifelse(exists("rate_info") && !is.null(rate_info),
        {
          cli::cli_li("rate")
          cli::cli_ul(string_rate_info)
        },
        ""
      )
      ifelse(exists("instance_info") && !is.null(instance_info),
        {
          cli::cli_li("instance")
          cli::cli_ul(string_instance_info)
        },
        ""
      )
      # Close container elements
      cli::cli_end()
    } # nocov end
  )
)

# Helper function for creating Rate objects
asRate <- function(x = NULL, auth = NULL) {
  Rate$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}
