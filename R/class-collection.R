# nolint start
#' @title R6 Class representing a Collection
#'
#' @description
#' R6 Class representing a resource for managing collections.
#' Wrapper for SevenBridges pageable resources.
#' Among the actual collection items it contains information regarding
#' the total number of entries available in on the server and resource href.
#'
#' @importFrom R6 R6Class
#' @export
Collection <- R6::R6Class(
  # nolint end
  "Collection",
  portable = FALSE,
  public = list(
    #' @field href API request URL.
    href = NULL,
    #' @field items Items returned in API response.
    items = NULL,
    #' @field links List of links (hrefs) for next page resources.
    links = NULL,
    #' @field total Total number of items.
    total = NULL,
    #' @field response Save raw API response.
    response = NULL,
    #' @field auth Seven Bridges Auth object.
    auth = NULL,

    #' @description Create a new Collection object.
    #' @param href API request URL.
    #' @param items Items returned in API response.
    #' @param links List of links (hrefs) for next page resources.
    #' @param total Total number of items.
    #' @param response Raw API response.
    #' @param auth Seven Bridges Auth object.
    initialize = function(href = NA, items = NA, links = NA, total = NA,
                          response = NA, auth = NA) {
      self$href <- href
      self$items <- items
      self$links <- links
      self$total <- total
      self$response <- response
      self$auth <- auth
    },
    # nocov start
    #' @description Print method for Collection class.
    #' @param n Number of items to print in console.
    #' @importFrom cli cli_text cli_h2
    #' @importFrom checkmate test_atomic
    #' @importFrom glue glue_col
    print = function(n = 10) {
      x <- as.list(self)
      if (length(x$items) == 0) {
        cli::cli_text(glue::glue("The list of items is empty."))
      }
      for (i in seq_len(length(x$items))) {
        if (i > n) {
          cli::cli_text()
          cli::cli_text(glue::glue_col("{blue Reached maximum of ", n, " item(s) to print.}")) # nolint
          break
        }
        if (checkmate::test_r6(x$items[[i]])) {
          cli::cli_h2(i)
          x$items[[i]]$print()
        } else {
          cli::cli_h2(i)
          string <- glue::glue_col("{green {names(x$items[[i]])}}: {x$items[[i]]}") # nolint
          cli::cli_li(string)
          # Close container elements
          cli::cli_end()
        }
      }
    }, # nocov end
    #' @description Return next page of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
    next_page = function(...) {
      checkmate::assert_list(self$links, null.ok = TRUE)
      if (length(self$links) == 0) {
        rlang::abort("No more entries to be returned.")
      }
      # nocov start
      for (i in seq_len(length(self$links))) {
        link <- self$links[[i]]
        if (tolower(link$rel) == "next") {
          res <- sevenbridges2::api(
            url = link$href,
            method = link$method,
            token = self$auth$get_token(),
            base_url = self$auth$url,
            ...
          )
          # Reload Collection object
          private$load(res, auth = self$auth)
          break
        }
        if (i == length(self$links)) {
          rlang::abort("You've reached the last page of results.")
        }
      }
    }, # nocov end
    #' @description Return previous page of results.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
    prev_page = function(...) {
      checkmate::assert_list(self$links, null.ok = TRUE)
      if (length(self$links) == 0) {
        rlang::abort("No more entries to be returned.") # nolint
      }
      # nocov start
      for (i in seq_len(length(self$links))) {
        link <- self$links[[i]]
        if (tolower(link$rel) == "prev") {
          res <- sevenbridges2::api(
            url = link$href,
            method = link$method,
            token = self$auth$get_token(),
            base_url = self$auth$url,
            ...
          )
          # Reload Collection object
          private$load(res, auth = self$auth)
          break
        }
        if (i == length(self$links)) {
          rlang::abort("You've reached the first page of results.")
        }
      }
    }, # nocov end
    #' @description Fetches all available items.
    #' @param ... Other query or API parameters that can be passed to api()
    #' function like advance_access, fields etc.
    #' @importFrom rlang abort
    all = function(...) {
      if (is.null(self$href)) {
        rlang::abort("Resource URL is empty or you've already fetched all results.") # nolint
      }
      # nocov start
      all_items <- self$items
      while (TRUE) {
        result <- tryCatch(
          expr = {
            self$next_page()
            page_items <- self$items
          },
          error = function(e) {
            e <- base::simpleError("You've reached the last page of results.")
          }
        )
        if (inherits(result, "error")) break
        all_items <- append(all_items, result)
      }
      self$items <- all_items
      self$href <- NULL
      self$links <- NULL
    }
  ), # nocov end
  private = list(
    # nocov start
    items_class = function() {
      if (length(self$items) > 0) {
        return(class(self$items[[1]])[[1]])
      } else {
        return(NULL)
      }
    },
    # Reload object to get new results
    load = function(res, auth) {
      # Get items class to convert its elements
      items_class <- private$items_class()
      if (!is.null(items_class)) {
        res$items <- do.call(
          glue::glue("as", items_class, "List"),
          list(x = res, auth = auth)
        )
      }

      self$initialize(
        href = res$href,
        items = res$items,
        links = res$links,
        total = res$total,
        auth = auth,
        response = attr(res, "response")
      )
      return(self)
    } # nocov end
  )
)

# nocov start
# Helper function for creating Collection objects
asCollection <- function(x, auth = NULL) {
  Collection$new(
    href = x$href,
    items = x$items,
    links = x$links,
    total = x$total,
    auth = auth,
    response = attr(x, "response")
  )
}
# nocov end
