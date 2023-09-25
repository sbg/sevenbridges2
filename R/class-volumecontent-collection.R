# nolint start
#' @title R6 Class representing a VolumeContentCollection
#'
#' @description
#' R6 Class representing a resource for managing volume content collections.
#'
#' @importFrom R6 R6Class
#' @export
VolumeContentCollection <- R6::R6Class(
  # nolint end
  "VolumeContentCollection",
  inherit = Collection,
  portable = FALSE,
  public = list(
    #' @field prefixes Prefixes on the volume, returned in API response.
    prefixes = NULL,

    #' @description Create new VolumeContentCollection object.
    #' @importFrom checkmate assert_list
    #' @param res Response containing VolumeContentCollection object fields.
    #' @param ... Other arguments.
    initialize = function(res = NA, ...) {
      # Initialize Collection class
      super$initialize(...)

      checkmate::assert_list(res$items, null.ok = FALSE)
      checkmate::assert_list(res$prefixes, null.ok = FALSE)

      self$items <- asVolumeFileList(x = res$items, auth = self$auth)
      self$prefixes <- asVolumePrefixList(x = res$prefixes, auth = self$auth)
    },
    # nocov start
    #' @description Print method for VolumeContentCollection class.
    #' @param n Number of items to print in console.
    #' @importFrom cli cli_text cli_h2
    #' @importFrom checkmate test_atomic
    #' @importFrom glue glue_col
    print = function(n = 10) {
      x <- as.list(self)

      cli::cli_h1("Volume files")
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

      cli::cli_h1("Volume prefixes")
      if (length(x$prefixes) == 0) {
        cli::cli_text(glue::glue("The list of prefixes is empty."))
      }
      for (i in seq_len(length(x$prefixes))) {
        if (i > n) {
          cli::cli_text()
          cli::cli_text(glue::glue_col("{blue Reached maximum of ", n, " item(s) to print.}")) # nolint
          break
        }
        if (checkmate::test_r6(x$prefixes[[i]])) {
          cli::cli_h2(i)
          x$prefixes[[i]]$print()
        } else {
          cli::cli_h2(i)
          string <- glue::glue_col("{green {names(x$prefixes[[i]])}}: {x$prefixes[[i]]}") # nolint
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
      for (link in self$links) {
        if (length(link[["next"]]) > 0) {
          res <- sevenbridges2::api(
            url = link[["next"]],
            method = "GET",
            token = self$auth$get_token(),
            base_url = self$auth$url,
            advance_access = TRUE,
            ...
          )

          private$load(res, auth = self$auth)
        }
      }
    }, # nocov end
    #' @description Return previous page of results.
    #' @importFrom rlang abort
    prev_page = function() {
      rlang::abort("Cannot paginate backwards.")
    },
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
      all_prefixes <- self$prefixes
      cond <- TRUE
      while (cond) {
        tryCatch(
          expr = {
            self$next_page()
            page_items <- self$items
            page_prefixes <- self$prefixes
            all_items <- append(all_items, page_items)
            all_prefixes <- append(all_prefixes, page_prefixes)
          },
          error = function(e) {
            cond <<- FALSE
          }
        )
      }
      self$items <- all_items
      self$prefixes <- all_prefixes
      self$href <- NULL
      self$links <- NULL
    }
  ), # nocov end
  private = list(
    # Reload object to get new results
    # nocov start
    load = function(res, auth) {
      self$initialize(
        href = res$href,
        res = res,
        links = res$links,
        auth = auth,
        response = attr(res, "response")
      )
      return(self)
    } # nocov end
  )
)

# nocov start
# Helper function for creating VolumeContentCollection objects
asVolumeContentCollection <- function(x = NULL, auth = NULL) {
  VolumeContentCollection$new(
    href = x$href,
    res = x,
    links = x$links,
    auth = auth,
    response = attr(x, "response")
  )
}
# nocov end
