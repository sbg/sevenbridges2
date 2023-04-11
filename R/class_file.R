# nolint start
#' @title R6 Class representing a file
#'
#' @description
#' R6 Class representing a resource for managing files.
#'
#' @importFrom R6 R6Class
File <- R6::R6Class(
  # nolint end
  "File",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field id Character used as file id
    id = NULL,
    #' @field name String used as file name
    name = NULL,
    #' @field size File size
    size = NULL,
    #' @field project Project project id if any, when returned by a API call,
    #' it usually return the project id and stored with the object.
    project = NULL,
    #' @field created_on Date created on
    created_on = NULL,
    #' @field modified_on Date modified on
    modified_on = NULL,
    #' @field storage List as storage type
    storage = NULL,
    #' @field origin List as origin
    origin = NULL,
    #' @field tags List as tags
    tags = NULL,
    #' @field metadata List for metadata associated with the file
    metadata = NULL,
    #' @field url File download url
    url = NULL,
    #' @field parent Parent folder ID
    parent = NULL,
    #' @field type \code{"FILE"} or \code{"FOLDER"}
    type = NULL,

    #' @description Create a new File object.
    #' @param id Character used as file id.
    #' @param name File name.
    #' @param size File size.
    #' @param project Project project id if any, when returned by a API call,
    #' it usually return the project id and stored with the object.
    #' @param parent Parent folder ID.
    #' @param type \code{"FILE"} or \code{"FOLDER"}
    #' @param created_on Date created on.
    #' @param modified_on Date modified on.
    #' @param storage List as storage type.
    #' @param origin List as origin.
    #' @param tags List as tags.
    #' @param metadata  List for metadata associated with the file.
    #' @param url  File download url.
    #' @param ... Other arguments.
    initialize = function(id = NA, name = NA, size = NA, project = NA,
                          parent = NA, type = NA, created_on = NA,
                          modified_on = NA, storage = NA, origin = NA,
                          tags = NA, metadata = NA, url = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$name <- name
      self$size <- format(utils::object.size(size), units = "auto")
      self$project <- project
      self$parent <- parent
      self$type <- type
      self$created_on <- created_on
      self$modified_on <- modified_on
      self$storage <- storage
      self$origin <- origin
      self$tags <- tags
      self$metadata <- metadata
      self$url <- url
    },
    # nocov start
    #' @description Print method for File class.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end cli_bullets
    print = function() {
      x <- as.list(self)

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      string <- glue::glue("{names(x)}: {x}")

      cli::cli_h1("File")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    }, # nocov end

    #' @description Detailed print method for File class.
    #'
    #' @details This method allows users to print all the fields from the
    #' Field object more descriptively.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    detailed_print = function() {
      x <- as.list(self)

      if (!is.null(x$tags) && length(x$tags) != 0) {
        file_tags <- x$tags
        names(file_tags) <- paste0("tag_", seq_along(file_tags))
        string_file_tags <- glue::glue(
          "{names(file_tags)}: {file_tags}"
        )
      }

      if (!is.null(x$metadata) && length(x$metadata) != 0) {
        file_metadata <- x$metadata
        string_file_metadata <- glue::glue(
          "{names(file_metadata)}: {file_metadata}"
        )
      }

      if (!is.null(x$origin) && length(x$origin) != 0) {
        file_origin <- x$origin
        string_file_origin <- glue::glue(
          "{names(file_origin)}: {file_origin}"
        )
      }

      if (!is.null(x$storage) && length(x$storage) != 0) {
        file_storage <- x$storage
        string_file_storage <- glue::glue(
          "{names(file_storage)}: {file_storage}"
        )
      }

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)
      # x <- purrr::discard(x, .p = ~ .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("File")
      cli::cli_li(string)

      ifelse(exists("file_tags") && !is.null(file_tags),
        {
          cli::cli_li("tags")
          cli::cli_ul(string_file_tags)
        },
        ""
      )
      ifelse(exists("file_metadata") && !is.null(file_metadata),
        {
          cli::cli_li("metadata")
          cli::cli_ul(string_file_metadata)
        },
        ""
      )
      ifelse(exists("file_origin") && !is.null(file_origin),
        {
          cli::cli_li("origin")
          cli::cli_ul(string_file_origin)
        },
        ""
      )
      ifelse(exists("file_storage") && !is.null(file_storage),
        {
          cli::cli_li("storage")
          cli::cli_ul(string_file_storage)
        },
        ""
      )
      # Close container elements
      cli::cli_end()
    },

    # update file  details ----------------------------------------------------
    #' @description This call updates the name, the full set metadata, and tags
    #'  for a specified file.
    #' @param name The new name of the file.
    #' @param metadata The metadata fields and their values that you want to
    #' update. This is a named list of key-value pairs. The keys and values are
    #' strings.
    #' @param tags The tags you want to update, represented as unnamed list of
    #' values to add as tags.
    #' @param ... Additional parameters that can be passed to the method.
    #' @importFrom utils modifyList
    #' @importFrom checkmate assert_string
    update_details = function(name = NULL,
                              metadata = NULL,
                              tags = NULL, ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      check_tags(tags)
      check_metadata(metadata)

      body <- list(
        "name" = name,
        "tags" = tags,
        "metadata" = metadata
      )

      body <- body[!sapply(body, is.null)]
      if (length(body) == 0) {
        rlang::abort("Please provide updated information.")
      }

      res <- sevenbridges2::api(
        path = paste0("files/", self$id),
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      asFile(res, self$auth)
    },

    #' @description Delete method for File class.
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end cli_bullets
    delete = function() {
      res <- sevenbridges2::api(
        path = paste0("files/", self$id),
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url
      )

      if (res$status_code == 204) {
        rlang::inform(message = glue::glue(
          "File {self$id} has been deleted."
        ))
      } else if (res$status_code %in% c("401", "403", "404", "503")) {
        msg <- httr::content(res, as = "parsed")$message
        rlang::abort(glue::glue("HTTP Status {res$status_code} : {msg}"))
      }
    }
  )
)

# Helper function for creating File objects
asFile <- function(x, auth = NULL) {
  File$new(
    href = x$href,
    id = x$id,
    name = x$name,
    size = x$size,
    project = x$project,
    parent = x$parent,
    type = x$type,
    created_on = x$created_on,
    modified_on = x$modified_on,
    storage = x$storage,
    origin = x$origin,
    metadata = x$metadata,
    tags = x$tags,
    auth = auth,
    response = attr(x, "response")
  )
}

# Helper function for creating a list of File objects
asFileList <- function(x, auth) {
  obj <- lapply(x$items, asFile, auth = auth)
  obj
}
