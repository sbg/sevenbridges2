# nolint start
#' @title Match results by criteria
#'
#' @description Get results by criteria.
#'
#' @param x Value(s) to find.
#' @param y Data input to search through.
#' @param exact Should it be an exact match or partial, default TRUE.
#' @param ignore.case Should it ignore cases, or not, default TRUE.
#'
#' @return Index of the matched element from the data provided.
#'
#' @noRd
m.fun <- function(x,
                  y,
                  exact = TRUE,
                  ignore.case = TRUE,
                  ...) {
  if (exact) {
    res <- pmatch(x, y, ...)
  } else {
    res <- unlist(sapply(x, function(i) {
      grep(i, y, ignore.case = ignore.case)
    }))
    if (is.matrix(res)) {
      res <- res[, 1]
    }
  }

  res
}

#' @title Match results by id and/or name
#'
#' @description Get results by id and/or name.
#'
#' @param obj Results list.
#' @param id Id of the resource.
#' @param name Name of the resource.
#' @param .id Name of the 'id' argument, set to 'id'.
#' @param .name Name of the 'name' argument, set to 'name'.
#' @param exact Should it be an exact match or partial, default TRUE.
#' @param ignore.case Should it ignore cases, or not, default TRUE.
#'
#' @return Subset of the result matching id or name.
#'
#' @noRd
m.match <- function(obj,
                    id = NULL,
                    name = NULL,
                    .id = "id",
                    .name = "name",
                    exact = TRUE,
                    ignore.case = TRUE) {
  # if no match, return whole list
  if (is.null(id)) {
    if (is.null(name)) {
      if (length(obj) == 1) {
        return(obj[[1]])
      } else {
        return(obj)
      }
    } else {
      # id is null, use name
      nms <- sapply(obj, function(x) {
        x[[.name]]
      })
      if (ignore.case) {
        name <- tolower(name)
        nms <- tolower(nms)
      }
      index <- m.fun(name, nms,
        exact = exact,
        ignore.case = ignore.case
      )
    }
  } else {
    # id is not NULL
    ids <- sapply(obj, function(x) {
      x[[.id]]
    })
    index <- m.fun(id, ids,
      exact = exact,
      ignore.case = ignore.case
    )
  }

  if (length(index) == 1 && is.na(index)) {
    message("Sorry, no matching.")
    return(NULL)
  } else {
    if (length(index) == 1) {
      obj[[index]]
    } else {
      obj[index]
    }
  }
}

#' @title Parse time to POSIXlt
#'
#' @description Parse time to POSIXlt for rate limit expiration datetime.
#'
#' @param reset_time_as_unix_epoch Time received from response.
#' @param origin Origin time as reference, default to "1970-01-01".
#' @param time_zone Time zone as reference.
#' @param use_milliseconds Does unix timestamp contain information about
#' milliseconds (default is FALSE).
#'
#' @importFrom rlang abort
#'
#' @noRd
parse_time <- function(reset_time_as_unix_epoch,
                       origin = "1970-01-01",
                       time_zone = "",
                       use_milliseconds = FALSE) {
  if (is_missing(reset_time_as_unix_epoch)) {
    return("unknown")
  }

  checkmate::assert_numeric(reset_time_as_unix_epoch)
  checkmate::assert_string(origin)
  checkmate::assert_string(time_zone)
  checkmate::assert_logical(use_milliseconds)

  if (use_milliseconds) {
    reset_time_as_unix_epoch <- reset_time_as_unix_epoch / 1000
  }
  reset_time_as_posixlt <- as.POSIXlt(reset_time_as_unix_epoch,
    origin = "1970-01-01", tz = time_zone
  )
  reset_date_time <- as.character(reset_time_as_posixlt)
  reset_time_zone <- reset_time_as_posixlt$zone
  return(paste0(reset_date_time, " ", reset_time_zone))
}

#' @description Customize underlying http logic for handle_url2.
#'
#' @param handle Handle.
#' @param url URL.
#' @param ... Additional arguments to pass.
#'
#' @importFrom utils modifyList
#' @importFrom rlang abort
#'
#' @noRd
handle_url2 <- function(handle = NULL, url = NULL, ...) {
  if (is.null(url) && is.null(handle)) {
    rlang::abort("Must specify at least one of url or handle.")
  }
  if (is.null(handle)) {
    handle <- httr::handle_find(url)
  }
  if (is.null(url)) {
    url <- handle$url
  }
  # workaround to bypass `:::` checks
  new <- eval(parse(text = "httr:::named(list(...))"))
  if (length(new) > 0 ||
    eval(parse(text = "httr:::is.url(url)"))) {
    old <- httr::parse_url(url)
    url <- build_url2(utils::modifyList(old, new))
  }

  list(handle = handle, url = url)
}

#' @description Customize underlying http logic for build_url2.
#'
#' @param url URL.
#'
#' @noRd
build_url2 <- function(url) {
  stopifnot(eval(parse(text = "httr:::is.url(url)")))
  scheme <- url$scheme
  hostname <- url$hostname
  if (!is.null(url$port)) {
    port <- paste0(":", url$port)
  } else {
    port <- NULL
  }
  path <- url$path
  if (!is.null(url$params)) {
    params <- paste0(";", url$params)
  } else {
    params <- NULL
  }
  if (is.list(url$query)) {
    url$query <- eval(parse(text = "httr:::compact(url$query)"))
    names <- curl::curl_escape(names(url$query))
    values <- as.character(url$query)
    query <- paste0(names, "=", values, collapse = "&")
  } else {
    query <- url$query
  }
  if (!is.null(query)) {
    stopifnot(is.character(query), length(query) == 1)
    query <- paste0("?", query)
  }
  if (is.null(url$username) && !is.null(url$password)) {
    rlang::abort("Cannot set password without username")
  }

  paste0(
    scheme,
    "://",
    url$username,
    if (!is.null(url$password)) {
      ":"
    },
    url$password,
    if (!is.null(url$username)) {
      "@"
    },
    hostname,
    port,
    "/",
    path,
    params,
    query,
    if (!is.null(url$fragment)) {
      "#"
    },
    url$fragment
  )
}

#' @description Customize underlying http logic for GET2.
#'
#' @param url URL.
#' @param config Configuration parameters.
#' @param handle How to handle URL.
#' @param ... Additional arguments to pass.
#'
#' @noRd
GET2 <- function(url = NULL,
                 config = list(),
                 ...,
                 handle = NULL) {
  # nocov start
  hu <- handle_url2(handle, url, ...)
  req <-
    eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

  return(eval(
    parse(text = "httr:::request_perform(req, hu$handle$handle)")
  ))
  # nocov end
}

#' @description Customize underlying http logic for POST2.
#'
#' @param url URL.
#' @param config Configuration parameters.
#' @param handle How to handle URL.
#' @param body Request body.
#' @param encode Encoding, can be one of: "json", "form", "multipart".
#' @param ... Additional arguments to pass.
#'
#' @noRd
POST2 <- function(url = NULL,
                  config = list(),
                  ...,
                  body = NULL,
                  encode = c("json", "form", "multipart"),
                  handle = NULL) {
  # nocov start
  encode <- match.arg(encode)
  hu <- handle_url2(handle, url, ...)
  req <-
    eval(
      parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)')
    )

  return(eval(
    parse(text = "httr:::request_perform(req, hu$handle$handle)")
  ))
  # nocov end
}


#' @title Flatten query parameters
#'
#' @description A httr query parameter can only have one value per name.
#'  This function takes any values that contain length > 1 vectors/lists
#'  and splits them up such that, for example, list(x = 1:2, y= "a") becomes
#'  list(x = 1, x = 2, y = "a").
#'
#' @param x List of query parameters.
#'
#' @return Flattened query params list.
#'
#' @importFrom checkmate test_atomic
#'
#' @noRd
flatten_query <- function(x) {
  if (all(sapply(x, checkmate::test_atomic)) &&
    all(lengths(x) <= 1)) {
    return(x)
  }
  do.call(
    "c",
    mapply(
      function(name, val) {
        x <- as.list(val)
        names(x) <- rep(name, length(val))
        x
      },
      names(x),
      x,
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )
  )
}

#' @title Set client info for API request header (User-Agent data)
#'
#' @description This function returns client info that will be stored
#' in headers for API requests, in order to track logs better.
#'
#' @importFrom utils packageDescription sessionInfo
#'
#' @noRd
setup_client_info <- function() {
  # Fill client data
  package_version <- paste0(suppressWarnings(
    utils::packageDescription("sevenbridges2", fields = c("Package", "Version"))
  ), collapse = "/")
  client_session_info <- utils::sessionInfo()
  client_os <- client_session_info$running
  client_platform <- client_session_info$platform
  client_r <- client_session_info$R.version$version.string

  client_info_list <- list(
    client_os = client_os,
    client_platform = client_platform,
    R = client_r
  )
  client_info_string <- glue::glue(
    "{package_version} (",
    glue::glue_collapse(
      glue::glue("{client_info_list}"),
      sep = "; "
    ),
    ")"
  )
  return(client_info_string)
}

#' @title Set headers for API request
#'
#' @description This function returns headers for API request,
#'  depending on the value of the authorization parameter.
#'
#' @param authorization Is the `token` an API
#'  auth token (`FALSE`) or an access token from the
#'  Seven Bridges single sign-on (`TRUE`)?
#' @param token API auth token or `access_token` for
#'  Seven Bridges single sign-on.
#' @param advance_access Enable advance access features?
#' Default is `FALSE`.
#' @param client_info Client info that will be send in the header.
#'
#' @importFrom checkmate assert_logical
#'
#' @return A named vector with headers for an API request.
#'
#' @noRd
set_headers <- function(authorization = FALSE,
                        token = NULL,
                        advance_access = getOption("sevenbridges2")$advance_access,
                        client_info = NULL) {
  if (is_missing(token)) {
    rlang::abort("Token is missing.")
  }
  checkmate::assert_logical(authorization, len = 1, null.ok = FALSE)
  checkmate::assert_logical(advance_access, len = 1, null.ok = FALSE)

  if (authorization) {
    headers <-
      c("Authorization" = paste("Bearer", token, sep = " ")) # nocov
  } else {
    headers <- c(
      "X-SBG-Auth-Token" = token,
      "Accept" = "application/json",
      "Content-Type" = "application/json",
      "User-Agent" = client_info
    )
  }

  # add optional advance access flag
  if (advance_access) {
    headers <- c(headers, "X-SBG-advance-access" = "advance")
  }

  return(headers)
}

#' @title Setup query parameters for API request
#'
#' @description This function prepares query parameters for API request.
#'
#' @param query Passed to httr package GET/POST call
#' @param limit The maximum number of collection items to return
#'  for a single request. Minimum value is `1`.
#'  The maximum value is `100` and the default value is `50`.
#'  This is a pagination-specific attribute.
#' @param offset The zero-based starting index in the entire collection
#'  of the first item to return. The default value is `0`.
#'  This is a pagination-specific attribute.
#' @param fields Selector specifying a subset of fields to include in the
#'  response. All API calls take this optional query parameter.
#'  This parameter enables you to specify the fields you want to be returned
#'  when listing resources (e.g. all your projects) or getting details of a
#'  specific resource (e.g. a given project). \cr \cr
#'  For example, `fields="id,name,size"` to return the fields
#'  id, name and size for files. Default value is set to
#'  `_all`, so all fields are always returned for each resource.
#'  More details please check
#'  [here](https://docs.sevenbridges.com/docs/the-api#section-general-api-information).
#'
#' @return List of query parameters.
#'
#' @noRd
setup_query <- function(query = NULL,
                        limit = getOption("sevenbridges2")$limit,
                        offset = getOption("sevenbridges2")$offset,
                        fields = NULL) {
  checkmate::test_list(query, null.ok = TRUE)
  checkmate::test_string(fields)

  # flatten and append query parameters
  query <-
    c(
      flatten_query(query),
      limit = as.integer(limit),
      offset = as.integer(offset),
      flatten_query(list(fields = fields))
    )

  idx <- !sapply(query, is.null)
  if (any(idx)) {
    query <- query[idx]
  } else {
    query <- NULL
  }

  return(query)
}

#' @title Setup body parameters for API request
#'
#' @description This function prepares body parameters for API request.
#'
#' @param method HTTP method to be used in the request.
#' @param body  HTTP request body - passed to httr package GET/POST/PUT/DELETE/PATCH call.
#'
#' @return Request body as JSON.
#'
#' @noRd
setup_body <- function(method, body = list()) {
  if (method %in% c("POST", "PATCH", "PUT")) {
    if (!is.list(body)) {
      rlang::abort("Body should be a list.")
    }
    if (length(body) == 0) {
      # Specific handling of POST with emtpy body
      return("{}")
    }
    body <-
      jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  }
  return(body)
}
# nolint end

#' @title Check and transform id
#'
#' @description Generic way to check if the supplied argument is an instance
#'  of a given class or ID is directly specified as a string.
#'  In the first case, ID is extracted from specified field
#'  of object and returned as string.
#'
#' @param x String with id or an instance of specified class.
#' @param class_name Expected class of instance from which ID will be extracted.
#' @param field_name Field name of to extract ID from. Default is "id".
#'
#' @return String with id.
#'
#' @importFrom checkmate assert_r6 assert_character
#'
#' @noRd
check_and_transform_id <-
  function(x, class_name, field_name = "id") {
    if (inherits(x, "R6")) {
      checkmate::assert_r6(
        x,
        classes = class_name,
        null.ok = TRUE,
        .var.name = checkmate::vname(x)
      )
      id <- x[[field_name]]
    } else {
      checkmate::assert_character(x,
        null.ok = FALSE,
        .var.name = checkmate::vname(x)
      )
      id <- x
    }
    return(id)
  }

#' @title Extract common query parameters
#'
#' @description Extract default values for common query params like
#'  limit, offset, etc. If they don't exist in the provided list of params
#'  or return the found value.
#'
#' @return Parameter value.
#'
#' @noRd
extract_common_query_params <- function(args, param_name) {
  if (!is_missing(args[[param_name]])) {
    param_value <- args[[param_name]]
  } else {
    param_value <- getOption("sevenbridges2")[[param_name]]
  }
  return(param_value)
}
