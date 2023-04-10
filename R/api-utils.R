# nolint start
#' Match results by criteria
#'
#' Get results by criteria
#' @param x value(s) to find
#' @param y data input to search through
#' @param exact should it be an exact match or partial, default TRUE
#' @param ignore.case should it ignore cases, or not, default TRUE
#'
#' @return index of the matched element from the data provided
#'
#' @noRd
m.fun <- function(x, y, exact = TRUE, ignore.case = TRUE, ...) {
  if (exact) {
    res <- pmatch(x, y, ...)
  } else {
    res <- unlist(sapply(x, function(i) {
      grep(i, y, ignore.case = ignore.case)
    }))
    if (is.matrix(res)) res <- res[, 1]
  }

  res
}

#' Match results by id and/or name
#'
#' Get results by id and/or name
#' @param obj results list
#' @param id id of the resource
#' @param name name of the resource
#' @param .id name of the 'id' argument, set to 'id'
#' @param .name name of the 'name' argument, set to 'name'
#' @param exact should it be an exact match or partial, default TRUE
#' @param ignore.case should it ignore cases, or not, default TRUE
#'
#' @return subset of the result matching id or name
#'
#' @noRd
m.match <- function(obj,
                    id = NULL, name = NULL,
                    .id = "id", .name = "name",
                    exact = TRUE, ignore.case = TRUE) {
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
      nms <- sapply(obj, function(x) x[[.name]])
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
    ids <- sapply(obj, function(x) x[[.id]])
    index <- m.fun(id, ids,
      exact = exact,
      ignore.case = ignore.case
    )
  }

  if (length(index) == 1 && is.na(index)) {
    message("sorry, no matching ")
    return(NULL)
  } else {
    if (length(index) == 1) {
      obj[[index]]
    } else {
      obj[index]
    }
  }
}

#' Parse time to POSIXlt for rate limit expiration datetime
#'
#' @param reset_time_as_unix_epoch time received from response
#' @param origin origin time as reference, default to "1970-01-01"
#' @param time_zone time_zone as reference
#' @param use_milliseconds does unix timestamp contain information about
#' milliseconds (default is FALSE)
#'
#' @noRd
parse_time <- function(reset_time_as_unix_epoch, origin = "1970-01-01",
                       time_zone = "", use_milliseconds = FALSE) {
  if (is_missing(reset_time_as_unix_epoch)) {
    return("unknown")
  }
  if (use_milliseconds) {
    reset_time_as_unix_epoch <- reset_time_as_unix_epoch / 1000
  }
  reset_time_as_posixlt <- as.POSIXlt(reset_time_as_unix_epoch,
    origin = "1970-01-01", tz = ""
  )
  reset_date_time <- as.character(reset_time_as_posixlt)
  reset_time_zone <- reset_time_as_posixlt$zone
  return(paste0(reset_date_time, " ", reset_time_zone))
}

#' Customize underlying http logic for handle_url2
#'
#' @param handle handle
#' @param url url
#' @param ... additional arguments to pass
#' @importFrom utils modifyList
#' @importFrom rlang abort
#'
#' @noRd
handle_url2 <- function(handle = NULL, url = NULL, ...) {
  if (is.null(url) && is.null(handle)) {
    rlang::abort("Must specify at least one of url or handle")
  }
  if (is.null(handle)) handle <- httr::handle_find(url)
  if (is.null(url)) url <- handle$url
  # workaround to bypass `:::` checks
  new <- eval(parse(text = "httr:::named(list(...))"))
  if (length(new) > 0 || eval(parse(text = "httr:::is.url(url)"))) {
    old <- httr::parse_url(url)
    url <- build_url2(utils::modifyList(old, new))
  }

  list(handle = handle, url = url)
}

#' Customize underlying http logic for build_url2
#'
#' @param url url
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

  paste0(scheme, "://", url$username, if (!is.null(url$password)) {
    ":"
  }, url$password, if (!is.null(url$username)) {
    "@"
  }, hostname, port, "/", path, params, query, if (!is.null(url$fragment)) {
    "#"
  }, url$fragment)
}

#' Customize underlying http logic for GET2
#'
#' @param url url
#' @param config config params
#' @param handle how to handle url
#' @param ... additional args to pass
#'
#' @noRd
GET2 <- function(url = NULL, config = list(), ..., handle = NULL) {
  # nocov start
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
  # nocov end
}

#' Customize underlying http logic for POST2
#'
#' @param url url
#' @param config config params
#' @param handle how to handle url
#' @param body request body
#' @param encode encoding, can be one of: "json", "form", "multipart"
#' @param ... additional args to pass
#'
#' @noRd
POST2 <- function(url = NULL, config = list(), ...,
                  body = NULL, encode = c("json", "form", "multipart"),
                  handle = NULL) {
  # nocov start

  encode <- match.arg(encode)
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
  # nocov end
}


#' Flatten query parameters
#'
#' @description A httr query parameter can only have one value per name.
#' This function takes any values that contain lenght > 1 vectors/lists
#' and splits them up such that, for example, list(x=1:2, y="a") becomes
#' list(x=1, x=2, y="a").
#'
#' @param x List of query parameters.
#' @return Flattened query params list.
#'
#' @noRd
flatten_query <- function(x) {
  if (all(sapply(x, is.atomic)) && all(lengths(x) <= 1)) {
    return(x)
  }
  do.call("c", mapply(function(name, val) {
    x <- as.list(val)
    names(x) <- rep(name, length(val))
    x
  }, names(x), x, USE.NAMES = FALSE, SIMPLIFY = FALSE))
}




#' Set headers for API request
#' @description This function returns headers for API request,
#' depending on the value of the authorization parameter.
#' @param authorization Logical. Is the \code{token} an API
#' auth token (\code{FALSE}) or an access token from the
#' Seven Bridges single sign-on (\code{TRUE})?
#' @param token API auth token or \code{access_token} for
#' Seven Bridges single sign-on.
#' @param advance_access Enable advance access features?
#' Default is \code{FALSE}.
#'
#' @return A named vector with headers for an API request.
#' @noRd
set_headers <- function(authorization = FALSE, token = NULL, advance_access = getOption("sevenbridges2")$advance_access) {
  if (is_missing(token)) {
    rlang::abort("Token is missing.")
  }

  if (authorization) {
    headers <- c("Authorization" = paste("Bearer", token, sep = " ")) # nocov
  } else {
    headers <- c(
      "X-SBG-Auth-Token" = token,
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    )
  }

  # add optional advance access flag
  if (advance_access) headers <- c(headers, "X-SBG-advance-access" = "advance")

  return(headers)
}


#' Setup query parameters for API request
#' @description This function prepares query parameters for API request.
#' @param query Passed to httr package GET/POST call
#' @param limit How many results to return
#' @param offset The point at which to start displaying them
#' @param fields All API calls take the optional query parameter fields.
#' This parameter enables you to specify the fields you want to be returned
#' when listing resources (e.g. all your projects) or getting details of a
#' specific resource (e.g. a given project). For example, fields="id,name,size"
#' to return the fields id, name and size for files. More details please check
#' \url{https://docs.sevenbridges.com/docs/the-api#section-general-\n
#' api-information}
#'
#' @return List of query parameters.
#' @noRd
setup_query <- function(query = NULL, limit = getOption("sevenbridges2")$limit, offset = getOption("sevenbridges2")$offset, fields = NULL) {
  # flatten and append query parameters
  query <- c(flatten_query(query), limit = as.integer(limit), offset = as.integer(offset), flatten_query(list(fields = fields)))

  idx <- !sapply(query, is.null)
  if (any(idx)) {
    query <- query[idx]
  } else {
    query <- NULL
  }

  return(query)
}

#' Setup body parameters for API request
#' @description This function prepares body parameters for API request.
#' @param method HTTP method to be used in the request.
#' @param body  HTTP request body - passed to httr package GET/POST/PUT/DELETE/PATCH call.
#'
#' @return Request body as JSON.
#' @noRd
setup_body <- function(method, body = list()) {
  if (method %in% c("POST", "PATCH", "PUT")) {
    # stopifnot(is.list(body))
    if (!is.list(body)) {
      rlang::abort("Body should be a list.")
    }
    body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  }
  return(body)
}
# nolint end
