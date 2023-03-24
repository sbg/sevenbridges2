# nolint start
#' Match results by criteria
#'
#' Get results by criteria
#' @param x value(s) to find
#' @param y data input to search through
#' @param exact should it be an exact match or partial, default TRUE
#' @param ignore.case should it ignore cases, or not, default TRUE
#'
#'
#' @return index of the matched element from the data provided
#'
#' @keywords internal
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
#'
#' @return subset of the result matching id or name
#'
#' @keywords internal
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

#' Check request status
#'
#' Check request status
#' @param req API request
#' @param as desired type output (contents of a request): raw, text or parsed
#'
#' @return request content or the message
#' @importFrom httr status_code
#'
#' @keywords internal
status_check <- function(req, as = "parsed", ...) {
  if (httr::status_code(req) %in% c("200", "201", "202", "204")) {
    # Check this !!!
    if (!is.null(req$request$headers["X-SBG-Auth-Token"])) {
      req$request$headers["X-SBG-Auth-Token"] <- "<your_auth_token>"
    }
    res <- httr::content(req, as = as, ...)
    if (!is.null(res)) {
      attr(res, "response") <- req
    }
    return(res)
  } else if (httr::status_code(req) %in% c("401", "403", "404", "503")) {
    msg <- httr::content(req, as = as, ...)$message
    stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
  } else {
    if ("message" %in% names(content(req, as = as, ...))) {
      msg <- httr::content(req, as = as, ...)$message
    } else {
      msg <- NULL
    }

    if (is.null(msg)) {
      if (httr::status_code(req) %in% names(codes)) {
        msg <- codes[[httr::status_code(req)]]
      }
      if (is.null(msg)) {
        print(content(req, as = as, ...))
        stop(paste("Error of unknown type occured", httr::status_code(req)))
      } else {
        stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
      }
    } else {
      stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
    }
  }
}


parse_time <- function(reset_time_as_unix_epoch, origin = "1970-01-01",
                       time_zone = "") {
  if (is_missing(reset_time_as_unix_epoch)) return("unknown")
  reset_time_as_posixlt <- as.POSIXlt(reset_time_as_unix_epoch,
                                      origin = "1970-01-01", tz = ""
  )
  reset_date_time <- as.character(reset_time_as_posixlt)
  reset_time_zone <- reset_time_as_posixlt$zone
  return(paste0(reset_date_time, " ", reset_time_zone))
}


# customize underlying http logic
# (handle_url2, build_url2, GET2, POST2)
#' @importFrom utils modifyList
handle_url2 <- function(handle = NULL, url = NULL, ...) {
  if (is.null(url) && is.null(handle)) {
    stop("Must specify at least one of url or handle")
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
    stop("Cannot set password without username")
  }

  paste0(scheme, "://", url$username, if (!is.null(url$password)) {
    ":"
  }, url$password, if (!is.null(url$username)) {
    "@"
  }, hostname, port, "/", path, params, query, if (!is.null(url$fragment)) {
    "#"
  }, url$fragment)
}

GET2 <- function(url = NULL, config = list(), ..., handle = NULL) {
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}

POST2 <- function(url = NULL, config = list(), ...,
                  body = NULL, encode = c("json", "form", "multipart"),
                  multipart = TRUE, handle = NULL) {
  if (!missing(multipart)) {
    warning("multipart is deprecated, please use encode argument instead",
      call. = FALSE
    )
    encode <- ifelse(multipart, "multipart", "form")
  }

  encode <- match.arg(encode)
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}
# nolint end
