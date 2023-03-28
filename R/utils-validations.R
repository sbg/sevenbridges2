# Utils-validations
#' Check request status
#'
#' Check request status
#' @param req API request
#' @param as desired type output (contents of a request): raw, text or parsed
#'
#' @return request content or the message
#' @importFrom httr status_code
#'
#' @noRd
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

#' Check if input value is missing
#'
#' @param input value to check
#' @noRd
is_missing <- function(input) {
  isTRUE(
    checkmate::test_scalar_na(input, null.ok = TRUE) ||
      input == "" ||
      length(input) == 0
  )
}

#' Check limit parameter
#'
#' @param limit limit value
#' @noRd
check_limit <- function(limit) {
  msg <- "Limit must be integer number between 1 and 100."
  if (!is.numeric(limit)) {
    rlang::abort(msg)
  }
  limit_cast <- suppressWarnings(as.integer(limit))
  if (is_missing(limit_cast)) {
    rlang::abort(msg)
  }
  if (limit_cast > 100 || limit_cast <= 0) {
    rlang::abort(msg)
  }
}

#' Check offset parameter
#'
#' @param offset offset value
#' @noRd
check_offset <- function(offset) {
  msg <- "Offset must be integer number >= 0."
  if (!is.numeric(offset)) {
    rlang::abort(msg)
  }
  offset_cast <- suppressWarnings(as.integer(offset))
  if (is_missing(offset_cast)) {
    rlang::abort(msg)
  }
  if (offset_cast < 0) {
    rlang::abort(msg)
  }
}
