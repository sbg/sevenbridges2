#' Core HTTP logic for Seven Bridges API
#'
#' Core HTTP logic for Seven Bridges API
#'
#' Used for advanced users and the core method for higher level API
#' in this package.
#'
#' @param token API auth token or \code{access_token} for
#' Seven Bridges single sign-on.
#' @param version API version number, default is \code{v2}.
#' @param path path connected with \code{base_url}.
#' @param method one of \code{"GET"}, \code{"POST"},
#' \code{"PUT"}, \code{"DELETE"}, or \code{"PATCH"}.
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE/PATCH call.
#' @param encode If the body is a named list, how should it be
#' encoded? Can be one of \code{"json"} (application/json),
#' \code{"form"} (application/x-www-form-urlencoded),
#' or \code{"multipart"} (multipart/form-data).
#' Default is \code{"json"}.
#' For \code{"multipart"}, list elements can be strings
#' or objects created by \code{\link[httr]{upload_file}}.
#' For "form", elements are coerced to strings and escaped,
#' use \code{I()} to prevent double-escaping.
#' For \code{"json"}, parameters are automatically "unboxed"
#' (i.e. length 1 vectors are converted to scalars). To preserve
#' a length 1 vector as a vector, wrap in \code{I()}.
#' @param limit How many results to return
#' @param offset The point at which to start displaying them
#' @param advance_access Enable advance access features?
#' Default is \code{FALSE}.
#' @param authorization Logical. Is the \code{token} an API
#' auth token (\code{FALSE}) or an access token from the
#' Seven Bridges single sign-on (\code{TRUE})?
#' @param fields All API calls take the optional query parameter fields.
#' This parameter enables you to specify the fields you want to be returned
#' when listing resources (e.g. all your projects) or getting details of a
#' specific resource (e.g. a given project). For example, fields="id,name,size"
#' to return the fields id, name and size for files. More details please check
#' \url{https://docs.sevenbridges.com/docs/the-api#section-general-api-information}
#'
#' @param base_url default is \code{"https://api.sbgenomics.com/v2"}
#' @param ... Other arguments passed to GET/POST/PUT/DELETE/PATCH call.
#'
#' @return returned request list of httr
#'
#' @references
#' \url{https://docs.sevenbridges.com/page/api}
#'
#' @importFrom  httr PUT DELETE PATCH status_code content handle_find add_headers
#' @importFrom curl curl_escape
#'
#' @export api
#' @examples
#' token <- "your_token"
#' # list projects
#' \dontrun{
#' api(token = token, path = "projects", method = "GET")
#' }
api <- function(token = NULL, version = "v2", path = NULL,
                method = c("GET", "POST", "PUT", "DELETE", "PATCH"),
                query = NULL, body = list(),
                encode = c("json", "form", "multipart"),
                limit = getOption("sevenbridges2")$limit,
                offset = getOption("sevenbridges2")$offset,
                advance_access = getOption("sevenbridges2")$advance_access,
                authorization = FALSE,
                fields = NULL,
                #base_url = paste0("https://api.sbgenomics.com/", version, "/"),
                base_url = NULL,
                ...) {
  if (is.null(token)) stop("token must be provided")

  method <- match.arg(method)
  encode <- match.arg(encode)

  if (authorization) {
    headers <- c("Authorization" = paste("Bearer", token, sep = " "))
  } else {
    headers <- c(
      "X-SBG-Auth-Token" = token,
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    )
  }

  # add optional advance access flag
  if (advance_access) headers <- c(headers, "X-SBG-advance-access" = "advance")

  # setup query
  query <- c(query, list(limit = limit, offset = offset, fields = fields))
  idx <- !sapply(query, is.null)
  if (any(idx)) {
    query <- query[idx]
  } else {
    query <- NULL
  }

  switch(method,
    GET = {
      GET2(paste0(base_url, path),
        httr::add_headers(.headers = headers),
        query = query, ...
      )
    },
    POST = {
      stopifnot(is.list(body))
      body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
      POST2(paste0(base_url, path),
        httr::add_headers(.headers = headers),
        query = query,
        body = body, encode = encode, ...
      )
    },
    PUT = {
      stopifnot(is.list(body))
      body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
      httr::PUT(paste0(base_url, path),
        httr::add_headers(.headers = headers),
        body = body, encode = encode, ...
      )
    },
    DELETE = {
      httr::DELETE(
        paste0(base_url, path),
        httr::add_headers(.headers = headers), ...
      )
    },
    PATCH = {
      stopifnot(is.list(body))
      body <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
      httr::PATCH(paste0(base_url, path),
        httr::add_headers(.headers = headers),
        body = body,
        encode = encode, ...
      )
    }
  )
}
