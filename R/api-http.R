#' Core HTTP logic for Seven Bridges API
#'
#' Used for advanced users and the core method for higher level API
#' in this package.
#'
#' @param token API authentication token or `access_token` for
#'  Seven Bridges single sign-on.
#' @param path Path connected with `base_url`.
#' @param method One of `"GET"`, `"POST"`, `"PUT"`, `"DELETE"`, or `"PATCH"`.
#' @param query Query parameters passed to httr package GET/POST call.
#' @param body Body content passed to httr package GET/POST/PUT/DELETE/PATCH
#'  call.
#' @param encode If the body is a named list, how should it be
#'  encoded? Can be one of `"json"` (application/json),
#'  `"form"` (application/x-www-form-urlencoded),
#'  or `"multipart"` (multipart/form-data).
#'  Default is `"json"`.
#'  For `"multipart"`, list elements can be strings
#'  or objects created by [httr::upload_file()].
#'  For "form", elements are coerced to strings and escaped,
#'  use `I()` to prevent double-escaping.
#'  For `"json"`, parameters are automatically "unboxed"
#'  (i.e. length 1 vectors are converted to scalars). To preserve
#'  a length 1 vector as a vector, wrap in `I()`.
#' @param limit The maximum number of collection items to return
#'  for a single request. Minimum value is `1`.
#'  The maximum value is `100` and the default value is `50`.
#'  This is a pagination-specific attribute.
#' @param offset The zero-based starting index in the entire collection
#'  of the first item to return. The default value is `0`.
#'  This is a pagination-specific attribute.
#' @param advance_access Enable advance access features?
#'  Default is `FALSE`.
#' @param authorization Logical. Is the `token` an API
#'  authentication token (`FALSE`) or an access token from the
#'  Seven Bridges single sign-on (`TRUE`)?
#' @param fields Selector specifying a subset of fields to include in the
#'  response. All API calls take this optional query parameter.
#'  This parameter enables you to specify the fields you want to be returned
#'  when listing resources (e.g. all your projects) or getting details of a
#'  specific resource (e.g. a given project). \cr \cr
#'  For example, `fields="id,name,size"` to return the fields
#'  id, name and size for files. Default value is set to
#'  `_all`, so all fields are always returned for each resource.
#'  More details please check
#'  \url{https://docs.sevenbridges.com/docs/the-api#section-general-\n
#'  api-information}
#'
#' @param base_url Platform URL, default is NULL.
#' @param url Full url of the resource. If `url` is provided,
#'  other parameters like `base_url`, `path`, `query`, `limit`,
#'  `offset` and `fields` will be ignored.
#' @param ... Other arguments passed to GET/POST/PUT/DELETE/PATCH call.
#'
#' @return Response in form of a list.
#'
#' @references
#' <https://docs.sevenbridges.com/page/api>
#'
#' @importFrom  httr PUT DELETE PATCH status_code content handle_find
#'  add_headers parse_url
#' @importFrom  curl curl_escape
#' @importFrom rlang abort
#'
#' @export api
#' @examples
#' token <- "your_token"
#' # list projects
#' \dontrun{
#' api(token = token, path = "projects", method = "GET")
#' }
#'
api <- function(token = NULL, path = NULL,
                method = c("GET", "POST", "PUT", "DELETE", "PATCH"),
                query = NULL, body = list(),
                encode = c("json", "form", "multipart"),
                limit = getOption("sevenbridges2")$limit,
                offset = getOption("sevenbridges2")$offset,
                advance_access = getOption("sevenbridges2")$advance_access,
                authorization = FALSE,
                fields = "_all",
                base_url = NULL,
                url = NULL,
                ...) {
  if (is_missing(token)) rlang::abort("Token must be provided.")
  if (is_missing(base_url) && is_missing(url)) {
    rlang::abort("API address from the preferred platform must be provided or full url to the resource.") # nolint
  }

  method <- match.arg(method)
  encode <- match.arg(encode)

  if (is_missing(url)) {
    url <- paste0(base_url, path)
    check_limit(limit)
    check_offset(offset)

    # setup query
    query <- setup_query(
      query = query,
      limit = limit,
      offset = offset,
      fields = fields
    )
  } else {
    # nocov start
    parsed_url <- httr::parse_url(url)
    url <- paste0(
      parsed_url$scheme, "://",
      parsed_url$hostname, "/",
      parsed_url$path
    )
    query <- parsed_url$query
  } # nocov end

  # set headers
  headers <- set_headers(
    authorization = authorization,
    token = token,
    advance_access = advance_access
  )

  # setup body
  body <- setup_body(method = method, body = body)

  req <- switch(method,
    GET = {
      GET2(url,
        httr::add_headers(.headers = headers),
        query = query, ...
      )
    },
    # nocov start
    POST = {
      POST2(url,
        httr::add_headers(.headers = headers),
        query = query,
        body = body, encode = encode, ...
      )
    },
    PUT = {
      httr::PUT(url,
        httr::add_headers(.headers = headers),
        body = body, encode = encode, ...
      )
    },
    DELETE = {
      httr::DELETE(
        url,
        httr::add_headers(.headers = headers), ...
      )
    },
    PATCH = {
      httr::PATCH(url,
        httr::add_headers(.headers = headers),
        body = body,
        encode = encode, ...
      )
    }
    # nocov end
  )

  while (TRUE) {
    count <- 0
    for (i in seq_along(error_handlers)) {
      handled_response <- error_handlers[[i]](req)
      if (!setequal(req, handled_response)) {
        req <- handled_response
        break
      }
      count <- count + 1
    }
    if (count == 3) break
  }

  total_items <- req$headers[["x-total-matching-query"]]
  res <- status_check(req)
  res$total <- total_items

  return(res)
}
