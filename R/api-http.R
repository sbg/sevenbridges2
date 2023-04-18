#' Core HTTP logic for Seven Bridges API
#'
#' Used for advanced users and the core method for higher level API
#' in this package.
#'
#' @param token API auth token or `access_token` for
#' Seven Bridges single sign-on.
#' @param path path connected with `base_url`.
#' @param method one of `"GET"`, `"POST"`,
#' `"PUT"`, `"DELETE"`, or `"PATCH"`.
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE/PATCH call.
#' @param encode If the body is a named list, how should it be
#' encoded? Can be one of `"json"` (application/json),
#' `"form"` (application/x-www-form-urlencoded),
#' or `"multipart"` (multipart/form-data).
#' Default is `"json"`.
#' For `"multipart"`, list elements can be strings
#' or objects created by [httr::upload_file()].
#' For "form", elements are coerced to strings and escaped,
#' use `I()` to prevent double-escaping.
#' For `"json"`, parameters are automatically "unboxed"
#' (i.e. length 1 vectors are converted to scalars). To preserve
#' a length 1 vector as a vector, wrap in `I()`.
#' @param limit How many results to return
#' @param offset The point at which to start displaying them
#' @param advance_access Enable advance access features?
#' Default is `FALSE`.
#' @param authorization Logical. Is the `token` an API
#' auth token (`FALSE`) or an access token from the
#' Seven Bridges single sign-on (`TRUE`)?
#' @param fields All API calls take the optional query parameter fields.
#' This parameter enables you to specify the fields you want to be returned
#' when listing resources (e.g. all your projects) or getting details of a
#' specific resource (e.g. a given project). For example, fields="id,name,size"
#' to return the fields id, name and size for files. Default value is set to
#' '_all', so all fields are always returned for each resource.
#' More details please check
#' \url{https://docs.sevenbridges.com/docs/the-api#section-general-\n
#' api-information}
#'
#' @param base_url platform URL, default is NULL.
#' @param ... Other arguments passed to GET/POST/PUT/DELETE/PATCH call.
#'
#' @return returned request list of httr
#'
#' @references
#' <https://docs.sevenbridges.com/page/api>
#'
#' @importFrom  httr PUT DELETE PATCH status_code content handle_find
#' add_headers
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
                ...) {
  if (is_missing(token)) rlang::abort("token must be provided")
  if (is_missing(base_url)) {
    rlang::abort("API address from the preferred platform must be provided")
  }
  check_limit(limit)
  check_offset(offset)

  url <- paste0(base_url, path)
  method <- match.arg(method)
  encode <- match.arg(encode)

  # set headers
  headers <- set_headers(authorization = authorization, token = token)

  # setup query
  query <- setup_query(
    query = query,
    limit = limit,
    offset = offset,
    fields = fields
  )


  # setup body
  body <- setup_body(method = method, body = body)

  switch(method,
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
}
