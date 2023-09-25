#' Rate limit sleeper
#'
#' Pauses the execution if rate limit is breached.
#' @param req API request
#'
#' @return request Response object
#'
#' @importFrom httr rerequest
#' @importFrom glue glue_col
#' @importFrom rlang inform
#'
#' @noRd
rate_limit_sleeper <- function(req) {
  while (req$status_code == 429) {
    headers <- req$headers

    # Count amount of time to wait before next api call
    remaining_time <-
      parse_time(as.numeric(headers$`x-ratelimit-reset`))
    diff <-
      as.numeric(difftime(remaining_time, as.POSIXlt(Sys.time()), units = "secs")) # nolint
    sleep <- max(diff, 0)

    # Inform user about rate limit reached and amount of time they will wait
    rlang::inform(glue::glue_col("Rate limit reached! Waiting for {green {sleep}s}.")) # nolint
    Sys.sleep(sleep)

    req <- httr::rerequest(req)
  }
  return(req)
}

#' Maintenance sleeper
#'
#' Pauses the execution if sevenbridges api is under maintenance.
#' @param req API request
#' @param sleep Time to sleep in between the requests.
#'
#' @return request Response object
#'
#' @importFrom httr status_code rerequest
#' @importFrom glue glue_col
#' @importFrom rlang inform
#'
#' @noRd
maintenance_sleeper <- function(req, sleep = 300) {
  while (req$status_code == 503) {
    rlang::inform(glue::glue_col("Service unavailable: Response={green {req}}.")) # nolint

    response_body <- httr::content(req)

    if ("code" %in% names(response_body)) {
      if (response_body[["code"]] == 0) {
        rlang::inform(glue::glue_col(
          "API Maintenance in progress! Waiting for {green {sleep}s}."
        )) # nolint
        Sys.sleep(sleep)
        req <- httr::rerequest(req)
      }
    }
  }

  return(req)
}

#' General error sleeper
#'
#' Pauses the execution if response status code is > 500.
#' @param req API request
#'
#' @return request Response object
#' @importFrom httr status_code
#'
#' @noRd
general_error_sleeper <- function(req, sleep = 300) {
  while (req$status_code >= 500) {
    # Inform user about rate limit reached and amount of time they will wait
    rlang::inform(
      glue::glue_col(
        "Caught {green {req$status_code}} status code!
        Waiting for {green {sleep}s}."
      )
    ) # nolint
    Sys.sleep(sleep)
    req <- httr::rerequest(req)
  }
  return(req)
}

error_handlers <-
  list(
    rate_limit_sleeper,
    maintenance_sleeper,
    general_error_sleeper
  )
