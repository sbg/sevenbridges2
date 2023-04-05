testthat::test_that("Utility function parse_time works (seconds)", {
  # unix timestamp doesn't contain the information about milliseconds
  test_unix_timestamp <- 1489700093

  human_readable_date <- parse_time(test_unix_timestamp,
    origin = "1970-01-01",
    time_zone = ""
  )

  testthat::expect_equal(human_readable_date, "2017-03-16 22:34:53 CET",
    label = "Epoch conversion to human-readable date went wrong."
  )
})

testthat::test_that("Utility function parse_time works (milliseconds)", {
  # unix timestamp contains the information about milliseconds
  test_unix_timestamp <- 2555971200000

  human_readable_date <- parse_time(test_unix_timestamp,
    origin = "1970-01-01",
    time_zone = "", use_milliseconds = TRUE
  )

  testthat::expect_equal(human_readable_date, "2050-12-30 01:00:00 CET",
    label = "Epoch conversion to human-readable date went wrong."
  )
})

testthat::test_that("Utility function parse_time works (unknown)", {
  # unix timestamp is missing
  test_unix_timestamp <- NA

  human_readable_date <- parse_time(test_unix_timestamp,
    origin = "1970-01-01",
    time_zone = "", use_milliseconds = TRUE
  )

  testthat::expect_equal(human_readable_date, "unknown",
    label = "Epoch conversion to human-readable date went wrong."
  )
})

testthat::test_that("Utility function flatten_query works", {
  # Load predefined unflattened query params list
  unflattened_query_params_list <- readRDS(testthat::test_path(
    "test_data",
    "query_params_list.RDS"
  ))

  # Use the flatten_query function
  flattened_query_params_list <- flatten_query(unflattened_query_params_list)

  # Define the expected output
  expected_resulting_list <- list(
    limit = 50,
    offset = 0,
    fields = "created_by",
    fields = "name",
    fields = "id"
  )

  keys <- names(flattened_query_params_list)

  # Compare two lists
  testthat::expect_equal(
    flattened_query_params_list[keys],
    expected_resulting_list[keys]
  )
})

testthat::test_that("Utility function handle_url2 works", {
  # Check call without url and handle
  err <- testthat::expect_error(handle_url2(url = NULL, handle = NULL))
  testthat::expect_equal(
    err$message,
    "Must specify at least one of url or handle"
  )

  # Test output - url provided
  result <- handle_url2(
    url = "https://api.sbgenomics.com/v2/user/",
    query = list(limit = 50, offset = 50)
  )
  testthat::expect_true(!is.null(result$handle))
  testthat::expect_true(checkmate::test_class(result$handle,
    classes = c("handle")
  ))
  testthat::expect_true(checkmate::test_class(result$handle$handle,
    classes = c("curl_handle")
  ))
  testthat::expect_true(!is.null(result$url))
  testthat::expect_equal(
    result$url,
    "https://api.sbgenomics.com/v2/user/?limit=50&offset=50"
  )

  # nolint start

  # Load auth object
  # loaded_handle_obj <- readRDS(testthat::test_path("test_data", "handle_object.RDS"))
  #
  # result <- handle_url2(handle = loaded_handle_obj, query = list(limit = 50, offset = 50))
  #
  # testthat::expect_true(!is.null(result$handle))
  # testthat::expect_true(checkmate::test_class(result$handle,
  #                                             classes = c("handle")
  # ))
  # testthat::expect_true(checkmate::test_class(result$handle$handle,
  #                                             classes = c("curl_handle")
  # ))
  # testthat::expect_true(!is.null(result$url))
  # testthat::expect_equal(result$url, "https://api.sbgenomics.com/?limit=50&offset=50")

  # nolint end
})

testthat::test_that("Utility function build_url2 works", {
  # test build_url2 output
  url_test_object <- readRDS(testthat::test_path(
    "test_data",
    "url_test_object.RDS"
  ))

  # apply build_url2 function to generate final url
  resulting_url <- build_url2(url_test_object)

  testthat::expect_equal(
    resulting_url,
    "https://api.sbgenomics.com/v2/user/?limit=50&offset=0"
  )


  # check if the function throws an error if the provided url object contains
  # password without username
  url_test_object <- readRDS(testthat::test_path(
    "test_data",
    "url_test_object_with_password_without_username.RDS"
  ))

  # apply build_url2 function to generate final url
  err <- testthat::expect_error(build_url2(url_test_object))
  testthat::expect_equal(err$message, "Cannot set password without username")
})

testthat::test_that("Utility function set_headers works
                    when authorization = FALSE", {
  token <- stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

  # Test set_headers when authorization parameter is FALSE (default)
  headers <- set_headers(token = token)

  testthat::expect_equal(typeof(headers), "character",
    label = glue::glue("Headers should be a vector of characters, not
                       {typeof(headers)}.")
  )
  testthat::expect_equal(length(headers),
    3L,
    label = "Headers vector should have three elements: X-SBG-Auth-Token,
    Accept and Content-Type"
  )
  testthat::expect_equal(names(headers), c(
    "X-SBG-Auth-Token", "Accept",
    "Content-Type"
  ),
  label = "Elements in headers vector do not have expected names."
  )
  testthat::expect_equal(unname(headers), c(
    token, "application/json",
    "application/json"
  ),
  label = "Headers elements are not as-expected."
  )
})

testthat::test_that("Utility function set_headers works when authorization =
                    FALSE and advance_access = TRUE", {
  token <- stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

  # Test set_headers when authorization parameter is FALSE (default)
  headers <- set_headers(token = token, advance_access = TRUE)

  testthat::expect_equal(typeof(headers), "character",
    label = glue::glue("Headers should be a vector of characters, not
                       {typeof(headers)}.")
  )
  testthat::expect_equal(length(headers),
    4L,
    label = "Headers vector should have three elements: X-SBG-Auth-Token,
    Accept, Content-Type and X-SBG-advance-access "
  )
  testthat::expect_equal(names(headers), c(
    "X-SBG-Auth-Token", "Accept",
    "Content-Type",
    "X-SBG-advance-access"
  ),
  label = "Elements in headers vector do not have expected names."
  )
  testthat::expect_equal(unname(headers), c(
    token, "application/json",
    "application/json", "advance"
  ),
  label = "Headers elements are not as-expected."
  )
})

testthat::test_that("Utility function set_headers works when authorization =
                    TRUE", {
  token <- stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

  # Test set_headers when authorization parameter is TRUE
  headers <- set_headers(authorization = TRUE, token = token)

  testthat::expect_equal(typeof(headers), "character",
    label = glue::glue("Headers should a character, not {typeof(headers)}.")
  )
  testthat::expect_equal(length(headers), 1L,
    label = "Headers should have only one value."
  )
  testthat::expect_equal(names(headers), "Authorization",
    label = "The name of the headers element should be Authorization."
  )
  testthat::expect_equal(unname(headers), glue::glue("Bearer {token}"),
    label = "Headers element is not as-expected."
  )
})

testthat::test_that("Utility function set_headers throws an error if token is
                    not provided", {
  err <- testthat::expect_error(set_headers(token = NULL))
  testthat::expect_equal(err$message, "Token is missing.")
})


testthat::test_that("Utility function setup_query works", {
  query <- list(limit = 10L, offset = 5L)
  fields <- c("name", "id", "created_by")

  query <- setup_query(
    query = query,
    limit = getOption("sevenbridges2")$limit,
    offset = getOption("sevenbridges2")$offset,
    fields = fields
  )

  # Set expected query list
  expected_query_list <- list(
    limit = 10L,
    offset = 5,
    fields = "name",
    fields = "id",
    fields = "created_by"
  )

  keys <- names(query)

  # Compare the two lists
  testthat::expect_equal(query[keys], expected_query_list[keys])
})

testthat::test_that("Utility function setup_body works", {
  method <- sample(c("POST", "PATCH", "PUT"), 1)

  # Check if the function setup_query throws an error when body is not a list
  test_body <- c(name = "test")
  err <- testthat::expect_error(setup_body(method = method, body = test_body))
  testthat::expect_equal(err$message, "Body should be a list.")

  # Check setup_query function output
  test_body <- readRDS(testthat::test_path(
    "test_data",
    "new_project_body.RDS"
  ))

  testthat::expect_true(is.list(test_body))

  body_param_json <- setup_body(method = method, body = test_body)
  expected_body_json <- readRDS(testthat::test_path(
    "test_data",
    "new_project_expected_body_json.RDS"
  ))
  testthat::expect_equal(body_param_json, expected_body_json)
})

testthat::test_that("Utility function m.fun works", {
  # test output when exact parameter is FALSE, and ignore.case is TRUE
  term <- "api"
  search_through_vector <- c(
    "element 1", "element 2 api", "element 3",
    "element 4 API", "element 5", "element 6", "api", "element 8"
  )

  match_index_vector <- m.fun(
    x = term,
    y = search_through_vector,
    exact = FALSE,
    ignore.case = TRUE
  )

  testthat::expect_equal(
    match_index_vector, c(2, 4, 7)
  ) # unnamed vector of match indexes

  # test output when exact parameter is FALSE, and ignore.case is FALSE
  match_index_vector <- m.fun(
    x = term,
    y = search_through_vector,
    exact = FALSE,
    ignore.case = FALSE
  )

  testthat::expect_equal(
    match_index_vector, c(2, 7)
  ) # unnamed vector of match indexes

  # test output when exact parameter is TRUE, and ignore.case is TRUE
  match_index_vector <- m.fun(
    x = term,
    y = search_through_vector,
    exact = TRUE,
    ignore.case = TRUE
  )

  testthat::expect_equal(match_index_vector, c(7)) # named vector

  # test output when exact parameter is FALSE,
  # and ignore.case is TRUE and there is only one match
  term <- "element 1"

  match_index_vector <- m.fun(
    x = term, y = search_through_vector,
    exact = FALSE, ignore.case = TRUE
  )

  testthat::expect_equal(
    match_index_vector, c(`element 1` = 1)
  ) # named vector of match indexes
})

testthat::test_that("Utility function m.match works", {
  # Test output when exact parameter is FALSE,
  # ignore.case is TRUE (id = NULL and name != NULL)
  search_through_list <- list(
    list(name = "project 1", id = "asdf1234"),
    list(name = "project 2", id = "qwer9876"),
    list(name = "project 3", id = "xyzq2234"),
    list(name = "project 3", id = "mnbv0192"),
    list(name = "project 4", id = "aeio5647")
  )

  matchings <- m.match(
    obj = search_through_list, id = NULL, name = "project 3",
    exact = FALSE, ignore.case = TRUE
  )

  # Set expected matchings list
  expected_matchings <- list(
    list(name = "project 3", id = "xyzq2234"),
    list(name = "project 3", id = "mnbv0192")
  )

  keys <- names(matchings)

  # Compare the two lists
  testthat::expect_equal(matchings[keys], expected_matchings[keys])


  # Test output when exact parameter is FALSE, ignore.case is TRUE
  # (id != NULL and name = NULL)
  search_through_list <- list(
    list(name = "project 1", id = "asdf1234"),
    list(name = "project 2", id = "qwer9876"),
    list(name = "project 3", id = "xyzq2234"),
    list(name = "project 3", id = "mnbv0192"),
    list(name = "project 4", id = "aeio5647")
  )

  matchings <- m.match(
    obj = search_through_list, id = "xyzq2234", name = NULL,
    exact = FALSE, ignore.case = TRUE
  )

  # Set expected matchings list
  expected_matchings <- list(name = "project 3", id = "xyzq2234")

  keys <- names(matchings)

  # Compare the two lists
  testthat::expect_equal(matchings[keys], expected_matchings[keys])

  # Test output when exact parameter is FALSE, and ignore.case is FALSE
  # (no matchings)
  matchings <- m.match(
    obj = search_through_list, id = NULL, name = "PROJECT 3",
    exact = FALSE, ignore.case = FALSE
  )

  testthat::expect_equal(matchings, list())

  # Test output when exact parameter is FALSE, and ignore.case is TRUE
})
