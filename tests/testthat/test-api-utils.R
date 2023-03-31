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
  testthat::expect_equal(flattened_query_params_list[keys], expected_resulting_list[keys])
})
