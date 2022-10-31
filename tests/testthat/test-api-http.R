test_that("Status check function works properly for unauthorized request", {
  resp <- sevenbridges2::api(
    token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"), # fake token
    path = "user/",
    method = "GET"
  )

  testthat::expect_equal(httr::status_code(resp), 401L)

  processed_response <- try(status_check(resp), silent = TRUE)

  if (class(processed_response) == "try-error") {
    error_message <- trimws(processed_response[1], which = c("both", "left", "right"), whitespace = "[\t\r\n]")

    testthat::expect_equal(error_message, "Error : HTTP Status 401: Unauthorized")
  } else if (class(processed_response) == "list") {
    testthat::fail(message = "Failure has been forced - a valid response has been obtained, which is not in accordance with the initial assumption of the test.")
  }
})


test_that("Status check function works properly for undefined resource request", {
  resp <- sevenbridges2::api(
    token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"), # fake token
    path = "wizards/", # non-existent resource
    method = "GET"
  )

  testthat::expect_equal(httr::status_code(resp), 404L)

  processed_response <- try(status_check(resp), silent = TRUE)

  if (class(processed_response) == "try-error") {
    error_message <- trimws(processed_response[1], which = c("both", "left", "right"), whitespace = "[\t\r\n]")

    testthat::expect_equal(error_message, "Error : HTTP Status 404: /v2/wizards/ not found")
  } else if (class(processed_response) == "list") {
    testthat::fail(message = "Failure has been forced - a valid response was obtained, which is not in accordance with the initial assumption of the test.")
  }
})
