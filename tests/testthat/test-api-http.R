test_that("Api function throws error when token is not provided", {
  tokens <- c(NA, "", NULL, c(), list())
  for (token in tokens) {
    testthat::expect_error(
      sevenbridges2::api(
        token = token,
        path = "user/",
        method = "GET",
        base_url = "https://api.sbgenomics.com/v2/"
      ),
      "token must be provided"
    )
  }
})

test_that("Api function throws error when url is not provided", {
  urls <- c(NA, "", NULL, c(), list())
  for (url in urls) {
    testthat::expect_error(
      sevenbridges2::api(
        token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"),
        path = "user/",
        method = "GET",
        base_url = url
      ),
      "API address from the preferred platform must be provided"
    )
  }
})

test_that("check_limit function passes when limit is valid", {
  limits <- c(1L, 50L, 88L, 7, 56)
  for (limit in limits) {
    testthat::expect_silent(check_limit(limit))
  }
})

test_that("check_limit function throws error when limit is not valid", {
  limits <- c(-1, "limit", 0, 1500, FALSE)
  for (limit in limits) {
    testthat::expect_error(
      check_limit(limit),
      "Limit must be integer number between 1 and 100."
    )
  }
})

test_that("check_offset function passes when offset is valid", {
  offsets <- c(1L, 50L, 488L, 90, 23)
  for (offset in offsets) {
    testthat::expect_silent(check_offset(offset))
  }
})

test_that("check_offset function throws error when offset is not valid", {
  offsets <- c(-10, "offset", TRUE)
  for (offset in offsets) {
    testthat::expect_error(
      check_offset(offset),
      "Offset must be integer number >= 0."
    )
  }
})

test_that("Api function throws error when method is not valid", {
  methods <- c(NA, "", NULL, "PUTT")
  for (method in methods) {
    testthat::expect_error(
      sevenbridges2::api(
        token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"),
        path = "user/",
        method = method,
        base_url = "https://api.sbgenomics.com/v2/"
      )
    )
  }
})

test_that("Api function throws error when encoding is not valid", {
  encodings <- c(NA, "", NULL, "something else")
  for (encode in encodings) {
    testthat::expect_error(
      sevenbridges2::api(
        token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"),
        path = "user/",
        method = "GET",
        base_url = "https://api.sbgenomics.com/v2/",
        encode = encode
      )
    )
  }
})

test_that("Status check function works properly for unauthorized request", {
  resp <- sevenbridges2::api(
    token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"),
    # fake token
    path = "user/",
    method = "GET",
    base_url = "https://api.sbgenomics.com/v2/"
  )

  testthat::expect_equal(httr::status_code(resp), 401L)

  processed_response <- try(status_check(resp), silent = TRUE)

  if (inherits(processed_response, "try-error")) {
    error_message <- trimws(processed_response[1],
      which = c("both", "left", "right"),
      whitespace = "[\t\r\n]"
    )

    testthat::expect_equal(
      error_message,
      "Error in status_check(resp) : HTTP Status 401: Unauthorized"
    )
  } else if (inherits(processed_response, "list")) {
    testthat::fail(message = "Failure has been forced - a valid response has
                   been obtained, which is not in accordance with the initial
                   assumption of the test.")
  }
})


test_that("Status check function works properly for undefined resource
          request", {
  resp <- sevenbridges2::api(
    token = stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]"),
    # fake token
    path = "wizards/", # non-existent resource
    method = "GET",
    base_url = "https://api.sbgenomics.com/v2/"
  )

  testthat::expect_equal(httr::status_code(resp), 404L)

  processed_response <- try(status_check(resp), silent = TRUE)

  if (inherits(processed_response, "try-error")) {
    error_message <- trimws(processed_response[1],
      which = c("both", "left", "right"),
      whitespace = "[\t\r\n]"
    )

    testthat::expect_equal(
      error_message,
      "Error in status_check(resp) : HTTP Status 404: /v2/wizards/ not found"
    )
  } else if (inherits(processed_response, "list")) {
    testthat::fail(message = "Failure has been forced - a valid response was
                   obtained, which is not in accordance with the initial
                   assumption of the test.")
  }
})
