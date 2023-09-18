test_that("Api function throws error when token is not provided", {
  test_function <- function(token) {
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
  lapply(bad_tokens, test_function)
})

test_that("Api function throws error when url is not provided", {
  test_function <- function(url) {
    testthat::expect_error(
      sevenbridges2::api(
        token = dummy_token,
        path = "user/",
        method = "GET",
        base_url = url
      ),
      "API address from the preferred platform must be provided"
    )
  }
  lapply(bad_urls, test_function)
})

test_that("Api function throws error when method is not valid", {
  test_function <- function(method) {
    testthat::expect_error(
      sevenbridges2::api(
        token = dummy_token,
        path = "user/",
        method = method,
        base_url = "https://api.sbgenomics.com/v2/"
      )
    )
  }
  lapply(bad_methods, test_function)
})

test_that("Api function throws error when encoding is not valid", {
  test_function <- function(encode) {
    testthat::expect_error(
      sevenbridges2::api(
        token = dummy_token,
        path = "user/",
        method = "GET",
        base_url = "https://api.sbgenomics.com/v2/",
        encode = encode
      )
    )
  }
  lapply(bad_encodings, test_function)
})

test_that("Status check function works properly for unauthorized request", {
  testthat::expect_error(sevenbridges2::api(
    token = dummy_token,
    # fake token
    path = "user/",
    method = "GET",
    base_url = "https://api.sbgenomics.com/v2/"
  ))
})


test_that("Status check function works properly for undefined resource
          request", {
  testthat::expect_error(sevenbridges2::api(
    token = dummy_token,
    # fake token
    path = "wizards/", # non-existent resource
    method = "GET",
    base_url = "https://api.sbgenomics.com/v2/"
  ))
})
