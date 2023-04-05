testthat::test_that("Init authentication works", {
  # Generate dummy token
  test_token <- stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

  # Create dummy authentication object
  auth <- suppressMessages(
    sevenbridges2::Auth$new(
      from = "direct",
      platform = "aws-us",
      token = test_token
    )
  )

  testthat::expect_true(checkmate::test_class(auth, classes = "Auth"))
  testthat::expect_equal(auth$get_token(), test_token)
  # Check the number of characters in the token written in the env variable
  testthat::expect_equal(nchar(auth$get_token()), 32L)

  # Check if token contains lower cas letters and digits only
  testthat::expect_equal(grepl("^[a-z0-9]+$", auth$get_token()), TRUE)

  # Check auth object fields
  testthat::expect_equal(auth$authorization, FALSE)
  testthat::expect_equal(auth$config_file, NULL)
  testthat::expect_equal(auth$from, "direct")
  testthat::expect_equal(auth$fs, NA)
  testthat::expect_equal(auth$platform, "aws-us")
  testthat::expect_equal(auth$profile_name, NULL)
  testthat::expect_equal(auth$sysenv_token, NULL)
  testthat::expect_equal(auth$sysenv_url, NULL)
  testthat::expect_equal(auth$url, "https://api.sbgenomics.com/v2/")

  # Clear env variable
  Sys.unsetenv("SB_AUTH_TOKEN")


  # Check if the initialization function returns an error if both
  # platform and url are provided
  err <- testthat::expect_error(
    suppressMessages(
      sevenbridges2::Auth$new(
        from = "direct",
        platform = "aws-us",
        token = test_token,
        url = "https://api.sbgenomics.com/v2/"
      )
    )
  )

  expected_error <- "`platform` and `url` cannot be set simultaneously"
  testthat::expect_equal(err$message, expected_error)

  # Check of the initialization method throws an error if neither of
  # those two (platform and url) are provided
  err <- testthat::expect_error(
    suppressMessages(
      sevenbridges2::Auth$new(
        from = "direct",
        platform = NULL,
        token = test_token,
        url = NULL
      )
    )
  )
  expected_error <- "`platform` and `url` are not set, please, set one of them."
  testthat::expect_equal(err$message, expected_error)

  # Check error message when wrong platform name is provided
  err <- testthat::expect_error(
    suppressMessages(
      sevenbridges2::Auth$new(
        from = "direct",
        platform = "Platform 9¾",
        token = test_token,
        url = NULL
      )
    )
  )
  expected_error <- "Platform does not exist, please check its spelling\n
  (case-sensitive)"
  testthat::expect_equal(err$message, expected_error)

  # Check error message when token is not provided
  # Check error message when wrong platform name is provided
  err <- testthat::expect_error(
    suppressMessages(
      sevenbridges2::Auth$new(
        from = "direct",
        platform = "aws-us",
        token = NULL,
        url = NULL
      )
    )
  )
  expected_error <- '`token` must be set when `from = "direct"`'
  testthat::expect_equal(err$message, expected_error)
})


testthat::test_that("Init authentication from env works", {
  # Set env variables with sbg_set_env function
  test_sysenv_url_name <- "TEST_AUTH_FROM_ENV_URL_NAME"
  test_sysenv_token_name <- "TEST_AUTH_FROM_ENV_TOKEN_NAME"

  sbg_set_env(
    url = "https://test-url.com/",
    token = stringi::stri_rand_strings(
      n = 1, length = 32,
      pattern = "[a-z0-9]"
    ),
    sysenv_url_name = test_sysenv_url_name,
    sysenv_token_name = test_sysenv_token_name
  )

  # Create dummy authentication object
  auth <- suppressMessages(
    sevenbridges2::Auth$new(
      from = "env",
      sysenv_url = test_sysenv_url_name,
      sysenv_token = test_sysenv_token_name
    )
  )

  testthat::expect_true(checkmate::test_class(auth, classes = "Auth"))

  testthat::expect_equal(auth$get_token(), sbg_get_env(test_sysenv_token_name))
  # Check the number of characters in the token written in the env variable
  testthat::expect_equal(nchar(auth$get_token()), 32L)

  # Check if token contains lower cas letters and digits only
  testthat::expect_equal(grepl("^[a-z0-9]+$", auth$get_token()), TRUE)

  # Check auth object fields
  testthat::expect_equal(auth$authorization, FALSE)
  testthat::expect_equal(auth$config_file, NULL)
  testthat::expect_equal(auth$from, "env")
  testthat::expect_equal(auth$fs, NA)
  testthat::expect_equal(auth$platform, NULL)
  testthat::expect_equal(auth$profile_name, NULL)
  testthat::expect_equal(auth$sysenv_token, test_sysenv_token_name)
  testthat::expect_equal(auth$sysenv_url, test_sysenv_url_name)
  testthat::expect_equal(auth$url, sbg_get_env(test_sysenv_url_name))

  # Clear env variables
  Sys.unsetenv(test_sysenv_url_name)
  Sys.unsetenv(test_sysenv_token_name)
})



testthat::test_that("Init authentication from config file works", {
  # Create dummy authentication object
  auth <- suppressMessages(
    sevenbridges2::Auth$new(
      from = "file",
      config_file = testthat::test_path(
        "test_data",
        "sbg_credentials_test_file"
      ),
      profile_name = "BioDataCatalyst",
      sysenv_url = "TEST_SYSENV_URL_NAME",
      sysenv_token = "TEST_SYSENV_TOKEN_NAME"
    )
  )

  testthat::expect_true(checkmate::test_class(auth, classes = "Auth"))

  testthat::expect_equal(
    auth$get_token(),
    sbg_get_env(paste0(auth$profile_name, "_token"))
  )
  # Check the number of characters in the token written in the env variable
  testthat::expect_equal(nchar(auth$get_token()), 32L)

  # Check if token contains lower cas letters and digits only
  testthat::expect_equal(grepl("^[a-z0-9]+$", auth$get_token()), TRUE)

  # Check auth object fields
  testthat::expect_equal(auth$authorization, FALSE)
  testthat::expect_equal(
    auth$config_file,
    testthat::test_path(
      "test_data",
      "sbg_credentials_test_file"
    )
  )
  testthat::expect_equal(auth$from, "file")
  testthat::expect_equal(auth$fs, NA)
  testthat::expect_equal(auth$platform, "f4c")
  testthat::expect_equal(auth$profile_name, "BioDataCatalyst")

  testthat::expect_equal(
    auth$url,
    sbg_get_env(paste0(auth$profile_name, "_url"))
  )

  # Clear env variables
  Sys.unsetenv(paste0(auth$profile_name, "_token"))
  Sys.unsetenv(paste0(auth$profile_name, "_url"))
})





testthat::test_that("Calling the api method with no arguments returns a list of
                    all API paths", {
  # Generate dummy token
  dummy_token <-
    stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]") # fake token

  # Create dummy authentication object
  auth <- suppressMessages(
    sevenbridges2::Auth$new(
      from = "direct",
      platform = "aws-us",
      token = dummy_token
    )
  )

  testthat::expect_true(checkmate::test_class(auth, classes = "Auth"))

  api_paths <- auth$api()

  testthat::expect_equal(is.list(api_paths), TRUE)
  testthat::expect_equal(length(api_paths), 12L)

  expected_api_paths <- readRDS(testthat::test_path(
    "test_data",
    "all_api_paths.RDS"
  ))

  keys <- names(api_paths)

  # Compare the two lists
  expect_equal(api_paths[keys], expected_api_paths[keys])
})
