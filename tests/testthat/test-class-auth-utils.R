testthat::test_that("Utility normalize_url function works properly", {
  test_urls <- c(
    "https://api.sbgenomics.com/v2/",
    "https://api.sbgenomics.com/v2"
  )

  # Randomly select one of two urls
  chosen_test_url <- sample(test_urls, 1)

  if (!grepl("/$", chosen_test_url)) {
    # Original URL doesn't have "/" at the end - add it
    testthat::expect_equal(normalize_url(chosen_test_url),
      paste0(chosen_test_url, "/"),
      label = "Normalized URL should contain a `/` character at the end."
    )
  } else {
    # Original URL already has "/" at the end - don't change it
    testthat::expect_equal(normalize_url(chosen_test_url), chosen_test_url,
      label = "The URL should remain unchanged after normalization."
    )
  }
})



test_that("Utility function sbg_parse_config works", {
  # Set config file path
  config_file <- testthat::test_path("test_data", "sbg_credentials_test_file")

  # Load config list from text file (equivalent to sevenbridges credentials
  # file (with fake tokens))
  config_list <- sbg_parse_config(config_file)

  # Load saved config list from RDS file
  expected_config_list <- readRDS(testthat::test_path(
    "test_data",
    "expected_config_list.RDS"
  ))
  keys <- names(config_list)

  # Compare the two lists
  testthat::expect_equal(config_list[keys], expected_config_list[keys])
})



testthat::test_that("Utility function sbg_set_env works", {
  # Set env variables with sbg_set_env function
  test_sysenv_url_name <- "API_ENDPOINT_TEST"
  test_sysenv_token_name <- "AUTH_TOKEN_TEST"

  # Call sbg_set_env function
  sbg_set_env(
    url = "https://test_url.com", token = "12312121231212",
    sysenv_url_name = test_sysenv_url_name,
    sysenv_token_name = test_sysenv_token_name
  )

  # Check url
  testthat::expect_equal(Sys.getenv(test_sysenv_url_name),
    "https://test_url.com",
    label = "Environment variable for API endpoint wasn't set correctly."
  )

  # Check token
  testthat::expect_equal(Sys.getenv(test_sysenv_token_name),
    "12312121231212",
    label = "Environment variable for AUTH token wasn't set correctly."
  )

  # Unset test env variables
  Sys.unsetenv(test_sysenv_url_name)
  Sys.unsetenv(test_sysenv_token_name)
})


testthat::test_that("Utility function sbg_get_env works", {
  # Set test env variable
  Sys.setenv("ENV_VARIABLE_TEST" = "123454321")

  testthat::expect_equal(sbg_get_env("ENV_VARIABLE_TEST"),
    "123454321",
    label = "The value of the fetched environment variable does not correspond
    to the expected value."
  )
})


testthat::test_that("Utility function sbg_get_env returns proper message if env
                    variable does not exist (is blank).", {
  # Generate random string as env variable name
  test_env_variable_name <- stringi::stri_rand_strings(
    n = 1, length = 6,
    pattern = "[a-zA-Z0-9]"
  )
  res <- try(sbg_get_env(test_env_variable_name), silent = TRUE)

  # Remove white spaces (\n at the end of a string)
  res <- trimws(res[1],
    which = c("both", "left", "right"),
    whitespace = "[\t\r\n]"
  )

  # Define expected message
  expected_message <- paste0(
    "Error : Environment variable ", test_env_variable_name,
    " is blank, please check if it is set correctly"
  )

  testthat::expect_equal(res, expected_message,
    label = "Test variable should be blank."
  )
})


testthat::test_that("Utility function sbg_platform_lookup works", {
  # go through all available platforms
  for (name in names(sbg_baseurl)) {
    # normalized url
    testthat::expect_equal(sbg_platform_lookup(sbg_baseurl[[name]]), name)
    # non-normalized url
    testthat::expect_equal(sbg_platform_lookup(gsub(
      "/$", "",
      sbg_baseurl[[name]]
    )), name)
  }

  # undefined url
  testthat::expect_equal(sbg_platform_lookup("https://dummy-url.com"), NULL)
})
