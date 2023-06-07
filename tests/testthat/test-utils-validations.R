testthat::test_that("Function check_tags throws an error if the provided tags
                    argument is not a list", {
  err <- testthat::expect_error(check_tags(tags = "test_tag"))
  # nolint start
  testthat::expect_equal(err$message, "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')")
  # nolint end
})

testthat::test_that("Function check_settings works", {
  # Check if the function throws an error if settings argument is not a list
  err <- testthat::expect_error(check_settings(settings = "test_string"))

  testthat::expect_equal(err$message, "Settings must be provided as a list.")

  # Check if it throws an appropriate error if the provided settings list
  # contains an element with invalid name
  err <- testthat::expect_error(check_settings(settings = list(
    locked = FALSE,
    controlled = FALSE,
    width = 10L
  )))

  # nolint start
  testthat::expect_equal(err$message, "Argument width is not a valid settings field.")
  # nolint end

  # Check if the function check_settings throws an error when settings list
  # elements have invalid types
  valid_input_names <- c(
    "locked", "controlled", "use_interruptible_instances",
    "use_memoization", "allow_network_access",
    "use_elastic_disk", "location", "intermediate_files"
  )

  settings_field_types <- list(
    locked = "logical",
    controlled = "logical",
    use_interruptible_instances = "logical",
    use_memoization = "logical",
    allow_network_access = "logical",
    location = "character",
    intermediate_files = "list"
  )

  for (field in names(settings_field_types)) {
    # provide settings as a list with a field containing some invalid value
    # (for example, integer)
    input_list <- list()
    input_list[[field]] <- 10L

    err <- testthat::expect_error(check_settings(settings = input_list))
    # nolint start
    expected_error <- glue::glue("Assertion on '{field}' failed: Must be of type '{settings_field_types[field]}' (or 'NULL'), not 'integer'.")
    # nolint end
    testthat::expect_equal(err$message, expected_error)

    if (field == "intermediate_files") {
      # check error message if retention field is not valid (not character)
      input_intermediate_files <- list(
        intermediate_files = list(retention = 15L)
      )
      err <- testthat::expect_error(
        check_settings(settings = input_intermediate_files)
      )
      # nolint start
      expected_error <- glue::glue("Assertion on 'intermediate_files$retention' failed: Must be of type 'character' (or 'NULL'), not '{typeof(input_intermediate_files$intermediate_files$retention)}'.")
      # nolint end
      testthat::expect_equal(err$message, expected_error)

      # check error message if duration field is not valid (not character)
      input_intermediate_files <- list(
        intermediate_files =
          list(duration = "24")
      )
      err <- testthat::expect_error(check_settings(
        settings =
          input_intermediate_files
      ))
      # nolint start
      expected_error <- glue::glue("Assertion on 'intermediate_files$duration' failed: Must be of type 'integer' (or 'NULL'), not '{typeof(input_intermediate_files$intermediate_files$duration)}'.")
      # nolint end
      testthat::expect_equal(err$message, expected_error)
    }
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

test_that("check_folder_name function works", {
  valid_names <- c("New_folder", "MyFolder", "Inputs")
  for (name in valid_names) {
    testthat::expect_silent(check_folder_name(name))
  }
})

test_that("check_folder_name function throws error when expected", {
  invalid_names <- c(
    NULL, "", c(), NA,
    "New folder", "__inputs", "Another new folder"
  )
  for (name in invalid_names) {
    testthat::expect_error(check_folder_name(name))
  }
})

test_that("check_metadata function throws error when metadata is not valid", {
  metadata_values <- c("test", 1, NULL, TRUE, c("test"))
  for (metadata in metadata_values) {
    testthat::expect_error(
      check_metadata(metadata)
    )
  }
})

test_that("check_download_path function throws error when parameters are not valid", { # nolint
  # Negative test use case for directory_path parameter
  testthat::expect_error(
    check_download_path(
      directory_path = "non-existent-directory"
    ),
    "Destination directory non-existent-directory does not exist."
  )

  # Negative test use cases for filename parameter
  # 1) is_missing returns TRUE
  filename_invalid_values <- c(NA, NULL)
  for (filename in filename_invalid_values) {
    testthat::expect_error(
      check_download_path(directory_path = getwd(), filename = filename),
      "The filename parameter is missing."
    )
  }
  # 2) is_missing returns FALSE, but the filename parameter value is not valid
  filename_invalid_values <- list(
    15L,
    c("test_file_1.txt", "test_file_2.txt"),
    list("test_file.txt")
  )
  for (filename in filename_invalid_values) {
    testthat::expect_error(
      check_download_path(directory_path = getwd(), filename = filename),
      "The filename parameter should be a length-one string."
    )
  }
})

test_that("check_retry_count function throws error when count is invalid", { # nolint
  # Negative test use cases for count parameter
  invalid_retry_count <- c(-1, "retry", 0, FALSE)
  for (retry_count in invalid_retry_count) {
    testthat::expect_error(
      check_retry_params(
        input = retry_count,
        parameter_to_validate = "count"
      ),
      "retry_count parameter must be a positive integer number."
    )
  }
})

test_that("check_retry_params function throws error when timeout is not valid", { # nolint
  # Negative test use cases for timeout parameter
  invalid_retry_timeout <- c(-1, "retry", 0, FALSE)
  for (retry_timeout in invalid_retry_timeout) {
    testthat::expect_error(
      check_retry_params(
        input = retry_timeout,
        parameter_to_validate = "timeout"
      ),
      "retry_timeout parameter must be a positive integer number."
    )
  }
})


test_that("check_app_copy_strategy function throws error when strategy missing", { # nolint
  # Negative test use cases for missing strategy parameter
  testthat::expect_error(
    check_app_copy_strategy(
      strategy = NULL
    ),
    "Please provide the copy strategy"
  )
})


test_that("check_app_copy_strategy function throws error when provided strategy is invalid", { # nolint
  # Negative test use case for invalid strategy parameter
  # Valid values: clone, direct_clone, direct, transient
  supported_app_copy_strategies <- getOption("sevenbridges2")$APP_COPY_STRATEGIES # nolint

  invalid_strategy_param <- "test_strategy"

  testthat::expect_error(
    check_app_copy_strategy(
      strategy = invalid_strategy_param
    ),
    label = "The provided copy strategy (test_strategy) is not supported. Please use one of the following strategies: clone, direct, clone_direct, transient" # nolint
  )
})


test_that("check_file_path function throws error when provided file_path parameter is not valid", { # nolint
  # Negative test use case for invalid file_path parameter
  invalid_file_path <- "/path/to/nonexisting-file.cwl"

  testthat::expect_error(
    check_file_path(
      file_path = invalid_file_path
    ),
    label = "File {magenta /path/to/nonexisting-file.cwl} does not exist."
  )
})

test_that("check_volume_params works", {
  # Pass invalid args, volume_type
  testthat::expect_error(check_volume_params(args = NULL))
  testthat::expect_error(check_volume_params(
    args = list("arg1" = "arg1"),
    volume_type = "some-other-type"
  ))

  # Test with valid params
  valid_args <- list(
    name = "volume_name",
    bucket = "bucket_name",
    prefix = "",
    access_mode = "RW",
    description = NULL,
    properties = list("some-property" = "value"),
    endpoint = "some-endpoint",
    root_url = "some-url",
    credentials = list("key" = "just-string")
  )
  testthat::expect_no_error(check_volume_params(args = valid_args))

  # Create invalid params list
  invalid_args <- list(
    name = 123,
    bucket = list("some-name"),
    prefix = NA,
    access_mode = "some-other",
    description = FALSE,
    properties = c("some-property" = "value"),
    endpoint = list("some-endpoint"),
    root_url = TRUE,
    credentials = "just-string"
  )

  # Pass invalid name
  testthat::expect_error(check_volume_params(args = invalid_args["name"]))
  # Pass invalid bucket
  testthat::expect_error(
    check_volume_params(args = c(valid_args["name"], invalid_args["bucket"]))
  )
  # Pass invalid prefix
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"], valid_args["bucket"],
      invalid_args["prefix"]
    ))
  )
  # Pass invalid access_mode
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"],
      valid_args["bucket"],
      valid_args["prefix"],
      invalid_args["access_mode"]
    ))
  )
  # Pass invalid description
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"],
      valid_args["bucket"],
      valid_args["prefix"],
      valid_args["access_mode"],
      invalid_args["description"]
    ))
  )
  # Pass invalid properties
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"],
      valid_args["bucket"],
      valid_args["prefix"],
      valid_args["access_mode"],
      valid_args["description"],
      invalid_args["properties"]
    ))
  )
  # Pass invalid endpoint
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"],
      valid_args["bucket"],
      valid_args["prefix"],
      valid_args["access_mode"],
      valid_args["description"],
      valid_args["properties"],
      invalid_args["endpoint"]
    ))
  )
  # Pass invalid root_url
  testthat::expect_error(
    check_volume_params(args = c(
      valid_args["name"],
      valid_args["bucket"],
      valid_args["prefix"],
      valid_args["access_mode"],
      valid_args["description"],
      valid_args["properties"],
      invalid_args["root_url"]
    ))
  )
})

test_that("transform_configuration_param works", {
  # Provide configuration as valid list
  config_list <- list(
    "field1" = "value1",
    "field2" = 123,
    "field3" = list(
      "subfield" = "subfield-value"
    ),
    "field4" = "something"
  )
  transformed_str <- transform_configuration_param(configuration = config_list)
  testthat::expect_type(transformed_str, "character")
  testthat::expect_true(startsWith(transformed_str, prefix = "{\n"))

  # Provide configuration as path to JSON file
  config_json_path <- testthat::test_path(
    "test_data",
    "volumes_configuration_params.json"
  )
  transformed_str <- transform_configuration_param(configuration = config_json_path) # nolint
  testthat::expect_type(transformed_str, "character")
  testthat::expect_true(startsWith(transformed_str, prefix = "{\n"))
})

test_that("transform_configuration_param throws error when needed", {
  # Provide configuration as NULL
  testthat::expect_error(
    transform_configuration_param(configuration = NULL),
    regexp = "Invalid configuration format! \n Please, provide a string path to the JSON file or a named list.", # nolint
    fixed = TRUE
  )

  # Provide configuration as empty list
  testthat::expect_error(
    transform_configuration_param(configuration = list()),
    regexp = "Invalid configuration format! \n Please, provide a string path to the JSON file or a named list.", # nolint
    fixed = TRUE
  )

  # Provide configuration as unnamed list
  testthat::expect_error(
    transform_configuration_param(configuration = list("unnamed list")),
    regexp = "Invalid configuration format! \n Please, provide a string path to the JSON file or a named list.", # nolint
    fixed = TRUE
  )

  # Provide configuration as invalid json path
  config_path <- file.path("unnoun", "path", "to", "file.json")
  testthat::expect_error(
    transform_configuration_param(configuration = config_path)
  )

  # Provide configuration as non-string type
  testthat::expect_error(
    transform_configuration_param(configuration = c("field1", "field2"))
  )
})
