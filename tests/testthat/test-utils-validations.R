testthat::test_that("Function check_tags throws an error if the provided tags
                    argument is not a list", {
  err <- testthat::expect_error(check_tags(tags = "test_tag"))
  # nolint start
  testthat::expect_equal(err$message, "Tags parameter must be an unnamed list of tags. For example: tags = list('my_tag_1', 'my_tag_2')")
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

test_that("check_metadata function throws error when metadata is not valid", {
  metadata_values <- c("test", 1, NULL, TRUE, c("test"))
  for (metadata in metadata_values) {
    testthat::expect_error(
      check_metadata(metadata)
    )
  }
})
