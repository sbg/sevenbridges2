testthat::test_that("Function check_tags throws an error if the provided tags argument is not a list", {

  err <- testthat::expect_error(check_tags(tags = "test_tag"))

  testthat::expect_equal(err$message, "Tags parameter must be an unnamed list of tags. For example: tags = list('my_tag_1', 'my_tag_2')")

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

  testthat::expect_equal(err$message, "Argument width is not a valid settings field.")



  # Check if the function check_settings throws an error when settings list elements
  # have invalid types
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
    # provide settings as a list with a field containing some invalid value (for example, integer)
    input_list <-  list()
    input_list[[field]] <- 10L

    err <- testthat::expect_error(check_settings(settings = input_list))
    expected_error <- glue::glue("Assertion on '{field}' failed: Must be of type '{settings_field_types[field]}' (or 'NULL'), not 'integer'.")
    testthat::expect_equal(err$message, expected_error)
  }
})
