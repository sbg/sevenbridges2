test_that("Volume initialization works", {
  # Item object creation works
  testthat::expect_no_error(Volume$new(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_s3_volume_obj,
    classes = c("Item", "Volume"),
    public = c(
      "URL", "id", "name", "service", "access_mode", "active", "created_on",
      "modified_on", "get_file", "list_files",
      "delete", "reactivate", "deactivate", "update"
    )
  )
})

test_that("Volume print method works", {
  testthat::expect_snapshot(setup_s3_volume_obj$print())
})

test_that("Volume update method throws error when expected", {
  # Pass non-string description
  testthat::expect_error(
    setup_s3_volume_obj$update(description = 1234),
    regexp = "Assertion on 'description' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass non-string access_mode
  testthat::expect_error(
    setup_s3_volume_obj$update(access_mode = TRUE),
    regexp = "Assertion on 'access_mode' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass invalid access_mode
  testthat::expect_error(
    setup_s3_volume_obj$update(access_mode = "Read-only"),
    regexp = "Access mode must be RW or RO.",
    fixed = TRUE
  )
  # Pass non-list service param
  testthat::expect_error(
    setup_s3_volume_obj$update(service = 1234),
    regexp = "Assertion on 'service' failed: Must be of type 'list' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid service param
  testthat::expect_error(
    setup_s3_volume_obj$update(
      service = list(
        some_field1 = "value",
        some_field2 = NULL
      )
    ),
    regexp = "Assertion on 'service' failed: Contains missing values (element 2).", # nolint
    fixed = TRUE
  )
})

test_that("Volume deactivation method throws error when expected", {
  # Try to deactivate volume that is already deactivated
  setup_s3_volume_obj$active <- FALSE
  testthat::expect_error(
    setup_s3_volume_obj$deactivate(),
    regexp = "The volume my_new_volume is already deactivated.", # nolint
    fixed = TRUE
  )
  setup_s3_volume_obj$active <- TRUE
})

test_that("Volume reactivation method throws error when expected", {
  # Try to reactivate volume that is already active
  testthat::expect_error(
    setup_s3_volume_obj$reactivate(),
    regexp = "The volume my_new_volume is already active.", # nolint
    fixed = TRUE
  )
})

test_that("Volume deletion method throws error when expected", {
  # Try to delete volume that is not deactivated
  testthat::expect_error(
    setup_s3_volume_obj$delete(),
    regexp = "The volume my_new_volume must be deactivated first in order to be able to delete it.", # nolint
    fixed = TRUE
  )
})

test_that("Volume list_file method throws error when expected", {
  # Pass non-string parent
  testthat::expect_error(
    setup_s3_volume_obj$list_files(parent = 1234),
    regexp = "Assertion on 'parent' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass invalid parent
  testthat::expect_error(
    setup_s3_volume_obj$list_files(parent = NA),
    regexp = "Assertion on 'parent' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string fields
  testthat::expect_error(
    setup_s3_volume_obj$list_files(fields = 1234),
    regexp = "Assertion on 'fields' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass invalid fields
  testthat::expect_error(
    setup_s3_volume_obj$list_files(fields = c("field1", "field2")),
    regexp = "Fields parameter can contain subset of values: 'href', 'location', 'volume', 'type', 'metadata', '_all'", # nolint
    fixed = TRUE
  )
  # Pass non-string link
  testthat::expect_error(
    setup_s3_volume_obj$list_files(link = 1234),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$list_files(link = NA),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string continuation_token
  testthat::expect_error(
    setup_s3_volume_obj$list_files(continuation_token = 1234),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$list_files(continuation_token = NA),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume get_file method throws error when expected", {
  # Pass non-string file_location
  testthat::expect_error(
    setup_s3_volume_obj$get_file(file_location = 1234),
    regexp = "Assertion on 'file_location' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass invalid file_location
  testthat::expect_error(
    setup_s3_volume_obj$get_file(file_location = NA),
    regexp = "Assertion on 'file_location' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string link
  testthat::expect_error(
    setup_s3_volume_obj$get_file(link = 1234), # nolint
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$get_file(link = NA),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )

  # Check empty args
  testthat::expect_error(
    setup_s3_volume_obj$get_file(),
    regexp = "Empty arguments are not allowed. Please, provide either file_location or link.", # nolint
    fixed = TRUE
  )
  # Check if both args are provided
  testthat::expect_error(
    setup_s3_volume_obj$get_file(
      file_location = "file-location",
      link = "link"
    ),
    regexp = "Please, provide either file_location or link, not both.",
    fixed = TRUE
  )
})
