test_that("Upload and Part objects initialization works", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")

  # Test with upload object that has 8 parts
  upload_obj <- Upload$new(
    path = path,
    filename = "new_name.txt",
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    part_size = 7 * 1024^2,
    auth = test_auth_obj
  )

  testthat::expect_equal(upload_obj$upload_id, NULL)
  testthat::expect_equal(upload_obj$filename, "new_name.txt")
  testthat::expect_false(upload_obj$initialized)
  testthat::expect_equal(upload_obj$part_length, 8)
  testthat::expect_length(upload_obj$parts, 8)
  testthat::expect_true(all(sapply(
    upload_obj$parts,
    function(x) inherits(x, "Part")
  )))

  # Check Part objects structure
  first_part <- upload_obj$parts[[1]]
  testthat::expect_equal(first_part$part_number, 1)
  testthat::expect_equal(first_part$part_size, upload_obj$part_size)
  testthat::expect_true(all(sapply(list(
    first_part$etag,
    first_part$expires,
    first_part$url
  ), is.na)))

  last_part <- tail(upload_obj$parts, 1)[[1]]
  testthat::expect_equal(last_part$part_number, 8)
  testthat::expect_lt(last_part$part_size, upload_obj$part_size)
  testthat::expect_true(all(sapply(list(
    last_part$etag,
    last_part$expires,
    last_part$url
  ), is.na)))

  # Test with upload object that has 1 part
  upload_obj <- Upload$new(
    path = path,
    filename = "new_name.txt",
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    part_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  testthat::expect_equal(upload_obj$upload_id, NULL)
  testthat::expect_equal(upload_obj$filename, "new_name.txt")
  testthat::expect_false(upload_obj$initialized)
  testthat::expect_equal(upload_obj$part_length, 1)
  testthat::expect_length(upload_obj$parts, 1)
  testthat::expect_true(all(sapply(
    upload_obj$parts,
    function(x) inherits(x, "Part")
  )))
})

test_that("upload$init function throws error when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")
  upload_obj <- Upload$new(
    path = path,
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  upload_obj$initialized <- TRUE
  testthat::expect_error(
    upload_obj$init(),
    "Upload has already been initialized."
  )
})

test_that("upload$info function throws error when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")
  upload_obj <- Upload$new(
    path = path,
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  testthat::expect_error(
    upload_obj$info(),
    "Upload has not been initialized yet"
  )

  # Set initialized to TRUE
  upload_obj$initialized <- TRUE
  testthat::expect_error(
    upload_obj$info(list_parts = 123),
    "Assertion on 'list_parts' failed: Must be of type 'logical', not 'double'."
  ) # nolint
})


test_that("upload$start function throws error when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")
  upload_obj <- Upload$new(
    path = path,
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  testthat::expect_error(
    upload_obj$start(),
    "Upload has not been initialized yet"
  )
})

test_that("upload$abort function throws error when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")
  upload_obj <- Upload$new(
    path = path,
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  testthat::expect_error(
    upload_obj$abort(),
    "Upload has not been initialized yet"
  )
})

test_that("part$upload_info_part and part$upload_complete_part functions
          throw errors when needed", {
  # Load auth object
  test_auth_obj <- readRDS(testthat::test_path("test_data", "auth.RDS"))
  path <- testthat::test_path("test_data", "auth.RDS")
  upload_obj <- Upload$new(
    path = path,
    overwrite = TRUE,
    parent = "parent-id",
    file_size = 50 * 1024^2,
    auth = test_auth_obj
  )

  part <- upload_obj$parts[[1]]
  testthat::expect_error(
    part$upload_info_part(upload_id = 123),
    regexp = "Assertion on 'x' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    part$upload_complete_part(upload_id = 123),
    regexp = "Assertion on 'x' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
})
