test_that("VolumeFile initialization works", {
  # VolumeFile object creation works
  testthat::expect_no_error(asVolumeFile(auth = setup_auth_object))

  # VolumeFile object class and methods are set
  checkmate::assert_r6(
    setup_volume_file_obj,
    classes = c("VolumeFile"),
    public = c(
      "href", "URL", "location", "type", "storage_type", "volume", "metadata",
      "import", "reload", "list_files"
    )
  )

  # Pass inputs for directories
  param_list_res <- list(
    href = "resource-href",
    prefix = "my_new_folder",
    volume = "my_s3_volume"
  )
  vol_file_dir <- VolumeFile$new(param_list_res, auth = setup_auth_object)
  testthat::expect_equal(
    vol_file_dir$location,
    param_list_res$prefix
  )
  testthat::expect_equal(vol_file_dir$type, "PREFIX")

  # Pass inputs for files
  param_list_res <- list(
    href = "resource-href",
    location = "my_new_file3.txt",
    type = "FILE",
    storage_type = "s3",
    volume = "my_s3_volume",
    metadata = list(metadata_field = "metadata-value")
  )
  vol_file_file <- VolumeFile$new(param_list_res, auth = setup_auth_object)
  testthat::expect_equal(vol_file_file$location, param_list_res$location)
  testthat::expect_equal(vol_file_file$type, "FILE")
})

test_that("VolumeFile print method works", {
  testthat::expect_snapshot(setup_volume_file_obj$print())
})

test_that("VolumeFile list_file method throws error when expected", {
  # Use volume file object of type "FILE" instead of "PREFIX"
  testthat::expect_error(
    setup_volume_file_obj$list_files(),
    regexp = "This is not a volume folder. Listing volume folder files is possible only on VolumeFile objects of type 'PREFIX'.", # nolint
    fixed = TRUE
  )
  # Pass non-string continuation_token
  testthat::expect_error(
    setup_volume_file_dir_obj$list_files(continuation_token = 1234),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_volume_file_dir_obj$list_files(continuation_token = NA),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
})
