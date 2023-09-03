test_that("VolumeFile initialization works", {
  # VolumeFile object creation works
  testthat::expect_no_error(asVolumeFile(auth = setup_auth_object))

  # VolumeFile object class and methods are set
  checkmate::assert_r6(
    setup_volume_file_obj,
    classes = c("VolumeFile"),
    public = c(
      "href", "location", "type", "storage_type", "volume", "metadata",
      "import"
    )
  )
})

test_that("asVolumeFile function works", {
  # Pass inputs for directories
  param_list_res <- list(
    href = "resource-href",
    prefix = "my_new_folder",
    volume = "my_s3_volume"
  )
  vol_file_dir <- asVolumeFile(param_list_res, auth = setup_auth_object)
  testthat::expect_equal(
    vol_file_dir$location,
    paste0(param_list_res$prefix, "/")
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
  vol_file_file <- asVolumeFile(param_list_res, auth = setup_auth_object)
  testthat::expect_equal(vol_file_file$location, param_list_res$location)
  testthat::expect_equal(vol_file_file$type, "FILE")
})

test_that("VolumeFile print method works", {
  testthat::expect_snapshot(setup_volume_file_obj$print())
})
