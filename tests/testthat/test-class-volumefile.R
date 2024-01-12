test_that("VolumeFile initialization works", {
  # VolumeFile object creation works
  testthat::expect_no_error(asVolumeFile(auth = setup_auth_object))

  # VolumeFile object class and methods are set
  checkmate::assert_r6(
    setup_volume_file_obj,
    classes = c("VolumeFile"),
    public = c(
      "href", "URL", "location", "type", "volume", "metadata",
      "import", "reload"
    )
  )
})

test_that("VolumeFile print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_volume_file_obj$print())
})
