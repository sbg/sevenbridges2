test_that("VolumePrefix initialization works", {
  # VolumePrefix object creation works
  testthat::expect_no_error(asVolumePrefix(auth = setup_auth_object))

  # VolumePrefix object class and methods are set
  checkmate::assert_r6(
    setup_volume_prefix_obj,
    classes = c("VolumePrefix"),
    public = c(
      "href", "URL", "prefix", "volume",
      "import", "list_contents"
    )
  )
  res_params1 <- list(
    prefix = "my_new_folder2",
    volume = "my_s3_volume",
    href = "resource-href"
  )
  prefix_obj <- asVolumePrefix(x = res_params1, auth = setup_auth_object)
  # Check whether the '/' was added at the end of the prefix name
  testthat::expect_equal(prefix_obj$prefix, paste0(res_params1$prefix, "/"))
  res_params2 <- list(
    prefix = "my_new_folder2/",
    volume = "my_s3_volume",
    href = "resource-href"
  )
  prefix_obj <- asVolumePrefix(x = res_params2, auth = setup_auth_object)
  # Check whether the prefix name is not changed since it contains '/' at the
  # end
  testthat::expect_equal(prefix_obj$prefix, res_params2$prefix)
})

test_that("VolumePrefix print method works", {
  testthat::expect_snapshot(setup_volume_prefix_obj$print())
})

test_that("VolumePrefix list_contents method throws error when expected", {
  # Pass non-string continuation_token
  testthat::expect_error(
    setup_volume_prefix_obj$list_contents(continuation_token = 1234),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_volume_prefix_obj$list_contents(continuation_token = NA),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
})
