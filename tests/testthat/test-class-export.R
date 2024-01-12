test_that("Export initialization works", {
  # Item object creation works
  testthat::expect_no_error(asExport(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_export_obj,
    classes = c("Item", "Export"),
    public = c(
      "id", "source", "destination", "overwrite", "state", "result",
      "properties", "started_on", "finished_on", "reload"
    )
  )
  # Check whether destination contains volume and location fields
  testthat::expect_true(
    all(c("volume", "location") %in% names(setup_export_obj$destination))
  )
  # Check whether source contains file field
  testthat::expect_true(
    all(c("file") %in% names(setup_export_obj$source))
  )
  # Check whether result is of class File
  testthat::expect_true(
    checkmate::test_r6(setup_export_obj$result, classes = "File")
  )
})

test_that("Export print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_export_obj$print())
})
