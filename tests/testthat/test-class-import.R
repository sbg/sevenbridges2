test_that("Import initialization works", {
  # Item object creation works
  testthat::expect_no_error(asImport(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_import_obj,
    classes = c("Item", "Import"),
    public = c(
      "id", "source", "destination", "overwrite", "autorename", "result",
      "preserve_folder_structure", "started_on", "finished_on", "state",
      "reload"
    )
  )
  # Check whether source contains volume and location fields
  testthat::expect_true(
    all(c("volume", "location") %in% names(setup_import_obj$source))
  )
  # Check whether destination contains project and name fields
  testthat::expect_true(
    all(c("project", "name") %in% names(setup_import_obj$destination))
  )
  # Check whether result is of class File
  testthat::expect_true(
    checkmate::test_r6(setup_import_obj$result, classes = "File")
  )
})

test_that("Import print method works", {
  testthat::skip_on_ci()
  testthat::expect_snapshot(setup_import_obj$print())
})
