test_that("Volumes initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Volumes$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_volumes_obj,
    classes = c("Resource", "Volumes"),
    public = c(
      "URL", "query", "get",
      "create_s3_using_iam_user", "create_s3_using_iam_role",
      "create_google_using_iam_user", "create_google_using_iam_role"
    )
  )
})

test_that("Volumes get works", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_volumes_obj$get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_volumes_obj$get, test_bad_id))
})
