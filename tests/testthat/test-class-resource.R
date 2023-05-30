test_that("Resource initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Resource$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_resource_obj,
    classes = c("Resource"),
    public = c("query", "get", "delete")
  )
})

test_that("Resource query works", {
  # Query fails when no path is provided
  testthat::expect_error(setup_resource_obj$query(path = NULL),
    regexp = "Please provide path parameter!",
    fixed = TRUE
  )
})

test_that("Resource get works", {
  # Setup test parameters for test
  test_cls <- list(auth = "foo", URL = list(get = "foo"))
  test_cls_no_auth <- list(auth = NULL, URL = list(get = "foo"))
  test_cls_no_get <- list(auth = "foo", URL = list(get = NULL))
  test_id <- "foo"

  # Query fails when no cls is provided
  testthat::expect_error(setup_resource_obj$get(cls = NULL),
    regexp = "Please provide cls parameter!",
    fixed = TRUE
  )

  # Query fails when no auth in cls parameter
  testthat::expect_error(
    setup_resource_obj$get(cls = test_cls_no_auth, id = test_id),
    regexp = "Your cls parameter doesn't have field auth!",
    fixed = TRUE
  )

  # Test no get URL in cls
  testthat::expect_error(
    setup_resource_obj$get(cls = test_cls_no_get, id = test_id),
    regexp = "Unable to retrieve resource!",
    fixed = TRUE
  )

  # Query fails when no id is provided
  testthat::expect_error(
    setup_resource_obj$get(cls = test_cls, id = NULL),
    regexp = "Please provide id parameter!",
    fixed = TRUE
  )
})

test_that("Resource delete works", {
  # Setup test parameters for test
  test_cls <- list(auth = "foo", URL = list(delete = "foo"))
  test_cls_no_auth <- list(auth = NULL, URL = list(delete = "foo"))
  test_cls_no_delete <-
    list(auth = "foo", URL = list(delete = NULL))
  test_id <- "foo"

  # Query fails when no cls is provided
  testthat::expect_error(setup_resource_obj$delete(cls = NULL),
    regexp = "Please provide cls parameter!",
    fixed = TRUE
  )

  # Query fails when no auth in cls parameter
  testthat::expect_error(
    setup_resource_obj$delete(cls = test_cls_no_auth, id = test_id),
    regexp = "Your cls parameter doesn't have field auth!",
    fixed = TRUE
  )

  # Test no delete URL in cls
  testthat::expect_error(
    setup_resource_obj$delete(cls = test_cls_no_delete, id = test_id),
    regexp = "Resource can not be deleted!",
    fixed = TRUE
  )

  # Query fails when no id is provided
  testthat::expect_error(
    setup_resource_obj$delete(cls = test_cls, id = NULL),
    regexp = "Please provide id parameter!",
    fixed = TRUE
  )
})
