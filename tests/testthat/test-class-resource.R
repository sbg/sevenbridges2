test_that("Resource initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Resource$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_resource_obj,
    classes = c("Resource"),
    public = c("query", "get", "delete", "URL")
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
  test_no_id <- list(id = NULL)

  # Set delete URL on setup_resource_obj to be empty
  setup_resource_obj$URL <- list()
  # Query fails when resource doesn't have delete URL
  testthat::expect_error(setup_resource_obj$delete(id = "some_id"),
    regexp = "Resource can not be deleted!",
    fixed = TRUE
  )

  # Set delete URL on setup_resource_obj
  setup_resource_obj$URL <- list("delete" = "delete_url")
  # Query fails when no id is provided
  testthat::expect_error(do.call(setup_resource_obj$delete(), test_no_id),
    regexp = "Please provide id parameter!",
    fixed = TRUE
  )
})
