test_that("Collection initialization works", {
  # Collection object creation works
  testthat::expect_no_error(Collection$new(auth = setup_auth_object))

  # VolumeFile object class and methods are set
  checkmate::assert_r6(
    setup_collection_obj,
    classes = c("Collection"),
    public = c(
      "href", "items", "links", "next_page", "prev_page", "auth", "response"
    )
  )
})

test_that("Collection's pagination method next_page() throws error when needed", { # nolint
  collection_obj <- Collection$new(
    href = "some-href",
    items = list(),
    links = list(),
    response = list(raw = "raw-response-list"),
    auth = setup_auth_object
  )
  testthat::expect_error(collection_obj$next_page(),
    regexp = "No more entries to be returned.",
    fixed = TRUE
  )
  # Test non-list links field
  collection_obj$links <- "some-string"
  testthat::expect_error(collection_obj$next_page(),
    regexp = "Assertion on 'self$links' failed: Must be of type 'list' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
  # Test when you reached the last page of results and have only prev links
  collection_obj$links <- list(list(href = "link-to-prev-page", method = "GET", rel = "prev")) # nolint
  testthat::expect_error(collection_obj$next_page(),
    regexp = "You've reached the last page of results.", # nolint
    fixed = TRUE
  )
})

test_that("Collection's pagination method prev_page() throws error when needed", { # nolint
  collection_obj <- Collection$new(
    href = "some-href",
    items = list(),
    links = list(),
    response = list(raw = "raw-response-list"),
    auth = setup_auth_object
  )
  testthat::expect_error(collection_obj$prev_page(),
    regexp = "No more entries to be returned.",
    fixed = TRUE
  )
  # Test non-list links field
  collection_obj$links <- "some-string"
  testthat::expect_error(collection_obj$prev_page(),
    regexp = "Assertion on 'self$links' failed: Must be of type 'list' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )
  # Test when you reached the first page of results and have only next links
  collection_obj$links <- list(list(href = "link-to-next-page", method = "GET", rel = "next")) # nolint
  testthat::expect_error(collection_obj$prev_page(),
    regexp = "You've reached the first page of results.", # nolint
    fixed = TRUE
  )
})

test_that("Collection print method works", {
  testthat::skip_on_ci()
  testthat::expect_snapshot(setup_collection_obj$print())
})

test_that("Collection's method all() throws error when needed", {
  collection_obj <- setup_collection_obj$clone()
  collection_obj$href <- NULL
  testthat::expect_error(collection_obj$all(),
    regexp = "Resource URL is empty or you've already fetched all results.", # nolint
    fixed = TRUE
  )
})
