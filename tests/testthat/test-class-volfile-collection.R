test_that("VolumeFileCollection initialization works", {
  # VolumeFileCollection object creation works
  testthat::expect_error(VolumeFileCollection$new(auth = setup_auth_object),
    regexp = "Assertion on 'items' failed: Must be of type 'list', not 'logical'.", # nolint
    fixed = TRUE
  )

  # VolumeFile object class and methods are set
  checkmate::assert_r6(
    setup_volfile_collection_obj,
    classes = c("VolumeFileCollection", "Collection"),
    public = c(
      "href", "items", "links", "next_page", "prev_page", "auth", "response"
    )
  )
  testthat::expect_true(all(
    sapply(
      setup_volfile_collection_obj$items,
      function(x) checkmate::test_r6(x, classes = "VolumeFile")
    )
  ))
})

test_that("VolumeFileCollection's pagination methods throw error when needed", { # nolint
  volfile_collection_obj <- VolumeFileCollection$new(
    href = "some-href",
    items = list(),
    prefixes = list(),
    links = list(),
    response = list(raw = "raw-response-list"),
    auth = setup_auth_object
  )
  testthat::expect_error(volfile_collection_obj$next_page(),
    regexp = "No more entries to be returned.",
    fixed = TRUE
  )
  # Test non-list links field
  volfile_collection_obj$links <- "some-string"
  testthat::expect_error(volfile_collection_obj$next_page(),
    regexp = "Assertion on 'self$links' failed: Must be of type 'list' (or 'NULL'), not 'character'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(volfile_collection_obj$prev_page(),
    regexp = "Cannot paginate backwards.",
    fixed = TRUE
  )
})

test_that("VolumeFileCollection print method works", {
  testthat::expect_snapshot(setup_volfile_collection_obj$print())
})
