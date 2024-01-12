test_that("VolumeContentCollection initialization works", {
  # VolumeContentCollection object creation works
  testthat::expect_error(asVolumeContentCollection(auth = setup_auth_object),
    regexp = "Assertion on 'res$items' failed: Must be of type 'list', not 'NULL'.", # nolint
    fixed = TRUE
  )

  # VolumeFile and VolumePrefix objects class and methods are set
  checkmate::assert_r6(
    setup_volcont_collection_obj,
    classes = c("VolumeContentCollection", "Collection"),
    public = c(
      "href", "items", "prefixes", "links", "next_page", "prev_page", "auth",
      "response"
    )
  )
  testthat::expect_true(all(
    sapply(
      setup_volcont_collection_obj$items,
      function(x) checkmate::test_r6(x, classes = "VolumeFile")
    )
  ))
  testthat::expect_true(all(
    sapply(
      setup_volcont_collection_obj$prefixes,
      function(x) checkmate::test_r6(x, classes = "VolumePrefix")
    )
  ))
})

test_that("VolumeContentCollection's pagination methods throw error when needed", { # nolint

  volfile_collection_obj <- setup_volcont_collection_obj$clone()
  # Empty links
  volfile_collection_obj$links <- list()

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

test_that("VolumeContentCollection's method all() throws error when needed", {
  collection_obj <- setup_volcont_collection_obj$clone()
  collection_obj$href <- NULL
  testthat::expect_error(collection_obj$all(),
    regexp = "Resource URL is empty or you've already fetched all results.", # nolint
    fixed = TRUE
  )
})

test_that("VolumeContentCollection print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_volcont_collection_obj$print())
})
