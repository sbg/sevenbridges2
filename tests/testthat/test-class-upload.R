test_that("Upload object initialization works", {
  # Upload object creation throws error when path is missing
  testthat::expect_error(Upload$new(auth = setup_auth_object))

  # Upload object class and methods are set
  checkmate::assert_r6(
    setup_upload_object,
    classes = c("Upload"),
    public = c(
      "upload_id", "path", "project", "parent", "filename", "overwrite",
      "file_size", "part_size", "part_length", "parts", "initialized", "auth",
      "print", "init", "info", "start", "abort"
    ),
    private = c(
      "generate_parts", "upload_complete_all"
    )
  )
  testthat::expect_equal(setup_upload_object$part_length, 8)
  testthat::expect_length(setup_upload_object$parts, 8)
  testthat::expect_true(all(sapply(
    setup_upload_object$parts,
    function(x) inherits(x, "Part")
  )))
})

test_that("Part object initialization works", {
  # Part object creation works
  testthat::expect_no_error(Part$new(auth = setup_auth_object))

  # Part object class and methods are set
  checkmate::assert_r6(
    setup_upload_object$parts[[1]],
    classes = c("Part"),
    public = c(
      "part_number", "part_size", "url", "expires", "headers",
      "success_codes", "report", "etag", "auth", "response",
      "print", "upload_info_part", "upload_complete_part"
    )
  )

  first_part <- setup_upload_object$parts[[1]]
  testthat::expect_equal(first_part$part_number, 1)
  testthat::expect_equal(first_part$part_size, setup_upload_object$part_size)

  last_part <- tail(setup_upload_object$parts, 1)[[1]]
  testthat::expect_equal(last_part$part_number, 8)
  testthat::expect_lt(last_part$part_size, setup_upload_object$part_size)
})

test_that("upload$init function throws error when needed", {
  setup_upload_object$initialized <- TRUE
  testthat::expect_error(
    setup_upload_object$init(),
    regexp = "Upload has already been initialized.",
    fixed = TRUE
  )
  setup_upload_object$initialized <- FALSE
})

test_that("upload$info function throws error when needed", {
  testthat::expect_error(
    setup_upload_object$info(),
    regexp = "Upload has not been initialized yet",
    fixed = TRUE
  )

  # Set initialized to TRUE
  setup_upload_object$initialized <- TRUE
  testthat::expect_error(
    setup_upload_object$info(list_parts = 123),
    regexp = "Assertion on 'list_parts' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
  setup_upload_object$initialized <- FALSE
})


test_that("upload$start function throws error when needed", {
  testthat::expect_error(
    setup_upload_object$start(),
    regexp = "Upload has not been initialized yet",
    fixed = TRUE
  )
})

test_that("upload$abort function throws error when needed", {
  testthat::expect_error(
    setup_upload_object$abort(),
    regexp = "Upload has not been initialized yet",
    fixed = TRUE
  )
})

test_that("part$upload_info_part and part$upload_complete_part functions
          throw errors when needed", {
  test_part <- setup_upload_object$parts[[1]]
  testthat::expect_error(
    test_part$upload_info_part(upload_id = 123),
    regexp = "Assertion on 'upload_id' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    test_part$upload_complete_part(upload_id = 123),
    regexp = "Assertion on 'upload_id' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})
