test_that("User initialization works", {
  # Item object creation works
  testthat::expect_no_error(asUser(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_user_object,
    classes = c("Item", "User"),
    public = c(
      "tags", "role", "zip_code", "country", "state", "city",
      "address", "phone", "affiliation", "last_name", "first_name",
      "email", "username", "URL", "href", "auth", "response", "reload", "print"
    )
  )
})

test_that("User print method works", {
  testthat::expect_snapshot(setup_user_object$print())
})
