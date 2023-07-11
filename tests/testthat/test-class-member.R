test_that("Member initialization works", {
  # Item object creation works
  testthat::expect_no_error(
    Member$new(
      id = "test-user",
      username = "test-user", email = "test-user@gmail.com", type = "USER",
      permissions = Permission$new()
    )
  )

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_project_member_object,
    classes = c("Item", "Member"),
    public = c(
      "id", "username", "email", "type", "permissions", "href",
      "auth", "response", "print"
    )
  )
})

test_that("Member print method works", {
  testthat::expect_snapshot(setup_project_member_object$print())
})
