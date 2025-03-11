test_that("Team initialization works", {
  # Item object creation works
  testthat::expect_no_error(asTeam(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_team_obj,
    classes = c("Item", "Team"),
    public = c(
      "id", "name", "print", "reload"
    )
  )
})

test_that("Team print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_no_error(setup_team_obj$print())
  testthat::expect_snapshot(setup_team_obj$print())
})
