test_that("Team initialization works", {
  # Item object creation works
  testthat::expect_no_error(asTeam(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_team_obj,
    classes = c("Item", "Team"),
    public = c(
      "id", "name", "print", "reload", "list_members", "add_member",
      "remove_member", "rename", "delete"
    )
  )
})

test_that("Team print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_no_error(setup_team_obj$print())
  testthat::expect_snapshot(setup_team_obj$print())
})


test_that("Team list_members method throws errors when expected", {
  # Limit parameter is not valid
  negative_limit <- list(limit = -1)
  string_limit <- list(limit = "limit")
  big_limit <- list(limit = 1500)

  testthat::expect_error(
    do.call(setup_team_obj$list_members, negative_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_team_obj$list_members, string_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_team_obj$list_members, big_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )

  # Offset parameter is not valid
  negative_offset <- list(offset = -10)
  string_offset <- list(offset = "offset")

  testthat::expect_error(
    do.call(setup_team_obj$list_members, negative_offset),
    regexp = "Offset must be integer number >= 0.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_team_obj$list_members, string_offset),
    regexp = "Offset must be integer number >= 0.",
    fixed = TRUE
  )
})


test_that("Team add_member method throws errors when expected", {
  # 1. Missing user parameter
  missing_user <- list(user = NULL)

  testthat::expect_error(
    do.call(setup_team_obj$add_member, missing_user),
    regexp = "Please provide a username or a User object to add the member to the team.", # nolint
    fixed = TRUE
  )

  # 2. Pass invalid user parameter
  testthat::expect_error(
    setup_team_obj$add_member(
      user = setup_file_obj
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'User', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_team_obj$add_member(
      user = 1234
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Team remove_member method throws errors when expected", {
  # 1. Missing user parameter
  missing_user <- list(user = NULL)

  testthat::expect_error(
    do.call(setup_team_obj$remove_member, missing_user),
    regexp = "Please provide a username or a User object to remove the member from the team.", # nolint
    fixed = TRUE
  )

  # 2. Pass invalid user parameter
  testthat::expect_error(
    setup_team_obj$remove_member(
      user = setup_file_obj
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'User', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_team_obj$remove_member(
      user = 1234
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Team rename method throws errors when expected", {
  # 1. Missing name parameter
  missing_name_param <- list(name = NULL)

  testthat::expect_error(
    do.call(setup_team_obj$rename, missing_name_param),
    regexp = "Please provide the new name for the team.", # nolint
    fixed = TRUE
  )

  # 2. Pass invalid name parameter
  testthat::expect_error(
    setup_team_obj$rename(
      name = 1234
    ),
    regexp = "Assertion on 'name' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
})
