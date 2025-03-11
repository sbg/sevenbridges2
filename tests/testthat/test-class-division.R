test_that("Division initialization works", {
  # Item object creation works
  testthat::expect_no_error(asDivision(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_division_obj,
    classes = c("Item", "Division"),
    public = c(
      "id", "name", "print", "reload", "list_teams", "list_members",
      "remove_member"
    )
  )
})

test_that("Division print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_no_error(setup_division_obj$print())
  testthat::expect_snapshot(setup_division_obj$print())
})

test_that("Division list_teams method throws errors when expected", {
  # Setup test parameters for test
  test_wrong_list_all_num <- list(list_all = 1234)
  test_wrong_list_all_string <- list(list_all = "true")
  test_wrong_list_all_list <- list(list_all = list(TRUE))


  # Query fails when bad 'list_all' parameters are provided
  # nolint start
  testthat::expect_error(do.call(setup_div_with_admin$list_teams, test_wrong_list_all_num),
    regexp = "Assertion on 'list_all' failed: Must be of type 'logical', not 'double'.",
    fixed = TRUE
  )

  testthat::expect_error(do.call(setup_div_with_admin$list_teams, test_wrong_list_all_string),
    regexp = "Assertion on 'list_all' failed: Must be of type 'logical', not 'character'.",
    fixed = TRUE
  )

  testthat::expect_error(do.call(setup_div_with_admin$list_teams, test_wrong_list_all_list),
    regexp = "Assertion on 'list_all' failed: Must be of type 'logical', not 'list'.",
    fixed = TRUE
  )
  # nolint end
})

test_that("Division list_members method throws errors when expected", {
  # Setup test parameters for test
  test_non_supported_role <- list(role = "MANAGER")
  test_invalid_role_param_type <- list(role = 1234)
  test_invalid_role_list <- list(role = list("ADMIN"))
  test_invalid_role_logical <- list(role = TRUE)

  # Query fails when non-supported 'role' parameter is provided
  # nolint start
  testthat::expect_error(do.call(setup_div_with_admin$list_members, test_non_supported_role),
    regexp = "Assertion on 'role' failed: Must be element of set {'MEMBER','ADMIN','EXTERNAL_COLLABORATOR'}, but is 'MANAGER'.",
    fixed = TRUE
  )
  # nolint end

  # Query fails when invalid 'role' parameter type is provided
  # nolint start
  testthat::expect_error(do.call(setup_div_with_admin$list_members, test_invalid_role_param_type),
    regexp = "Assertion on 'role' failed: Must be element of set {'MEMBER','ADMIN','EXTERNAL_COLLABORATOR'}, but types do not match (numeric != character).",
    fixed = TRUE
  )

  testthat::expect_error(do.call(setup_div_with_admin$list_members, test_invalid_role_list),
    regexp = "Assertion on 'role' failed: Must be element of set {'MEMBER','ADMIN','EXTERNAL_COLLABORATOR'}, but is not atomic scalar.",
    fixed = TRUE
  )

  testthat::expect_error(do.call(setup_div_with_admin$list_members, test_invalid_role_logical),
    regexp = "Assertion on 'role' failed: Must be element of set {'MEMBER','ADMIN','EXTERNAL_COLLABORATOR'}, but types do not match (logical != character).",
    fixed = TRUE
  )
  # nolint end

  # Limit parameter is not valid
  negative_limit <- list(limit = -1)
  string_limit <- list(limit = "limit")
  big_limit <- list(limit = 1500)

  testthat::expect_error(
    do.call(setup_div_with_admin$list_members, negative_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_div_with_admin$list_members, string_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_div_with_admin$list_members, big_limit),
    regexp = "Limit must be integer number between 1 and 100.",
    fixed = TRUE
  )

  # Offset parameter is not valid
  negative_offset <- list(offset = -10)
  string_offset <- list(offset = "offset")

  testthat::expect_error(
    do.call(setup_div_with_admin$list_members, negative_offset),
    regexp = "Offset must be integer number >= 0.",
    fixed = TRUE
  )
  testthat::expect_error(
    do.call(setup_div_with_admin$list_members, string_offset),
    regexp = "Offset must be integer number >= 0.",
    fixed = TRUE
  )
})

test_that("Division remove_member method throws error when expected", {
  # Pass invalid user param
  # Remover: Division member (user) with the 'ADMIN' role
  testthat::expect_error(
    setup_div_with_admin$remove_member(user = NULL),
    regexp = "Please provide a username or a User object to remove the member from the division.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_div_with_admin$remove_member(
      user = setup_file_obj
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'User', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_div_with_admin$remove_member(
      user = 1234
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # Remove a division member without having the 'ADMIN' role
  # Remover: Division member (user) with 'MEMBER' role
  testthat::expect_error(
    setup_div_with_member$remove_member(user = "some-id/some-username"),
    regexp = "You don't have the required permissions to remove members from this division. Only users with the 'ADMIN' role can perform this action.", # nolint
    fixed = TRUE
  )

  # Remover: Division member with 'EXTERNAL_COLLABORATOR' role
  testthat::expect_error(
    setup_div_with_ext_collab$remove_member(user = "some-id/fleur_delacour"),
    regexp = "You don't have the required permissions to remove members from this division. Only users with the 'ADMIN' role can perform this action.", # nolint
    fixed = TRUE
  )
})
