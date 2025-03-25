test_that("Teams query() method fails when intended", {
  # 1. Missing division parameter
  no_division <- list(division = NULL)
  testthat::expect_error(
    do.call(setup_teams_obj$query, no_division),
    regexp = "Please provide the division ID or Division object you're querying.", # nolint
    fixed = TRUE
  )

  # 2. Division parameter of wrong type
  division_num <- list(division = 123)
  testthat::expect_error(
    do.call(setup_teams_obj$query, division_num),
    regexp = "Assertion on 'division' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. Division parameter of wrong class
  division_proj <- list(division = setup_project_obj)
  testthat::expect_error(
    do.call(setup_teams_obj$query, division_proj),
    regexp = "Assertion on 'division' failed: Must inherit from class 'Division', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  # 4. list_all parameter of wrong type
  params <- list(division = setup_division_obj, list_all = 123)
  testthat::expect_error(
    do.call(setup_teams_obj$query, params),
    regexp = "Assertion on 'list_all' failed: Must be of type 'logical', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Teams get() method fails when intended", {
  # 1. Missing id parameter
  no_id <- list(id = NULL)
  testthat::expect_error(
    do.call(setup_teams_obj$get, no_id),
    regexp = "Please provide the 'id' parameter.",
    fixed = TRUE
  )

  # 2. ID parameter of wrong type
  id_num <- list(id = 123)
  testthat::expect_error(
    do.call(setup_teams_obj$get, id_num),
    regexp = "Assertion on 'id' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. ID parameter of wrong class
  id_proj <- list(id = setup_project_obj)
  testthat::expect_error(
    do.call(setup_teams_obj$get, id_proj),
    regexp = "Assertion on 'id' failed: Must inherit from class 'Team', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
})

test_that("Teams create() method fails when intended", {
  # 1. Missing division or name parameter
  no_division <- list(division = NULL, name = "new-name")
  testthat::expect_error(
    do.call(setup_teams_obj$create, no_division),
    regexp = "Division or new team name is missing. Please provide both parameters.", # nolint
    fixed = TRUE
  )

  no_name <- list(division = "division-id", name = NULL)
  testthat::expect_error(
    do.call(setup_teams_obj$create, no_name),
    regexp = "Division or new team name is missing. Please provide both parameters.", # nolint
    fixed = TRUE
  )

  # 2. Division parameter of wrong type
  division_num <- list(division = 123, name = "new-name")
  testthat::expect_error(
    do.call(setup_teams_obj$create, division_num),
    regexp = "Assertion on 'division' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. Division parameter of wrong class
  division_proj <- list(division = setup_project_obj, name = "new-name")
  testthat::expect_error(
    do.call(setup_teams_obj$create, division_proj),
    regexp = "Assertion on 'division' failed: Must inherit from class 'Division', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )

  # 4. name parameter of wrong type
  params <- list(division = setup_division_obj, name = 123)
  testthat::expect_error(
    do.call(setup_teams_obj$create, params),
    regexp = "Assertion on 'name' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Teams delete() method fails when intended", {
  # 1. Missing team parameter
  no_team <- list(team = NULL)
  testthat::expect_error(
    do.call(setup_teams_obj$delete, no_team),
    regexp = "Please provide the team ID as string or as Team object.",
    fixed = TRUE
  )

  # 2. Team parameter of wrong type
  team_num <- list(team = 123)
  testthat::expect_error(
    do.call(setup_teams_obj$delete, team_num),
    regexp = "Assertion on 'team' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # 3. Team parameter of wrong class
  team_proj <- list(team = setup_project_obj)
  testthat::expect_error(
    do.call(setup_teams_obj$delete, team_proj),
    regexp = "Assertion on 'team' failed: Must inherit from class 'Team', but has classes 'Project','Item','R6'.", # nolint
    fixed = TRUE
  )
})
