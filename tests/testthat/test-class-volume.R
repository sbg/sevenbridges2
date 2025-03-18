test_that("Volume initialization works", {
  # Item object creation works
  testthat::expect_no_error(asVolume(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_s3_volume_obj,
    classes = c("Item", "Volume"),
    public = c(
      "URL", "id", "name", "service", "access_mode", "active", "created_on",
      "modified_on", "get_file", "list_contents",
      "delete", "reactivate", "deactivate", "update",
      "list_members", "get_member", "add_member", "add_member_team",
      "add_member_division", "remove_member", "modify_member_permissions",
      "list_imports", "reload"
    ), private = c("add_member_general")
  )
})

test_that("Volume print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_s3_volume_obj$print())
})

test_that("Volume update method throws error when expected", {
  # Pass non-string description
  testthat::expect_error(
    setup_s3_volume_obj$update(description = 1234),
    regexp = "Assertion on 'description' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass non-string access_mode
  testthat::expect_error(
    setup_s3_volume_obj$update(access_mode = TRUE),
    regexp = "Assertion on 'access_mode' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass invalid access_mode
  testthat::expect_error(
    setup_s3_volume_obj$update(access_mode = "Read-only"),
    regexp = "Access mode must be RW or RO.",
    fixed = TRUE
  )
  # Pass non-list service param
  testthat::expect_error(
    setup_s3_volume_obj$update(service = 1234),
    regexp = "Assertion on 'service' failed: Must be of type 'list' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid service param
  testthat::expect_error(
    setup_s3_volume_obj$update(
      service = list(
        some_field1 = "value",
        some_field2 = NULL
      )
    ),
    regexp = "Assertion on 'service' failed: Contains missing values (element 2).", # nolint
    fixed = TRUE
  )
})

test_that("Volume deactivation method throws error when expected", {
  # Try to deactivate volume that is already deactivated
  setup_s3_volume_obj$active <- FALSE
  testthat::expect_error(
    setup_s3_volume_obj$deactivate(),
    regexp = "The volume my_new_volume is already deactivated.", # nolint
    fixed = TRUE
  )
  setup_s3_volume_obj$active <- TRUE
})

test_that("Volume reactivation method throws error when expected", {
  # Try to reactivate volume that is already active
  testthat::expect_error(
    setup_s3_volume_obj$reactivate(),
    regexp = "The volume my_new_volume is already active.", # nolint
    fixed = TRUE
  )
})

test_that("Volume deletion method throws error when expected", {
  # Try to delete volume that is not deactivated
  testthat::expect_error(
    setup_s3_volume_obj$delete(),
    regexp = "The volume my_new_volume must be deactivated first in order to be able to delete it.", # nolint
    fixed = TRUE
  )
})

test_that("Volume list_contents method throws error when expected", {
  # Pass non-string prefix
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(prefix = 1234),
    regexp = "Assertion on 'prefix' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass invalid prefix
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(prefix = NA),
    regexp = "Assertion on 'prefix' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string link
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(link = 1234),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(link = NA),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string continuation_token
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(continuation_token = 1234),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$list_contents(continuation_token = NA),
    regexp = "Assertion on 'continuation_token' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume get_file method throws error when expected", {
  # Pass non-string location
  testthat::expect_error(
    setup_s3_volume_obj$get_file(location = 1234),
    regexp = "Assertion on 'location' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  # Pass invalid location
  testthat::expect_error(
    setup_s3_volume_obj$get_file(location = NA),
    regexp = "Assertion on 'location' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )
  # Pass non-string link
  testthat::expect_error(
    setup_s3_volume_obj$get_file(link = 1234), # nolint
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'double'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$get_file(link = NA),
    regexp = "Assertion on 'link' failed: Must be of type 'character' (or 'NULL'), not 'logical'.", # nolint
    fixed = TRUE
  )

  # Check empty args
  testthat::expect_error(
    setup_s3_volume_obj$get_file(),
    regexp = "Empty arguments are not allowed. Please provide either location or link.", # nolint
    fixed = TRUE
  )
  # Check if both args are provided
  testthat::expect_error(
    setup_s3_volume_obj$get_file(
      location = "file-location",
      link = "link"
    ),
    regexp = "Please provide either location or link, not both.",
    fixed = TRUE
  )
})

test_that("Volume add_member method for USER type throws error when expected", {
  # Pass invalid user param
  testthat::expect_error(
    setup_s3_volume_obj$add_member(
      user = setup_file_obj,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$add_member(
      user = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'user' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume remove_member method throws error when expected", {
  # Pass invalid member param
  testthat::expect_error(
    setup_s3_volume_obj$remove_member(
      member = setup_file_obj
    ),
    regexp = "Assertion on 'member' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(setup_s3_volume_obj$remove_member(member = 1234),
    regexp = "Assertion on 'member' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume get_member() method throws error when expected", {
  # Pass invalid 'member' param
  testthat::expect_error(
    setup_s3_volume_obj$get_member(
      member = setup_file_obj
    ),
    regexp = "Assertion on 'member' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(setup_s3_volume_obj$get_member(member = 1234),
    regexp = "Assertion on 'member' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume modify_member_permissions() method throws error when expected", { # nolint
  # Pass invalid member param
  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = setup_file_obj,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'member' failed: Must inherit from class 'Member', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'member' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )

  # Pass invalid permissions params
  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = 1234
    ),
    regexp = "Assertion on 'permissions' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = list()
    ),
    regexp = "Assertion on 'names(permissions)' failed: Must be a subset of {'read','copy','write','admin'}, not empty.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = list(read = 123, copy = FALSE, admin = FALSE)
    ),
    regexp = "Assertion on 'permissions' failed: May only contain the following types: {logical}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE,
        run = TRUE
      )
    ),
    regexp = "Assertion on 'permissions' failed: Must have length <= 4, but has length 5.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = 1234
    ),
    regexp = "Assertion on 'permissions' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$modify_member_permissions(
      member = "test-username",
      permissions = list(readme = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'names(permissions)' failed: Must be a subset of {'read','copy','write','admin'}, but has additional elements {'readme'}", # nolint
    fixed = TRUE
  )
})

test_that("Volume add_member_team method for TEAM type throws error when expected", { # nolint
  # Pass invalid team param
  testthat::expect_error(
    setup_s3_volume_obj$add_member_team(
      team = setup_file_obj,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'team' failed: Must inherit from class 'Team', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$add_member_team(
      team = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'team' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume add_member_division method for DIVISION type throws error when expected", { # nolint
  # Pass invalid division param
  testthat::expect_error(
    setup_s3_volume_obj$add_member_division(
      division = setup_file_obj,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'division' failed: Must inherit from class 'Division', but has classes 'File','Item','R6'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$add_member_division(
      division = 1234,
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'division' failed: Must be of type 'character', not 'double'.", # nolint
    fixed = TRUE
  )
})

test_that("Volume add_member_general method for any type throws error when expected", { # nolint
  # 1. Pass invalid 'member' param
  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = NULL
    ),
    regexp = "Assertion on 'member' failed: Must be of type 'string', not 'NULL'.", # nolint
    fixed = TRUE
  )
  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = 123
    ),
    regexp = "Assertion on 'member' failed: Must be of type 'string', not 'double'.", # nolint
    fixed = TRUE
  )

  # 2. Pass invalid 'permissions' param
  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = 1234
    ),
    regexp = "Assertion on 'permissions' failed: Must be of type 'list', not 'double'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list()
    ),
    regexp = "Assertion on 'permissions' failed: Must have length 4, but has length 0.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(read = 123, copy = FALSE, write = 234, admin = FALSE)
    ),
    regexp = "Assertion on 'permissions' failed: May only contain the following types: {logical}, but element 1 has type 'numeric'.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(read = TRUE, copy = TRUE)
    ),
    regexp = "Assertion on 'permissions' failed: Must have length 4, but has length 2.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE,
        run = TRUE
      )
    ),
    regexp = "Assertion on 'permissions' failed: Must have length 4, but has length 5.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(
        readme = TRUE, copyme = TRUE, admin = FALSE, write = FALSE
      )
    ),
    regexp = "Assertion on 'names(permissions)' failed: Must be a subset of {'read','copy','write','admin'}, but has additional elements {'readme','copyme'}", # nolint
    fixed = TRUE
  )

  # 3. Test invalid 'type' param
  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE
      ),
      type = NULL
    ),
    regexp = "Assertion on 'type' failed: Must be a subset of {'USER','TEAM','DIVISION'}, not empty.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE
      ),
      type = "MEMBER"
    ),
    regexp = "Assertion on 'type' failed: Must be a subset of {'USER','TEAM','DIVISION'}, but has additional elements {'MEMBER'}.", # nolint
    fixed = TRUE
  )

  testthat::expect_error(
    setup_s3_volume_obj$private$add_member_general(
      member = "test-username",
      permissions = list(
        read = TRUE, copy = TRUE, admin = FALSE, write = FALSE
      ),
      type = 123
    ),
    regexp = "Assertion on 'type' failed: Must be a subset of {'USER','TEAM','DIVISION'}, but has different type.", # nolint
    fixed = TRUE
  )
})
