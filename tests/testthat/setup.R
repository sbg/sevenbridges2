### Script for setting testing variables
devtools::load_all()

# Dummy token
dummy_token <-
  stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

# Different bad token types
bad_tokens <- list(NA, "", NULL, list())

# Different bad url types
bad_urls <- list(NA, "", NULL, list())

# Different bad method types
bad_methods <- c(NA, "", NULL, "PUTT")

# Different bad encoding types
bad_encodings <- c(NA, "", NULL, "something else")

credentials_path <- testthat::test_path(
  "test_data",
  "sbg_credentials_test_file"
)

# Auth object
setup_auth_object <-
  Auth$new(from = "file", config_file = credentials_path)

# Permission obj
setup_permission_obj <-
  Permission$new(
    write = TRUE,
    read = TRUE,
    copy = TRUE,
    execute = TRUE,
    admin = TRUE
  )
# Project obj
setup_project_obj <-
  Project$new(
    id = "project_id",
    name = "Project name",
    billing_group = "billing group",
    description = "Project description",
    type = "v2",
    tags = list("Tag1", "Tag2"),
    settings = list("locked" = FALSE),
    root_folder = "root_folder_id",
    created_by = "user1",
    created_on = Sys.time(),
    modified_on = Sys.time(),
    permissions = setup_permission_obj,
    category = "PRIVATE"
  )

# Resource_obj
setup_resource_obj <- Resource$new(auth = setup_auth_object)

# Apps obj
setup_apps_obj <- Apps$new(auth = setup_auth_object)

# Close session at the end of tests
withr::defer(teardown_env())
