### Script for setting testing variables

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
setup_auth_object <- Auth$new(from = "file", config_file = credentials_path)

# Resource_obj
setup_resource_obj <- Resource$new(auth = setup_auth_object)

# Close session at the end of tests
withr::defer(teardown_env())
