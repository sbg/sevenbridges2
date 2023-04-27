### Script for setting testing variables

# Dummy token
dummy_token <- stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

# Different bad token types
bad_tokens <- list(NA, "", NULL, list())

# Different bad url types
bad_urls <- list(NA, "", NULL, list())

# Different bad method types
bad_methods <- c(NA, "", NULL, "PUTT")

# Different bad encoding types
bad_encodings <- c(NA, "", NULL, "something else")



# Close session at the end of tests
# withr::defer(session$close(), teardown_env())
