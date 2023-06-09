test_that("Volumes initialization works", {
  # Resource object creation works
  testthat::expect_no_error(Volumes$new(auth = setup_auth_object))

  # Resource object class and methods are set
  checkmate::assert_r6(
    setup_volumes_obj,
    classes = c("Resource", "Volumes"),
    public = c(
      "URL", "query", "get",
      "create_s3_using_iam_user", "create_s3_using_iam_role",
      "create_google_using_iam_user", "create_google_using_iam_role"
    )
  )
})

test_that("Volumes get() throws error when needed", {
  # Setup test parameters for test
  test_no_id <- list(id = NULL)
  test_bad_id <- list(id = 1)

  # Get fails when no id is provided
  testthat::expect_error(do.call(setup_volumes_obj$get, test_no_id))

  # Get fails when bad id is provided
  testthat::expect_error(do.call(setup_volumes_obj$get, test_bad_id))
})

test_that("Creating AWS volumes with IAM User type throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_s3_using_iam_user())

  # Pass args as regular function params, but don't pass credentials
  main_args <- list(
    name = "volume_name",
    bucket = "bucket_name",
    prefix = "",
    access_mode = "RW",
    description = NULL,
    properties = list("some-property" = "value"),
    endpoint = "some-endpoint"
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_user,
    main_args
  ))
  # Pass args as regular function params, but don't pass access_key_id
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_user,
    append(
      main_args,
      list(secret_access_key = "secret-key")
    )
  )) # nolint
  # Pass args as regular function params, but don't pass secret_access_key
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_user,
    append(
      main_args,
      list(access_key_id = "some-key")
    )
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_s3_using_iam_user fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_user(from_path = test_invalid_path)
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_user(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "aws_iam_user_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_user(from_path = no_creds_json)
  )

  # Pass from_path as path to json that doesn't contain one of creds parameters
  not_all_creds_json <- testthat::test_path(
    "test_data",
    "aws_iam_user_test_not_all_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_user(from_path = not_all_creds_json)
  )
})

test_that("Creating AWS volumes with IAM Role type throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_s3_using_iam_role())

  # Pass args as regular function params, but don't pass credentials
  main_args <- list(
    name = "volume_name",
    bucket = "bucket_name",
    prefix = "",
    access_mode = "RW",
    description = NULL,
    properties = list("some-property" = "value"),
    endpoint = "some-endpoint"
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_role,
    main_args
  ))
  # Pass args as regular function params, but don't pass role_arn
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_role,
    append(
      main_args,
      list(external_id = "external_id-key")
    )
  )) # nolint
  # Pass args as regular function params, but don't pass external_id
  testthat::expect_error(do.call(
    setup_volumes_obj$create_s3_using_iam_role,
    append(
      main_args,
      list(role_arn = "role_arn-key")
    )
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_s3_using_iam_role fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_role(from_path = test_invalid_path)
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_role(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "aws_iam_role_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_role(from_path = no_creds_json)
  )

  # Pass from_path as path to json that doesn't contain one of creds parameters
  not_all_creds_json <- testthat::test_path(
    "test_data",
    "aws_iam_role_test_not_all_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_s3_using_iam_role(from_path = not_all_creds_json)
  )
})

test_that("Creating GC volumes with IAM User type throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_google_using_iam_user())

  # Pass args as regular function params, but don't pass credentials
  main_args <- list(
    name = "volume_name",
    bucket = "bucket_name",
    prefix = "",
    access_mode = "RW",
    description = NULL,
    properties = list("some-property" = "value"),
    root_url = "some-endpoint"
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_google_using_iam_user, # nolint
    main_args
  ))
  # Pass args as regular function params, but don't pass private_key
  testthat::expect_error(do.call(
    setup_volumes_obj$create_google_using_iam_user,
    append(
      main_args,
      list(client_email = "client_email")
    )
  )) # nolint
  # Pass args as regular function params, but don't pass client_email
  testthat::expect_error(do.call(
    setup_volumes_obj$create_google_using_iam_user,
    append(
      main_args,
      list(private_key = "some-private_key")
    )
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_google_using_iam_user fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_user(from_path = test_invalid_path) # nolint
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_user(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "gc_iam_user_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_user(from_path = no_creds_json)
  )

  # Pass from_path as path to json that doesn't contain one of creds parameters
  not_all_creds_json <- testthat::test_path(
    "test_data",
    "gc_iam_user_test_not_all_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_user(from_path = not_all_creds_json) # nolint
  )
})

test_that("Creating GC volumes with IAM Role type throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_google_using_iam_role())

  # Pass args as regular function params, but don't pass configuration
  main_args <- list(
    name = "volume_name",
    bucket = "bucket_name",
    prefix = "",
    access_mode = "RW",
    description = NULL,
    properties = list("some-property" = "value"),
    root_url = "some-endpoint"
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_google_using_iam_role, # nolint
    main_args
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_google_using_iam_role fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_role(from_path = test_invalid_path) # nolint
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_role(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "gc_iam_role_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_role(from_path = no_creds_json),
    regexp = "Configuration parameter within credentials is mandatory. \n Please, provide a path to JSON configuration file or a named list containing all configuration values.", # nolint
    fixed = TRUE
  )

  # Pass from_path as path to json that contains invalid path
  config_path_invalid_json <- testthat::test_path(
    "test_data",
    "gc_iam_role_test_config_path_invalid.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_role(from_path = config_path_invalid_json) # nolint
  )

  # Pass from_path as path to json that contains invalid params
  config_path_invalid_json <- testthat::test_path(
    "test_data",
    "gc_iam_role_test_config_path_invalid_list.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_google_using_iam_role(from_path = config_path_invalid_json) # nolint
  )
})

test_that("Creating Azure volumes throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_azure())

  # Pass args as regular function params, but don't pass credentials
  # and resource_id
  main_args <- list(
    name = "volume_name",
    description = NULL,
    endpoint = "some-endpoint",
    storage_account = "some-account",
    container = "bucket_name",
    prefix = ""
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_azure,
    main_args
  ))
  # Pass args as regular function params, but pass only tenant_id cred.param
  testthat::expect_error(do.call(
    setup_volumes_obj$create_azure,
    append(
      main_args,
      list(tenant_id = "tenant_id")
    )
  ))
  # Pass args as regular function params, but pass only tenant_id and client_id
  # cred.params
  testthat::expect_error(do.call(
    setup_volumes_obj$create_azure,
    append(
      main_args,
      list(tenant_id = "tenant_id", client_id = "some-client_id")
    )
  ))

  # Pass args as regular function params with credentials, but don't pass
  # resource_id
  testthat::expect_error(do.call(
    setup_volumes_obj$create_azure,
    append(
      main_args,
      list(
        tenant_id = "tenant_id",
        client_id = "some-client_id",
        client_secret = "client_secret_key"
      )
    )
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_azure fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_azure(from_path = test_invalid_path)
  )
  testthat::expect_error(
    setup_volumes_obj$create_azure(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "azure_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_azure(from_path = no_creds_json)
  )

  # Pass from_path as path to json that doesn't contain one of creds parameters
  not_all_creds_json <- testthat::test_path(
    "test_data",
    "azure_test_not_all_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_azure(from_path = not_all_creds_json)
  )

  # Pass from_path as path to json that doesn't contain properties param
  no_properties_json <- testthat::test_path(
    "test_data",
    "azure_test_no_properties.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_azure(from_path = no_properties_json)
  )
})

test_that("Creating Ali cloud volumes throws error when needed", {
  # Pass no args
  testthat::expect_error(setup_volumes_obj$create_ali_oss())

  # Pass args as regular function params, but don't pass credentials
  main_args <- list(
    name = "volume_name",
    description = NULL,
    endpoint = "some-endpoint",
    bucket = "bucket_name",
    prefix = ""
  )
  testthat::expect_error(do.call(
    setup_volumes_obj$create_ali_oss,
    main_args
  ))
  # Pass args as regular function params, but don't pass access_key_id
  testthat::expect_error(do.call(
    setup_volumes_obj$create_ali_oss,
    append(
      main_args,
      list(secret_access_key = "secret_access_key")
    )
  ))
  # Pass args as regular function params, but don't pass secret_access_key
  testthat::expect_error(do.call(
    setup_volumes_obj$create_ali_oss,
    append(
      main_args,
      list(access_key_id = "some-key")
    )
  ))

  # Pass from_path as non-string type
  test_invalid_path <- 123

  # Pass from_path as non-existing
  test_bad_path <- file.path("some", "nonexisting", "path")

  # create_ali_oss fails when invalid path is provided
  testthat::expect_error(
    setup_volumes_obj$create_ali_oss(from_path = test_invalid_path)
  )
  testthat::expect_error(
    setup_volumes_obj$create_ali_oss(from_path = test_bad_path)
  )

  # Pass from_path as path to json that doesn't contain credentials parameters
  no_creds_json <- testthat::test_path(
    "test_data",
    "ali_oss_test_no_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_ali_oss(from_path = no_creds_json)
  )

  # Pass from_path as path to json that doesn't contain one of creds parameters
  not_all_creds_json <- testthat::test_path(
    "test_data",
    "ali_oss_test_not_all_creds.json"
  )
  testthat::expect_error(
    setup_volumes_obj$create_ali_oss(from_path = not_all_creds_json)
  )
})
