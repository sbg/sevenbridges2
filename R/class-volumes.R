# nolint start
#' @title R6 Class representing volumes endpoints
#'
#' @description
#' R6 Class representing volumes resource endpoints.
#'
#' @importFrom R6 R6Class
#'
#' @export
Volumes <- R6::R6Class(
  "Volumes",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "query" = "storage/volumes",
      "get" = "storage/volumes/{id}",
      "create" = "storage/volumes",
      "delete" = "storage/volumes"
    ),

    # Initialize Volumes object ----------------------------------------------
    #' @description Create a new Volumes object.
    #'
    #' @param ... Other response arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all volumes you've registered --------------------------------------
    #' @description This call lists all the volumes you've registered.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'limit', 'offset', 'fields', etc.
    #'
    #' @return \code{\link{Collection}} of \code{\link{Volume}} objects.
    query = function(...) {
      # nocov start
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        ...
      )
      res$items <- asVolumeList(res, auth = self$auth)
      return(asCollection(res, auth = self$auth))
    }, # nocov end

    # Get single volume -------------------------------------------------------
    #' @description This call returns details of the specified volume.
    #'  The volume is referred to by its ID, which you can obtain by
    #'  making the call to list all the volumes you've registered.
    #'
    #' @param id The Volume ID consists of volume owner's name (for enterprise
    #'  users) and volume name in form `{volume_owner}/{volume_name}`,
    #'  or division name (if user belongs to some division) and volume
    #'  name in form `{division}/{volume_name}`. You can also get the Volume ID
    #'  for a volume by making the call to list all volumes you've registered.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #'
    #' @return \code{\link{Volume}} object.
    get = function(id) {
      if (is_missing(id)) {
        rlang::abort("Volume ID must be provided!")
      }
      checkmate::assert_string(id)
      # nocov start
      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE
      )
      return(asVolume(res, auth = self$auth))
    }, # nocov end

    # Delete volume -------------------------------------------------------
    #' @description This call deletes a volume you've created to refer to
    #' storage on Amazon Web Services or Google Cloud Storage. To be able to
    #' delete a volume, you first need to deactivate it and then delete all
    #' files on the Platform that were previously imported from the volume.
    #'
    #' Volumes are specified by their IDs, which you can obtain by using
    #' \code{Volumes$query()} to list files or by getting a single file
    #' using \code{Volumes$get()}.
    #'
    #' @param volume \code{\link{Volume}} object or volume ID.
    #' @param ... Other arguments that can be passed to core `api()` function
    #' as 'fields', etc.
    delete = function(volume, ...) {
      id <- check_and_transform_id(volume, "Volume")
      # nocov start
      res <- super$delete(
        id = id,
        advance_access = TRUE,
        ...
      )

      rlang::inform(
        message = glue::glue("Volume {id} has been deleted.")
      )
    }, # nocov end

    # Create new AWS Volume (IAM User type authentication type) ---------------
    #' @description Create new volume to connect to your s3 bucket on AWS cloud.
    #'  Volumes authorize the Platform to access and query objects on a
    #'  specified cloud storage (Amazon Web Services, Google Cloud Storage,
    #'  Azure or Ali cloud) on your behalf. This function uses
    #'  IAM User credentials to connect to your s3 bucket. \cr \cr
    #'
    #'  Read more about volume creation in our
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/create-a-volume-v2).
    # nolint end
    #'
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if from_path parameter
    #'  is not provided.
    #' @param access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    #' @param description An optional description of this volume.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this prefix
    #'  includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param bucket The name of the AWS S3 bucket you wish to register
    #'  as a volume. Required if `from_path` parameter is not provided.
    #' @param endpoint AWS API endpoint to use when accessing this
    #'  bucket. Default: `s3.amazonaws.com`.
    #' @param access_key_id AWS access key ID in form of string of the IAM user
    #'  shared with Seven Bridges to access this bucket.
    #'  Required if `from_path` parameter is not provided.
    #' @param secret_access_key AWS secret access key in form of string of the
    #'  IAM user shared with Seven Bridges to access this bucket.
    #'  Required if `from_path` parameter is not provided.
    #' @param properties Named list containing the properties of a specific
    #'  service. These values set the defaults for operations performed with
    #'  this volume. Individual operations can override these defaults by
    #'  providing a custom properties object. For AWS S3, there are:
    #'  \itemize{
    #'    \item `sse_algorithm` - S3 server-side encryption to use when
    #'      exporting to this bucket. Supported values:
    #'      `AES256` (SSE-S3 encryption), `aws:kms`, `null`
    #'      (no server-side encryption). Default: `AES256`.
    #'    \item `sse_aws_kms_key_id`: Applies to type: `s3`.
    #'      If AWS KMS encryption is used, this should be set to the required
    #'      KMS key. If not set and `aws:kms` is set as `sse_algorithm`,
    #'      default KMS key is used.
    #'    \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'      on during export. Supported values: any one of
    # nolint start
    #'      [S3 canned ACLs](https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl);
    # nolint end
    #'      `null` (do not apply canned ACLs). Default: `null`.
    #'  }
    #' @param from_path Path to JSON configuration file containing all
    #'  required information for registering a volume. If provided, it will
    #'  overwrite all previous parameters set.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_s3_using_iam_user = function(name = NULL,
                                        access_mode = "RW",
                                        description = NULL,
                                        prefix = NULL,
                                        bucket = NULL,
                                        endpoint = "s3.amazonaws.com",
                                        access_key_id = NULL,
                                        secret_access_key = NULL,
                                        properties = list("sse_algorithm" = "AES256"), # nolint
                                        from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          access_key_id = access_key_id,
          secret_access_key = secret_access_key
        )
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }
        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
      }

      check_volume_params(args)

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["access_key_id"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["secret_access_key"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "s3",
          bucket = args[["bucket"]],
          prefix = args[["prefix"]],
          endpoint = args[["endpoint"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    },

    # Create new AWS Volume (IAM Role type authentication type) ---------------
    #' @description Create new volume to connect to your s3 bucket on AWS cloud.
    #'  Volumes authorize the Platform to access and query objects on a
    #'  specified cloud storage (Amazon Web Services, Google Cloud Storage,
    #'  Azure or Ali cloud) on your behalf. This function uses
    #'  IAM Role credentials to connect to your s3 bucket.
    #'  In order to use these credentials, user must have specific user tag
    #'  enabled by Support team. \cr \cr
    #'
    #'  Read more about volume creation in our
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/create-a-volume-v2).
    # nolint end
    #'
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if from_path parameter
    #'  is not provided.
    #' @param access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    #' @param description An optional description of this volume.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this prefix
    #'  includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param bucket The name of the AWS S3 bucket you wish to register
    #'  as a volume. Required if `from_path` parameter is not provided.
    #' @param endpoint AWS API endpoint to use when accessing this
    #'  bucket. Default: `s3.amazonaws.com`.
    #' @param role_arn The ARN (Amazon Resource Name) of your role that
    #'  is used to connect your S3 bucket.
    #'  Required if `from_path` parameter is not provided.
    #' @param external_id Optional information that you can use in an
    #'  IAM role trust policy to designate who can assume the role.
    #'  Must be provided if it is configured in your role trust policy on AWS.
    #'  Required if `from_path` parameter is not provided.
    #' @param properties Named list containing the properties of a specific
    #'  service. These values set the defaults for operations performed with
    #'  this volume. Individual operations can override these defaults by
    #'  providing a custom properties object. For AWS S3, there are:
    #'  \itemize{
    #'    \item `sse_algorithm` - S3 server-side encryption to use when
    #'      exporting to this bucket. Supported values:
    #'      `AES256` (SSE-S3 encryption), `aws:kms`, `null`
    #'      (no server-side encryption). Default: `AES256`.
    #'    \item `sse_aws_kms_key_id`: Applies to type: `s3`.
    #'      If AWS KMS encryption is used, this should be set to the required
    #'      KMS key. If not set and `aws:kms` is set as `sse_algorithm`,
    #'      default KMS key is used.
    #'    \item `aws_canned_acl`: S3 canned ACL to apply on the object
    #'      on during export. Supported values: any one of
    # nolint start
    #'      [S3 canned ACLs](https://docs.aws.amazon.com/AmazonS3/latest/userguide/acl-overview.html#canned-acl);
    # nolint end
    #'      `null` (do not apply canned ACLs). Default: `null`.
    #'  }
    #' @param from_path Path to JSON configuration file containing all
    #'  required information for registering a volume. If provided, it will
    #'  overwrite all previous parameters set.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_s3_using_iam_role = function(name = NULL,
                                        access_mode = "RW",
                                        description = NULL,
                                        prefix = NULL,
                                        bucket = NULL,
                                        endpoint = "s3.amazonaws.com",
                                        role_arn = NULL,
                                        external_id = NULL,
                                        properties = list("sse_algorithm" = "AES256"), # nolint
                                        from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          role_arn = role_arn,
          external_id = external_id
        )
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }
        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
      }

      check_volume_params(args)

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["role_arn"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["external_id"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "s3",
          bucket = args[["bucket"]],
          prefix = args[["prefix"]],
          endpoint = args[["endpoint"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    },

    # Create new Google Cloud Volume (IAM User type authentication type) ------
    #' @description Create new volume to connect to your bucket on GCS.
    #'  Volumes authorize the Platform to access and query objects on a
    #'  specified cloud storage (Amazon Web Services, Google Cloud Storage,
    #'  Azure or Ali cloud) on your behalf. This function uses
    #'  IAM User credentials to connect with your GCS bucket. \cr \cr
    #'  Read more about volume creations in our
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/create-a-volume-v2).
    # nolint end
    #'
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if `from_path` parameter
    #'  is not provided.
    #' @param access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    #' @param description An optional description of this volume.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this prefix
    #'  includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param bucket The name of the GCS bucket you wish to register
    #'  as a volume. Required if `from_path` parameter is not provided.
    #' @param root_url Google Cloud Storage API endpoint for accessing
    #'  this bucket. Default: `https://www.googleapis.com`.
    #' @param client_email The client email address for the Google Cloud
    #'  service account to use for operations on this bucket. This can be found
    #'  in the JSON containing your service account credentials.
    #'  Required if `from_path` parameter is not provided.
    #' @param private_key Google Cloud Platform private key.
    #'  Required if `from_path` parameter is not provided.
    #' @param properties Named list containing the properties of a specific
    #'  service. These values set the defaults for operations performed with
    #'  this volume. Individual operations can override these defaults by
    #'  providing a custom properties object.
    #' @param from_path Path to JSON configuration file containing all
    #'  required information for registering a volume. If provided, it will
    #'  overwrite all previous parameters set.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_google_using_iam_user = function(name = NULL,
                                            access_mode = "RW",
                                            description = NULL,
                                            prefix = NULL,
                                            bucket = NULL,
                                            root_url = "https://www.googleapis.com", # nolint
                                            client_email = NULL,
                                            private_key = NULL,
                                            properties = NULL,
                                            from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          client_email = client_email,
          private_key = private_key
        )
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }
        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
      }

      check_volume_params(args)

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["client_email"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["private_key"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "gcs",
          bucket = args[["bucket"]],
          prefix = args[["prefix"]],
          root_url = args[["root_url"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    },

    # Create new Google Cloud Volume (IAM Role type authentication type) ------
    #' @description Create new volume to connect to your bucket on GCS.
    #'  Volumes authorize the Platform to access and query objects on a
    #'  specified cloud storage (Amazon Web Services, Google Cloud Storage,
    #'  Azure or Ali cloud) on your behalf. This function uses
    #'  IAM Role credentials to connect to your GCS bucket.
    #'  In order to use these credentials, user must have specific user tag
    #'  enabled by Support team. \cr \cr
    #'  Read more about volume creations in our
    # nolint start
    #'  [API documentation](https://docs.sevenbridges.com/reference/create-a-volume-v2).
    # nolint end
    #'
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if from_path parameter
    #'  is not provided.
    #' @param access_mode Signifies whether this volume should be used
    #'  for read-write (RW) or read-only (RO) operations. The access mode is
    #'  consulted independently of the credentials granted to Seven Bridges
    #'  when the volume was created, so it is possible to use a read-write
    #'  credentials to register both read-write and read-only volumes using it.
    #'  Default: `"RW"`.
    #' @param description An optional description of this volume.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this prefix
    #'  includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param bucket The name of the GCS bucket you wish to register
    #'  as a volume. Required if `from_path` parameter is not provided.
    #' @param root_url Google Cloud Storage API endpoint for accessing
    #'  this bucket. Default: `https://www.googleapis.com`.
    #' @param configuration Connection configuration parameters in JSON format
    #'  downloaded from the Google Cloud Console once prerequisites have been
    #'  set up. Could be provided as a named list, or as path to the downloaded
    #'  JSON file.
    #' @param properties Named list containing the properties of a specific
    #'  service. These values set the defaults for operations performed with
    #'  this volume. Individual operations can override these defaults by
    #'  providing a custom properties object.
    #' @param from_path Path to JSON configuration file containing all
    #'  required information for registering a volume. If provided, it will
    #'  overwrite all previous parameters set.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_google_using_iam_role = function(name = NULL,
                                            access_mode = "RW",
                                            description = NULL,
                                            prefix = NULL,
                                            bucket = NULL,
                                            root_url = "https://www.googleapis.com", # nolint
                                            configuration = NULL,
                                            properties = NULL,
                                            from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          configuration = transform_configuration_param(configuration)
        )
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }

        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
        if (!is_missing(args[["credentials"]])) {
          config_params <- args[["credentials"]][["configuration"]]
          configuration <- transform_configuration_param(config_params)
          args[["credentials"]][["configuration"]] <- configuration
        } else {
          rlang::abort("Configuration parameter within credentials is mandatory. \n Please, provide a path to JSON configuration file or a named list containing all configuration values.") # nolint
        }
      }

      check_volume_params(args)

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["configuration"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "gcs",
          bucket = args[["bucket"]],
          prefix = args[["prefix"]],
          root_url = args[["root_url"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    },

    # Create new AZURE Volume  -------------------------------------------------
    #' @description This call creates a new volume by attaching a Microsoft
    #'  Azure storage container to the Platform.
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if `from_path` parameter
    #'  is not provided.
    #' @param description An optional description of this volume.
    #' @param endpoint Specify a Microsoft Azure endpoint, only if you
    #'  are using an endpoint that is different from the default one
    #'  `https://(serviceaccount).blob.core.windows.net`. To make a non-default
    #'  endpoint work with the Platform, please first make sure it is supported
    #'  by Seven Bridges.
    #' @param storage_account The name of the storage account that holds the
    #'  container you want to attach as a volume.
    #' @param container The name of the container that you want to attach as
    #'  a Volume.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this prefix
    #'  includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param tenant_id Directory (tenant) ID of the application you created
    #'  on the Azure Portal for the purpose of attaching your storage container.
    #' @param client_id Application (client) ID of the application you
    #'  created on the Azure Portal for the purpose of attaching your storage
    #'  container.
    #' @param client_secret Value of the client secret you created on the Azure
    #'  Portal for the purpose of attaching your storage container.
    #' @param resource_id Resource ID of the Azure storage account. To get it,
    #'  go to the [Azure Portal](https://portal.azure.com/), open the storage
    #'  account's Overview page and click JSON View.
    #' @param from_path JSON configuration file containing all required
    #'  information for registering a volume.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_azure = function(name = NULL,
                            description = NULL,
                            endpoint = NULL,
                            storage_account = NULL,
                            container = NULL,
                            prefix = NULL,
                            tenant_id = NULL,
                            client_id = NULL,
                            client_secret = NULL,
                            resource_id = NULL,
                            from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          tenant_id = tenant_id,
          client_id = client_id,
          client_secret = client_secret
        )
        args[["properties"]] <- list(resource_id = resource_id)
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }
        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
      }

      # Azure volumes only have Read Only privileges
      args[["access_mode"]] <- "RO"
      check_volume_params(args, volume_type = "azure")

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["tenant_id"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["client_id"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["client_secret"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["properties"]][["resource_id"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "azure",
          endpoint = args[["endpoint"]],
          storage_account = args[["storage_account"]],
          container = args[["container"]],
          prefix = args[["prefix"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    },

    # Create new ALI (OSS) Volume  ---------------------------------------------
    #' @description Create new volume to connect to your bucket on ALI (OSS)
    #'  platform.
    #' @param name The name of the volume. It must be unique from all
    #'  other volumes for this user. Required if from_path parameter
    #'  is not provided.
    #' @param description An optional description of this volume.
    #' @param endpoint Specify an Ali Cloud endpoint.
    #' @param bucket The name of the ALI(OSS) bucket you wish to register
    #'  as a volume. Required if `from_path` parameter is not provided.
    #' @param prefix A service-specific string prefix to append to all objects
    #'  created in this volume. If the service supports folders, and this
    #'  prefix includes them, the API will attempt to create any missing folders
    #'  when it outputs a file.
    #' @param access_key_id ALI(OSS) access key ID of the user shared
    #'  with Seven Bridges to access this bucket.
    #'  Required if `from_path` parameter is not provided.
    #' @param secret_access_key ALI(OSS) secret access key of the user
    #'  shared with Seven Bridges to access this bucket.
    #'  Required if `from_path` parameter is not provided.
    #' @param properties Named list containing the properties of a specific
    #'  service. These values set the defaults for operations performed with
    #'  this volume. Individual operations can override these defaults by
    #'  providing a custom properties object.
    #' @param from_path JSON configuration file containing all required
    #'  information for registering a volume.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom glue glue
    #' @importFrom rlang abort
    #' @importFrom jsonlite fromJSON
    #'
    #' @return \code{\link{Volume}} object.
    create_ali_oss = function(name = NULL,
                              description = NULL,
                              endpoint = NULL,
                              bucket = NULL,
                              prefix = NULL,
                              access_key_id = NULL,
                              secret_access_key = NULL,
                              properties = NULL,
                              from_path = NULL) {
      if (is_missing(from_path)) {
        args <- as.list(environment())
        args[["credentials"]] <- list(
          access_key_id = access_key_id,
          secret_access_key = secret_access_key
        )
      } else {
        checkmate::assert_string(from_path)
        if (!file.exists(from_path)) {
          rlang::abort("File on provided path doesn't exist.")
        }
        args <- jsonlite::fromJSON(from_path, simplifyDataFrame = FALSE)
        args <- purrr::list_flatten(args, name_spec = "{inner}")
      }

      # Ali(OSS) volumes only have Read Only privileges
      args[["access_mode"]] <- "RO"
      check_volume_params(args)

      # Check credentials
      checkmate::assert_string(args[["credentials"]][["access_key_id"]],
        null.ok = FALSE
      )
      checkmate::assert_string(args[["credentials"]][["secret_access_key"]],
        null.ok = FALSE
      )
      # nocov start
      body <- list(
        name = args[["name"]],
        description = args[["description"]],
        access_mode = args[["access_mode"]],
        service = list(
          type = "OSS",
          bucket = args[["bucket"]],
          endpoint = args[["endpoint"]],
          prefix = args[["prefix"]],
          credentials = args[["credentials"]],
          properties = args[["properties"]]
        )
      )

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      return(asVolume(res, auth = self$auth)) # nocov end
    }
  )
)
