# nolint start
#' @title R6 Class representing volumes endpoint
#'
#' @description
#' R6 Class representing volumes resource endpoint
#'
#' @importFrom R6 R6Class
#' @export
Volumes <- R6::R6Class(
  "Volumes",
  # nolint end
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "storage/volumes",
      "get" = "storage/volumes/{id}",
      "create" = "storage/volumes"
    ),

    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },

    # List all volumes you've registered --------------------------------------
    #' @description This call lists all the volumes you've registered.
    #'
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    #' @importFrom checkmate assert_list
    query = function(...) {
      res <- super$query(
        path = self$URL[["query"]],
        advance_access = TRUE,
        ...
      )
      return(res)
      # return(asVolumeList(res, auth = self$auth))
    },

    # Get single volume -------------------------------------------------------
    #' @description This call returns details of the specified volume.
    #' The volume is referred to by its ID, which you can obtain by
    #' making the call to list all the volumes you've registered.
    #' @param id The Volume ID consists of volume owner's name (for enterprise
    #' users) and volume name in form {volume_owner}/{volume_name},
    #' or division name (if user belongs to some division) and volume
    #' name in form {division}/{volume_name}. You can also get the Volume ID
    #' for a volume by making the call to list all volumes you've registered.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    get = function(id) {
      if (is_missing(id)) {
        rlang::abort("Volume ID must be provided!")
      }
      checkmate::assert_string(id)

      res <- super$get(
        cls = self,
        id = id,
        advance_access = TRUE
      )
      return(res)
      # return(asVolume(res, auth = self$auth))
    },
    # Delete volume -------------------------------------------------------
    #' @description Please, use delete() operation on the exact Volume object.
    #' @importFrom rlang inform
    delete = function() {
      rlang::inform("Deleting volumes is possible to perform on the specific instance of class Volume.") # nolint
    },
    # Create new Volume (AWS IAM User type authentication type) ---------------
    #' @description Create new volume to connect to your s3 bucket on AWS cloud.
    #' Volumes authorize the Platform to access and query objects on a
    #' specified cloud storage (Amazon Web Services or Google Cloud Storage) on
    #' your behalf. This function uses IAM User credentials to connect with
    #' your s3 bucket.
    #'
    #' @param name The name of the volume. It must be unique from all
    #' other volumes for this user. Required if from_path parameter
    #' is not provided.
    #' @param bucket The name of the AWS S3 bucket you wish to register
    #' as a volume. Required if from_path parameter is not provided.
    #' @param prefix A service-specific prefix to append to all objects created
    #' in this volume. If the service supports folders, and this prefix
    #' includes them, the API will attempt to create any missing folders
    #' when it outputs a file.
    #' @param access_key_id Credentials required for IAM User authentication
    #' type. Required if from_path parameter is not provided.
    #' @param secret_access_key Credentials required for IAM User authentication
    #' type. Required if from_path parameter is not provided.
    #' @param access_mode Signifies whether this volume should be used
    #' for read-write (RW) or read-only (RO) operations. The access mode is
    #' consulted independently of the credentials granted to Seven Bridges
    #' when the volume was created, so it is possible to use a read-write
    #' credentials to register both read-write and read-only volumes using it.
    #' Default: `"RW"`.
    #' @param description An optional description of this volume.
    #' @param properties Named list containing the properties of a specific
    #' service. These values set the defaults for operations performed with this
    #' volume. Individual operations can override these defaults by providing a
    #' custom properties object. For AWS S3, there are:
    #' #' \itemize{
    #'    \item `sse_algorithm` - String. S3 server-side encryption to use when
    #'    exporting to this bucket. Supported values:
    #'    AES256 (SSE-S3 encryption), aws:kms, null (no server-side encryption).
    #'    Default: AES256.
    #'    \item `sse_aws_kms_key_Id`: String. Applies to type: s3.
    #'    If AWS KMS encryption is used, this should be set to the required KMS
    #'    key. If not set and aws:kms is set as sse_algorithm, default KMS key
    #'    is used.
    #'    \item `aws_canned_acl`: String. S3 canned ACL to apply on the object
    #'    on during export. Supported values: any one of S3 canned ACLs;
    #'    null (do not apply canned ACLs). Default: null.
    #' }
    #' @param endpoint AWS API endpoint to use when accessing this bucket.
    #' Default: s3.amazonaws.com
    #' @param from_path JSON configuration file containing all required
    #' information for registering a volume. Example schema can be found <here>.
    #'
    #' @return Volume object.
    create_s3_using_iam_user = function(name = NULL, bucket = NULL,
                                        prefix = NULL, access_key_id = NULL,
                                        secret_access_key = NULL,
                                        access_mode = c("RW", "RO"),
                                        description = NULL,
                                        properties = list(
                                          "sse_algorithm" = "AES256."
                                        ),
                                        endpoint = "s3.amazonaws.com",
                                        from_path = NULL) {
      if (is_missing(from_path)) {
        checkmate::assert_character(name,
          len = 1, null.ok = FALSE,
          typed.missing = TRUE
        )
        checkmate::assert_character(bucket,
          len = 1, null.ok = FALSE,
          typed.missing = TRUE
        )
        checkmate::assert_character(access_key_id,
          len = 1, null.ok = FALSE,
          typed.missing = TRUE
        )
        checkmate::assert_character(secret_access_key,
          len = 1, null.ok = FALSE,
          typed.missing = TRUE
        )
        checkmate::assert_character(prefix, len = 1, null.ok = TRUE)
        access_mode <- match.arg(access_mode)
        checkmate::assert_character(description, len = 1, null.ok = TRUE)
        checkmate::assert_list(properties)
        checkmate::assert_character(endpoint, len = 1, null.ok = FALSE)

        body <- list(
          name = name,
          description = description,
          access_mode = access_mode,
          service = list(
            type = "s3",
            bucket = bucket,
            prefix = prefix,
            endpoint = endpoint,
            credentials = list(
              access_key_id = access_key_id,
              secret_access_key = secret_access_key
            ),
            properties = properties
          )
        )
      } else {
        # utils validation helper function for checking from_path
        # and returning args list with all arguments loaded from json
        # body <- args
      }

      path <- glue::glue(self$URL[["create"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        advance_access = TRUE
      )

      res <- status_check(res)

      return(res)
      # return(asVolume(res, auth))
    }
  )
)
