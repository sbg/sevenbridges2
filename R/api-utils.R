m.fun <- function(x, y, exact = TRUE, ignore.case = TRUE, ...) {
  if (exact) {
    res <- pmatch(x, y, ...)
  } else {
    res <- unlist(sapply(x, function(i) {
      grep(i, y, ignore.case = ignore.case)
    }))
    if (is.matrix(res)) res <- res[, 1]
  }

  res
}


# match by id and name
m.match <- function(obj,
                    id = NULL, name = NULL,
                    .id = "id", .name = "name",
                    exact = TRUE, ignore.case = TRUE) {

  # if no match, return whole list
  if (is.null(id)) {
    if (is.null(name)) {
      if (length(obj) == 1) {
        return(obj[[1]])
      } else {
        return(obj)
      }
    } else {
      # id is null, use name
      nms <- sapply(obj, function(x) x[[.name]])
      if (ignore.case) {
        name <- tolower(name)
        nms <- tolower(nms)
      }
      index <- m.fun(name, nms,
                     exact = exact,
                     ignore.case = ignore.case
      )
    }
  } else {
    # id is not NULL
    ids <- sapply(obj, function(x) x[[.id]])
    index <- m.fun(id, ids,
                   exact = exact,
                   ignore.case = ignore.case
    )
  }

  if (length(index) == 1 && is.na(index)) {
    message("sorry, no matching ")
    return(NULL)
  } else {
    if (length(index) == 1) {
      obj[[index]]
    } else {
      obj[index]
    }
  }
}

#' Check request status
#'
#' Check request status
#' @param req API request
#' @param as desired type output (contents of a request): raw, text or parsed
#'
#' @return request content or the message
#'
#' @keywords internal
status_check <- function(req, as = "parsed", ...) {
  if (httr::status_code(req) %in% c("200", "201", "202", "204")) {

    # Check this !!!
    if (!is.null(req$request$headers["X-SBG-Auth-Token"])) {
      req$request$headers["X-SBG-Auth-Token"] <- "<your_auth_token>"
    }
    res <- httr::content(req, as = as, ...)
    if (!is.null(res)) {
      attr(res, "response") <- req
    }
    return(res)
  } else if (httr::status_code(req) %in% c("401", "403", "404", "503")) {
    msg <- httr::content(req, as = as, ...)$message
    stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
  } else {
    if ("message" %in% names(content(req, as = as, ...))) {
      msg <- httr::content(req, as = as, ...)$message
    } else {
      msg <- NULL
    }

    if (is.null(msg)) {
      if (httr::status_code(req) %in% names(codes)) {
        msg <- codes[[httr::status_code(req)]]
      }
      if (is.null(msg)) {
        print(content(req, as = as, ...))
        stop(paste("Error of unknown type occured", httr::status_code(req)))
      } else {
        stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
      }
    } else {
      stop(paste0("HTTP Status ", httr::status_code(req), ": ", msg), call. = FALSE)
    }
  }
}

# Status codes from sevenbridges API v2 specification
# https://docs.sevenbridges.com/reference#api-status-codes

codes <- list(

  # 0xxx: Platform maintenance errors
  "0" = "The Platform is currently under maintenance.",

  # 1xxx: General errors
  "1000" = "Rate limit exceeded! Check response headers.",

  # 2xxx: User errors
  "2000" = "User service is currently unavailable.",
  "2001" = "Not enough privileges to access requested user info.",
  "2002" = "Requested user does not exist.",
  "2003" = "Requested user already exists.",
  "2105" = "Please specify the division you are listing the users or teams for.",

  # 3xxx: Projects errors
  "3000" = "Project service is currently unavailable.",
  "3001" = "Not enough privileges to access the requested project/member.",
  "3002" = "Requested project or member does not exist.",
  "3003" = "Requested project/member already exists.",
  "3004" = "Owner's username must not be null or empty string.",
  "3005" = "Member username must not be null or empty string.",
  "3006" = "Project id must not be null or an empty string.",
  "3007" = "Project name must not be null or empty string.",
  "3008" = "Billing group id must not be null or empty string.",
  "3009" = "Project type must not be null or empty string.",
  "3010" = "Project type can be either v2 for standard projects or v1 for LEGACY projects.",
  "3011" = "Project permissions must not be null or an empty value.",
  "3012" = "Malformed project id. Expecting `owner/project`.",
  "3013" = "Please provide all permissions data.",

  # 4xxx: Billing errors
  "4000" = "Billing service is currently unavailable.",
  "4001" = "Insufficient privileges to access the requested billing group/invoice.",
  "4002" = "Requested billing group/invoice does not exist.",
  "4003" = "Requested billing group/invoice already exist.",
  "4004" = "Billing group id must not be null or an empty string.",
  "4005" = "Billing group id must be a valid UUID.",
  "4006" = "You are not a member of this billing group.",
  "4007" = "Invoice id must not be null or an empty string.",

  # 5xxx: Files errors
  "5000" = "File service is currently unavailable.",
  "5001" = "Insufficient privileges to access the requested file.",
  "5002" = "Requested file does not exist.",
  "5003" = "Requested file already exists.",
  "5004" = "File id must not be null or an empty string.",
  "5005" = "Malformed project query parameter. Expecting `?project=owner/project`.",
  "5006" = "Metadata validation failed.",
  "5007" = "File copy failed.",
  "5008" = "File renaming not allowed.",
  "5009" = "Modifying metadata is not allowed.",
  "5010" = "Metadata service is currently unavailable.",
  "5011" = "Modifying file tags is not allowed.",
  "5012" = "Invalid `type` supplied. Allowed values: [folder].",
  "5014" = "Insufficient privileges to copy the requested file.",
  "5015" = "Moving files between projects is not supported.",
  "5017" = "Downloading folders is not supported.",
  "5018" = "Copying folders is not supported.",
  "5019" = "Archiving folders is not supported.",
  "5020" = "Restoring folders is not supported.",
  "5021" = "Deleting non-empty folders is not supported.",
  "5022" = "The parent specified is not a folder.",
  "5023" = "Updating folder details is not supported.",
  "5024" = "Invalid name parameter. Check the documentation.",
  "5025" = "Updating folder metadata is not supported.",
  "5026" = "Destination folder is not found.",
  "5027" = "Updating folder tags is not supported.",
  "5029" = "Missing `parent` or `project` field. These fields must be included together.",
  "5030" = "Requested folder already exists.",
  "5031" = "Providing both `parent` and `project` is not allowed.",
  "5032" = "Insufficient privileges to move the requested file.",
  "5033" = "Invalid request please check the documentation.",
  "5034" = "Downloading files from an inactive volume is not supported.",
  "5035" = "Key must not be null or an empty string.",

  # 6xxx: Apps errors
  "6000" = "App service is currently unavailable.",
  "6001" = "Insufficient privileges to access the requested app/revision.",
  "6002" = "Requested app/revision does not exist.",
  "6003" = "Requested app/revision already exists.",
  "6004" = "App name must not be null or an empty string.",
  "6006" = "Project owner must not be null or an empty string.",
  "6007" = "Project must not be null or an empty string.",
  "6008" = "App revision must not be null or an empty string.",
  "6009" = "Destination project must not be null or an empty string.",
  "6010" = "Source app must not be null or an empty string.",
  "6011" = "Malformed app id. Expecting `owner/project/app_name/revision`.",
  "6012" = "Invalid visibility query parameter. Allowed values: [PUBLIC, PRIVATE].",

  # 7xxx: Tasks errors
  "7000" = "Task service is currently unavailable.",
  "7001" = "Insufficient privileges to access the requested task.",
  "7002" = "Requested task does not exist.",
  "7003" = "Requested task already exists.",
  "7004" = "Task ID must not be empty or null or an empty string.",
  "7005" = "Task ID must be a valid UUID.",
  "7006" = "Invalid task status. Allowed values: [QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, ABORTING, FAILED]",
  "7007" = "This action is only available for DRAFT tasks.",
  "7008" = "This action is only available for RUNNING tasks.",
  "7009" = "Invalid task action. Action can be performed only on DRAFT or RUNNING tasks.",
  "7010" = "Invalid task action. Action can be performed on DRAFT tasks.",
  "7011" = "Invalid task action. Action can be performed on tasks in the states: CREATING, RUNNING or QUEUED.",
  "7012" = "Missing inputs.",
  "7013" = "Invalid task action.",
  "7014" = "Action parameter must not be null or an empty string.",
  "7015" = "App Id must not be null or an empty string.",
  "7016" = "Invalid app url.",
  "7017" = "Only Common Workflow Language (CWL) tasks are supported.",
  "7018" = "Batch input property should reference input identifier or omitted. Empty value is not allowed.",
  "7019" = "Missing batch criteria.",
  "7020" = "Invalid batch type supplied. Allowed values: [criteria, item].",
  "7021" = "Batching can only be disabled if the task is submitted for execution.",
  "7022" = "Disabling batching action is only available for BATCH tasks.",
  "7023" = "Missing batch_by or batch_input fields. These fields must be included together.",
  "7024" = "Task can not be started due to validation errors.",
  "7026" = "Editing is available only for tasks which are in DRAFT status. Tasks which are in RUNNING and COMPLETED states can only be renamed.",

  # 8xxx: Upload errors
  "8000" = "Upload service is currently unavailable.",
  "8001" = "Insufficient privileges to access the requested upload.",
  "8002" = "Insufficient privileges to access the requested file.",
  "8003" = "Requested upload does not exist.",
  "8004" = "Requested file does not exist.",
  "8005" = "Requested file already exists.",
  "8006" = "Requested upload already exists.",
  "8007" = "Failed to complete upload.",
  "8008" = "Failed to reserve part for upload. Try again.",
  "8009" = "Failed to abort upload.",
  "8010" = "Malformed project id. Expecting `owner/project`.",
  "8011" = "Upload id must not be null or an empty string.",
  "8012" = "Part number is missing or invalid.",
  "8013" = "Invalid `init` request.",
  "8014" = "Invalid `part` report.",
  "8015" = "Invalid list of parts. Expecting an object with `parts`: [ array of part reports ].",

  # 9xxx: Volumes errors
  "9000" = "There was an error communicating with the service.",
  "9001" = "Could not obtain read access on the service.",
  "9002" = "Could not obtain cross-write access on the service.",
  "9003" = "Insufficient privileges to access the requested project.",
  "9004" = "Insufficient privileges to access the requested file.",
  "9005" = "Insufficient privileges to access the requested job.",
  "9006" = "Requested file cannot be exported.",
  "9007" = "Requested volume does not exist.",
  "9008" = "Requested job does not exist.",
  "9009" = "Requested file does not exist.",
  "9010" = "Requested volume name already exists.",
  "9011" = "Invalid request syntax.",
  "9012" = "Requested project or member does not exist.",
  "9013" = "Volume name must not be null or an empty string.",
  "9014" = "Volume name must consist of up to 32 English letters, numbers and underscores.",
  "9015" = "`access_mode` must be provided (either `RO` or `RW`).",
  "9016" = "`service` object must be provided.",
  "9017" = "`service` object is invalid. Check the documentation.",
  "9018" = "Cannot infer file name and none given.",
  "9019" = "`service` object is invalid. Check the documentation.",
  "9020" = "Insufficient privileges to access the requested volume.",
  "9021" = "Invalid time format. Check the documentation.",
  "9022" = "Invalid canned ACL selected (`aws_canned_acl`). Check the documentation.",
  "9032" = "Invalid server-side encryption selected (`sse_algorithm`). Check the documentation.",
  "9024" = "Invalid S3 storage class selected (`aws_storage_class`). Check the documentation.",
  "9025" = "Invalid private key given (`private_key`). Check the documentation.",
  "9026" = "The volume is not configured for writing access (`access mode` is not set to `RW`).",
  "9027" = "Exporting files across different cloud services is not yet supported.",
  "9028" = "Exporting files across different cloud services is not yet supported.",
  "9030" = "Volume name must consist of up to 32 English letters, numbers, and underscores.",
  "9057" = "This environment only supports `RO` buckets of type `GCS`.",
  "9058" = "This environment only supports `RO` buckets of type `S3`.",
  "9100" = "There was an error communicating with the service.",
  "9101" = "The volume is not configured for writing (`access mode` is not set to `RW`).",
  "9102" = "Insufficient privileges to access the requested project.",
  "9103" = "Location on volume is not accessible as configured.",
  "9104" = "Requested volume does not exist.",
  "9105" = "Location on volume not found.",
  "9106" = "Requested file does not exist.",
  "9107" = "Location on volume already contains a file.",
  "9108" = "Requested file already exists.",
  "9109" = "Requested file does not exist or not accessible.",

  # 10xxx: Manifest file errors
  "10101" = "Manifest file is not a valid .csv file.",
  "10102" = "Invalid manifest file header row format.",
  "10105" = "Manifest file contains too many rows.",
  "10110" = "Failed to submit manifest file.",
  "10208" = "Invalid or missing session ID.",
  "10210" = "Insufficient privileges on destination.",
  "10211" = "Job with provided id not found.",
  "10212" = "Resource not found.",
  "10213" = "Destination path not found.",
  "10214" = "The requested resource does not support wanted http method.",
  "10215" = "External resource unavailable.",
  "10216" = "Job is already hidden.",
  "10217" = "Unprocessable json error.",
  "10218" = "Unprocessable json - end of file.",
  "10220" = "Unexpected error occurred.",
  "10221" = "Insufficient privileges to access the requested destination.",
  "10222" = "Requested folder does not exist.",
  "10223" = "Requested file already exists.",
  "10224" = "Requested folder already exists.",
  "10225" = "Submitted source does not exist.",
  "10226" = "Insufficient privileges to access the requested source.",
  "10227" = "Destination path must be a folder.",
  "10228" = "Destination for source is missing.",
  "10229" = "Cannot copy to parent directory.",
  "10230" = "Sources cannot be empty or null.",
  "10231" = "There are duplicate sources.",
  "10232" = "Invalid request syntax. Check the documentation.",
  "10233" = "Provide full paths instead of file IDs.",
  "10234" = "List of paths for query deleting state is empty.",
  "10235" = "Invalid file name.",
  "10236" = "Invalid folder name.",

  # 13xxx: Additional errors
  "13000" = "Search service is currently unavailable.",
  "13001" = "Insufficient privileges to access the requested file.",
  "13002" = "Requested file does not exist.",
  "13003" = "Requested file already exists.",
  "13004" = "Consistency has been violated, please retry.",

  # 9xxxx: General validation errors
  "90000" = "Bad request.",
  "90001" = "Unauthorized.",
  "90002" = "Forbidden.",
  "90003" = "Not found.",
  "90004" = "Unexpected error happened.",
  "90005" = "Service unavailable.",
  "90006" = "Method not allowed.",
  "90007" = "Conflict.",
  "90008" = "Unsupported Media Type.",
  "90009" = "An error occurred during the decoding of the request content.",
  "90010" = "Not implemented, COPY and DELETE bulk operations are available."
)

# customize underlying http logic
# (handle_url2, build_url2, GET2, POST2)

handle_url2 <- function(handle = NULL, url = NULL, ...) {
  if (is.null(url) && is.null(handle)) {
    stop("Must specify at least one of url or handle")
  }
  if (is.null(handle)) handle <- httr::handle_find(url)
  if (is.null(url)) url <- handle$url
  # workaround to bypass `:::` checks
  new <- eval(parse(text = "httr:::named(list(...))"))
  if (length(new) > 0 || eval(parse(text = "httr:::is.url(url)"))) {
    old <- httr::parse_url(url)
    url <- build_url2(modifyList(old, new))
  }

  list(handle = handle, url = url)
}

build_url2 <- function(url) {
  stopifnot(eval(parse(text = "httr:::is.url(url)")))
  scheme <- url$scheme
  hostname <- url$hostname
  if (!is.null(url$port)) {
    port <- paste0(":", url$port)
  } else {
    port <- NULL
  }
  path <- url$path
  if (!is.null(url$params)) {
    params <- paste0(";", url$params)
  } else {
    params <- NULL
  }
  if (is.list(url$query)) {
    url$query <- eval(parse(text = "httr:::compact(url$query)"))
    names <- curl_escape(names(url$query))
    values <- as.character(url$query)
    query <- paste0(names, "=", values, collapse = "&")
  } else {
    query <- url$query
  }
  if (!is.null(query)) {
    stopifnot(is.character(query), length(query) == 1)
    query <- paste0("?", query)
  }
  if (is.null(url$username) && !is.null(url$password)) {
    stop("Cannot set password without username")
  }

  paste0(scheme, "://", url$username, if (!is.null(url$password)) {
    ":"
  }, url$password, if (!is.null(url$username)) {
    "@"
  }, hostname, port, "/", path, params, query, if (!is.null(url$fragment)) {
    "#"
  }, url$fragment)
}

GET2 <- function(url = NULL, config = list(), ..., handle = NULL) {
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}

POST2 <- function(url = NULL, config = list(), ...,
                  body = NULL, encode = c("json", "form", "multipart"),
                  multipart = TRUE, handle = NULL) {
  if (!missing(multipart)) {
    warning("multipart is deprecated, please use encode argument instead",
            call. = FALSE
    )
    encode <- ifelse(multipart, "multipart", "form")
  }

  encode <- match.arg(encode)
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}



