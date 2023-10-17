# Utils-validations
#' Check request status
#'
#' Check request status
#' @param req API request
#' @param as Desired type output (contents of a request): raw, text or parsed
#'
#' @return request content or the message
#' @importFrom httr status_code
#'
#' @noRd
status_check <- function(req, as = "parsed", ...) {
  # nocov start

  # If request is successful return response
  if (httr::status_code(req) %in% c("200", "201", "202", "204")) {
    if (!is.null(req$request$headers["X-SBG-Auth-Token"])) {
      req$request$headers["X-SBG-Auth-Token"] <- "<your_auth_token>"
    }
    res <- httr::content(req, as = as, ...)
    if (!is.null(res)) {
      attr(res, "response") <- req
    }
    return(res)
  }

  # If request is not successful return error message for standard error
  # messages
  if (httr::status_code(req) %in% c("401", "403", "404", "503")) {
    msg <- httr::content(req, as = as, ...)$message
    rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
  }

  # If request is not successful return error message for non-standard error
  # messages
  if (!is.null(httr::content(req, as = as, ...)$message)) {
    msg <- httr::content(req, as = as, ...)$message
    rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
  }

  if (httr::status_code(req) %in% names(codes)) {
    msg <- codes[[httr::status_code(req)]]
    rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
  }

  print(content(req, as = as, ...))
  rlang::abort(paste(
    "Error of unknown type occured: ",
    httr::status_code(req)
  ))

  # nocov end
}

#' Check if input value is missing
#'
#' @description This function checks whether the input
#'  value is a vector of minimum length 1, with no empty
#'  value and no all missing values.
#'  If the input value is not a vector, it checks only if
#'  the value is set at all (original meaning of 'missing' function)
#'  in order to be able to use it with other object types.
#'
#' @param input Value to check
#' @importFrom checkmate test_vector
#' @noRd
is_missing <- function(input) {
  if (missing(input)) {
    return(TRUE)
  }
  if (checkmate::test_vector(input, null.ok = TRUE)) {
    isTRUE(
      !checkmate::test_vector(input,
        min.len = 1,
        all.missing = FALSE
      ) ||
        isTRUE(input == "")
    )
  } else {
    return(FALSE)
  }
}

#' Check limit parameter
#'
#' @param limit Limit value
#' @noRd
check_limit <- function(limit) {
  msg <- "Limit must be integer number between 1 and 100."
  if (!is.numeric(limit)) {
    rlang::abort(msg)
  }
  limit_cast <- suppressWarnings(as.integer(limit))
  if (is_missing(limit_cast)) {
    rlang::abort(msg) # nocov
  }
  if (limit_cast > 100 || limit_cast <= 0) {
    rlang::abort(msg) # nocov
  }
}

#' Check offset parameter
#'
#' @param offset Offset value
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
check_offset <- function(offset) {
  msg <- "Offset must be integer number >= 0."
  if (!is.numeric(offset)) {
    rlang::abort(msg)
  }
  offset_cast <- suppressWarnings(as.integer(offset))
  if (is_missing(offset_cast)) {
    rlang::abort(msg) # nocov
  }
  if (offset_cast < 0) {
    rlang::abort(msg) # nocov
  }
}

#' Check tag parameters
#'
#' @param tags Tag values
#' @importFrom checkmate test_list
#' @importFrom rlang abort
#'
#' @noRd
check_tags <- function(tags) {
  if (!checkmate::test_list(tags,
    types = "character",
    null.ok = TRUE,
    names = "unnamed"
  )) {
    # nolint start
    rlang::abort(
      "Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')"
    )
    # nolint end
  }
}

#' Check project settings
#'
#' @param settings settings named list
# nolint start
#' @importFrom checkmate assert_logical assert_list assert_character assert_integer
# nolint end
#' @importFrom rlang abort
#' @noRd
check_settings <- function(settings) {
  if (!is.null(settings)) {
    msg <- "Settings must be provided as a list."
    if (!is.list(settings)) {
      rlang::abort(msg)
    }

    valid_input_names <- c(
      "locked",
      "controlled",
      "use_interruptible_instances",
      "use_memoization",
      "allow_network_access",
      "use_elastic_disk",
      "location",
      "intermediate_files"
    )

    invalid_element_names <-
      setdiff(names(settings), valid_input_names)

    if (length(invalid_element_names) > 0) {
      # nolint start
      rlang::abort(glue::glue(
        "Argument {invalid_element_names} is not a valid settings field."
      ))
      # nolint end
    }

    checkmate::assert_logical(settings$locked,
      .var.name = "locked",
      null.ok = TRUE
    )
    checkmate::assert_logical(settings$controlled,
      .var.name = "controlled",
      null.ok = TRUE
    )
    checkmate::assert_logical(
      settings$use_interruptible_instances,
      .var.name = "use_interruptible_instances",
      null.ok = TRUE
    )
    checkmate::assert_logical(settings$use_memoization,
      .var.name = "use_memoization",
      null.ok = TRUE
    )
    checkmate::assert_logical(settings$allow_network_access,
      .var.name = "allow_network_access",
      null.ok = TRUE
    )
    checkmate::assert_logical(settings$use_elastic_disk,
      .var.name = "use_elastic_disk",
      null.ok = TRUE
    )

    checkmate::assert_character(settings$location,
      .var.name = "location",
      null.ok = TRUE
    )

    if ("intermediate_files" %in% names(settings)) {
      checkmate::assert_list(settings$intermediate_files,
        .var.name = "intermediate_files",
        null.ok = TRUE
      )
      checkmate::assert_integer(
        settings$intermediate_files$duration,
        .var.name = "intermediate_files$duration",
        null.ok = TRUE
      )
      checkmate::assert_character(
        settings$intermediate_files$retention,
        .var.name = "intermediate_files$retention",
        null.ok = TRUE
      )
    }
  }
}

#' Check folder name
#'
#' @description This function checks if the provided folder name is valid.
#' @param name Name of the folder.
#' @noRd
check_folder_name <- function(name) {
  if (is_missing(name)) {
    rlang::abort("Please, provide the folder's name.")
  }
  checkmate::assert_string(name)
  if (substr(name, 1, 2) == "__") {
    rlang::abort("The folder name cannot start with \"__\"")
  }
  if (grepl("\\s", name)) {
    rlang::abort("The folder name cannot contain spaces in the name.")
  }
}
#' Check metadata
#'
#' @param metadata Metadata named list
#' @importFrom checkmate test_list
#' @importFrom rlang abort
#' @noRd
check_metadata <- function(metadata) {
  if (!checkmate::test_list(
    metadata,
    types = "character",
    null.ok = TRUE,
    names = "named",
    max.len = 1000
  )) {
    # nolint start
    rlang::abort(
      "Metadata parameter must be a named list of key-value pairs. For example: metadata <- list(metadata_key_1 = 'metadata_value_1', metadata_key_2 = 'metadata_value_2')"
    )
    # nolint end
  }
}

#' Transform metadata input
#' Transform metadata input to be in API acceptable form.
#'
#' @param metadata Metadata named list
#' @noRd
transform_metadata <- function(metadata) {
  metadata_names <- paste0("metadata.", names(metadata))
  names(metadata) <- metadata_names
  encoded_metadata <- lapply(metadata, function(x) {
    URLencode(x)
  })
  new_metadata <- flatten_query(encoded_metadata)
  return(new_metadata)
}

#' Check file download destination
#'
#' @param directory_path Directory path, string.
#' @param filename File name (base name)
#' @noRd
check_download_path <- function(directory_path, filename) {
  if (dir.exists(directory_path)) {
    if (is_missing(filename)) {
      rlang::abort("The filename parameter is missing.")
    } else if (!checkmate::test_string(filename)) {
      rlang::abort("The filename parameter should be a length-one string.")
    }
  } else {
    rlang::abort(glue::glue_col("Destination directory {directory_path} does not exist.")) # nolint
  }
}

#' Check retry parameters
#'
#' @description This function validates provided retry parameter
#'  used within the `download()` method of a `File` object.
#' @param input Value to check.
#' @param parameter_to_validate Retry parameter to be validated.
#' @noRd
check_retry_params <- function(input, parameter_to_validate) {
  if (parameter_to_validate == "count") {
    msg <- "retry_count parameter must be a positive integer number."
  } else if (parameter_to_validate == "timeout") {
    msg <- "retry_timeout parameter must be a positive integer number."
  }

  if (!is.numeric(input)) {
    rlang::abort(msg) # nocov
  }
  retry_param_cast <- suppressWarnings(as.integer(input))
  if (is_missing(retry_param_cast)) {
    rlang::abort(msg) # nocov
  }
  if (retry_param_cast <= 0) {
    rlang::abort(msg) # nocov
  }
}

#' Check size, part_size and part_length params for uploads
#'
#' @param size File size
#' @param part_size Part size
#' @importFrom rlang abort
#' @importFrom checkmate assert_numeric
#' @noRd
check_upload_params <- function(size, part_size) {
  checkmate::assert_numeric(
    size,
    lower = 0,
    len = 1,
    any.missing = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_numeric(
    part_size,
    lower = 0,
    len = 1,
    any.missing = FALSE,
    null.ok = FALSE
  )

  if (!(size >= 0 &&
    size <= getOption("sevenbridges2")$MAXIMUM_OBJECT_SIZE)) {
    # nolint start
    rlang::abort("File size must be between 0 - 5497558138880 (5TB), inclusive")
    # nolint end
  }
  if (!(
    part_size <= getOption("sevenbridges2")$MAXIMUM_PART_SIZE &&
      part_size >= getOption("sevenbridges2")$MINIMUM_PART_SIZE
  )) {
    # nolint start
    rlang::abort("Parameter part_size must be 5 MB to 5 GB, last part can be < 5 MB")
    # nolint end
  }
  part_length <- ifelse(size == 0, 1,
    as.integer(ceiling(size / part_size))
  )
  if (part_length < 1 ||
    part_length > getOption("sevenbridges2")$MAXIMUM_TOTAL_PARTS) {
    # nolint start
    rlang::abort("Total number of parts must be from 1 to 10,000 (inclusive). Please, modify part_size.")
    # nolint end
  }
}


#' Check app copy strategy
#'
#' @description This function checks if the provided strategy for app copy is
#'  valid.
#' @param strategy Strategy for app copy.
#' @importFrom rlang abort
#' @importFrom glue glue_col
#' @noRd
check_app_copy_strategy <- function(strategy) {
  if (is_missing(strategy)) {
    rlang::abort("Please provide the copy strategy.")
  }
  # nolint start
  supported_app_copy_strategies <-
    getOption("sevenbridges2")$APP_COPY_STRATEGIES
  if (!(strategy %in% supported_app_copy_strategies)) {
    rlang::abort(
      glue::glue_col(
        "The provided copy strategy ({magenta {strategy}}) is not supported. Please use one of the following strategies: ",
        "{green {paste(supported_app_copy_strategies, collapse = ', ')}}"
      )
    )
  }
  # nolint end
}

#' Check file path
#'
#' @description This function checks if the file with the provided path exists
#'  on the local disk.
#' @param file_path File path on the local disk.
#' @importFrom rlang abort
#' @importFrom glue glue_col
#' @noRd
check_file_path <- function(file_path) {
  if (!file.exists(file_path)) {
    rlang::abort(glue::glue_col("File {magenta {file_path}} does not exist."))
  }
}

#' Check all volume params when creating a volume
#'
#' @description This function checks parameters needed for
#'  value creation/update.
#' @param args Input parameters to check
#' @param volume_type Storage type, can be one of: s3, gcs, azure, oss
#'
#' @importFrom checkmate assert_list assert_string
#' @importFrom rlang abort
#' @importFrom glue glue_col
#' @noRd
check_volume_params <- function(args,
                                volume_type = c("s3", "gcs", "azure", "OSS")) {
  checkmate::assert_list(args, null.ok = FALSE)
  volume_type <- match.arg(volume_type)

  checkmate::assert_string(args[["name"]], null.ok = FALSE)

  if (volume_type %in% c("s3", "gcs", "OSS")) {
    checkmate::assert_string(args[["bucket"]], null.ok = FALSE)
  }

  # Azure specific check
  if (volume_type == "azure") {
    checkmate::assert_string(args[["container"]], null.ok = FALSE)
  }

  checkmate::assert_string(args[["access_mode"]], null.ok = TRUE)

  if (!is_missing(args[["access_mode"]]) &&
    !(args[["access_mode"]] %in% c("RW", "RO"))) {
    rlang::abort("Access mode must be RW or RO.")
  }

  checkmate::assert_string(args[["prefix"]], null.ok = TRUE)
  checkmate::assert_string(args[["description"]], null.ok = TRUE)
  checkmate::assert_list(args[["properties"]], null.ok = TRUE)

  if (!is_missing(args[["endpoint"]])) {
    checkmate::assert_string(args[["endpoint"]], null.ok = TRUE)
  } else if (!is_missing(args[["root_url"]])) {
    checkmate::assert_string(args[["root_url"]], null.ok = TRUE)
  }
}

#' Transform configuration parameter in GC (IAM Role) volume creation
#'
#' @description This function checks whether provided configuration parameter
#'  is a named list or a file path to the configuration JSON file.
#' @param configuration Path to JSON file or named list containing configuration
#'  parameters values for creating GC volume using IAM Role.
#'
#' @importFrom rlang abort
#' @importFrom checkmate test_list
#' @importFrom checkmate test_string
#' @importFrom jsonlite toJSON
#' @importFrom readr read_file
#' @noRd
transform_configuration_param <- function(configuration) {
  if (checkmate::test_list(configuration, min.len = 1, null.ok = FALSE) &&
    !is_missing(names(configuration))) {
    config_json_string <-
      as.character(jsonlite::toJSON(configuration, auto_unbox = TRUE, pretty = TRUE)) # nolint
  } else if (checkmate::test_string(configuration, null.ok = FALSE)) {
    # nolint
    config_json_string <- readr::read_file(configuration)
  } else {
    rlang::abort(
      "Invalid configuration parameter! \n Please, provide a string path to the JSON file or a named list." # nolint
    )
  }
  return(config_json_string)
}

#' Convert date to string
#'
#' @description This function converts date data type to string.
#' @param datetime Object to convert.
#'
#' @importFrom rlang abort
#' @importFrom checkmate test_posixct
#' @importFrom checkmate test_character
#' @noRd
check_and_transform_datetime <- function(datetime) {
  if (missing(datetime)) {
    rlang::abort("Date is required!")
  }
  if (checkmate::test_character(datetime)) {
    return(datetime)
  }
  if (checkmate::test_posixct(datetime)) {
    return(as.character(datetime))
  }
}


#' Check execution settings parameters
#'
#' @description Check execution settings parameters.
#' @param execution_settings List of execution settings parameters.
#'
#' @importFrom checkmate assert_list assert_string assert_int assert_logical
#' @noRd
check_execution_settings <- function(execution_settings = NULL) {
  checkmate::assert_list(
    execution_settings,
    min.len = 1,
    max.len = 4,
    any.missing = TRUE,
    null.ok = TRUE
  )
  if ("instance_type" %in% names(execution_settings)) {
    checkmate::assert_string(
      x = execution_settings[["execution_settings"]],
      null.ok = TRUE
    )
  }

  if ("max_parallel_instances" %in% names(execution_settings)) {
    checkmate::assert_int(
      x = execution_settings[["max_parallel_instances"]],
      null.ok = TRUE
    )
  }

  if ("use_memoization" %in% names(execution_settings)) {
    checkmate::assert_logical(
      x = execution_settings[["use_memoization"]],
      null.ok = TRUE
    )
  }

  if ("use_elastic_disk" %in% names(execution_settings)) {
    checkmate::assert_logical(
      x = execution_settings[["use_elastic_disk"]],
      null.ok = TRUE
    )
  }
}
