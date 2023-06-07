# Utils-validations
#' Check request status
#'
#' Check request status
#' @param req API request
#' @param as desired type output (contents of a request): raw, text or parsed
#'
#' @return request content or the message
#' @importFrom httr status_code
#'
#' @noRd
status_check <- function(req, as = "parsed", ...) {
  # nocov start
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
    rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
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
        rlang::abort(paste(
          "Error of unknown type occured",
          httr::status_code(req)
        ))
      } else {
        rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
      }
    } else {
      rlang::abort(paste0("HTTP Status ", httr::status_code(req), ": ", msg))
    }
  } # nocov end
}

#' Check if input value is missing
#'
#' @description This function checks whether the input
#' value is a vector of minimum length 1, with no empty
#' value and no all missing values.
#' If the input value is not a vector, it checks only if
#' the value is set at all (original meaning of 'missing' function)
#' in order to be able to use it with other object types.
#'
#' @param input value to check
#' @importFrom checkmate test_vector
#' @noRd
is_missing <- function(input) {
  if (checkmate::test_vector(input, null.ok = TRUE)) {
    isTRUE(
      !checkmate::test_vector(input,
        min.len = 1,
        all.missing = FALSE
      ) || isTRUE(input == "")
    )
  } else {
    missing(input)
  }
}

#' Check limit parameter
#'
#' @param limit limit value
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
#' @param offset offset value
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
#' @param tags tag values
#' @importFrom checkmate test_list assert_logical assert_character assert_list
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
    rlang::abort("Tags parameter must be an unnamed list of tags. For example: tags <- list('my_tag_1', 'my_tag_2')")
    # nolint end
  }
}

#' Check project settings
#'
#' @param settings settings named list
#' @importFrom checkmate assert_logical assert_list
#' assert_character assert_integer
#' @importFrom rlang abort
#' @noRd
check_settings <- function(settings) {
  if (!is.null(settings)) {
    msg <- "Settings must be provided as a list."
    if (!is.list(settings)) {
      rlang::abort(msg)
    }

    valid_input_names <- c(
      "locked", "controlled", "use_interruptible_instances",
      "use_memoization", "allow_network_access",
      "use_elastic_disk", "location", "intermediate_files"
    )

    invalid_element_names <- setdiff(names(settings), valid_input_names)

    if (length(invalid_element_names) > 0) {
      # nolint start
      rlang::abort(glue::glue("Argument {invalid_element_names} is not a valid settings field."))
      # nolint end
    }

    checkmate::assert_logical(settings$locked,
      .var.name = "locked", null.ok = TRUE
    )
    checkmate::assert_logical(settings$controlled,
      .var.name = "controlled", null.ok = TRUE
    )
    checkmate::assert_logical(settings$use_interruptible_instances,
      .var.name = "use_interruptible_instances", null.ok = TRUE
    )
    checkmate::assert_logical(settings$use_memoization,
      .var.name = "use_memoization", null.ok = TRUE
    )
    checkmate::assert_logical(settings$allow_network_access,
      .var.name = "allow_network_access", null.ok = TRUE
    )
    checkmate::assert_logical(settings$use_elastic_disk,
      .var.name = "use_elastic_disk", null.ok = TRUE
    )

    checkmate::assert_character(settings$location,
      .var.name = "location", null.ok = TRUE
    )

    if ("intermediate_files" %in% names(settings)) {
      checkmate::assert_list(settings$intermediate_files,
        .var.name = "intermediate_files", null.ok = TRUE
      )
      checkmate::assert_integer(settings$intermediate_files$duration,
        .var.name = "intermediate_files$duration", null.ok = TRUE
      )
      checkmate::assert_character(settings$intermediate_files$retention,
        .var.name = "intermediate_files$retention", null.ok = TRUE
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
  if (substr(name, 1, 2) == "__") {
    rlang::abort("The folder name cannot start with \"__\"")
  }
  if (grepl("\\s", name)) {
    rlang::abort("The folder name cannot contain spaces in the name.")
  }
}
#' Check metadata
#'
#' @param metadata settings named list
#' @importFrom checkmate test_list
#' @importFrom rlang abort
#' @noRd
check_metadata <- function(metadata) {
  if (!checkmate::test_list(metadata,
    types = "character",
    null.ok = TRUE,
    names = "named",
    max.len = 1000
  )) {
    # nolint start
    rlang::abort("Metadata parameter must be a named list of key-value pairs. For example: metadata <- list(metadata_key_1 = 'metadata_value_1', metadata_key_2 = 'metadata_value_2')")
    # nolint end
  }
}


#' Check file download destination
#'
#' @param directory_path directory path string
#' @param filename file name (base name)
#' @noRd
check_download_path <- function(directory_path, filename) {
  if (dir.exists(directory_path)) {
    if (is_missing(filename)) {
      rlang::abort("The filename parameter is missing.")
    } else if (!checkmate::test_character(filename, len = 1L)) {
      rlang::abort("The filename parameter should be a length-one string.")
    }
  } else {
    rlang::abort(glue::glue_col("Destination directory {directory_path} does not exist.")) # nolint
  }
}

#' Check retry parameters
#'
#' @description This function validates provided retry parameter
#' used within the `download()` method of a `File`object.
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
  checkmate::assert_numeric(size,
    lower = 0, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_numeric(part_size,
    lower = 0, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )

  if (!(size >= 0 && size <= getOption("sevenbridges2")$MAXIMUM_OBJECT_SIZE)) {
    # nolint start
    rlang::abort("File size must be between 0 - 5497558138880 (5TB), inclusive")
    # nolint end
  }
  if (!(part_size <= getOption("sevenbridges2")$MAXIMUM_PART_SIZE &&
    part_size >= getOption("sevenbridges2")$MINIMUM_PART_SIZE)) {
    # nolint start
    rlang::abort("Parameter part_size must be 5 MB to 5 GB, last part can be < 5 MB")
    # nolint end
  }
  part_length <- ifelse(size == 0, 1,
    as.integer(
      ceiling(size / part_size)
    )
  )
  if (part_length < 1 ||
    part_length >= getOption("sevenbridges2")$MAXIMUM_TOTAL_PARTS) {
    # nolint start
    rlang::abort("Total number of parts must be from 1 to 10,000 (inclusive). Please, modify part_size.")
    # nolint end
  }
}


#' Check app copy strategy
#'
#' @description This function checks if the provided strategy for app copy is
#' valid.
#' @param strategy Strategy for app copy.
#' @importFrom rlang abort
#' @importFrom glue glue_col
#' @noRd
check_app_copy_strategy <- function(strategy) {
  if (is_missing(strategy)) {
    rlang::abort("Please provide the copy strategy.")
  }
  # nolint start
  supported_app_copy_strategies <- getOption("sevenbridges2")$APP_COPY_STRATEGIES
  if (!(strategy %in% supported_app_copy_strategies)) {
    rlang::abort(
      glue::glue_col("The provided copy strategy ({magenta {strategy}}) is not supported. Please use one of the following strategies: ", "{green {paste(supported_app_copy_strategies, collapse = ', ')}}")
    )
  }
  # nolint end
}

#' Check file path
#'
#' @description This function checks if the file with the provided path exists
#' on the local disk.
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
#' @description This function checks parameters needed for value creation/update
#' @param args Input parameters to check
#' @param volume_type Storage type, can be one of: s3, gcs, azure, oss
#' @param auth_method Authentication method for s3 and gc volumes only, can be:
#' iam_user or iam_role.
#'
#' @importFrom checkmate assert_list
#' @importFrom checkmate assert_character
#' @importFrom rlang abort
#' @importFrom glue glue_col
#' @noRd
check_volume_params <- function(args,
                                volume_type = c("s3", "gcs", "azure", "ali"),
                                auth_method = NULL) {
  checkmate::assert_list(args, null.ok = FALSE)
  volume_type <- match.arg(volume_type)
  checkmate::assert_character(auth_method, null.ok = TRUE)

  checkmate::assert_character(args[["name"]],
    len = 1, null.ok = FALSE,
    typed.missing = TRUE
  )
  checkmate::assert_character(args[["bucket"]],
    len = 1, null.ok = FALSE,
    typed.missing = TRUE
  )
  checkmate::assert_character(args[["prefix"]],
    len = 1,
    typed.missing = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_character(args[["access_mode"]], len = 1, null.ok = TRUE)
  if (!is_missing(args[["access_mode"]]) &&
    !(args[["access_mode"]] %in% c("RW", "RO"))) {
    rlang::abort("Access mode must be RW or RO.")
  }
  checkmate::assert_character(args[["description"]],
    len = 1,
    typed.missing = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_list(args[["properties"]], null.ok = TRUE)

  if (!is_missing(args[["endpoint"]])) {
    checkmate::assert_character(args[["endpoint"]],
      len = 1,
      typed.missing = TRUE,
      null.ok = TRUE
    )
  } else if (!is_missing(args[["root_url"]])) {
    checkmate::assert_character(args[["root_url"]],
      len = 1,
      typed.missing = TRUE,
      null.ok = TRUE
    )
  }
  checkmate::assert_list(args[["credentials"]],
    null.ok = FALSE,
    types = "character", all.missing = FALSE
  )
}

#' Transform configuration parameter in GC (IAM Role) volume creation
#'
#' @description This function checks whether provided configuration parameter
#' is a named list or a file path to the configuration JSON file.
#' @param configuration Path to JSON file or named list containing configuration
#' parameters values for creating GC volume using IAM Role.
#'
#' @importFrom rlang abort
#' @importFrom checkmate test_list
#' @importFrom checkmate test_character
#' @importFrom jsonlite toJSON
#' @importFrom readr read_file
#' @noRd
transform_configuration_param <- function(configuration) {
  if (checkmate::test_list(configuration, min.len = 1, null.ok = FALSE)) {
    config_json_string <- as.character(
      jsonlite::toJSON(configuration, auto_unbox = TRUE, pretty = TRUE)
    )
  } else if (checkmate::test_character(configuration, len = 1, null.ok = FALSE, typed.missing = FALSE)) { # nolint
    config_json_string <- readr::read_file(configuration)
  } else {
    rlang::abort("Invalid configuration format! \n Please, provide a string path to the JSON file or a named list.") # nolint
  }
  return(config_json_string)
}
