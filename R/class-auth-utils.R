#' @title Get environment variables
#'
#' @description Get authentication environment variables for Seven Bridges API.
#'
#' @param x Name of the system environment variable.
#'
#' @return A value of the environment variable.
#'
#' @importFrom rlang abort
#' @importFrom glue glue
#'
#' @noRd
sbg_get_env <- function(x) {
  res <- Sys.getenv(x)
  if (res == "") {
    rlang::abort(glue::glue("Environment variable {x} is blank, please check if
                            it is set correctly"))
  }
  res
}

#' @title Set environment variables
#'
#' @description Set authentication environment variables for Seven Bridges API.
#'
#' @param url Base URL for API.
#' @param token Your authentication token.
#' @param sysenv_url_name Name for the url environment variable.
#' The default value is `r toString(sbg_default_sysenv_url)`.
#' @param sysenv_token_name Name for the token environment variable.
#' The default value is `r toString(sbg_default_sysenv_token)`.
#' @importFrom rlang abort
#'
#' @noRd
sbg_set_env <- function(url = NULL, token = NULL,
                        sysenv_url_name = sbg_default_sysenv_url,
                        sysenv_token_name = sbg_default_sysenv_token) {
  if (is.null(url) || is.null(token)) {
    rlang::abort("URL and token must be both specified.")
  }

  args <- list(url, token)
  names(args) <- c(
    sysenv_url_name,
    sysenv_token_name
  )
  do.call(Sys.setenv, args)
}

#' @title Read ini format file
#'
#' @param file Character string, path to ini file.
#'
#' @return Nested list keeping the hierarchical structure of the ini file.
#'
#' @importFrom stringr str_trim
#' @noRd
read_ini <- function(file) {
  # section name lines: starting with `[` ending with `]`
  pattern_section <- "^\\s*\\[\\s*(.+?)\\s*]"
  # key-value lines: key=value
  pattern_kv <- "^\\s*[^=]+=.+"

  x <- readLines(con = file, warn = FALSE)
  is_section <- grepl(pattern_section, x)
  idx_section <- which(is_section)
  count_section <- sum(is_section)

  cfg <- vector("list", count_section)

  # extract section names
  for (i in 1L:count_section) {
    names(cfg)[[i]] <- substr(
      x[idx_section[i]], 2L,
      nchar(x[idx_section[i]]) - 1L
    )
  }

  # extract key-value pairs
  range_section <- c(idx_section, length(x) + 1L)
  for (i in 1L:count_section) {
    for (j in (range_section[i] + 1L):(range_section[i + 1L] - 1L)) {
      tmp <- x[j]
      if (grepl(pattern_kv, tmp)) {
        kv <- stringr::str_trim(unlist(strsplit(tmp, "=")))
        cfg[[i]][kv[1L]] <- kv[2L]
      }
    }
  }

  # convert everything into a list
  for (i in 1L:count_section) cfg[[i]] <- as.list(cfg[[i]])

  cfg
}

#' @title Parse Seven Bridges user config file into a nested list
#'
#' @param file character string, path to config file
#'
#' @importFrom rlang abort
#' @return Nested list keeping the hierarchical structure of the config file
#'
#' @noRd
sbg_parse_config <- function(file) {
  f <- file.path(path.expand(file))
  if (file.exists(f)) {
    res <- try(read_ini(f), silent = TRUE)
    if (inherits(res, "try-error")) {
      rlang::abort("User config file format is incorrect.")
    }
  } else {
    rlang::abort(paste0("User config file: ", f, " does not exist."))
  }
  res
}

#' @title Normalise URL
#'
#' @description Add `/` to url ends.
#' @param x String, URL.
#'
#' @noRd
normalize_url <- function(x) {
  if (!grepl("/$", x)) {
    paste0(x, "/")
  } else {
    x
  }
}

#' @title Platform name reverse lookup
#'
#' @param baseurl String; Base URL.
#'
#' @noRd
sbg_platform_lookup <- function(baseurl) {
  x <- which(unlist(sbg_baseurl) == normalize_url(baseurl))
  if (length(x) > 0L) names(x) else NULL
}
