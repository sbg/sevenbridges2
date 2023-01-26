# Helper test functions

sbg_get_env <- function(x) {
  res <- Sys.getenv(x)
  if (res == "") {
    stop("Environment variable ", x,
         " is blank, please check if it is set correctly",
         call. = FALSE
    )
  }
  res
}


sbg_set_env <- function(url = NULL, token = NULL, sysenv_url_name = sbg_default_sysenv_url, sysenv_token_name = sbg_default_sysenv_token) {
  if (is.null(url) | is.null(token)) {
    stop("url and token must be both specified", call. = FALSE)
  }

  args <- list(url, token)
  names(args) <- c(
    sysenv_url_name,
    sysenv_token_name
  )
  do.call(Sys.setenv, args)
}
