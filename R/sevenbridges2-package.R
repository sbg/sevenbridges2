# onLoad function sets initial params for future queries
.onLoad <- function(libname, pkgname) {
  lst <- list(
    offset = 0,
    limit = 100,
    advance_access = FALSE,
    input_check = TRUE
  )

  options(sevenbridges = lst)
}
