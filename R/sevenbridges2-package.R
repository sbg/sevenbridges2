# Seven Bridges API base url
sbg_baseurl <- list(
  "cgc" = "https://cgc-api.sbgenomics.com/v2/",
  "aws-us" = "https://api.sbgenomics.com/v2/",
  "aws-eu" = "https://eu-api.sbgenomics.com/v2/",
  "ali-cn" = "https://api.sevenbridges.cn/v2/",
  "cavatica" = "https://cavatica-api.sbgenomics.com/v2/",
  "f4c" = "https://api.sb.biodatacatalyst.nhlbi.nih.gov/v2/"
)

# default platform
sbg_default_platform <- "aws-us"

# default user configuration file
sbg_default_config_file <- "~/.sevenbridges/credentials"
sbg_default_profile_name <- "default"

# default system environment variable names
sbg_default_sysenv_url <- "SB_API_ENDPOINT"
sbg_default_sysenv_token <- "SB_AUTH_TOKEN"

utils::globalVariables(c(
  "sbg_baseurl", "sbg_default_platform",
  "sbg_default_config_file", "sbg_default_profile_name",
  "sbg_default_sysenv_url", "sbg_default_sysenv_token"
))
# onLoad function sets initial params for future queries
.onLoad <- function(libname, pkgname) {
  lst <- list(
    offset = 0,
    limit = 50,
    advance_access = FALSE,
    input_check = TRUE
  )

  options(sevenbridges2 = lst)
}
