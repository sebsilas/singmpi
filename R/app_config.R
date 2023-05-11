
#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "singmpi")
}

#' Get currently installed package version
#' @noRd
app_version <- function() {
  tryCatch(utils::packageVersion(pkg = "singmpi"),
           error = function(e) {
             logging::logerror("singmpi is not installed")
             NA_character_
           })
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE value.
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @export
get_config <- function(
    value,
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    # Modify this if your config file is somewhere else:
    file = app_sys("config.yml"),
    use_parent = use_parent
  )
}
