# Variable, global to package's namespace. 
# This function is not exported to user space and does not need to be documented.
MYPKGOPTIONS <- settings::options_manager(
  master_secret = NULL,
  scratch_dir   = NULL,
  cache_location = NULL,
  assignments_pkg = NULL,
  assignments_dir = NULL,
  default_file_icon = 'fa-regular fa-download',
  timezone = '', # Defaults to current time zone
  lubridate_orders = c('HM ymd', 'ymd HM'),
  default_category_name = 'Uncategorized',
  errors_to_client = FALSE,
  force_callr = FALSE
)

# User function that gets exported:

#' Set or get options for the `flexTeaching3.api` package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{master_secret}}{(\code{character})  Master secret to use for generation of seeds. If NULL when the package is loaded, a random master seed is generated (with a warning). }
#'  \item{\code{scratch_dir}}{(\code{character})  Disk location to use a temporary space for rendering. If NULL, a temporary directory is used.}
#'  \item{\code{cache_location}}{(\code{character})  Disk location to save the content cache. If NULL, a temporary directory is used.}
#'  \item{\code{assignments_pkg}}{(\code{character}) Installed package to use for the assignments. If NULL, this package is used. }
#'  \item{\code{assignments_dir}}{(\code{character}) Directory to use for the assignments. If NULL, it will be determined from \code{assignments_pkg}.}
#'  \item{\code{default_file_icon}}{(\code{character}) Default icon to use for file downloads if none is specified. }
#'  \item{\code{timezone}}{(\code{character}) Default timezone to use for assignment restrictions. Empty string is system time zone. }
#'  \item{\code{lubridate_orders}}{(\code{character}) Time/date orders to use when parsing date-times using the lubridate package. }
#'  \item{\code{default_category_name}}{(\code{character}) The name of the category in which to place uncategorized assignments. }
#'  \item{\code{errors_to_client}}{(\code{logical}) Pass internal R error descriptions to the client interface? Doing this may pose a security risk, but may be helpful for debugging.}
#'  \item{\code{force_callr}}{(\code{logical}) Force use of callr for rednering all documents, regardless of assignment settings? May be helpful for debugging.}
#' }
#'
#' @export
#' @importFrom settings options_manager
ft3_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  MYPKGOPTIONS(...)
}