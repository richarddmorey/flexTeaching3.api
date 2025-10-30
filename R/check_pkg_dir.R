#' Check a package subfolder for flexTeaching3 resources
#'
#' @param pkg 
#'
#' @return The normalized support directory path, if valid.
ft3_check_pkg_dir <- function(pkg){
  if(find.package(pkg) == "")
    stop('Package "', pkg, '" does not appear to be installed.')
  subdir <- ft3_options('assignments_subdir')
  assignment_dir <- system.file(subdir, package = pkg)
  if(assignment_dir == "")
    stop('Package "', pkg, '" does not appear to have a subdirectory called "', subdir,'".')
  if(!dir.exists(file.path(assignment_dir, 'assignments')))
    stop('Directory "', assignment_dir,'" does not appear to have an "assignments" subdirectory.')
  assignment_dir |>
    file.path('assignments') |>
    ft3_get_assignments() |> 
    length() -> n_assignments
  if(n_assignments == 0)
    stop('Directory "', 
         assignment_dir |> file.path('assignments'),
         '" does not appear to have any valid assignments in the "assignments" subdirectory.'
         )
  return(normalizePath(assignment_dir))
}
