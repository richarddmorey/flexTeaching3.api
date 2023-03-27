#' Title
#'
#' @param file 
#' @param id 
#' @param seed 
#' @param solutions 
#' @param assignment_mode 
#' @param assignments_dir
#'
#' @return
#' @importFrom digest digest
#' @importFrom tools md5sum
#'
#' @examples
ft3_get_cache_key = function(file, id, seed, solutions, assignment_mode)
{
  full_path <- normalizePath(file)  
  file_md5 <- tools::md5sum(full_path)
  # Include both assignment name and file in case support files are different
  c(assignment = full_path,
    md5 = file_md5,
    id = trimws(id),
    seed = trimws(seed),
    solutions = solutions,
    assignment_mode = !assignment_mode
  ) |>
    digest::digest() 
}
