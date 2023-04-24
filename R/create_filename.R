#' Create a file name from flexTeaching parameters
#' 
#' This function is meant to be used in the YAML header for an assingment
#' that outputs a file to be downloaded. It is a useful default that can be 
#' replaced.
#' 
#' @param filename The original file name
#' @param assignment The assignment reference
#' @param id The user id parameter
#' @param seed The seed parameter
#' @param assignment_mode (logical) In assignment mode? 
#' @param solutions (logical) Solutions requested? 
#' @param file_content Full content of the file
#' @param ... Other potential parameters (not currently used)
#'
#' @return A character string to be used as a file name
#' @importFrom glue glue
#' @importFrom tools file_ext
#' @importFrom digest digest
#' @export
ft3_create_filename <- function(filename, assignment, id, seed, assignment_mode, solutions, file_content, ...){
  datestamp = format(Sys.time(), "%d%m%Y-%H%M%S%Z")
  ext = tools::file_ext(filename)
  if(nchar(ext)>0) ext = paste0('.',ext)
  bn  = filename |> tools::file_path_sans_ext() |> basename()
  mode = ifelse(assignment_mode, 'assignment', 'practice')
  if(mode == 'practice')
    id = glue::glue('{id}_{seed}')
  content_hash = digest::digest(file_content)
  glue::glue('{bn}_{assignment}_{mode}_{id}_{datestamp}_{content_hash}{ext}')
}
