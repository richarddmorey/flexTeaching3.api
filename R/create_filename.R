#' Title
#'
#' @param ... 
#'
#' @return
#' @importFrom glue glue
#' @importFrom tools file_ext
#' @importFrom digest digest
#' @export
#'
#' @examples
ft3_create_filename <- function(filename, assignment, id, seed, assignment_mode, solutions, file_content, ..){
  datestamp = format(Sys.time(), "%d%m%Y-%H%M%S%Z")
  ext = tools::file_ext(filename)
  bn  = filename |> tools::file_path_sans_ext() |> basename()
  mode = ifelse(assignment_mode, 'assignment', 'practice')
  if(mode == 'practice')
    id = glue::glue('{id}_{seed}')
  content_hash = digest::digest(file_content)
  glue::glue('{bn}_{assignment}_{mode}_{id}_{datestamp}_{content_hash}.{ext}')
}
