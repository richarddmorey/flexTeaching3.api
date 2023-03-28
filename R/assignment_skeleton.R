

#' Title
#'
#' @param target_dir The directory that is to contain the assignments  
#' @param title Title for the assignment
#' @param format One of 'html_document', 'html_fragment', or 'pdf_document'
#' @param ... Not currently used.
#'
#' @return The path of the file written.
#' @export
#' 
#' @importFrom glue glue
#'
#' @examples
#' 
ft3_assignment_skeleton <- function(target_dir, title = 'Example assignment', format = c('html_document', 'html_fragment', 'pdf_document'), ...){
  
  if( !(format %in% c('html_document', 'html_fragment', 'pdf_document')) )
    stop('Format must be one of "html_document", "html_fragment", or "pdf_document".')
  
  if(!dir.exists(target_dir))
    stop('Target directory ', target_dir, ' does not exist.')
  
  title <- menu <- gsub(pattern = '\n', replacement = ' ', x = title)
  ref <- abbreviate(title)
  
  dirs <- list.dirs(target_dir, recursive = FALSE) |> basename()
  dir_name <- make.unique(c(dirs, ref))[length(dirs) + 1]
  dir_path <- file.path(target_dir, dir_name)
  md_ret <- dir.create(dir_path)
  
  if(!md_ret)
    stop('Could not create directory ', dir_name, ' in folder ', target_dir, '.')
  
  seed_salt <- ft3_random_alpha_num(1, n_char = 20)
  data_salt <- ft3_random_alpha_num(1, n_char = 20)
  
  hide_before <- restrict_before <- format(Sys.time(), "%R %Y-%m-%d")
  
  file_path <- file.path(dir_path, 'index.Rmd')
  
  system.file('skeleton/index.Rmd', package = 'flexTeaching3.api') |>
    ft3_read_file_text() |>
    glue::glue() |>
    writeLines(con = file_path)
  
  return(file_path)
}
