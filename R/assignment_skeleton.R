

#' Title
#'
#' @param target_dir 
#' @param title 
#' @param short_title 
#' @param format 
#' @param ... Not currently used.
#'
#' @return
#' @export
#' 
#' @importFrom glue glue
#'
#' @examples
ft3_assignment_skeleton <- function(target_dir, title = 'Example assignment', short_title = abbreviate(title), format = c('html_document', 'html_fragment', 'pdf_document'), ...){
  
  if( !(format %in% c('html_document', 'html_fragment', 'pdf_document')) )
    stop('Format must be one of "html_document", "html_fragment", or "pdf_document".')
  
  if(!dir.exists(target_dir))
    stop('Target directory ', target_dir, ' does not exist.')
  
  title <- gsub(pattern = '\n', replacement = ' ', x = title)
  short_title <- gsub(pattern = '\n', replacement = ' ', x = short_title)

  nms <- ft3_get_assignments(target_dir) |> names()
  short_title <- make.unique(c(nms, short_title))[length(nms) + 1]
  
  dirs <- list.dirs(target_dir, recursive = FALSE) |> basename()
  dir_name <- make.unique(c(dirs, short_title))[length(dirs) + 1]
  dir_path <- file.path(target_dir, dir_name)
  md_ret <- dir.create(dir_path)
  
  if(!md_ret)
    stop('Could not create directory ', dir_name, ' in folder ', target_dir, '.')
  
  seed_salt <- ft3_random_alpha_num(1, n_char = 20)
  data_salt <- ft3_random_alpha_num(1, n_char = 20)
  
  hide_before <- restrict_before <- format(Sys.time(), "%R %Y-%m-%d")
  
  system.file('skeleton/index.Rmd', package = 'flexTeaching3.api') |>
    ft3_read_file_text() |>
    glue::glue() |>
    writeLines(con = file.path(dir_path, 'index.Rmd'))
}
