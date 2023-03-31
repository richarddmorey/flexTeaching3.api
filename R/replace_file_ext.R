
#' Replace the extension on a file path with a new extension
#'
#' @param file_name File name
#' @param ext New extension
#' @param ... Additional arguments to pass to tools::file_path_sans_ext() 
#'
#' @return Returns a character vector of new file names
#' @export
#'
#' @examples
#' 
#' ft3_replace_file_ext('test.csv', 'txt')
ft3_replace_file_ext <- function(file_name, ext, ...){
  file_name |>
    tools::file_path_sans_ext(...) |>
    paste0('.', ext)
}
