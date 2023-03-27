#' Title
#'
#' @param file_path 
#'
#' @return
#' @importFrom tools file_ext
#' @importFrom rvest read_html html_elements
#' @importFrom readr read_file
#' @importFrom assertthat assert_that is.string
#'
#' @examples
ft3_is_fragment <- function(file_path){
  
  assertthat::assert_that(is.string(file_path), file.exists(file_path))
  
  out_ext <- tools::file_ext(file_path) |> tolower()
  if(out_ext != 'html') return(FALSE)
  
  content <- readr::read_file(file_path)
  assertthat::assert_that(is.string(content))
  
  split(content, '\n')[1] |>
    grepl(
      pattern='^<!DOCTYPE ', x = _, 
      ignore.case = TRUE
      ) -> has_doctype
  
  if(has_doctype)
    return(FALSE)
  
  parsed_html <- rvest::read_html(content) 
  
  parsed_html |>
    rvest::html_elements('head,title') |>
    length() -> ntag
  
  return(ntag<1)
}
