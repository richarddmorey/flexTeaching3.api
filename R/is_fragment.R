
#' @importFrom tools file_ext
#' @importFrom rvest read_html html_elements
#' @importFrom assertthat assert_that is.string
ft3_is_fragment <- function(file_path){
  
  assertthat::assert_that(is.string(file_path), file.exists(file_path))
  
  out_ext <- tools::file_ext(file_path) |> tolower()
  if(out_ext != 'html') return(FALSE)
  
  content <- ft3_read_file_text(file_path)
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
