

#' @importFrom xml2 read_html read_xml xml_find_first xml_add_child
ft3_insert_iframe_resizer<- function(content){

  system.file('iframeResizerChild/resize_iframe_child_script.js', package = packageName()) |>
    ft3_read_file_text() |>
    paste0('<script>', x = _, '</script>') |>
    xml2::read_xml() -> script_content
  
  x <- xml2::read_html(content)
  
  x |>
    xml2::xml_find_first('body') |>
    xml2::xml_add_child(.value = script_content)
  
  x |>
    as.character()
}
