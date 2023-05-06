
#' @importFrom xml2 read_html read_xml xml_find_first xml_add_child
#' @importFrom purrr walk
ft3_insert_scripts<- function(content, script_files){
  
  x <- xml2::read_html(content)
  body <- xml2::xml_find_first(x, 'body')
  
  script_files |>
    purrr::walk(\(s){
      system.file(s, package = packageName()) |>
        ft3_read_file_text() |>
        paste0('<script>', x = _, '</script>') |>
        xml2::read_xml() |>
        xml2::xml_add_child(body, .value = _)
    })
  
  x |>
    as.character()
}
