
#' @importFrom yaml yaml_front_matter
#' @importFrom utils URLencode
ft3_assignment_settings_html <- function(assignment_file){
  settings_file <- dirname(assignment_file) |> file.path('index.yaml')
  ft3_settings = list()
  if(file.exists(settings_file)){
    ft3_settings = yaml::read_yaml(settings_file)
    if(!is.list(ft3_settings))
      ft3_settings = list()
  }
  ft3_settings$ref <- dirname(assignment_file) |> basename() |> utils::URLencode()
  return(ft3_settings)
}
