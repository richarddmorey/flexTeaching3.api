ft3_new_temp_space <- function(container_dir = ft3_options('cache_location')){
  subdir <- tempfile(pattern = 'dir') |> basename()
  temp_space_path <- file.path(container_dir, subdir)
  
  code <- dir.create(temp_space_path)
  if(!code){
    stop('Could not create temporary space ', temp_space_path, '.')
  }
  return(temp_space_path)
}

ft3_prepare_temp_space <- function(assignment_path, container_dir = ft3_options('cache_location')){
  temp_space_path <- ft3_new_temp_space(container_dir)
  assignment_dir <- assignment_path |> dirname()
  assignment_dir_name <- assignment_dir |> basename()
  code <- file.copy(assignment_dir, temp_space_path, recursive = TRUE)
  if(!code){
    stop('Could not copy assignment ', assignment_dir_name, ' to ', temp_space_path, '.')
  }
  temp_assignment_dir <- file.path(temp_space_path, assignment_dir_name)
  if(!dir.exists(temp_assignment_dir)){
    stop('Could not verify temporary assignment directory ', temp_assignment_dir, '.')
  }
  return(temp_assignment_dir)
}


