
ft3_assignment_list_handler <- function(.req, .res){
  
  .res$set_content_type("application/json")
  
  ft3_get_assignments_select2_http(
    current_assignment = .req$get_param_query('assignment')
    ) |>
    .res$set_body()
}

#' @importFrom utils object.size
ft3_assignment_content_config_handler <- function(.req, .res){
  pars <- ft3_get_pars(.req)
  
  settings = ft3_get_assignments()[[pars$assignment]]

  if(is.null(settings))
    ft3_http_error(404, paste("The assignment", pars$assignment, "was not found."))
  
  if(!isTRUE(pars$cache))
    ft3_check_assignment_restrictions(settings, pars)
  
  ft3_compile_doc_http(
    settings$file_,
    pars = pars,
    ft3_cache = get_cache(),
    scratch_dir = ft3_options('scratch_dir'),
    override_restriction = pars$cache
  ) -> content
  
  .res$set_content_type("application/json")
  
  if(isTRUE(pars$cache)){
    list(
      cache = 'OK', 
      size = utils::object.size(content) |> as.numeric()
      ) |>
      jsonlite::toJSON(auto_unbox = TRUE) |>
      .res$set_body()
    return()
  }
  
  content$content <- NULL
  content$files <- NULL
  
  # Check to make sure we don't leak javascript before assignment is available
  r <- try(ft3_check_assignment_restrictions(settings, pars, check_exam = FALSE))
  if(inherits(r, 'try-error'))
    content$js <- NULL
  
  
  content |>
    list(configuration = _) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    .res$set_body()
}

ft3_assignment_content_handler <- function(.req, .res){
 
  pars <- ft3_get_pars(.req)
  
  settings = ft3_get_assignments()[[pars$assignment]]
  
  ft3_check_assignment_restrictions(settings, pars)
  
  ft3_compile_doc_http(
    settings$file_,
    pars = pars,
    ft3_cache = get_cache(),
    scratch_dir = ft3_options('scratch_dir')
  ) -> content
  
  if(content$file_ext == 'html'){
    .res$set_content_type("text/html")
  }else if(content$file_ext == 'pdf'){
    .res$set_content_type("application/pdf")
  }else{
    ft3_http_error(
      status_code = 500,
      detail = paste0('Bad content file type ', content$file_ext, ' in assignment ', pars$assignment)
    )
  }
  .res$set_body(content$content)
}

ft3_assignment_seed_handler <- function(.req, .res){
  
  pars <- ft3_get_pars(.req)
  settings <- ft3_get_assignments()[[pars$assignment]]
  
  pars$seed <- ft3_seed_from_master(pars$masterseed, settings[['seed_salt']])
  
  .res$set_content_type("application/json")
  
  pars |>
    list(pars = _) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    .res$set_body()
}

ft3_assignment_cachekey_handler <- function(.req, .res){
  
  pars <- ft3_get_pars(.req)
  
  settings <- ft3_get_assignments()[[pars$assignment]]
  pars$solutions <- as.logical(toupper(pars$solutions)) && !pars$assignment_mode
  seed0 <-  ifelse(
    pars$assignment_mode,
    ft3_seed_from_master(ft3_options('master_secret'), settings[['seed_salt']]),
    pars$seed
  )
  cache_key <- ft3_get_cache_key(
    file = settings[['file_']], 
    id = pars$id, 
    seed = seed0, 
    solutions = pars$solutions, 
    assignment_mode = pars$assignment_mode
  )
  
  .res$set_content_type("application/json")
  
  list(cached = !cachem::is.key_missing(get_cache()$get(cache_key))) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    .res$set_body()
}

ft3_assignment_file_list_handler <- function(.req, .res){

  pars <- ft3_get_pars(.req)
  settings <- ft3_get_assignments()[[pars$assignment]]
  
  ft3_check_assignment_restrictions(settings, pars, FALSE)
  
  .res$set_content_type("application/json")
  
  # Add in access date restrictions
  
  settings$files |>
    list(file_list = _) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    .res$set_body()
}

ft3_assignment_file_handler <- function(.req, .res){
  
  pars <- ft3_get_pars(.req)
  settings <- ft3_get_assignments()[[pars$assignment]]
  
  ft3_check_assignment_restrictions(settings, pars)

  files <- ft3_compile_doc_http(
    settings$file_,
    pars = pars, 
    ft3_cache = get_cache(),
    scratch_dir = ft3_options('scratch_dir')
    )$files
  
  if(is.null(files[[pars$file]])){
    ft3_http_error(
      status_code = 400,
      detail = paste0('Filename ', pars$file ,' not found for assignment ', pars$assignment)
    )
  }else{
    contents <- files[[pars$file]]$content
    name <- files[[pars$file]]$name
  }
  
  .res$set_content_type("application/octet-stream")
  .res$set_header('Content-Disposition', paste0('inline; filename="',name,'"'))
  .res$set_body(contents)
}



