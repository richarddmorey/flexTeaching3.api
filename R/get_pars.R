ft3_get_pars <- function(.req, assignment = NULL, file = NULL){
  c(
    'id',
    'seed',
    'solutions',
    'assignment_mode',
    'masterseed',
    'cache') |>
    sapply(.req$get_param_query, USE.NAMES = TRUE, simplify = FALSE) -> pars
  
  c(
    'assignment',
    'file'
  ) |>
    sapply(.req$get_param_path, USE.NAMES = TRUE, simplify = FALSE) -> pars_path
  
  pars = c(pars, pars_path)
  
  if(!is.null(pars$masterseed)){
    pars$masterseed <- trimws(pars$masterseed)
  }
  if(!is.null(pars$id)){
    pars$id <- trimws(pars$id)
  }else{
    pars$id <- ''
  }
  if(!is.null(pars$seed)){
    pars$seed <- trimws(pars$seed)
  }else{
    pars$seed <- ''
  }
  if(!is.null(pars$assignment_mode)){
   pars$assignment_mode <- as.logical(toupper(pars$assignment_mode))
  } 
  if(!is.null(pars$solutions)){
    pars$solutions <- as.logical(toupper(pars$solutions)) && !pars$assignment_mode
  }
  if(!is.null(pars$cache)){
    pars$cache <- as.logical(toupper(pars$cache))
  }

  return(pars)
}

