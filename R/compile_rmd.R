#' Get the flextTeaching settings from the yaml header of an assignment
#'
#' @param assignment_file Path to the assignment source
#'
#' @return
#' @importFrom tools file_ext
#' @importFrom rmarkdown yaml_front_matter
#'
#' @examples
ft3_assignment_settings_rmd <- function(assignment_file){
  settings <- rmarkdown::yaml_front_matter(assignment_file)
  ft3_settings <- settings$params$flexTeaching$value
  return(ft3_settings)
}

#' Compile an Rmd flexTeaching assignment
#'
#' @param rmd_file Path to the Rmd assignment file to be compiled
#' @param pars The parameters passed from the client
#' @param ft3_cache The cachem cache (if relevant)
#' @param scratch_dir Where should the scratch files be written?
#'
#' @return
#' @importFrom withr with_seed
#' @importFrom callr r
#' @importFrom rmarkdown render
#' @importFrom knitr knit_hooks
#'
#' @examples
ft3_compile_doc_rmd <- function(rmd_file, pars, local_seed, temp_space_path, ...){
  settings <- ft3_assignment_settings(rmd_file)
  use_callr <- ifelse(is.null(settings$use_callr), TRUE, settings$use_callr)
  temp_rmd_file = file.path(temp_space_path, basename(rmd_file))
  compile_expressions <- expression(
    knitr::knit_hooks$set(ft3_seed_status = flexTeaching3.api::ft3_seed_status_hook),
    rmarkdown::render(
      input = temp_rmd_file, 
      quiet = TRUE,
      params = list(flexTeaching_pars = pars)
    )
  )
  if(use_callr){
    outfile <- callr::r(
      \(expr, temp_rmd_file, seed, settings, pars) 
        withr::with_seed(seed, { lapply(expr, eval, envir = new.env())[[2]] }),
      args = list(expr = compile_expressions, 
                  temp_rmd_file = temp_rmd_file,
                  seed = local_seed, settings = settings, pars = pars)
    )
  }else{
    #stop('Compiling documents outside of callr::r not supported.')
    outfile <- withr::with_seed(
      local_seed, { lapply(expr, eval, envir = new.env())[[2]] }
    )
  }
  return(outfile)
}

