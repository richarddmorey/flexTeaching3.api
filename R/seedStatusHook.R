#' knitr hook to reset seed after running chunk
#'
#' This is automatically made available when .Rmd files are compiled.
#' See https://yihui.org/knitr/hooks/#chunk-hooks
#'
#' @param before Whether the code is running before the chunk is evaluated
#' @param options List of options in the current chunk
#' @param envir Environment in which the code chunk is evaluated
#' @param name The name of the hook 
#' 
#' @return Returns a hook function
#' @export
#'
#' @examples
#' 
#' knitr::knit_hooks$set(ft3_seed_status = flexTeaching3.api::ft3_seed_status_hook)
ft3_seed_status_hook <- local({
  initial_seed <- NULL
  
  function(before, options, envir, name) {
    opt = options[[name]]
    if( !(opt %in% c("warn","reset")) )
      stop("Unknown value for option ", name," : ", opt)
    
    if (before){
      initial_seed <<- if(exists(".Random.seed", envir = .GlobalEnv, inherits=FALSE)) 
        .Random.seed
    }else{
      seed_change = !identical(.Random.seed, initial_seed)
      if(opt == "reset"){
        if(!is.null(initial_seed))
          .Random.seed <<- initial_seed
        else
          rm(".Random.seed", envir = .GlobalEnv)
      }else if(seed_change && opt == "warn" ){
        txt = paste("**The seed has changed during execution of chunk: ", options$label, "**", collapse="")
        warning(txt)
        return(txt)
      }
    }
  }
})
