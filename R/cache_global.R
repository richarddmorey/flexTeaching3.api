.pkg_global_env <- new.env( parent = emptyenv() )

#' @importFrom cachem cache_disk
create_cache <- function(
    cache_options = list(
      dir = ft3_options('cache_location'),
      destroy_on_finalize=TRUE
      ),
    name = 'ft_cache',
    overwrite = FALSE,
    quiet = FALSE
    )
{
  if(overwrite || !exists(name, .pkg_global_env)){
    ft_cache = do.call(
      what = cachem::cache_disk, 
      args = cache_options
    )
    if(!quiet)
      message('Cache created at ', ft_cache$info()$dir)
    assign(name, ft_cache, envir=.pkg_global_env)
  }
  invisible(exists(name, .pkg_global_env))
}

get_cache <- function(name = 'ft_cache'){
  if(!exists(name, .pkg_global_env)){
    stop('Cache has not yet been created.')
  }
  .pkg_global_env[[name]]
}
