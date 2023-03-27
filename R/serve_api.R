
#' Title
#'
#' @param cache_options
#' @param assignments_pkg 
#' @param assignments_dir
#' @param http_port 
#' @param log_options
#'
#' @return
#' @export
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom cachem cache_disk is.key_missing
#' @importFrom RestRserve Application BackendRserve CORSMiddleware
#' @importFrom base64enc base64encode
#' @importFrom assertthat assert_that is.dir
#'
#' @examples
ft3_serve_api <- function(
  cache_options = list(
    dir = ft3_options('cache_location'),
    destroy_on_finalize=TRUE
    ),
  assignments_pkg = ft3_options('assignments_pkg'),
  assignments_dir = NULL,
  http_port = 8080, 
  log_options = list(level = 'off')
)
{
  
  find.package(assignments_pkg)
  
  if(is.null(ft3_options('scratch_dir'))){
    ft3_options('scratch_dir' = tempfile(pattern='dir'))
    dir.create(ft3_options('scratch_dir'))
  }

  assertthat::assert_that(is.dir(ft3_options('scratch_dir')))
  
  master_secret = ft3_options('master_secret')
  if(is.null(master_secret)){
    ft3_animal_seed(
      ft3_random_alpha_num(),
      ft3_random_alpha_num()
    ) |>
      ft3_options(master_secret = _)
    paste0(
      '\nMaster secret was NULL. Setting ft3_options("master_secret") to "',
      ft3_options('master_secret'),
      '".\n\n'
    ) |>
    warning()
  }
  
  if(is.null(assignments_pkg))
    assignments_pkg <- packageName()
  
  apkg_dir <- system.file(
    'ft3_pkg',
    package = assignments_pkg
  )
  assertthat::assert_that(is.dir(apkg_dir))
  
  if(is.null(assignments_dir))
    ft3_options('assignments_dir' = file.path(apkg_dir, 'assignments'))
  assertthat::assert_that(is.dir(ft3_options('assignments_dir')))
  
  create_cache(cache_options)
  
    
  app = RestRserve::Application$new(
    middleware = list(RestRserve::CORSMiddleware$new())
  )

  app$add_get(
    path = "/ft3/api/v1/assignments", 
    FUN = ft3_assignment_list_handler
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}/configuration', 
    FUN = ft3_assignment_content_config_handler,
    'regex'
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}', 
    FUN = ft3_assignment_content_handler,
    'regex'
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}/seed', 
    FUN = ft3_assignment_seed_handler,
    'regex'
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}/cachekey', 
    FUN = ft3_assignment_cachekey_handler,
    'regex'
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}/files', 
    FUN = ft3_assignment_file_list_handler,
    'regex'
  )
  
  app$add_get(
    path = '/ft3/api/v1/assignments/{assignment}/files/{file}', 
    FUN = ft3_assignment_file_handler,
    'regex'
  )
  
  app$logger = do.call(what = RestRserve::Logger$new, args = log_options)
  backend = RestRserve::BackendRserve$new()
  backend$start(app, http_port = http_port)
}
