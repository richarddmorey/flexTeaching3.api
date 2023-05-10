# https://cheatsheetseries.owasp.org/cheatsheets/HTML5_Security_Cheat_Sheet.html#tabnabbing
# https://stackoverflow.com/a/71195901/1129889
extra_headers_mw = RestRserve::Middleware$new(
  process_request = function(request, response) TRUE,
  process_response = function(request, response){
    response$set_header('Referrer-Policy', 'no-referrer') # Prevent tabnab
    response$set_header('Access-Control-Expose-Headers', 'Content-Disposition') # allow cross-domain filename access
    # This is to handle the fact that the server rejects the pre-flight
    # request. I've tried to make this a targeted by-pass of the pre-flight 
    if(
      request$method == 'OPTIONS' &&
      'authorization' %in% request$headers[['access-control-request-headers']] &&
      request$headers[['access-control-request-method']] == 'GET'
    )
      response$set_status_code(200)
  },
  id = "extra_headers"
)

auth_mw = RestRserve::AuthMiddleware$new(
  auth_backend = RestRserve::AuthBackendBearer$new(FUN = ft3_check_auth_tokens), 
  routes = "/", match = "partial",
  id = "auth_middleware"
)

#' Start the flexTeaching3 API using RestRserve
#' 
#' This function is meant to be run from a script, not in an interactive session.
#' See the file 'start_api.R' script included with this package.
#'
#' @param cache_options List of options to be passed to cachem::disk_cache()
#' @param assignments_pkg Package containing the assignments
#' @param assignments_dir Optional directory containing the assignments; if not specified, will be inferred from assignments_pkg.
#' @param http_port Port to start the server
#' @param log_options List of options for the log, to be passed to RestRserve::Logger$new()
#' @param ... Further arguments to be passed to the RestRserve backend's start() method
#'
#' @return The result from the RestRserve backend's start() method
#' @export
#' 
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom cachem cache_disk is.key_missing
#' @importFrom RestRserve Application BackendRserve CORSMiddleware
#' @importFrom assertthat assert_that is.dir
#' @importFrom utils packageName
#'
ft3_serve_api <- function(
  cache_options = list(
    dir = ft3_options('cache_location'),
    destroy_on_finalize=TRUE
    ),
  assignments_pkg = ft3_options('assignments_pkg'),
  assignments_dir = NULL,
  http_port = 8080, 
  log_options = list(level = 'off'),
  ...
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
    assignments_pkg <- utils::packageName()
  
  apkg_dir <- system.file(
    'ft3_pkg',
    package = assignments_pkg
  )
  assertthat::assert_that(is.dir(apkg_dir))
  
  if(is.null(assignments_dir))
    ft3_options('assignments_dir' = file.path(apkg_dir, 'assignments'))
  assertthat::assert_that(is.dir(ft3_options('assignments_dir')))
  
  create_cache(cache_options)
  
  mw = list(RestRserve::CORSMiddleware$new(), extra_headers_mw)
  if(length(ft3_options('auth_tokens'))>0){
    mw = c(mw, auth_mw)
  }
  
  app = RestRserve::Application$new(
    middleware = mw
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
  backend$start(app, http_port = http_port, ...)
}
