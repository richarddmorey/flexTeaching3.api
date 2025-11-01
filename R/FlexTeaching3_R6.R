
#' R6 Class Representing a set of flexTeaching3 assignments
#'
#' @description
#'
#' @details
#' 
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom assertthat assert_that
#' 
#' @examples
#' # example code
#' ft3 <- FlexTeaching3$new(assignment_pkg = 'flexTeaching3.api', master_secret="s3cR3t")
#' rm(ft3)

FlexTeaching3 <- R6::R6Class(
  "FlexTeaching3",
  public = list(
    #' Report settings in use by the FlexTeaching3 object
    #'
    #' @return A list containing the relevant settings
    #'
    settings = function(){
      list(
        scratch_dir   = private$scratch_dir,
        master_secret = private$master_secret,
        support_dir   = private$support_dir,
        assignments   = private$assignments
      )
    },
    #' Get the header and footer for the assignments (for inserting
    #' into the interface)
    #'
    #' @return A JSON string containing the header and footer
    header_footer_json = function(){
      jsonlite::toJSON(private$header_footer,auto_unbox = TRUE)
    },
    #' Get a json listing of assignments appropriate for making a select box 
    #'
    #' @param current_assignment Assignment which, if hidden, will become visible because it is currently selected.
    #'
    #' @return A JSON string
    #' 
    assignments_json = function(current_assignment = NULL){
      private$support_dir |>
        file.path('assignments') |>
        ft3_get_assignments_select2(current_assignment)
    },
    #' Compile assignment and return the data
    #'
    #' @param assignment Assignment to compile
    #' @param id (character) id
    #' @param seed (character) seed
    #' @param solutions (logical) Whether to include solutions
    #' @param assignment_mode (logical) Use assignment mode
    #' @param use_cache (logical) Use the cached version, if it exists?
    #' @param cache_only (logical) If TRUE, don't return document
    #' @param ... Arguments to pass to ft3_compile_doc
    #'
    #' @return A list containing the assignment data
    #' 
    assignment_content = function(assignment, id, seed, solutions, assignment_mode, use_cache = TRUE, cache_only = FALSE, ...){
      if(is.null(private$assignments[[assignment]]))
        stop('Assignment "', assignment,'" does not appear in the assignments list.')
      doc_file <- private$assignments[[assignment]]$file_
      if(!file.exists(doc_file))
        stop('Assignment document "',doc_file,'" does not exist.')
      if(cache_only && !use_cache)
        stop('use_cache must be true if cache_only is true.')
      pars <- list(
        assignment      = assignment,
        id              = id,
        seed            = seed, 
        solutions       = solutions,
        assignment_mode = assignment_mode,
        cache           = cache_only
        )
      ft3_compile_doc(
        doc_file      = doc_file,
        pars          = pars,
        ft3_cache     = private$cache,
        scratch_dir   = private$scratch_dir,
        use_cache     = isTRUE(use_cache),
        master_secret = private$master_secret,
        ...
      )
    },
    #' Compile assignment and return configuration data only
    #'
    #' @param assignment Assignment to compile
    #' @param id (character) id
    #' @param seed (character) seed
    #' @param solutions (logical) Whether to include solutions
    #' @param assignment_mode (logical) Use assignment mode
    #' @param use_cache (logical) Use the chached version, if it exists?
    #' @param cache_only (logical) If TRUE, don't return document
    #' @param ... Arguments to pass to ft3_compile_doc
    #'
    #' @return A list containing the assignment configuration
    #' 
    assignment_settings = function(assignment, id, seed, solutions, assignment_mode, use_cache = TRUE, cache_only = FALSE, ...){
      content <- self$assignment_content(assignment, id, seed, solutions, assignment_mode, use_cache, cache_only, ...)

      if(isTRUE(cache_only)){
        list(
          cache = 'OK', 
          size = utils::object.size(content) |> as.numeric()
        ) |>
          jsonlite::toJSON(auto_unbox = TRUE) -> x
        return(x)
      }
      
      settings = private$assignments[[assignment]]
      
      pars <- list(
        assignment      = assignment,
        id              = id,
        seed            = seed, 
        solutions       = solutions,
        assignment_mode = assignment_mode,
        cache           = cache_only
      )
      
      content$content <- NULL
      content$files <- NULL
      
      # Check to make sure we don't leak javascript before assignment is available
      r <- try(ft3_check_assignment_restrictions(settings, pars, check_exam = FALSE))
      if(inherits(r, 'try-error'))
        content$js <- NULL

      Filter(Negate(is.null), content) |>
        list(configuration = _) |>
        jsonlite::toJSON(auto_unbox = TRUE)
    },
    #' Get the assignment seed for a particular master seed
    #'
    #' @param assignment The assignment id
    #' @param master_seed The master seed
    #'
    #' @return list containing the master seed and assignment seed
    assignment_seed = function(assignment, master_seed){

      settings <- private$assignments[[assignment]]
      seed <- ft3_seed_from_master(master_seed, settings[['seed_salt']])
      
      list(
        pars = c(
          assignment = assignment,
          seed = seed, 
          masterseed = master_seed
        )
      ) 
    },
    #' Check whether cache key exists for parameters
    #'
    #' @param assignment Assignment to compile
    #' @param id (character) id
    #' @param seed (character) seed
    #' @param solutions (logical) Whether to include solutions
    #' @param assignment_mode (logical) Use assignment mode
    #'
    #' @return List containing the cache status
    assignment_cachekey = function(assignment, id, seed, solutions, assignment_mode){

      settings <- private$assignments[[assignment]]
      seed0 <-  ifelse(
        assignment_mode,
        ft3_seed_from_master(private$master_secret, settings[['seed_salt']]),
        seed
      )
      cache_key <- ft3_get_cache_key(
        file = settings[['file_']], 
        id = id,
        seed = seed0, 
        solutions = solutions, 
        assignment_mode = assignment_mode
      )
      
      list(cached = !cachem::is.key_missing(private$cache$get(cache_key)))
    },
    #' Get list of files for an assignment
    #'
    #' @param assignment 
    #' @param assignment_mode 
    #'
    #' @return A JSON list of files
    assignment_files_json = function(assignment, assignment_mode){
      
      pars = c(assignment_mode = isTRUE(assignment_mode))
      
      settings <- private$assignments[[assignment]]
      ft3_check_assignment_restrictions(settings, pars, FALSE)
      
      settings$files |>
        list(file_list = _) |>
        jsonlite::toJSON(auto_unbox = TRUE) 
    },
    #' Compile assignment and return configuration data only
    #'
    #' @param assignment Assignment to compile
    #' @param file The id of the file to return
    #' @param id (character) id
    #' @param seed (character) seed
    #' @param solutions (logical) Whether to include solutions
    #' @param assignment_mode (logical) Use assignment mode
    #' @param use_cache (logical) Use the cached version, if it exists?
    #' @param cache_only (logical) If TRUE, don't return document
    #' @param ... Arguments to pass to ft3_compile_doc
    #'
    #' @return A list containing the file content and name
    assignment_file_content = function(assignment, file, id, seed, solutions, assignment_mode, use_cache = TRUE, ...){
      pars <- list(
        assignment      = assignment,
        id              = id,
        seed            = seed, 
        solutions       = solutions,
        assignment_mode = assignment_mode,
        cache           = FALSE
      )
      settings <- private$assignments[[assignment]]
      
      ft3_check_assignment_restrictions(settings, pars)
      file_content = self$assignment_content(assignment, id, seed, solutions, assignment_mode, use_cache, cache_only = FALSE, ...)$files[[file]]
      
      if(is.null(file_content))
        stop('Filename ', file ,' not found for assignment ', assignment)
      
      list(
        contents = file_content$content,
        name = file_content$name
      )
    },
    #' @description
    #' Create a new FlexTeaching3 object
    #' 
    #' @param scratch_dir Directory to use for creating/storing temporary files and cache. Defaults to a tempdir()
    #' @param master_secret Master seed used for generating assignment seeds
    #' @param cachedisk_settings an optional list of disk cache settings (see ?cachem::cache_disk)
    #' @param assignment_pkg Package containing flexTeaching3 assignment content
    #' @importFrom assertthat assert_that is.string
    #' @importFrom cachem cache_disk
    #' @return A new FlexTeaching3 object
    #' 
    initialize = function(scratch_dir = NULL, assignment_pkg, master_secret, cachedisk_settings = list()) {
      assertthat::assert_that(
        is.list(cachedisk_settings),
        msg = "cachedisk_settings must be a (potentially empty) list."
      )
      assertthat::assert_that(
        assertthat::is.string(master_secret),
        nchar(master_secret) > 4,
        msg = "master_secret must be a length 1 character vector containing a string with at least 5 characters."
      )
      if(is.null(scratch_dir)){
        scratch_dir <- tempfile(pattern = 'dir')
        message("Creating scratch directory: ", scratch_dir)
        dir.create(scratch_dir) || stop("Could not create scratch directory: ", scratch_dir, call. = FALSE)
      }
      if(!dir.exists(scratch_dir))
        stop("Scratch directory ", scratch_dir, " does not exist.", call. = FALSE)
      if(file.access(scratch_dir, mode = 2) != 0)
        stop('Cannot write to scratch directory "',scratch_dir, '".')
      if(is.null(cachedisk_settings$dir))
        cachedisk_settings$dir <- scratch_dir
      private$support_dir <- ft3_check_pkg_dir(assignment_pkg)
      private$cache <- do.call(cachem::cache_disk, cachedisk_settings)
      private$scratch_dir <- scratch_dir
      private$master_secret <- master_secret
      private$support_dir |>
        file.path('assignments') |>
        ft3_get_assignments() -> private$assignments
      
      # Headers
      headers_fn <- file.path(private$support_dir, 'headers.html') 
      headers_content <- ifelse(file.exists(headers_fn), ft3_read_file_text(headers_fn), '')
      
      footers_fn <- file.path(private$support_dir, 'footers.html') 
      footers_content <- ifelse(file.exists(footers_fn), ft3_read_file_text(footers_fn), '')
      
      list(
        headers = headers_content,
        footers = footers_content
      ) -> private$header_footer
    }
  ),
  
  private = list(
    cache = NULL,
    support_dir = NULL,
    master_secret = NULL,
    scratch_dir = NULL,
    assignments = NULL,
    header_footer = NULL,
    finalize = function() {
      private$cache$destroy()
    }
  )
)


