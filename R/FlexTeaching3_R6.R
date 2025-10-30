
#' R6 Class Representing a set of flexTeaching3 assignments
#'
#' @description
#'
#' @details
#' 
#' @export
#' 
#' @examples
#' # example code
#' ft3 <- FlexTeaching3$new()
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
    #'
    #' @return A list containing the assignment data

    assignment = function(assignment, id, seed, solutions, assignment_mode, use_cache = TRUE, cache_only = FALSE, ...){
      if(is.null(private$assignments[[assignment]]))
        stop('Assignment "', assignment,'" does not appear in the assignments list.')
      doc_file <- private$assignments[[assignment]]$file_
      if(!file.exists(doc_file))
        stop('Assignment document "',doc_file,'" does not exist.')
      if(cache_only && !use_cache)
        stop('use_cache must be true if cache_only is true.')
      pars <- list(
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
    },
    #' @description
    #' Clean up when object is destroyed
    #'
    finalize = function() {
      private$cache$destroy()
    }
  ),
  
  private = list(
    cache = NULL,
    support_dir = NULL,
    master_secret = NULL,
    scratch_dir = NULL,
    assignments = NULL
  )
)


