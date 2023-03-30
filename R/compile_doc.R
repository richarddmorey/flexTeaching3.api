
#' @importFrom lubridate now
#' @importFrom cachem is.key_missing 
#' @importFrom tools file_ext
ft3_compile_doc <- function(doc_file, pars, ft3_cache, scratch_dir = ft3_options('cache_location'), ...){
  
  doc_file <- normalizePath(doc_file)
  settings <- ft3_assignment_settings(doc_file)
  pars$fingerprint = ft3_get_fingerprint(pars$seed, pars$id, settings, pars$assignment_mode)
  seed0 <-  ifelse(
    pars$assignment_mode,
    ft3_seed_from_master(ft3_options('master_secret'), settings[['seed_salt']]),
    pars$seed
  )
  cache_key = ft3_get_cache_key(
    file = doc_file,
    id = pars$id,
    seed = seed0,
    solutions = pars$solutions,
    assignment_mode = pars$assignment_mode
  )
  local_seed <- ft3_assignment_seed(
    id = pars$id, 
    seed = seed0,
    data_salt = settings[['data_salt']]
  )
  if(!is.null(ft3_cache)){
    out <- ft3_cache$get(cache_key)
  }
  if(is.null(ft3_cache) || cachem::is.key_missing(out))
  {
    ext <- tools::file_ext(doc_file) |> tolower()
    temp_space_path <- ft3_prepare_temp_space(doc_file, scratch_dir)
    on.exit({
      unlink(temp_space_path, recursive = TRUE, force = TRUE)
    })
    if(ext %in% c('rmd','rmarkdown')){
        outfile <- ft3_compile_doc_rmd(rmd_file = doc_file, pars = pars, local_seed = local_seed, temp_space_path = temp_space_path, ...)
    }else{
      stop('Unknown document type ', ext)
    }
    # Check file content type
    out_ext <- tools::file_ext(outfile) |> tolower()
    if(out_ext == 'html'){
      content <- ft3_read_file_text(path = outfile)
      if(!ft3_is_fragment(outfile))
        content <- ft3_insert_iframe_resizer(content)
    }else{
      content <- ft3_read_file_raw(path = outfile)
    }
    # output content and settings
    out <- list(
      files = ft3_read_assignment_output(outfile, settings, pars),
      content = content,
      file_ext = out_ext,
      pars = pars,
      iframe = !ft3_is_fragment(outfile),
      fingerprint = pars$fingerprint,
      js = settings[['on_load']]
    )
    if(!is.null(ft3_cache) && (is.null(settings$cache) || settings$cache))
      ft3_cache$set(cache_key, out)
  }
  return(out)
}

#' @importFrom tools file_ext
ft3_assignment_settings <- function(doc_file){
  ext <- tools::file_ext(doc_file) |> tolower()
  doc_file <- normalizePath(doc_file)
  if(ext %in% c('rmd','rmarkdown')){
    settings = ft3_assignment_settings_rmd(doc_file)
  }else{
    stop('Unknown document type ', ext)
  }
  settings$file_ = doc_file
  do.call(ft3_assignment_default_settings, settings)
}


#' @importFrom purrr map set_names
#' @importFrom utils URLdecode
ft3_read_assignment_output <- function(outfile, settings, pars){
  outfile_dir <- dirname(outfile)
  names(settings$files) |>
    purrr::map(\(fn){
      fp <- file.path(outfile_dir, fn)
      if(!file.exists(fp)){
        stop('File ', fn, ' was not created for assignment ', utils::URLdecode(settings$ref))
      }
      to.read <- file(fp, "rb")
      content <- readBin(to.read, "raw", file.size(fp))
      close(to.read)
      fn_function <- settings$files[[fn]]$filename
      if(is.null(fn_function)){
        new_fn <- fn 
      }else{
        new_fn = try(
          do.call(eval(parse(text=fn_function)), list(
            filename   = fn, 
            assignment = pars$assignment,
            id         = pars$id, 
            seed       = pars$seed, 
            assignment_mode = pars$assignment_mode, 
            solutions = pars$solution,
            file_content = content
          )
          )
        )
        if(inherits(new_fn, 'try-error'))
          stop('Could not determine filename using ',
               fn_function, 
               ' for assignment ', utils::URLdecode(settings$ref))
      }
      # Return list
      list(
        name = new_fn,
        content = content,
        label = settings$files[[fn]]$label,  
        icon = ifelse(
          is.null(settings$files[[fn]]$icon),  
          ft3_options('default_file_icon'),
          settings$files[[fn]]$icon) 
      )
    }) -> out
  purrr::set_names(out, names(settings$files))
}
