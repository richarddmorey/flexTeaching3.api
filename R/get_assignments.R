
ft3_get_assignments = function(assignments_dir =  ft3_options('assignments_dir')){
  potential_assignments = list.dirs(assignments_dir, recursive = FALSE, full.names = FALSE) |> 
    file.path(assignments_dir, x = _) |>
    normalizePath()
  
  potential_assignments |>
    lapply(\(d){
      d |>
        dir(
          pattern = '^index.(rmd|html)$',
          ignore.case = TRUE,
          include.dirs = FALSE,
          full.names = TRUE
          ) -> index_files
      if(length(index_files) == 0) return(NULL)
      index_file <- sort(index_files, decreasing = TRUE)[1]
      ft3_assignment_settings(index_file)
    }) |>
    Filter(Negate(is.null), x = _) -> assignments
  
  nm = sapply(assignments, \(e) e$ref )
  
  if(any(duplicated(nm)))
    stop(
      'All assignment ref elements must be unique. Offenders: ', 
      paste(nm[duplicated]), collapse = ', '
      )
    
  names(assignments) <- nm
  
  return(assignments)

}


#' @importFrom purrr map_df map_lgl map2_lgl map_dbl
#' @importFrom lubridate now as_datetime
#' @importFrom dplyr mutate across filter arrange bind_rows if_else
ft3_get_assignments_simple <- function(assignments_dir = ft3_options('assignments_dir'), current_assignment = NULL){
  a <- ft3_get_assignments(assignments_dir)
  if(length(a)<=1L)
    stop('No assignments found in ', normalizePath(assignments_dir), '.')
  
  # To prevent check notes
  hide_before <- restrict_before <- value <- category <- sortkey <- NULL
  
  a |>
    purrr::map_df(\(e){
      dplyr::bind_rows(
        label = e$menu,
        value = e$ref,
        hide_before = e$hide_before,
        restrict_before = e$restrict_before,
        category = e$category,
        sortkey = e$sortkey,
        iframe  = e$iframe
        )
    }) |>
    dplyr::mutate(
      dplyr::across(c(hide_before, restrict_before),
                    ~ purrr::map_dbl(., ft3_parse_date) |> lubridate::as_datetime()
                ),
      disabled = purrr::map_lgl(restrict_before, ~ !is.na(.) && . > lubridate::now())
    ) |>
    dplyr::filter(
      # Remove those that are invisible; but note that if the current selection is accessible
      # yet invisible, we keep it IN the menu.
      purrr::map2_lgl(.x = hide_before, .y = value,
                      ~ is.na(.x) || .x <= lubridate::now() || (!is.null(current_assignment) && current_assignment == .y)
      )
    ) |>
    dplyr::arrange(
      category, sortkey, value
    )
}

#' @importFrom purrr map
#' @importFrom dplyr select rename
#' @importFrom jsonlite toJSON
ft3_get_assignments_select2 <- function(assignments_dir = ft3_options('assignments_dir'), ...){
  
  assignments_df <- ft3_get_assignments_simple(assignments_dir, ...) 
  assignments_split <- split(assignments_df, assignments_df$category)
  
  # To prevent check notes
  label <- value <- disabled <- NULL
  
  names(assignments_split) |>
    purrr::map(\(n){
      list(
        text = n,
        children = assignments_split[[n]] |>
          dplyr::select(label, value, disabled) |>
          dplyr::rename(
            id = value,
            text = label
          )
      )
    }) |>
    jsonlite::toJSON(auto_unbox = TRUE)
}

#' @importFrom tools md5sum
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom utils URLencode
ft3_assignment_default_settings <- function(
    file_ = stop('The file_ argument must be set in assignment_default_settings().'),
    title = dirname(file_) |> basename(),
    menu = title,
    ref = dirname(file_) |> basename() |> utils::URLencode(),
    seed_salt = tools::md5sum(file_),
    data_salt = tools::md5sum(file_),
    hide_before = NA_character_,
    restrict_before = NA_character_,
    fragment = FALSE,
    use_callr = TRUE,
    files = NULL,
    category = ft3_options('default_category_name'),
    sortkey = title,
    on_load = NA_character_,
    exam = FALSE,
    cache = TRUE,
    all_same = TRUE,
    ...
){
  
  assertthat::assert_that(assertthat::is.string(file_))
  assertthat::assert_that(file.exists(file_))
  
  assertthat::assert_that(assertthat::is.string(title))
  assertthat::assert_that(assertthat::is.string(ref))
  assertthat::assert_that(assertthat::is.string(menu))  
  assertthat::assert_that(assertthat::is.string(seed_salt))
  assertthat::assert_that(assertthat::is.string(data_salt))
  assertthat::assert_that(assertthat::is.string(hide_before))
  assertthat::assert_that(assertthat::is.string(restrict_before))
  assertthat::assert_that(assertthat::is.string(category))
  assertthat::assert_that(assertthat::is.string(sortkey))
  assertthat::assert_that(assertthat::is.string(on_load))
  
  assertthat::assert_that(assertthat::is.flag(use_callr))
  assertthat::assert_that(assertthat::is.flag(fragment))
  assertthat::assert_that(assertthat::is.flag(exam))
  assertthat::assert_that(assertthat::is.flag(cache))
  assertthat::assert_that(assertthat::is.flag(all_same))
  
  c(as.list(environment()), list(...))
}


