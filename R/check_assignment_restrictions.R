

#' @importFrom lubridate now
ft3_check_assignment_restrictions <- function(settings, pars, check_exam = TRUE){
  
  # Error if assignment is an exam and you've asked for practice
  if(check_exam && !is.null(settings$exam) && settings$exam && !pars$assignment_mode)
    ft3_http_error(403, 'This assignment is unavailable in practice mode.')
  
  # Error if unavailable due to time restriction
  restriction_date <- ft3_parse_date(settings$restrict_before)
  
  if(is.null(restriction_date) || is.na(restriction_date)){
    settings$file_ |> 
      dirname() |> 
      basename() |>
      paste0('There was a problem parsing the restriction date for assignment ', x=_) |>
      ft3_http_error(500, detail = _)
  }
  if(restriction_date > lubridate::now())
    ft3_http_error(403, 'This assignment is not yet available.')
}