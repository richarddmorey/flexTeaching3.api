

#' @importFrom lubridate now
ft3_check_assignment_restrictions <- function(settings, pars, check_exam = TRUE){
  
  # Error if assignment is an exam and you've asked for practice
  if(check_exam && !is.null(settings$exam) && settings$exam && !pars$assignment_mode)
    ft3_http_error(403, 'This assignment is unavailable in practice mode.')
  
  # Error if unavailable due to time restriction
  if(ft3_parse_date(settings$restrict_before) > lubridate::now())
    ft3_http_error(403, 'This assignment is not yet available.')
}