
#' @importFrom lubridate parse_date_time as_datetime
ft3_parse_date <- function(date){
  ifelse(
    is.na(date) || is.null(date),
    lubridate::as_datetime(-Inf),
    lubridate::parse_date_time(
      date,
      orders = ft3_options('lubridate_orders'),
      tz = ft3_options('timezone'))
  )
}
