
#' @importFrom httpproblems http_problem
#' @importFrom jsonlite toJSON
#' @importFrom RestRserve HTTPError raise
ft3_http_error <- function(status_code, detail, error = NULL){
  httpproblems::http_problem(status = status_code, detail = detail) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    RestRserve::HTTPError$error(
      status_code = status_code, 
      body = _,
      headers = list(
        Server = getOption("RestRserve.headers.server"),
        "Access-Control-Allow-Origin" = "*"
        )
      ) -> err
    err$set_content_type('application/json')
    RestRserve::raise(err)
}


#' @importFrom crayon strip_style
ft3_http_error_wrapper = function(FUN, .custom_message = NULL, .status_code = 500, ...){
  fun_name <- substitute(FUN)
  if(is.null(.custom_message)){
    if (length(fun_name) == 1) {
      .custom_message <- paste("There was an error in", fun_name)
    } else {
      .custom_message <- paste("There was an error in", fun_name[3], "from package", fun_name[2])
    }  
  }
  function(...){
    tryCatch({
      FUN(...)
    },
    error = function(e){
      e |> 
        conditionMessage() |> 
        paste0('; ', conditionCall(e)) |>
        crayon::strip_style() |>
        gsub(pattern = '\n', replacement = ' ', x = _) |> 
        paste('Error:', x = _) |>
        ifelse(ft3_options('errors_to_client'), yes = _, '') |>
        paste(.custom_message, x = _) |>
        ft3_http_error(status_code = .status_code, detail = _, error = e) 
    })
  }
} 

ft3_get_assignments_select2_http <- ft3_http_error_wrapper(
  ft3_get_assignments_select2,
  .custom_message = 'The assignment list could not be generated.'
)

ft3_compile_doc_http <- ft3_http_error_wrapper(
  ft3_compile_doc,
  .custom_message = 'The assignment could not be generated.'
)
