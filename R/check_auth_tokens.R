
ft3_check_auth_tokens <- function(token){
  allowed_tokens = ft3_options('auth_tokens')
  if(length(allowed_tokens) == 0) return(TRUE)
  res = FALSE
  try({
    res = token %in% allowed_tokens
  }, silent = TRUE)
  return(res)
}
