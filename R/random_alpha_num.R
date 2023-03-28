
ft3_random_alpha_num <- function(n = 1, n_char = 10) {
  replicate(n, {
    c(letters, 0:9) |>
      sample(size = n_char, replace = TRUE) |>
      paste0(collapse = '')
  })
}
