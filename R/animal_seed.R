#' Create animal string
#'
#' @param seed 
#'
#' @importFrom digest sha1
#' @importFrom R.utils withSeed
#' @return
#'
ft3_animal_seed <- function(seed, salt){
  c(seed, salt) |>
    trimws() |>
    digest::sha1() |>
    ft3_alp2int() -> int_hash

  animals = R.utils::withSeed(
    {
      with(name_parts, 
      {
        tolower(paste(sample(adjectives, 1), sample(animals, 1), sep='_'))
      })      
    }, int_hash)
  
  alpha_num = R.utils::withSeed({ft3_random_alpha_num(1)},
                                int_hash)
  return(paste(animals, alpha_num,  sep = "_"))
}


