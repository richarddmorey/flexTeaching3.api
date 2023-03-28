
#' @importFrom digest sha1
ft3_assignment_seed <- function(id, seed, data_salt){

  c(id, seed, data_salt) |>
    trimws() |>
    digest::sha1() |>
    ft3_alp2int()
}

ft3_seed_from_master <- function(seed, seed_salt){
  ft3_animal_seed(seed, seed_salt)
}
