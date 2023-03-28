
#' @importFrom digest digest
ft3_get_fingerprint = function(seed, id, settings, assignment_mode){
  seed0 <- ifelse(
    assignment_mode,
    ft3_seed_from_master(ft3_options('master_secret'), settings[['seed_salt']]),
    seed
  )
  c(seed0, id, settings[['ref']]) |>
    digest::digest()
}
