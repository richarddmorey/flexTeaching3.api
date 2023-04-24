
#' @importFrom digest digest
#' @importFrom utils type.convert
ft3_alp2int <- function(x) {
  # https://stackoverflow.com/a/10913336/1129889
  hexval <- paste0("0x", digest::digest(x,"crc32"))
  intval <- utils::type.convert(hexval, as.is = FALSE) %% .Machine$integer.max
  return(as.integer(intval))
}
