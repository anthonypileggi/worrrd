
#' Prepare a word(s)
#' @param x word list (character/vector)
prepare_words <- function(x) {
  stringr::str_replace_all(toupper(x), " |-|'|\\.", "")
}