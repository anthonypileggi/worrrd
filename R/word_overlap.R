#' Compute overlap score for a vector of words
#' @param words vector of words (character/vector)
word_overlap <- function(words) {
  purrr::map_int(
    seq_along(words),
    function(i) {
      sum(
        purrr::map_int(
          stringr::str_split(words[i], "")[[1]],
          function(letter) {
            sum(stringr::str_detect(words[-i], letter))
          }
        )
      )
    }
  )
}