
#' Create a crossword puzzle
#' @param words a vector of hidden words (character/vector)
#' @param r number of rows
#' @param c number of columns
#' @export
crossword <- function(words = c("finding", "needles", "inside", "haystacks"),
                      r = 10,
                      c = 10) {

  # uppercase everything; ignore spaces
  words <- toupper(words)
  words <- stringr::str_replace_all(words, " ", "")

  # create empty matrix to store crossword
  x <- matrix(NA, nrow = r, ncol = c)

  # iteratively add words to the board
  i <- 0
  w <- NULL
  while (length(words) > 0 & i < 100) {
    i <- i + 1
    x <- gamer:::add_word(x, sample(words, 1), must_intersect = !is.null(w))
    w <- unique(attr(x, "positions")$word)
    words <- setdiff(words, w)
  }

  # add clues
  attr(x, "clues") <- attr(x, "positions") %>%
    dplyr::group_by(word) %>%
    dplyr::slice(1) %>%
    group_by(dir) %>%
    dplyr::mutate(
      n = dplyr::row_number()
    )

  x
}

#' Plot a crossword puzzle
#' @param x a crossword object
#' @export
plot_crossword <- function(x, solution = FALSE) {
  require(ggplot2)
  g1 <- ggplot(attr(x, "positions")) +
    geom_tile(aes(x = i, y = j, group = word), color = "black", fill = "lightgray", alpha = 1) +
    geom_text(aes(x = i, y = j, label = n), size = 3, nudge_y = .35, nudge_x = -.35, color = "red", data = attr(x, "clues")) +
    scale_y_reverse() +
    theme_void() +
    theme(aspect.ratio = ncol(x) / nrow(x))
  if (solution)
    g1 <- g1 + geom_text(aes(x = i, y = j, label = letters))
  g1
}

