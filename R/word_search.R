#' Create a wordsearch puzzle
#' @param words a vector of hidden words (character/vector)
#' @param r number of rows
#' @param c number of columns
#' @export
wordsearch <- function(words = c("finding", "needles", "inside", "haystacks"),
                        r = 10,
                        c = 10) {

  # create empty matrix
  x <- matrix(NA, nrow = r, ncol = c)

  # check conditions
  words <- toupper(words)
  words <- stringr::str_replace_all(words, " ", "")
  words <- words[nchar(words) <= max(c(r, c))]    # remove words that won't fit
  if (length(words) == 0) {
    message("No words can be placed.  Try a larger grid-size, or shorter words.")
    return(NULL)
  }

  # iteratively add words to the board
  for (i in seq_along(words)) {
    x_new <- add_word(x, words[i])
    if (identical(x, x_new))
      break
    x <- x_new
  }

  # update word list (based on what was placed)
  new_words <- unique(attr(x, "positions")$word)
  message(paste0("Found positions for ", length(new_words), "/", length(words), " words."))
  words <- new_words

  # save solution
  solution <- x

  # fill remaining matrix with random letters
  x[is.na(x)] <- sample(LETTERS, sum(is.na(x)), replace = TRUE)

  out <-
    list(
      search = x,
      words = words,
      solution = solution
    )
  as_wordsearch(out)
}


# Constructors =============================================================

#' Assign an object to the `wordsearch` class
#' @param x an object containing wordsearch data
#' @export
as_wordsearch <- function(x) {
  if (!is_wordsearch(x))
    class(x) <- append("wordsearch", class(x))
  x
}

#' Check if an object is of the `wordsearch` class
#' @param x an R object to check
#' @export
is_wordsearch <- function(x) {
  inherits(x, "wordsearch")
}


# Methods ===================================================================

#' Draw a wordsearch puzzle
#' @param x wordsearch object
#' @param solution show solution? (logical/scalar)
#' @param letter_size size of letters (numeric/scalar)
#' @import ggplot2
#' @export
plot.wordsearch <- function(x, solution = FALSE, letter_size = 8) {
  require(ggplot2)
  ids <- expand.grid(i = 1:nrow(x$search), j = 1:ncol(x$search))
  xt <-
    purrr::map2_df(
      ids$i,
      ids$j,
      ~tibble::tibble(
        i = .x,
        j = .y,
        value = x$search[.x, .y],
        word = !is.na(x$solution[.x, .y])
        )
    )

  g1 <- ggplot(xt) +
    geom_text(aes(x = i, y = j, label = value), size = letter_size) +
    annotate("rect",
      xmin = 0.5, ymin = 0.5, xmax = max(xt$i) + 0.5, ymax = max(xt$j) + 0.5,
      alpha = 0, color = "black"
    ) +
    scale_y_reverse() +
    theme_void() +
    theme(aspect.ratio = ncol(x$search) / nrow(x$search))

  if (solution)
    g1 <- g1 + geom_line(aes(x = i, y = j, group = word), color = "red", data = attr(x$search, "positions"))

  g1
}
