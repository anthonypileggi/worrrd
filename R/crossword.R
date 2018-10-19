#' Create a crossword puzzle
#' @param words a vector of words (character/vector)
#' @param clues a vector a clues (character/vector)
#' @param r number of rows (numeric/scalar)
#' @param c number of columns (numeric/scalar)
#' @importFrom magrittr "%>%"
#' @export
crossword <- function(words,
                      clues,
                      r = 50,
                      c = 50) {

  # check inputs
  if (length(words) != length(clues))
    stop("Invalid input: `words` and `clues` must have the same length.")

  # prepare word list
  #   - uppercase everything; ignore spaces
  words <- prepare_words(words)
  n <- length(words)

  # save clues for use later
  df <- tibble::tibble(word = words, clue = clues)

  # TODO: automatically determine r/c based on content of `words`

  # create empty matrix to store crossword
  x <- matrix(NA, nrow = r, ncol = c)

  # iteratively add words to the board
  #   - force words to be intersecting after placing the first word
  #     TODO: place first word in board center
  #   - each time a word is added, reshuffle the remaining words
  #   - if no word can be placed, break loop
  while (length(words) > 0) {
    word_added <- FALSE
    for (word in sample(words)) {
      x <- add_word(x, word, must_intersect = n > length(words))
      if (word %in% unique(attr(x, "positions")$word)) {
        words <- setdiff(words, word)
        word_added <- TRUE
        break
      }
    }
    if (!word_added)
      break
  }

  # status report
  if (length(words) > 0)
    message(paste0("Could not place the following words:\n\n", paste0(words, collapse = "\n")))
  message(paste0("Found positions for ", n - length(words), "/", n, " words."))

  # TODO: trim matrix padding

  # add clues
  attr(x, "clues") <- attr(x, "positions") %>%
    dplyr::group_by(word) %>%
    dplyr::slice(1) %>%
    dplyr::group_by(dir) %>%
    dplyr::mutate(
      n = dplyr::row_number()
    ) %>%
    dplyr::left_join(df, by = "word")

  as_crossword(x)
}


# Constructors =============================================================

#' Assign an object to the `crossword` class
#' @param x an object containing crossword data
#' @export
as_crossword <- function(x) {
  if (!is_crossword(x))
    class(x) <- append("crossword", class(x))
  x
}

#' Check if an object is of the `crossword` class
#' @param x an R object to check
#' @export
is_crossword <- function(x) {
  inherits(x, "crossword")
}


# Methods ==================================================================

#' Print a crossword puzzle
#' @export
print.crossword <- function(x) {
  clues <- attr(x, "clues")
  cat(paste("Crossword Puzzle\n"))
  cat(paste("Contains", nrow(clues), "clues.\n"))
  cat(paste("There are", sum(clues$dir == "across"), "across and", sum(clues$dir == "down"), "down.\n"))
  invisible(x)
}

#' Plot a crossword puzzle
#' @param x a crossword object (see \code{\link{crossword}})
#' @param solution show solution? (logical/scalar)
#' @param clues show clues? (logical/scalar)
#' @export
plot.crossword <- function(x, solution = FALSE, clues = FALSE, title = "Crossword Puzzle") {
  require(ggplot2)
  g1 <- ggplot(attr(x, "positions")) +
    geom_tile(aes(x = i, y = j, group = word), color = "black", fill = "white", alpha = 1) +
    geom_text(aes(x = i, y = j, label = n), size = 2, nudge_y = .35, nudge_x = -.35, color = "black", data = attr(x, "clues")) +
    ggtitle(title) +
    scale_y_reverse() +
    theme_void() +
    theme(
      aspect.ratio = ncol(x) / nrow(x),
      panel.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
      )

  if (solution)
    g1 <- g1 + geom_text(aes(x = i, y = j, label = letters), color = "red")

  if (clues) {
    gc <-
      purrr::map(
        c("Across", "Down"),
        ~attr(x, "clues") %>%
          dplyr::mutate(
            clue = paste0(n, ". ", clue)
          ) %>%
          dplyr::filter(dir == "across") %>%
          ggplot(aes(x = 1, y = n)) +
          geom_text(aes(label = clue), hjust = 0) +
          annotate("text", x = 1, y = 0, label = paste0("underline(bold(", .x, "))"), parse = TRUE) +
          theme_void() +
          scale_y_reverse() +
          xlim(1, 1.5)
      )
    g1 <- gridExtra::grid.arrange(g1, gc[[1]], gc[[2]], layout_matrix = rbind(c(1, 2), c(1, 3)))
  }

  g1
}

