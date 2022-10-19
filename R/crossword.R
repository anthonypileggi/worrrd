#' Create a crossword puzzle
#' @param words a vector of words (character/vector)
#' @param clues a vector a clues (character/vector)
#' @param r number of rows (numeric/scalar)
#' @param c number of columns (numeric/scalar)
#' @param method generate puzzle using 'optimal' or 'random' word order, where
#'               the optimal order will place words with the most overlap first
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # Example 1 ----
#' x <- crossword(words = c("apple", "pear", "banana"), clues = c("red fruit", "bartlett", "green until yellow"))
#' plot(x, solution = T)
#'
#' # Example 2 ---
#' dat <-
#' dplyr::tribble(
#'   ~word,   ~clue,
#'   "dog",   "Bark. Bark. Bark.",
#'   "cat",   "Purrr",
#'   "horse", "Neighhhhh",
#'   "frog",  "Ribbit Ribbit",
#'   "cow",   "Moooooooo",
#'   "fox",   "Nee Nee Nee (What does the ____ say?)",
#'   "sheep", "Bleat",
#'   "snake", "Hissss",
#'   "duck",  "Quack",
#'   "bird",  "Chirp"
#' )
#' ex1 <- crossword(words = dat$word, clues = dat$clue, r = 40, c = 40)
#' plot(ex1, solution = TRUE, clues = TRUE)
#'
#' @export
crossword <- function(words,
                      clues,
                      r = 50,
                      c = 50,
                      method = c("optimal", "random")) {

  # check inputs
  if (length(words) != length(clues))
    stop("Invalid input: `words` and `clues` must have the same length.")
  method <- match.arg(method)

  # prepare word list
  #   - uppercase everything; ignore spaces
  words <- prepare_words(words)
  n <- length(words)

  # -- do not allow duplicates/repeats in word list
  if (any(duplicated(words)))
    stop("Must provide a set of words without duplicates.")

  # -- remove words that won't fit
  id <- nchar(words) <= max(c(r, c))
  words <- words[id]
  clues <- clues[id]
  if (length(words) == 0) {
    message("No words can be placed.  Try a larger grid-size, or shorter words.")
    return(NULL)
  }

  # save clues for use later
  df <- tibble::tibble(word = words, clue = clues)

  # TODO: automatically determine r/c based on content of `words`

  # create empty matrix to store crossword
  x <- matrix(NA, nrow = r, ncol = c)

  # iterate: add words to the board
  #   - force words to be intersecting after placing the first word
  #     TODO: place first word in board center
  #   - each time a word is added, reshuffle the remaining words
  #   - if no word can be placed after a reshuffle, give up (i.e., break loop)
  while (length(words) > 0) {
    word_added <- FALSE
    if (method == "optimal") {
      wts <- word_overlap(words)
      ids <- rank(-wts, ties.method = "first")
      words <- words[ids]
    } else {
      words <- sample(words)
    }

    for (word in words) {
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

  # # TODO: trim matrix padding
  # p <- attr(x, "positions")
  # row_range <- range(p$i)
  # minr <- min(p$i) - 1
  # maxr <- max(p$i) + 1
  # minc <- min(p$j) - 1
  # maxc <- max(p$j) + 1
  # x[minr:maxr, minc:maxc]  # trim matrix
  #
  # p %>%
  #   dplyr::mutate(
  #     i = i - minr + 1,
  #     j = j - minc + 1
  #   )
  # #  -- how to adjust 'id'?
  # #  -- how to deal with words on edges?

  # add clues
  # TODO: account for duplicate words (which is allowed)
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
#' @param x a crossword object (see \code{\link{crossword}})
#' @param ... additional printing args
#' @export
print.crossword <- function(x, ...) {
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
#' @param title puzzle title (character/scalar)
#' @param legend_size letter size of word list; set to NULL to auto-size (numeric/scalar)
#' @param ... additional printing args
#' @export
plot.crossword <- function(x,
                           solution = FALSE,
                           clues = FALSE,
                           title = "Crossword Puzzle",
                           legend_size = 4,
                           ...) {

  g1 <- ggplot2::ggplot(attr(x, "positions")) +
    ggplot2::geom_tile(aes(x = .data$j, y = .data$i, group = .data$word), color = "black", fill = "white", alpha = 1) +
    ggplot2::geom_text(aes(x = .data$j, y = .data$i, label = .data$n), size = 2, nudge_y = .35, nudge_x = -.35, color = "black", data = attr(x, "clues")) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void() +
    ggplot2::theme(
      aspect.ratio = ncol(x) / nrow(x),
      panel.background = element_rect(fill = "black"),
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
      )

  if (solution)
    g1 <- g1 + ggplot2::geom_text(aes(x = .data$j, y = .data$i, label = .data$letters), color = "red")

  if (clues) {
    g2 <-
      purrr::map(
        c("Across", "Down"),
        function(d) {
          xt <- attr(x, "clues") %>%
          dplyr::mutate(
            clue = paste0(.data[["n"]], ". ", .data[["clue"]])
          ) %>%
          dplyr::filter(
            dir == tolower(d)
          )
          nn <- max(xt$n)
          ggplot2::ggplot(xt) +
            # ggtext::geom_richtext(
            #   aes(x = 1, y = .data$n, label = .data$clue),
            #   fill = NA,
            #   size = legend_size,
            #   label.color = NA,  # remove background and outline
            #   label.padding = grid::unit(rep(0, 4), "pt")  # remove padding
            # ) +
            ggplot2::geom_text(aes(x = 1, y = .data$n, label = .data$clue), hjust = 0, size = legend_size) +
            ggplot2::annotate("text", x = 1, y = 0, label = paste0("underline(bold(", toupper(d), "))"), parse = TRUE) +
            ggplot2::theme_void() +
            ggplot2::scale_y_reverse() +
            ggplot2::xlim(.8, 1.5)
        }
      )
    g3 <- cowplot::plot_grid(g2[[1]], g2[[2]], nrow = 2, rel_widths = c(1, 1))
    g1 <- cowplot::plot_grid(g1, g3, nrow = 1, rel_widths = c(3/4, 1/4))
    #g1 <- gridExtra::grid.arrange(g1, gc[[1]], gc[[2]], layout_matrix = rbind(c(1, 2), c(1, 3)))
  }

  g1
}

