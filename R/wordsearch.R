#' Create a wordsearch puzzle
#' @param words a vector of hidden words (character/vector)
#' @param r number of rows
#' @param c number of columns
#' @param image path to an image that the resulting grid should look like.NULL for no shape
#' @export
wordsearch <- function(words = c("finding", "needles", "inside", "haystacks"),
                       r = 10,
                       c = 10,
                       image = NULL) {

  # create empty matrix
  x <- matrix(NA, nrow = r, ncol = c)

  # prepare the word list
  words <- prepare_words(words)

  # check conditions
  words <- words[nchar(words) <= max(c(r, c))]    # remove words that won't fit
  if (length(words) == 0) {
    message("No words can be placed.  Try a larger grid-size, or shorter words.")
    return(NULL)
  }

  # generate shape file (if provided)
  if (!is.null(image))
    image <- image_matrix(image, r, c)

  # iteratively add words to the board
  for (i in seq_along(words)) {
    x_new <- add_word(x, words[i], shape_matrix = image)
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
  ids <- is.na(x)
  if (!is.null(image))
    ids <- ids & image
  x[ids] <- sample(LETTERS, sum(ids), replace = TRUE)

  out <-
    list(
      search = x,
      words = words,
      solution = solution,
      shape_matrix = image
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

#' Print a wordsearch puzzle
#' @export
print.wordsearch <- function(x) {
  cat(paste("Wordsearch\n"))
  cat(paste("Rows:", nrow(x$search), "\n"))
  cat(paste("Columns:", ncol(x$search), "\n"))
  cat(paste("Hidden Words:", length(x$words), "\n"))
  cat(paste("Custom Shape:", ifelse(is.null(x$shape_matrix), "No", "Yes"), "\n"))
  invisible(x)
}

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
