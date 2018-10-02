#' Create a word search
#' @param words a vector of hidden words (character/vector)
#' @param r number of rows
#' @param c number of columns
#' @export
word_search <- function(words = c("finding", "needles", "inside", "haystacks"),
                        r = 10,
                        c = 10) {

  # create empty matrix
  x <- matrix(NA, nrow = r, ncol = c)

  # check conditions
  words <- tolower(words)
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
  x[is.na(x)] <- sample(letters, sum(is.na(x)), replace = TRUE)

  list(
    search = x,
    words = words,
    solution = solution
  )
}


#' Compute maximum word size, based on a word_search matrix
#' @param x word_search matrix
max_word_size <- function(x) {

  # setup activation matrix
  r <- nrow(x)
  c <- ncol(x)
  A <- matrix(1, nrow = r, ncol = c)
  A[!is.na(x)] <- 0

  # compute max word sizes
  out <- list(across = A, down = A)
  for (i in 1:r) {
    for (j in 1:c) {
      out$across[i, j] <- sum(cumprod(A[i, j:c]))
      out$down[i, j] <- sum(cumprod(A[i:r, j]))
    }
  }
  out
}


#' Add a word to a wordsearch matrix
#' @param x wordsearch matrix
#' @param word the word to add (character/scalar)
add_word <- function(x, word = "finding") {

  # update matrix with possible insertion points
  r <- nrow(x)
  c <- ncol(x)
  M <- max_word_size(x)
  n <- nchar(word)

  # choose randomly (uniform) from all possible positions
  v <- purrr::map_int(M, ~sum(.x >= n))
  if (sum(v) == 0)
    return(x)
  d <- sample(names(M), 1, prob = v / sum(v))
  pos <- sample(which(M[[d]] >= n), 1)

  # TODO: catch if we run out of insertion points

  # add word at the chosen position
  id <- switch(d,
    down = pos:(pos + n - 1),
    across = seq(pos, pos + (n - 1) * r, by = r)
      )
  sword <- stringr::str_split(word, "")[[1]]
  x[id] <- sword

  # TODO: store position (for drawing solution lines)
  positions <- tibble::tibble(
    word = word,
    letters = sword,
    id = id
  )
  if (is.null(attr(x, "positions"))) {
    attr(x, "positions") <- positions
  } else {
    attr(x, "positions") <- dplyr::bind_rows(attr(x, "positions"), positions)
  }

  x
}


#' Draw a wordsearch
#' @param x wordsearch object
#' @param solution show solution? (logical/scalar)
#' @param letter_size size of letters (numeric/scalar)
#' @import ggplot2
#' @export
plot_word_search <- function(x, solution = FALSE, letter_size = 8) {
  require(ggplot2)
  ids <- expand.grid(i = 1:nrow(x$search), j = 1:ncol(x$search))
  xt <-
    purrr::map2_df(
      ids$i,
      ids$j,
      ~tibble::tibble(i = .x, j = .y, value = x$search[.x, .y], word = !is.na(x$solution[.x, .y]))
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

  if (solution) {
    xs <-
      dplyr::mutate(
        attr(x$search, "positions"),
        i = (id - 1) %% nrow(x$search) + 1,
        j = floor((id - 1) / nrow(x$search)) + 1
      )
    g1 <- g1 + geom_line(aes(x = i, y = j, group = word), color = "red", data = xs)
  }

  g1
}