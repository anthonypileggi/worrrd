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

#' Get possible intersection points based on the current board and a provided word
#' @param x wordsearch matrix
#' @param word the word to add (character/scalar)
word_intersections <- function(x, word = "needles") {
  r <- nrow(x)
  c <- ncol(x)
  A <- matrix(0, nrow = r, ncol = c)
  out <- list(across = A, down = A)
  sword <- stringr::str_split(toupper(word), "")[[1]]
  for (i in 1:r) {
    for (j in 1:c) {
      if (!is.na(x[i, j])) {
        ids <- x[i, j] == sword
        if (any(ids)) {
          for (id in which(ids)) {

            # down
            xseq <- (i - id + 1):(i + length(ids) - id)
            if (all(xseq > 0 & xseq <= r)) {
              cids <- which(is.na(x[xseq, j]))                                    # find non-crossing points
              if (j > 1 && all(is.na(x[xseq[cids], j - 1])))                      # check left (minus crossing points)
                if (j < c && all(is.na(x[xseq[cids], j + 1])))                    # check right (minus crossing points)
                  if (xseq[1] > 1 && is.na(x[xseq[1] - 1, j]))                    # check above
                    if (tail(xseq, 1) < r && is.na(x[tail(xseq, 1) + 1, j]))      # check below
                      if (all(is.na(x[xseq, j]) | x[xseq, j] == sword))
                        out$down[xseq[1], j] <- 1
            }


            # across
            yseq <-  (j - id + 1):(j + length(ids) - id)
            if (all(yseq > 0 & yseq <= c)) {
              cids <- which(is.na(x[i, yseq]))                                    # find non-crossing points
              if (i > 1 && all(is.na(x[i - 1, yseq[cids] ])))                     # check above (minus crossing points)
                if (i < r && all(is.na(x[i + 1, yseq[cids] ])))                   # check below (minus crossing points)
                  if (yseq[1] > 1 && is.na(x[i, yseq[1] - 1]))                    # check left (before)
                    if (tail(yseq, 1) < c && is.na(x[i, tail(yseq, 1) + 1]))      # check right (after)
                      if (all(is.na(x[i, yseq]) | x[i, yseq] == sword))
                        out$across[i, yseq[1] ] <- 1
            }

          }
        }
      }
    }
  }
  out
}


#' Add a word to a wordsearch matrix
#' @param x wordsearch matrix
#' @param word the word to add (character/scalar)
add_word <- function(x, word = "finding", must_intersect = FALSE) {

  r <- nrow(x)
  c <- ncol(x)
  n <- nchar(word)

  # update matrix with possible insertion points
  M <-
    purrr::map2(
      gamer:::max_word_size(x),
      gamer:::word_intersections(x, word),
      function(a, b)
        (!must_intersect & a >= n) | b == 1
    )

  # choose randomly (uniform) from all possible positions
  v <- purrr::map_int(M, sum)
  if (sum(v) == 0)
    return(x)
  d <- sample(names(M), 1, prob = v / sum(v))
  pos <- which(M[[d]])
  if (length(pos) > 1)
    pos <- sample(pos, 1)

  # add word at the chosen position
  id <- switch(d,
    down = pos:(pos + n - 1),
    across = seq(pos, pos + (n - 1) * r, by = r)
      )
  sword <- stringr::str_split(word, "")[[1]]
  x[id] <- sword

  # store position (for drawing solution lines)
  positions <- tibble::tibble(
    word = word,
    letters = sword,
    id = id,
    i = (id - 1) %% r + 1,
    j = floor((id - 1) / r) + 1,
    dir = d
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

  if (solution)
    g1 <- g1 + geom_line(aes(x = i, y = j, group = word), color = "red", data = attr(x$search, "positions"))

  g1
}