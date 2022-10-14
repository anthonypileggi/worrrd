#' Add a word to a word matrix
#' @param x word matrix
#' @param word the word to add (character/scalar)
#' @param must_intersect force the added word to intersect with >1 word (logical/scalar)
#' @param shape_matrix a binary matrix generated from a call to \code{\link{image_matrix}}
#' @return word matrix with word added (if possible)
add_word <- function(x,
                     word = "finding",
                     must_intersect = FALSE,
                     shape_matrix = NULL) {

  r <- nrow(x)
  c <- ncol(x)
  n <- nchar(word)

  # update matrix with possible insertion points
  #   - if 'must_intersect' is true, prioritize the maximal crossing-points
  #   - account for a 'shape_matrix', if present
  M <-
    purrr::map2(
      max_word_size(x, shape_matrix),
      word_intersections(x, word),
      function(a, b)
        (!must_intersect & a >= n) | (b > 0 & b == max(b))
    )

  # choose randomly (uniform) from all possible positions
  #   - if 'must_intersect' is true, choose from (maximal) crossing-points only
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

  # store position (for drawing solution lines & crossword puzzles)
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
