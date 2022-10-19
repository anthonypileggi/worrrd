#' Get possible intersection points based on the current board and a provided word
#' @param x word matrix
#' @param word the word to add (character/scalar)
#' @return for each direction, a matrix of crossing-point counts
#' @importFrom utils tail
word_intersections <- function(x, word = "needles") {

  r <- nrow(x)
  c <- ncol(x)
  A <- matrix(0, nrow = r, ncol = c)
  out <- list(across = A, down = A)
  sword <- stringr::str_split(toupper(word), "")[[1]]

  # search the entire word matrix for possible intersecting insertion points
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
                        out$down[xseq[1], j] <- length(xseq) - length(cids)       # save the intersection count for each
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
                        out$across[i, yseq[1] ] <- length(yseq) - length(cids)    # save the intersection count for each
            }

          }
        }
      }
    }
  }

  out
}