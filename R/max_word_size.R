#' Compute maximum word size, based on the current word matrix
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