#' Prepare a worrrd object for printing
#' @param x ggplot object
#' @param filename name of file
#'
#' @examples
#' \donttest{
#' words <- c("dog", "cat", "horse", "frog", "cow", "fox")
#' ex1 <- wordsearch(words, r = 10, c = 10)
#' my_puzzle <- plot(ex1, solution = FALSE)
#' printable(my_puzzle, "my_wordsearch.pdf")
#' }
#'
#' @return filename of pdf puzzle
#' @export
printable <- function(x, filename = "plot.pdf") {
  ggplot2::ggsave(
    filename,
    plot = x,
    width = 11,
    height = 8.5
  )
}