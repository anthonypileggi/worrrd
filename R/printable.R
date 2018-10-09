#' Prepare a gamer object for printing
#' @param x ggplot object
#' @param filename name of file
#' @export
printable <- function(x, filename = "plot.pdf") {
  ggplot2::ggsave(
    filename,
    plot = x,
    width = 11,
    height = 8.5
  )
}