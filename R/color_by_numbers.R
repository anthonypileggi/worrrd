

#' Convert an image to a color-by-numbers
#' @param img color image to convert
#' @param n_colors number of colors to include
color_by_numbers <- function(img) {

  img <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 400)

  # find all unique colors
  x <- img[[1]]
  df <- purrr::map2(1:dim(x)[2], 1:dim(x)[3], function(i, j) paste(x[,i,j], collapse = ","))
  unique(unlist(df))

  # get image outline
  img_outline <- image_flatten(img, 'Modulate')
  # - round colors to black/white (to emphasize lines)

  # replace colors with white

  # insert numbers where colors once were
  # - find center-of-mass of the removed color

}