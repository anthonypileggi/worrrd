
#' Convert an image to a 0/1 matrix
#' @param img image path
#' @param r number of rows (numeric/sclar)
image_matrix <- function(img = "https://upload.wikimedia.org/wikipedia/commons/9/96/Tux_Paint_banana.svg",
                          r = 10,
                          c = 10) {

  # load image
  img <- magick::image_read(img)

  # scale to size of word_grid
  img <- magick::image_scale(img, paste0(r,"x", c, "!"))

  # convert the image to a black/white matrix representation
  C <- purrr::map(1:4, ~img[[1]][.x, , ] == "ff")     # white = 'ff ff ff ff'
  outline <- C[[1]] + C[[2]] + C[[3]] + C[[4]]
  outline <- outline != 4
  outline
}