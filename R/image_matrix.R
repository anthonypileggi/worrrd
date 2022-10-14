#' Convert an image to a 0/1 matrix
#' @param img full path to image (character/scalar)
#' @param rows number of rows (numeric/scalar)
#' @param columns number of columns (numeric/scalar)
image_matrix <- function(img = "https://upload.wikimedia.org/wikipedia/commons/9/96/Tux_Paint_banana.svg",
                          rows = 10,
                          columns = 10) {

  # load image
  img <- magick::image_read(img)

  # scale to size of word_grid
  img <- magick::image_scale(img, paste0(rows,"x", columns, "!"))

  # convert the image to a black/white matrix representation
  C <- purrr::map(1:dim(img[[1]])[1], ~img[[1]][.x, , ] == "ff")     # white = 'ff ff ff ff'
  outline <- Reduce("+", C) / length(C)
  if (any(outline == 1)) {
  #if (all(unique(as.numeric(outline)) %in% c(0, 1))) {
    outline <- outline != 1          # if colors retained across 4 matrices
  } else {
    outline <- outline != 0          # if colors pushed to last matrix (note: why is this happening??)
  }

  outline
}