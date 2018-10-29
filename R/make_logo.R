#' Make the 'worrrd' logo
#' @importFrom magrittr "%>%"
make_logo <- function() {

  require(ggplot2)

  # map coordinates
  w1 <-
    tibble::tibble(
      x = rep(3, 6),
      y = 3:-2,
      label = stringr::str_split("WORRRD", "")[[1]]
    )

  w2 <-
    tibble::tibble(
      x = 1:6,
      y = rep(1, 6),
      label = stringr::str_split("WORRRD", "")[[1]]
    )

  # draw via ggplot
  dplyr::bind_rows(w1, w2) %>%
    ggplot(aes(x = x, y = y)) +
    geom_tile(fill = "gray", color = "black", alpha = .5) +
    geom_text(aes(label = label), size = 3) +
    theme_void()
  ggsave("logo.png", width = 1, height = 1)

  # resize via magick
  img <- magick::image_read("logo.png")
  img <- magick::image_scale(img, "x100")
  magick::image_write(img, "logo.png")
}