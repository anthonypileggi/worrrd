#' Create a wordsearch puzzle
#' @param words a vector of hidden words (character/vector)
#' @param clues a vector of word clues (optional; character/vector)
#' @param r number of rows
#' @param c number of columns
#' @param image path to an image that the resulting grid should look like.NULL for no shape
#' @export
wordsearch <- function(words = c("finding", "needles", "inside", "haystacks"),
                       clues = words,
                       r = 10,
                       c = 10,
                       image = NULL) {

  # create empty matrix
  x <- matrix(NA, nrow = r, ncol = c)

  # prepare the word list
  words <- prepare_words(words)

  # check conditions
  words <- words[nchar(words) <= max(c(r, c))]    # remove words that won't fit
  if (length(words) == 0) {
    message("No words can be placed.  Try a larger grid-size, or shorter words.")
    return(NULL)
  }

  # generate shape file (if provided)
  shape_matrix <- NULL
  if (!is.null(image))
    shape_matrix <- image_matrix(image, r, c)

  # add words to the board (one-at-a-time)
  for (i in seq_along(words)) {
    x_new <- add_word(x, words[i], shape_matrix = shape_matrix)
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
  ids <- is.na(x)
  if (!is.null(image))
    ids <- ids & shape_matrix
  x[ids] <- sample(LETTERS, sum(ids), replace = TRUE)

  # convert to a 'wordsearch' class object
  out <-
    list(
      search = x,
      words = words,
      clues = clues,
      solution = solution,
      image = image,
      shape_matrix = shape_matrix
    )
  as_wordsearch(out)
}


# Constructors =============================================================

#' Assign an object to the `wordsearch` class
#' @param x an object containing wordsearch data
#' @export
as_wordsearch <- function(x) {
  if (!is_wordsearch(x))
    class(x) <- append("wordsearch", class(x))
  x
}

#' Check if an object is of the `wordsearch` class
#' @param x an R object to check
#' @export
is_wordsearch <- function(x) {
  inherits(x, "wordsearch")
}


# Methods ===================================================================

#' Print details for a wordsearch puzzle
#' @export
print.wordsearch <- function(x) {
  cat(paste("Wordsearch\n"))
  cat(paste("Rows:", nrow(x$search), "\n"))
  cat(paste("Columns:", ncol(x$search), "\n"))
  cat(paste("Hidden Words:", length(x$words), "\n"))
  cat(paste("Clues:", ifelse(!identical(prepare_words(x$words), prepare_words(x$clues)), "No", "Yes"), "\n"))
  cat(paste("Custom Shape:", ifelse(is.null(x$shape_matrix), "No", "Yes"), "\n"))
  invisible(x)
}

#' Draw a wordsearch puzzle
#' @param x wordsearch object (class: wordsearch)
#' @param solution show solution? (logical/scalar)
#' @param clues show clues? (logical/scalar)
#' @param title puzzle title (character/scalar)
#' @param puzzle_size letter size of puzzle; ignore to auto-size (numeric/scalar)
#' @param legend_size letter size of word list; set to NULL to auto-size (numeric/scalar)
#' @import ggplot2
#' @export
plot.wordsearch <- function(x,
                            solution = FALSE,
                            clues = TRUE,
                            title = "",
                            puzzle_size = NULL,
                            legend_size = NULL) {
  require(ggplot2)
  ids <- expand.grid(i = 1:nrow(x$search), j = 1:ncol(x$search))
  xt <-
    purrr::map2_df(
      ids$i,
      ids$j,
      ~tibble::tibble(
        i = .x,
        j = .y,
        value = x$search[.x, .y],
        word = !is.na(x$solution[.x, .y]),
        outline = ifelse(is.null(x$shape_matrix), TRUE, x$shape_matrix[.x, .y])
        )
    )

  # draw wordsearch
  g1 <- xt %>%
    dplyr::filter(!is.na(value)) %>%
    ggplot()
  if (is.null(puzzle_size)) {
    g1 <- g1 +
      ggfittext::geom_fit_text(
        aes(x = i, y = j, label = value),
        grow = T,
        min.size = 0
      )
  } else {
    g1 <- g1 +
      geom_text(
        aes(x = i, y = j, label = value),
        size = puzzle_size
      )
  }
  g1 <- g1 +
    scale_y_reverse() +
    theme_void() +
    theme(
      aspect.ratio = ncol(x$search) / nrow(x$search)
      )

  # add solution (upon request)
  if (solution) {
    g1 <- g1 +
      geom_line(
        aes(x = i, y = j, group = word),
        color = "red",
        data = attr(x$search, "positions")
        )
  }


  # use custom shape
  #  -- if using a 'shape_matrix', add the outline
  #  -- otherwise, add a border
  if (is.null(x$shape_matrix)) {
    g1 <- g1 +
      annotate("rect",
        xmin = 0.5, ymin = 0.5,
        xmax = max(xt$i) + 0.5, ymax = max(xt$j) + 0.5,
        alpha = 0, color = "black"
      )
  } else {
    g1 <- g1 +
      geom_tile(
        aes(x = i, y = j),
        alpha = .1, fill = "gray", color = "gray",
        data = dplyr::filter(xt, outline)
        )
  }

  # add title (upon request)
  if (title != "") {
    g1 <- g1 +
      ggtitle(title) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")
      )
  }

  # TODO: add a background image

  # draw word list; merge with puzzle
  if (clues) {
    tmp <- dplyr::tibble(
      i = 1,
      j = 1:length(x$clues),
      word = x$clues
    )
    g2 <- ggplot(tmp)
    if (is.null(legend_size)) {
      g2 <- g2 +
        ggfittext::geom_fit_text(
          aes(x = i, y = j, label = word),
          reflow = T,
          grow = F,      # NOTE: grow=T slows this process...
          min.size = 0
        )
    } else {
      g2 <- g2 +
        # geom_text(
        #   aes(x = i, y = j, label = word),
        #   size = legend_size,
        #   hjust = 0.5
        # )
        geom_richtext(
          aes(x = i, y = j, label = word),
          fill = NA,
          size = legend_size,
          label.color = NA,  # remove background and outline
          label.padding = grid::unit(rep(0, 4), "pt")  # remove padding
        )
    }
    g2 <- g2 +
      ggtitle(expression(underline("Word List"))) +
      theme_void() +
      scale_y_reverse() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
    g1 <- cowplot::plot_grid(g1, g2, nrow = 1, rel_widths = c(3/4, 1/4))
  }

  g1
}
