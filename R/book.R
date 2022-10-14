#' Create a puzzle book
#' @param input_file yaml file containing book details/contents
#' @param output_file full path to output file (with .pdf extension)
#' @param solutions include solutions (logical/scalar)
#' @export
book <- function(input_file = system.file("book.yml", package = "worrrd"),
                 output_file = "book.pdf",
                 solutions = TRUE) {

  # load content
  p <- yaml::read_yaml(input_file)

  # create .Rmd file
  rmd_header <- glue::glue_data(p,
'---
title: {p$title}
author: {p$author}
output: pdf_document
params:
  config_file: {input_file}
---
')

rmd_prelims <- '

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, include = TRUE, fig.width = 8, fig.height = 8)
```

```{r, include = FALSE}
library(worrrd)
library(ggplot2)

# load book contents
p <- yaml::read_yaml(params$config_file)

# parse r-code (words only)
p$pages <- purrr::map(
  p$pages,
  function(pgs) {
    if (length(pgs$words) == 1 && stringr::str_detect(pgs$words, "\\`.*\\`"))
      pgs$words <- eval(parse(text = stringr::str_replace_all(pgs$words, "\\`", "")))
    pgs
  }
)

# create all wordsearches
out <- purrr::map(p$pages, ~wordsearch(words = .x$words, r = p$rows, c = p$cols, image = .x$image))
```
'

rmd_cover <- glue::glue("
## A Message from the Author
Enjoy this awesome wordsearch book!

\\newpage

  ")

rmd_content <-
  purrr::map_chr(
    1:length(p$pages),
    function(i) {
      glue::glue(.open = "..", .close = "..",
'# `r p$pages[[..i..]]$name`
```{r}
plot(out[[..i..]], solution = FALSE)
```

\\newpage

'
        )
    })

if (solutions == T) {
  rmd_solutions <-
    purrr::map_chr(
      1:length(p$pages),
      function(i) {
        glue::glue(.open = "..", .close = "..",
                   '# `r p$pages[[..i..]]$name` SOLUTION
```{r}
plot(out[[..i..]], solution = TRUE)
```

\\newpage

'
        )
      })
  rmd_content <- c(rmd_content, rmd_solutions)
}



  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(c(rmd_header, rmd_prelims, rmd_cover, rmd_content), con = rmd_file)

  # render as pdf
  rmarkdown::render(rmd_file, output_dir = getwd(), output_file = output_file, quiet = TRUE)
  unlink(rmd_file)
  message("New book can be found at ", output_file, ".")

  return(output_file)
}
