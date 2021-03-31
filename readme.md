
<!-- README.md is generated from README.Rmd. Please edit that file -->

# worrrd <img src="man/figures/logo.png" align="right" />

## Overview

Build word games using R. Ya hearrrd?

Features:

-   Crossword Puzzle
-   Wordsearch

## Installation

``` r
install.packages("devtools")
devtools::install_github("anthonypileggi/worrrd")
```

## Wordsearch

Create your very own custom wordsearch! Worrrd.

``` r
library(worrrd)

# Simple word search
words <- c("alligator", "crocodile", "squirrel", "swordfish", "german shepherd", "panda", "wolf")
x <- wordsearch(words, r = 20, c = 20)
#> Found positions for 7/7 words.
plot(x, solution = TRUE)
#> Loading required package: ggplot2
```

![](man/figures/README-wordsearch-easy-1.png)<!-- -->

You can make it as hard as you want. Go bananas!

``` r
# All 50 States
library(datasets)
x <- wordsearch(state.name, r = 50, c = 50)
#> Found positions for 50/50 words.
plot(x, solution = TRUE, letter_size = 2, legend_size = 2)
```

![](man/figures/README-wordsearch-hard-1.png)<!-- -->

### Custom Shapes

Wait, what? You want to make it in the shape of a banana? I mean, that’s
kinda weird… but why not!

``` r
banana <- "https://upload.wikimedia.org/wikipedia/commons/9/96/Tux_Paint_banana.svg"
words <- c("banana", "apple", "grapes", "orange", "pear", "kumquat")
x <- wordsearch(words, r = 20, c = 20, image = banana)
#> Found positions for 6/6 words.
cowplot::ggdraw() +
  cowplot::draw_image(banana) +
  cowplot::draw_plot(plot(x, solution = TRUE))
```

![](man/figures/README-wordsearch-banana-1.png)<!-- -->

This lion was hungry. Can you find the antelope he ate?

``` r
lion <- "https://us.123rf.com/450wm/miceking/miceking1506/miceking150601500/40903456-stock-vector-lion-silhouette.jpg"
x <- wordsearch("antelope", r = 40, c = 40, image = lion)
#> Found positions for 1/1 words.
plot(x, solution = TRUE, letter_size = 2)
```

![](man/figures/README-wordsearch-lion-1.png)<!-- -->

## Crossword Puzzle

Oh, you’re one of the more sophisticated types that craves a crossword
puzzle? Worrrd.

``` r
words <- row.names(state.x77)
clues <- paste0(state.region, " state covering ", scales::comma(state.x77[, "Area"]), " square miles.")
x <- crossword(words, clues, r = 40, c = 40)
#> Found positions for 50/50 words.
x
#> Crossword Puzzle
#> Contains 50 clues.
#> There are 27 across and 23 down.
```

``` r
plot(x, solution = TRUE)
```

![](man/figures/README-crossword-1.png)<!-- -->
