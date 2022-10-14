
<!-- README.md is generated from README.Rmd. Please edit that file -->

# worrrd <img src="man/figures/logo.png" align="right" />

## Overview

Build word games using R. Ya hearrrd?

Features:

-   Crossword Puzzle
-   Wordsearch

## Installation

``` r
# install.packages("devtools")
devtools::install_github("anthonypileggi/worrrd")
```

## Wordsearch

Create your very own custom wordsearch! Worrrd.

``` r
library(worrrd)

# Simple wordsearch
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
plot(x, solution = TRUE, puzzle_size = 2, legend_size = 2)
```

![](man/figures/README-wordsearch-hard-1.png)<!-- -->

### Puzzle Book

If you want to make a printable wordsearch book, you can provide a .yml
file.

**my_book.yaml**

``` yaml
title: I made a wordsearch!
author: Unicorn Cupcake
type: wordsearch
rows: 20
cols: 20
pages:
  - name: Fruity Fun
    words: [apple, orange, banana, grapes, kiwi, strawberry, watermelon, lemon, lime]
  - name: Animal Mayhem
    words: [dog, cat, fish, wolf, horse, shark, crocodile, alligator, iguana]
    image: "https://us.123rf.com/450wm/miceking/miceking1506/miceking150601500/40903456-stock-vector-lion-silhouette.jpg"

  - name: Searchin in the USA
    words: "`state.name`"
```

Then you can generate a [pdf puzzle
book](https://github.com/anthonypileggi/worrrd/blob/master/inst/book.pdf).

``` r
book(input_file = "my_book.yaml", output_file = "my_puzzle_book")
```

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
#> There are 25 across and 25 down.
```

``` r
plot(x)
```

![](man/figures/README-crossword-1.png)<!-- -->

``` r
plot(x, solution = TRUE)
```

![](man/figures/README-crossword-2.png)<!-- -->

## Future Plans

-   Allow crossword in the puzzle book

## Known Issues

-   Will not work with duplicate words
