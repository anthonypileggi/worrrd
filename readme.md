## Gamer

Games for kids.

## Wordsearch

Create a custom wordsearch!

```
# Simple word search
words <- c("finding", "needles", "inside", "haystacks")
x <- word_search(words, r = 10, c = 20)
plot_word_search(x, solution = TRUE)
```

You can make it as hard as you want!

```
# All 50 States
library(datasets)
x <- word_search(state.name, r = 40, c = 40)
plot_word_search(x, solution = TRUE)
```

## Crossword Puzzle

Oh, you're one of the more sophicated types that craves a crossword puzzle?  Worrrd.

```
x <- crossword()
plot_crossword(x, solution = TRUE)
```

Oh, is that too easy for you?  How about a harder one, champ?

```
x <- crossword(state.name, r = 40, c = 40)
plot_crossword(x, solution = TRUE)
```

## Ideas

### Static (i.e., print and bring with you)

1. Wordsearch
2. Color by Numbers
3. Math worksheet

### Interactive

1. Memory
