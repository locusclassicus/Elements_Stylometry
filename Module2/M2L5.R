## word embeddings
load("/Users/olga/R_Workflow/Elements_Stylometry/data/AristotleTidy.Rdata")

library(tidytext)

## filter out words that are used only rarely
library(dplyr)
Aristotle.tidy <- Aristotle.tidy %>% add_count(word) %>% filter(n >= 50) %>% select(-n)

## create a nested dataframe
library(tidyr)
nested_words <- Aristotle.tidy %>% nest(words = c(word))

## introducing slide function
## https://cran.r-project.org/web/packages/slider/slider.pdf
## on the dot prefix https://design.tidyverse.org/dots-prefix.html
library(slider)
# Use `.before`, `.after`, and `.step` to control the window
slide(.x = 1:5, .f = ~.x, .before = 1)
slide(.x = 1:5, .f = ~.x + 2, .before = 1)
slide(1:5, ~.x, .before = 1, .after = 1)
# The `.step` controls how the window is shifted along `.x`,
# allowing you to "skip" iterations if you only need a less granular result
slide(1:10, ~.x, .before = 2, .step = 1)

## slide on tibbles
tbl <- tibble(a = c(1, 2, 3), b = c(3, 4, 5))
slide(tbl, ~.x, .before = 1)

## introducing safely
library(purrr)
safe_log <- safely(log)
safe_log(10)
safe_log("a")

safe_mutate <- safely(mutate) ## will need it later

## introducing map2
skipgrams <- slide(tbl, ~.x, .before = 1)
out <- map2(.x = skipgrams, .y = 1:length(skipgrams), 
            ~ safe_mutate(.x, window_id = .y)) ## add a window id 

## create a slide_windows() function
slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(tbl, 
                            ~.x, 
                            .after = window_size - 1, 
                            .step = 1, 
                            .complete = TRUE)
  

safe_mutate <- safely(mutate)

out <- map2(skipgrams, 1:length(skipgrams), 
            ~ safe_mutate(.x, window_id = .y))
## Transpose turns a list-of-lists "inside-out"; 
## it turns a pair of lists into a list of pairs, 
## or a list of pairs into pair of lists. 
out %>% transpose() %>% pluck("result") %>% compact() %>% bind_rows()
}
# compact() discards elements that are NULL or that have length zero


