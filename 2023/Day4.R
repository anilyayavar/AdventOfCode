library(tidyverse)
library(unglue)
input4 <- readLines("2023/data/input4.txt")
patterns <- "Card{=\\s+}{card}: {wins} | {draws}"

## Part-1
unglue_data(input4, patterns = patterns) %>% 
  mutate(wins = str_extract_all(wins, "\\d+"),
         wins = map(wins, as.integer),
         draws = str_extract_all(draws, "\\d+"),
         draws = map(draws, as.integer),
         score = map2_int(draws, wins,
                         ~ sum(.x %in% .y)),
         score = ifelse(score == 0, 0, 2^(score-1))) %>% 
  summarise(ans = sum(score))


## Part-2
d <- unglue_data(input4, patterns = patterns) %>% 
  mutate(wins = str_extract_all(wins, "\\d+"),
         wins = map(wins, as.integer),
         draws = str_extract_all(draws, "\\d+"),
         draws = map(draws, as.integer),
         score = map2_int(draws, wins,
                          ~ sum(.x %in% .y))) 

# Initialise Loop
ans <- 0
seqi <- c()

for(i in 1:220){
  seqi <- c(i, seqi)
  card_nums <- sum(i == seqi)
  seq_n <- seq(from = i + 1L, length.out = d$score[i])
  seq_n <- rep(seq_n, card_nums)
  seqi <- c(seqi, seq_n)
  ans <- ans + card_nums
} 

ans






i
i == seqi
seq_n

seq(from = 5, length.out = 1)
