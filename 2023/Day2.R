library(dplyr)
library(tidyr)
library(stringr)
library(unglue)

input2 <- readLines('2023/data/input2.txt')
patterns <- "Game {game}: {draws}"

data2 <- unglue::unglue_data(input2, patterns = patterns, convert = TRUE) 

dr_counts <- max(str_count(data2$draws, ";")) + 1

possible <- c(red = 12, green = 13, blue = 14)

data2 <- data2 %>% 
  separate(col = draws, sep = ";", into = paste0("draw", seq_len(dr_counts)), fill = "right") %>% 
  pivot_longer(starts_with('draw'), names_to = "drawNo", values_drop_na = TRUE) %>% 
  mutate(value = str_squish(value)) %>% 
  separate(col = value, into = paste0("ball", 1:3), sep = ",", fill = "right") %>% 
  pivot_longer(starts_with("ball"), names_to = NULL, values_drop_na = TRUE) %>% 
  mutate(value = str_squish(value)) %>% 
  separate(col = value, into = c("counts", "color"), sep = " ", convert = TRUE) 

#  Part-1
data2 %>% 
  group_by(game, drawNo) %>% 
  filter(any(counts > possible[color])) %>% 
  pull(game) %>% 
  unique() %>% 
  # there are 100 games
  {sum(1:100) - sum(.)}

#  Part-2
data2 %>% 
  group_by(game, color) %>% 
  slice_max(order_by = counts, n = 1, with_ties = FALSE) %>% 
  group_by(game) %>% 
  summarise(answer = prod(counts)) %>% 
  summarise(answer = sum(answer)) %>% 
  pull(answer)



