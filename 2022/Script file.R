library(tidyverse)


############### day-2

input <- readLines('2022/input4.txt')

input %>% str_split(',') %>% 
  map(~ str_split(.x, '-') %>% unlist() %>% as.integer()) %>% 
  map_lgl(~ ((.x[1] <= .x[3]) & (.x[2] >= .x[3])) | ( (.x[1] <= .x[4]) & (.x[2] >= .x[4]) ) ) %>% 
  sum()

length(input)

1000-32
  