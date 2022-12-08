library(tidyverse)


############### day-2

input <- readLines('2022/input3.txt')


  map2_chr(
    str_sub(input, start = 1, end = nchar(input)/2),
    str_sub(input, start = (nchar(input)/2)+1, end = -1L),
    ~ intersect(unlist(str_split(.x, '')), unlist(str_split(.y, '')))
  ) %>% 
    {sum(match(., letters), na.rm = T) + sum(match(., LETTERS) +26, na.rm = T)}


  