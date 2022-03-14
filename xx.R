library(tidyverse)

input <- read_lines('Previous Years/2020/input18.txt')

# define helper functions
`%a%` <- function(x,y) x+y
`%s%` <- function(x,y) x-y
`%m%` <- function(x,y) x*y
`%d%` <- function(x,y) x/y

# modify input
input <- reduce2(list('\\+', '\\-', '\\*', '\\/'), list('%a%', '%s%', '%m%', '%d%'),
        .init = input,
        function(.x, .y, .z) map(.x, function(.a) str_replace_all(.a, .y, .z )))

# final answer
map_dbl(input, ~ eval(parse(text = .x))) %>% 
  sum()



