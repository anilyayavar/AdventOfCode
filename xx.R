library(tidyverse)

input <- read_lines('Previous Years/2020/input12.txt')

input

init_state <- list(face = 1+0i, point = 0+0i)

init_state


mod_input <- map(input, ~ list(dir = substr(.x, 1, 1),
                  amt = as.integer(substr(.x, 2, nchar(.x)))))

my_fun <- function(.x, .y){
  if(.y$dir == 'E'){
    list(face = .x$face, point = .x$point + .y$amt)
  } else if (.y$dir == 'W'){
    list(face = .x$face, point = .x$point - .y$amt)
  } else if (.y$dir == 'N'){
    list(face = .x$face, point = .x$point + complex(real = 0, imaginary = .y$amt))
  } else if (.y$dir == 'S'){
    list(face = .x$face, point = .x$point - complex(real = 0, imaginary = .y$amt))
  } else if (.y$dir == 'F'){
    list(face = .x$face, point = .x$point + .x$face * .y$amt) 
  } else if (.y$dir == 'R'){
    if (.y$amt == 90){
      list(face = .x$face * (0-1i), point = .x$point)
    } else if (.y$amt == 180){
      list(face = .x$face * -1, point = .x$point)
    } else if (.y$amt == 270){
      list(face = .x$face * (0+1i), point = .x$point)
    }
  } else if (.y$dir == 'L'){
    if (.y$amt == 90){
      list(face = .x$face * (0+1i), point = .x$point)
    } else if (.y$amt == 180){
      list(face = .x$face * -1, point = .x$point)
    } else if (.y$amt == 270){
      list(face = .x$face * (0-1i), point = .x$point)
    }
  }
}

reduce(mod_input, .init = init_state, my_fun) %>% 
  {.$point} %>% 
  {abs(Re(.)) + abs(Im(.))}


