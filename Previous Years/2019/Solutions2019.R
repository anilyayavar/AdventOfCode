library(tidyverse)

#Day 1
input <- readLines('Previous Years/2019/input1.txt')

# part-1
answer1_1 <- function(input){
  input <- input |> as.double()
  sum(floor(input/3) -2)
}

answer1_1(input)

# part-2

answer1_2 <- function(input){
  x <- input
  y <- rep(0, length(input))
  while(any(x>0)){
    x <- reduce(1, .init = x, ~ pmax(0, floor(as.numeric(.x)/3)-2))
    y <- y + x
  }
  sum(y)
}

answer1_2(input)

#### ............DAY 2 ...........####
