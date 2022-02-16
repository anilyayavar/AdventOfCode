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

input <- readLines('Previous Years/2019/input2.txt') %>% str_split(',') %>% unlist %>% as.integer()

## part-1
answer2_1 <- function(x){
  #set initial values
  x[2] <- 12
  x[3] <- 2
  # set i (iteration)
  i <- 1
  # while loop
  while(TRUE){
    if(x[4*(i-1)+1] ==99){
      break
    } else if (x[4*(i-1)+1] == 1){
      x[x[4*i] +1] <- x[x[4*(i-1)+2] +1] + x[x[4*(i-1)+3] +1]
    } else {
      x[x[4*i] +1] <- x[x[4*(i-1)+2] +1] * x[x[4*(i-1)+3] +1]
    }
    i <- i +1
  }
  # final answer
  x[1]
}

answer2_1(input)

## part-2


answer2_2 <- function(input){
  # set x
  mat <- matrix(rep(0, 100*100), nrow = 100)
  x <- input
  for(j in seq(100) -1){
    for(k in seq(100)-1){
      
      #set initial values
      x[2] <- j
      x[3] <- k
      # set i (iteration)
      i <- 1
      # while loop
      while(TRUE){
        if(x[4*(i-1)+1] ==99){
          break
        } else if (x[4*(i-1)+1] == 1){
          x[x[4*i] +1] <- x[x[4*(i-1)+2] +1] + x[x[4*(i-1)+3] +1]
        } else {
          x[x[4*i] +1] <- x[x[4*(i-1)+2] +1] * x[x[4*(i-1)+3] +1]
        }
        i <- i +1
      }
      # input mat value
      mat[j+1, k+1] <- x[1]
      # reset x
      x <- input
      
    }
  }
  # final answer
  100*(which(mat == 19690720, arr.ind = TRUE)[1]-1)+(which(mat == 19690720, arr.ind = TRUE)[2]-1)
}

answer2_2(input)
