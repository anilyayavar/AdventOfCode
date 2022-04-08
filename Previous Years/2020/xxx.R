library(tidyverse)

install.packages('tictoc')
library(tictoc)

input <- c(15L, 5L, 1L, 4L, 7L, 0L)

game <- rep(0, 30000000)
for (i in 1:(length(input) - 1)) {
  number <- input[[i]]
  game[number + 1] <- i
}


last_number <- last(input)


for (i in (length(input) + 1):30000000) {
  occurred_last <- game[[last_number + 1]]
  game[[last_number + 1]] <- i - 1
  last_number <- if (occurred_last == 0) {
    0
  } else {
    i - 1 - occurred_last
  }
}


last_number

input <- read_lines('Previous Years/2020/input12.txt')

unique(substr(input, 1, 1))

as.integer(gsub('^\\w', '', input))
complex(real = 1, imaginary = 0)

init <- list(coor = 0+0i, dir = 0+1i)

Conj(-1+0i)

my_fun <- function(coor, dir, deg){
  
}


my_fun2 <- function(.x, .y){
  if(!substr(.y, 1, 1) %in% c('R', 'L')){
    dir <- .x$dir
    if(substr(.y, 1, 1) == 'N'){
      coor <- .x$coor + complex(real = 0, imaginary = as.integer(gsub('^\\w', '', .y)))
    } else if (substr(.y, 1, 1) == 'S'){
      coor <- .x$coor - complex(real = 0, imaginary = as.integer(gsub('^\\w', '', .y)))
    } else if (substr(.y, 1, 1) == 'E'){
      coor <- .x$coor + complex(imaginary = 0, real = as.integer(gsub('^\\w', '', .y)))
    } else if (substr(.y, 1, 1) == 'W'){
      coor <- .x$coor - complex(imaginary = 0, real = as.integer(gsub('^\\w', '', .y)))
    } else {
      coor <- (.x$dir * as.integer(gsub('^\\w', '', .y))) + .x$coor
    }
  } else{
    coor <- .x$coor
    if(substr(.y, 1, 1) == 'R'){
      
    }
  }
}



library(tidyverse)
input <- read_lines('Previous Years/2020/input10.txt')
input %>% 
  as.integer()