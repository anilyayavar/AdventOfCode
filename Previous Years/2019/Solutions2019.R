library(tidyverse)

# -------Day 1: The Tyranny of the Rocket Equation-----
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

##################################################################################################################################

#--- Day 2: 1202 Program Alarm ---

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


###############################################################################################################################

#### Day3

# TO BE DONE


#############################################################################################################################

### ---Day 4 SECURE CONTAINER---

#part-1
answer4_1 <- function(){
  x <- map_lgl(152085:670283,
               ~ {y <- str_split(as.character(.x), '') %>% 
                 unlist %>% 
                 as.numeric() %>% 
                 diff ;
               all(y >= 0) & any(y == 0)
               })
  sum(x)
}

answer4_1()

# part-2 
answer4_2 <- function(){
  x <- map_lgl(152085:670283,
               ~{y <- str_split(as.character(.x), '') %>% 
                 unlist;
               (2 %in% rle(y)$lengths) & all(diff(as.integer(y)) >= 0)})
  sum(x)
}

answer4_2()

############################################################################

#--- Day 5: Sunny with a Chance of Asteroids ---


#--- Day 8: Space Image Format ---

# part-1
input8 <- readLines('Previous Years/2019/input8.txt')
answer8_1 <- function(input8, pixels = 150){
  input_mat <- input8 %>% str_split('') %>% 
    unlist %>% 
    as.integer() %>% 
    matrix(byrow = T, ncol = pixels)
  layer <- input_mat[rowSums(input_mat == 0) %>% which.min(),]
  sum(layer == 1)*sum(layer==2)
}

answer8_1(input8)
answer8_1('0222112222120000', pixels = 4)


#part-2
# install.packages('reshape2')
library(reshape2)

answer8_2 <- function(input8, width = 25, length = 6){
  input_mat <- input8 %>% str_split('') %>% 
    unlist %>% 
    as.integer() %>% 
    matrix(byrow = F, nrow = width*length)
  
  # 150 pixels are in rows, layers in cols
  # split pixels for each layer
  
  m <- map_int(unname(split(input_mat, seq(150))), ~ .x[which.max(.x != 2)]) %>% 
    matrix(byrow = T, ncol = 25)
  
  # final plot
  melt(m) %>% 
    subset(value == 1) %>% 
    ggplot(aes(Var2, Var1)) +
    geom_point(size = 2, fill = "blue", shape = 21) +
    scale_y_reverse()
  
}

answer8_2(input8)
#LRFKU

#---------------------------------------











