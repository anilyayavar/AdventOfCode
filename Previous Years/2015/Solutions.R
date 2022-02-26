library(tidyverse)

#--- Day 1: Not Quite Lisp ---

input <- readLines('Previous Years/2015/input1.txt')
# part-1
answer1_1 <- function(input){
  input_vec <- input %>% str_split('') %>% unlist
  floors <- c(-1, 1)[1+(input_vec == '(')]
  sum(floors)
}

answer1_1(input)
# part-2
answer1_2 <- function(input){
  input_vec <- input %>% str_split('') %>% unlist
  floors <- c(-1, 1)[1+(input_vec == '(')]
  which.max(cumsum(floors)<0)
}

answer1_2(input)

######################################################################################
#--- Day 2: I Was Told There Would Be No Math ---

input <- readLines('Previous Years/2015/input2.txt')

#part-1
answer2_1 <- function(input){
  input_list <- input %>% 
    str_split('x') %>% 
    map(as.integer)
  
  
  l <- map_int(input_list, ~ sort(.x)[1])
  b <- map_int(input_list, ~ sort(.x)[2])
  h <- map_int(input_list, ~ sort(.x)[3])
  #answer
  sum(3*l*b + 2*l*h + 2*b*h)
}

answer2_1(input)

#part-2

answer2_2 <- function(input){
  input_list <- input %>% 
    str_split('x') %>% 
    map(as.integer)
  
  
  l <- map_int(input_list, ~ sort(.x)[1])
  b <- map_int(input_list, ~ sort(.x)[2])
  h <- map_int(input_list, ~ sort(.x)[3])
  #answer
  sum(2*(l+b)+l*b*h)
}

answer2_2(input)

##########################################################################################
#--- Day 3: Perfectly Spherical Houses in a Vacuum ---

input <- readLines('Previous Years/2015/input3.txt')
## part-1
answer3_1 <- function(input){
  input_vec <- input %>% 
    str_split('') %>% 
    unlist
  
  input_vec <- ifelse(input_vec== '>', 1, 
                      ifelse(input_vec == '<', -1, 
                             ifelse(input_vec == '^', 1i, -1i)))
  
  input_vec <- c(0+0i, input_vec)
  
  length(unique(cumsum(input_vec)))
}

answer3_1(input)

# part-2

answer3_2 <- function(input){
  input_vec <- input %>% 
    str_split('') %>% 
    unlist
  
  input_vec <- ifelse(input_vec== '>', 1, 
                      ifelse(input_vec == '<', -1, 
                             ifelse(input_vec == '^', 1i, -1i)))
  #answer
  split(input_vec, seq_along(input_vec) %% 2) %>% 
    map(~ c(0+0i, .x) %>% 
          cumsum()) %>% 
    reduce(union) %>% 
    length
}

answer3_2(input)

