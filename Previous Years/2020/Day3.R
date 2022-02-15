library(tidyverse)

answer3_1 <- function(input){
    mat.in <- input %>% 
    str_split('') %>% 
    unlist %>% 
    matrix(nrow = length(input), byrow = TRUE)
  
  sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*3 ) %% ncol(mat.in)) +1 ]) == '#')
}

input_samp <- readLines('Previous Years/2020/samp3.txt')
input <- readLines('Previous Years/2020/input3.txt')
answer3_1(input)

### part-2

answer3_2 <- function(input){
  mat.in <- input %>% 
    str_split('') %>% 
    unlist %>% 
    matrix(nrow = length(input), byrow = TRUE)
  #calculating five slope tree maps
  slope1 <- sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*1 ) %% ncol(mat.in)) +1 ]) == '#')
  slope2 <- sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*3 ) %% ncol(mat.in)) +1 ]) == '#')
  slope3 <- sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*5 ) %% ncol(mat.in)) +1 ]) == '#')
  slope4 <- sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*7 ) %% ncol(mat.in)) +1 ]) == '#')  
  slope5 <- sum(map_chr(seq(1+nrow(mat.in)/2), ~mat.in[2*.x -1, ((.x -1) %% ncol(mat.in)) +1 ]) == '#')
  #final result
  prod(map_int(1:5, ~ get(paste0('slope', .x))))
  
}

answer3_2(input_samp)
answer3_2(input)


