library(tidyverse)

samp <- readLines('Day9/inputsamp.txt')
answer9_1 <- function(input){
  mat.in <- input %>% 
    str_split('') %>% 
    unlist %>% 
    as.integer() %>% 
    matrix(nrow = length(input), byrow = TRUE)
  
  mat.in
  # row below
  A<- rbind(mat.in[-1,], rep(10, ncol(mat.in))) - mat.in
  # row above
  B<-rbind(rep(10, ncol(mat.in)),mat.in[-nrow(mat.in),]) - mat.in
  # col before
  C<-cbind(rep(10,nrow(mat.in)), mat.in[,-ncol(mat.in)]) - mat.in
  # col after
  D<-cbind(mat.in[,-1], rep(10,nrow(mat.in))) - mat.in
  
  sum(mat.in[which(A>0&B>0&C>0&D>0, arr.ind = T)] +1)
}

input <- readLines('Day9/input.txt')

answer9_1(input)
