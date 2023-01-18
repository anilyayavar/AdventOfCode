library(tidyverse)
x <- '12111'

x
snafu <- function(x){
  str_split(x, '') %>% 
    unlist() %>% 
    rev() %>% 
    {case_when(. == '-' ~ '-1',
               . == '=' ~ '-2',
               TRUE ~ .)} %>% 
    as.integer() %>% 
    imap(~ .x * (5^(.y-1))) %>% 
    unlist() %>% 
    sum()
}

input <- readLines('2022/input25.txt')

map(input, snafu) %>% 
  unlist() 

snafu(input[3])
install.packages('gmp')

x <- input[3]
str_split(x, '') %>% 
  unlist() %>% 
  rev() %>% 
  {case_when(. == '-' ~ '-1',
             . == '=' ~ '-2',
             TRUE ~ .)} %>% 
  as.integer() %>% 
  imap(~ .x * (5^(.y-1))) %>% 
  unlist() %>% 
  sum()
