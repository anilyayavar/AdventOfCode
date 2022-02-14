library(tidyverse)
################## day10
input <- readLines('input10.txt')
answer10_1 <- function(input){
  repl_chars <- c('[]', '{}', '()', '<>')
  # remove matching pairs
  while (any(map_lgl(repl_chars, ~any(str_detect(input, fixed(.x)))))) {
    input <- reduce(repl_chars, .init = input, ~str_remove_all(.x, fixed(.y)))
  }
  input
  # finding Corrupt lines
  corrupt_chars <- c(']', ')', '}', '>')
  corrupt_lines <- input[reduce(map(corrupt_chars, ~str_detect(input, fixed(.x))), `|`)]
  corrupt_chars2 <- c('[', '(', '{', '<')
  first_corrupt_chars <- substr(reduce(corrupt_chars2, .init = corrupt_lines, ~ str_remove_all(.x, fixed(.y))), 1, 1)
  sum(case_when(first_corrupt_chars == '}' ~ 1197,
                first_corrupt_chars == '>' ~ 25137,
                first_corrupt_chars == ')' ~ 3,
                first_corrupt_chars == ']' ~ 57))
}

answer10_1(input)


# part2
input <- readLines('input10.txt')
answer10_2 <- function(input){
  repl_chars <- c('[]', '{}', '()', '<>')
  # remove matching pairs
  while (any(map_lgl(repl_chars, ~any(str_detect(input, fixed(.x)))))) {
    input <- reduce(repl_chars, .init = input, ~str_remove_all(.x, fixed(.y)))
  }
  input
  # finding Corrupt lines
  corrupt_chars <- c(']', ')', '}', '>')
  incomplete_lines <- input[!reduce(map(corrupt_chars, ~str_detect(input, fixed(.x))), `|`)]
  incomplete_lines <- stri_reverse(incomplete_lines)
  #final answer
  reduce2(c('(', '{', '[', '<'), 
          c('1', '3', '2', '4'), 
          .init = incomplete_lines, 
          ~ str_replace_all(..1, fixed(..2), ..3)) %>% 
    {.[which(. == (as.double(.) %>% 
                     sort() %>% 
                     median()))]} %>% 
    strtoi(5)
}

answer10_2(input)

