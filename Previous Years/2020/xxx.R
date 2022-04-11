library(tidyverse)


input <- read_lines('Previous Years/2020/input11.txt')


mat <- str_split(input, '') %>% 
  unlist %>% 
  matrix(nrow=length(input), byrow = TRUE)

mat

# Define neighborhood function
neighbors <- function(i, j, input_mat){
  # prepare a grid of neigborhood indices
  g <- setdiff(expand.grid((i-1):(i+1), (j-1):(j+1)), expand.grid(i, j))
  # neighbourhood values
  unlist(pmap(g, ~ tryCatch(input_mat[c(...)[1], c(...)[2]], error = function(e) NULL)))
}

#initial Matrix
mat_1 <- ifelse(mat=='L', '#', mat)

one_iteration <- function(i, j, input_mat){
  if(input_mat[i, j] == '#'){
    if(sum(neighbors(i, j, input_mat=input_mat) == '#') >= 4){
      'L'
    } else{
      '#'
    }
  } else if (input_mat[i, j] == 'L'){
    if(sum(neighbors(i, j, input_mat = input_mat)=='#') == 0){
      '#'
    } else {
      'L'
    }
  } else {
    '.'
  }
}

# Vectorise the above helper function
one_iter_vec <- Vectorize(one_iteration, vectorize.args = c('i', 'j'))

# condition for stable state
my_con <- function(i, j, input_mat){
  if(input_mat[i, j] == '#'){
    sum(neighbors(i, j, input_mat=input_mat) == '#') >= 4
  } else if (input_mat[i, j] == 'L'){
    sum(neighbors(i, j, input_mat = input_mat)=='#') == 0
  } else
    FALSE
}
# vectorize above condition
my_con_vect <- Vectorize(my_con, vectorize.args = c('i', 'j'))

while(any(outer(seq(nrow(mat_1)), seq(ncol(mat_1)), my_con_vect, mat_1))){
  mat_1 <- outer(seq(nrow(mat_1)), seq(ncol(mat_1)), one_iter_vec, mat_1)
}

sum(mat_1 == '#')


### day 13
library(tidyverse)
input <- read_lines('Previous Years/2020/input13.txt')

my_time <- input[1] %>% as.integer()

buses <- input[2] %>% 
  str_split(',') %>% 
  unlist %>% 
  {.[. != 'x']} %>% 
  as.integer()

((ceiling(my_time / buses) * buses) - my_time )

(my_time / buses) %>% 
  ceiling() %>% 
  {. * buses} %>% 
  { . - my_time}
  


?ceiling
trunc(12.3)
ceiling(1:2 + 0.5)
