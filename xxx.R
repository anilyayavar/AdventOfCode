library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(digest)
input <- read_lines('Previous Years/2016/input4.txt')


#######################

input <- read_lines('Previous Years/2016/input6.txt')

########################

input <- read_lines('Previous Years/2018/input6.txt')
patterns <- '#{carpet_id=\\d+} @ {mar_w=\\d+},{mar_h=\\d+}: {carpet_w=\\d+}x{carpet_h=\\d+}'
##################################

dat <- input %>% str_split(', ') %>% 
  map_dfr(~ set_names(as.integer(.x), c('x', 'y')))

min(dat$x)
min(dat$y)
max(dat$x)
max(dat$y)

nrow(dat)


#####################

my_sw <- function(x){
  switch(x, 'jmp' = 'nop', 'nop' = 'jmp', 'acc' = 'acc')
}

my_sw_v <- Vectorize(my_sw, vectorize.args = 'x')
my_sw_v(dat$op[1:5])

sum(dat$op[visits] != 'acc')

########################

input <- read_lines('Previous Years/2020/input9.txt') %>% 
  as.numeric()

input

answer9_1 <- function(input, preamble=25){
  # my preamble
  n <- preamble
  # seq
  ss <- seq(n+1,length(input), by = 1)
  # helper function
  my_fun <- function(.x){
    prev <- seq(.x-n, .x-1, by=1)
    
    min_r <- input[prev] %>% 
      sort() %>% 
      head(2) %>% 
      sum()
    
    max_r <- input[prev] %>% 
      sort() %>% 
      tail(2) %>% 
      sum()
    
    !between(input[.x], min_r, max_r)
  }
  # final answer
  input[ss[map_lgl(ss, my_fun)]]
}

answer9_1(input, 25)

###############
#day21-2020

library(tidyverse)

library(unglue)


input <- read_lines('Previous Years/2018/input7.txt')

#####

input <- 9445
input_grid <- expand.grid(1L:298L, 1L:298L) %>% 
  transmute(x = complex(real=Var1, imaginary = Var2)) %>% 
  pull(x)

grid_n <- function(x, n){
  e <- expand.grid(seq_len(n)-1,seq_len(n)-1)
  x + complex(real=e$Var1, imaginary = e$Var2)
}


power_1 <- function(x){
  rack <- Re(x) + 10
  (((rack * Im(x) +input)*rack) %/% 100) %% 10
}

power_g <- Vectorize(power_1, vectorize.args = 'x')

for(i in 1:300){
  for(j in 1:300){
    for(k in seq_len(301-max(i, j))){
      
    }
  }
}


map_int(input_grid, ~ grid33(.x) %>% 
          power_g() %>% 
          sum() %>% 
          as.integer) %>% 
  which.max() %>% 
  {input_grid[.]} 
