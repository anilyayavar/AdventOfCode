``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(unglue)
```

### Day 1

### [— Day 1: Inverse Captcha —](https://adventofcode.com/2017/day/1)

#### Part-1

``` r
input <- read_lines('input1.txt')

answer1_1 <- function(input){
  # parse input in a better format
  code <- input %>% str_split('') %>% 
    unlist %>% as.integer()
  # answer
  sum(code[lead(code, default = code[1]) == code])
}

answer1_1(input)
```

    ## [1] 1119

#### Part-2

``` r
answer1_2 <- function(input){
  # parse input in a better format
  code <- input %>% str_split('') %>% 
    unlist %>% as.integer()
  # centre point of circle
  n <- seq_len(length(code)/2)
  # answer
  sum(code[c(code[-n], code[n]) == code])
}
answer1_2(input)
```

    ## [1] 1420

### Day 2

### [— Day 2: Corruption Checksum —](https://adventofcode.com/2017/day/2)

#### Part-1

``` r
input <- read_lines('input2.txt')

answer2_1 <- function(input){
  input %>% str_split('\\t') %>% 
  map(as.integer) %>% 
  map_int(~max(.x)-min(.x)) %>% 
  sum()
}

answer2_1(input)
```

    ## [1] 53460

#### Part-2

``` r
answer2_2 <- function(input){
  input %>% str_split('\\t') %>% 
  map(as.integer) %>% 
  map(~ combn(.x, 2)[,apply(combn(.x, 2), 2, FUN = function(a) a[1] %% a[2] == 0 | a[2] %% a[1] == 0)]) %>% 
  map_dbl(~ max(.x)/min(.x)) %>% 
  sum()
}
answer2_2(input)
```

    ## [1] 282

### Day-3

### [— Day 3: Spiral Memory —](https://adventofcode.com/2017/day/3)

#### Part-1

``` r
answer3_1 <- function(input=312051){
  # movement map function
  my_fun <- function(.x){
    if(.x %% 2 == 0){
      rep(c(-1+0i,0-1i), each=.x)
    } else {
      rep(c(1+0i,0+1i), each=.x)
    }
  }
  #we need to figure out that desired number is in floor(sqrt(input)) +1 th circle/spiral outwards
  moves<-c(0+0i,unlist(map(seq_len(floor(sqrt(input)) + 1), my_fun)))
  # position
  pos <- cumsum(moves)
  # access port location on imaginary numbers
  access_port <- pos[input]
  # manhattan distance
  abs(Re(access_port))+abs(Im(access_port))
}

answer3_1()
```

    ## [1] 430

#### Part-2

Don’t forget to check out [this sequence.](https://oeis.org/A141481)

``` r
answer3_2 <- function(input=312051){
  # define helper function
  my_fun <- function(.x){
    if(.x %% 2 == 0){
      rep(c(-1+0i,0-1i), each=.x)
    } else {
      rep(c(1+0i,0+1i), each=.x)
    }
  }
  # find moves similar to part-1
  moves<-c(0+0i,unlist(map(seq_len(floor(sqrt(input)) + 1), my_fun)))
  pos <- cumsum(moves)
  # define a new function to get neighbors
  get_neighbors <- function(.x){
    n1 <- complex(real = Re(.x) +1, imaginary = Im(.x))
    n2 <- complex(real = Re(.x) -1, imaginary = Im(.x))
    n3 <- complex(real = Re(.x), imaginary = Im(.x)+1)
    n4 <- complex(real = Re(.x), imaginary = Im(.x)-1)
    n5 <- complex(real = Re(.x) +1, imaginary = Im(.x)+1)
    n6 <- complex(real = Re(.x) -1, imaginary = Im(.x)-1)
    n7 <- complex(real = Re(.x)-1, imaginary = Im(.x)+1)
    n8 <- complex(real = Re(.x)+1, imaginary = Im(.x)-1)
    unlist(mget(paste0('n',1:8)))
  }
  # let's start the desired sequence
  my_seq <- c(1)
  # first value as counter
  n <- 1
  # while loop
  while(all(my_seq < input)){
    x <- length(my_seq)
    my_seq <- append(my_seq, sum(my_seq[which(pos[seq_len(x)] %in% get_neighbors(pos[x+1]))]))
    n <- n+1
  }
  # answer
  max(my_seq)
}

answer3_2(312051)
```

    ## [1] 312453

### Day-4

### [— Day 4: High-Entropy Passphrases —](https://adventofcode.com/2017/day/4)

#### Part-1

``` r
input <- read_lines('input4.txt')

answer4_1 <- function(input){
  str_split(input, ' ') %>%                                                         # split the input
  map_lgl(~ !any(duplicated(.x))) %>%                                               # count non-duplicated strings using purrr::map_lgl
  sum()
}
answer4_1(input)
```

    ## [1] 383

#### Part-2

``` r
answer4_2 <- function(input){
  str_split(input, ' ') %>% 
  map_lgl(function(.a) !any(duplicated(str_split(.a, '') %>%                      # sort individual strings alphabetically before proceeding
                                         map_chr(~ sort(.x) %>% 
                                                   paste0(collapse = ''))))) %>% 
  sum()
}
answer4_2(input)
```

    ## [1] 265

### Day-5

### [— Day 5: A Maze of Twisty Trampolines, All Alike —](https://adventofcode.com/2017/day/5)

#### Part-1

``` r
input <- read_lines('input5.txt') %>% as.integer()

answer5_1 <- function(input){
  my_seq <- input
  jumps <- 0
  pos <- 1
  # initiate a while loop
  while(pos <= length(my_seq)){
    jumps <- jumps +1
    new_pos <- pos + my_seq[pos]
    my_seq[pos] <- my_seq[pos] +1
    pos <- new_pos
  }
  # answer
  jumps
}
answer5_1(input)
```

    ## [1] 351282

#### Part-2

``` r
answer5_2 <- function(input){
  my_seq <- input
  jumps <- 0
  pos <- 1
  while(pos <= length(my_seq)){
    jumps <- jumps +1
    new_pos <- pos + my_seq[pos]
    # extra condition in while loop
    if (my_seq[pos]<3){
      my_seq[pos] <- my_seq[pos] +1
    } else {
      my_seq[pos] <- my_seq[pos] -1
    }
    pos <- new_pos
  }
  # answer
  jumps
}

answer5_2(input)
```

    ## [1] 24568703

### Day-6

### [— Day 6: Memory Reallocation —](https://adventofcode.com/2017/day/6)

#### Part-1

``` r
input <- read_lines('input6.txt') 

answer6_1 <- function(input){
  # parse input in a better format
  input <- input %>% 
    str_split('\\t') %>% 
    unlist %>% 
    as.integer()
  # initiate while loop
  memory_banks <- list(input)
  while(!any(duplicated(memory_banks))){
    cur_state <- memory_banks[[length(memory_banks)]]
    m <- which.max(cur_state)
    l <- cur_state[m]
    s <- ((seq(m+1, by = 1, length.out=l)-1) %% 16)+1
    cur_state[m] <- 0
    cur_state[s] <- cur_state[s] +1
    memory_banks <- append(memory_banks, list(cur_state))
  }
  # final answer is 1 less than length of our list (memory_banks)
  length(memory_banks)-1
}

answer6_1(input)
```

    ## [1] 6681

#### Part-2

``` r
answer6_2 <- function(input){
  # parse input in a better format
  input <- input %>% 
    str_split('\\t') %>% 
    unlist %>% 
    as.integer()
  # initiate while loop
  memory_banks <- list(input)
  while(!any(duplicated(memory_banks))){
    cur_state <- memory_banks[[length(memory_banks)]]
    m <- which.max(cur_state)
    l <- cur_state[m]
    s <- ((seq(m+1, by = 1, length.out=l)-1) %% 16)+1
    cur_state[m] <- 0
    cur_state[s] <- cur_state[s] +1
    memory_banks <- append(memory_banks, list(cur_state))
  }
  # final answer 
  length(memory_banks) - which.max(map_lgl(memory_banks, ~ all(.x == memory_banks[[length(memory_banks)]])))
}

answer6_2(input)
```

    ## [1] 2392

### Day-7

### [— Day 7: Recursive Circus —](https://adventofcode.com/2017/day/7)

#### Part-1

``` r
input <- read_lines('input7.txt')

answer7_1 <- function(input){
  #patterns for unglue
  patterns <- c('{node=\\w+} ({val=\\d+}) -> {sub_nodes=.*}',
                '{node=\\w+} ({val=\\d+})')
  
  dat <- unglue::unglue_data(input, patterns = patterns)
  # individual sub nodes only
  sub_nodes <- dat$sub_nodes[!is.na(dat$sub_nodes)] %>% 
    str_split(', ') %>% 
    unlist
  # desired value
  dat$node[!dat$node %in% sub_nodes]
}

answer7_1(input)
```

    ## [1] "hlqnsbe"

#### Part-2

### Day-8

### [— Day 8: I Heard You Like Registers —](https://adventofcode.com/2017/day/8)

#### Part-1

``` r
input <- read_lines('input8.txt')

answer8_1 <- function(input){
  # pattern to unglue
  patterns <- '{var=\\w+} {iter=\\w+} {val=[-]?[0-9]+} if {cond=.*}'
  dat <- unglue::unglue_data(input, patterns = patterns)
  vars <- unique(dat$var)
  my_vec <- rep(0, length(vars))
  names(my_vec) <- vars
  
  # modify data
  dat <- dat %>% 
    mutate(cond = gsub('^(\\w+)', 'my_vec[["\\1"]]', cond),
           val = as.numeric(val))
  # for loop to modify my_vec
  for(i in seq_along(dat$cond)){
    if(eval(parse(text = dat$cond[i]))){
      if (dat$iter[i] == 'inc'){
        my_vec[dat$var[i]] <- my_vec[dat$var[i]] + dat$val[i]
      } else {
        my_vec[dat$var[i]] <- my_vec[dat$var[i]] - dat$val[i]
      }
    }
  }
  # answer
  max(my_vec)
}

answer8_1(input)
```

    ## [1] 2971

#### Part-2

``` r
answer8_2 <- function(input){
  # pattern to unglue
  patterns <- '{var=\\w+} {iter=\\w+} {val=[-]?[0-9]+} if {cond=.*}'
  dat <- unglue::unglue_data(input, patterns = patterns)
  vars <- unique(dat$var)
  my_vec <- rep(0, length(vars))
  names(my_vec) <- vars
  my_vec
  # initiate a new vector
  max_val <- 0
  dat <- dat %>% 
    mutate(cond = gsub('^(\\w+)', 'my_vec[["\\1"]]', cond),
           val = as.numeric(val))
  
  # slight modification in for loop to store max value in each successful iteration
  for(i in seq_along(dat$cond)){
    if(eval(parse(text = dat$cond[i]))){
      if (dat$iter[i] == 'inc'){
        my_vec[dat$var[i]] <- my_vec[dat$var[i]] + dat$val[i]
      } else {
        my_vec[dat$var[i]] <- my_vec[dat$var[i]] - dat$val[i]
      }
      max_val <- append(max_val, max(my_vec))
    }
  }
  # answer
  max(max_val)
}

answer8_2(input)
```

    ## [1] 4254

### Day-9

### [— Day 9: Stream Processing —](https://adventofcode.com/2017/day/9)

#### Part-1

#### Part-2
