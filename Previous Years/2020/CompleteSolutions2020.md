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

### [](https://adventofcode.com/2020/day/1)

#### Part-1

#### Part-2

### Day 2

### [](https://adventofcode.com/2020/day/2)

#### Part-1

#### Part-2

### Day 3

### [— Day 3: Toboggan Trajectory —](https://adventofcode.com/2020/day/3)

#### Part-1

``` r
input <- readLines('input3.txt')
answer3_1 <- function(input){
    mat.in <- input %>% 
    str_split('') %>% 
    unlist %>% 
    matrix(nrow = length(input), byrow = TRUE)
  
  sum(map_chr(seq(nrow(mat.in)), ~mat.in[.x, (((.x -1)*3 ) %% ncol(mat.in)) +1 ]) == '#')
}

answer3_1(input)
```

    ## [1] 250

#### Part-2

``` r
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

answer3_2(input)
```

    ## [1] 1592662500

### Day 4

### [](https://adventofcode.com/2020/day/4)

#### Part-1

#### Part-2

### Day 5

### [](https://adventofcode.com/2020/day/5)

#### Part-1

#### Part-2

### Day 6

### [— Day 6: Custom Customs —](https://adventofcode.com/2020/day/6)

#### Part-1

``` r
input <- readLines('input6.txt')
answer6_1 <- function(x){
  
  i1 <- !nzchar(x)
  inp_l <- unname(split(x[!i1], cumsum(i1)[!i1]))
  
  sum(map_int(inp_l, ~reduce(str_split(.x, ''), union) %>% length))
}

answer6_1(input)
```

    ## [1] 6382

#### Part-2

``` r
answer6_2 <- function(x){
  
  i1 <- !nzchar(x)
  inp_l <- unname(split(x[!i1], cumsum(i1)[!i1]))
  
  sum(map_int(inp_l, ~reduce(str_split(.x, ''), intersect) %>% length))
}

answer6_2(input)
```

    ## [1] 3197

### Day 7

### [](https://adventofcode.com/2020/day/7)

#### Part-1

#### Part-2

### Day 8

### [— Day 8: Handheld Halting —](https://adventofcode.com/2020/day/8)

#### Part-1

``` r
input <- read_lines('input8.txt')

answer8_1 <- function(input){
  patterns <- '{op} {amt}'
  dat <- unglue::unglue_data(input, patterns = patterns, convert = TRUE)
  # initial values for iteration
  acc <- 0
  visits <- c()
  cur_state <- 1
  # While loop
  while(! cur_state %in% visits){
    if(dat$op[cur_state] == 'acc'){
      visits <- append(visits, cur_state)
      acc <- acc + dat$amt[cur_state]
      cur_state <- cur_state +1
    } else if (dat$op[cur_state] == 'jmp'){
      visits <- append(visits, cur_state)
      cur_state <- cur_state + dat$amt[cur_state]
    } else {
      visits <- append(visits, cur_state)
      cur_state <- cur_state +1
    }
  }
  # answer
  acc
}

answer8_1(input)
```

    ## [1] 1528

#### Part-2

### Day 9

### [— Day 9: Encoding Error —](https://adventofcode.com/2020/day/9)

#### Part-1

``` r
input <- read_lines('input9.txt') %>% as.numeric()

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
```

    ## [1] 1721308972

#### Part-2

### Day 10

### [](https://adventofcode.com/2020/day/10)

#### Part-1

#### Part-2

### Day 11

### [](https://adventofcode.com/2020/day/11)

#### Part-1

#### Part-2
