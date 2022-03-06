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
