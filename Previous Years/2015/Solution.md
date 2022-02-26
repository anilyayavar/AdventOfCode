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
library(digest)
```

## Day 1

### [Not Quite Lisp](https://adventofcode.com/2015/day/1)

#### part-1

``` r
input <- readLines('input1.txt')
```

    ## Warning in readLines("input1.txt"): incomplete final line found on 'input1.txt'

``` r
# part-1
answer1_1 <- function(input){
  input_vec <- input %>% str_split('') %>% unlist
  floors <- c(-1, 1)[1+(input_vec == '(')]
  sum(floors)
}

answer1_1(input)
```

    ## [1] 74

#### part-2

``` r
answer1_2 <- function(input){
  input_vec <- input %>% str_split('') %>% unlist
  floors <- c(-1, 1)[1+(input_vec == '(')]
  # answer
  which.max(cumsum(floors)<0)
}

answer1_2(input)
```

    ## [1] 1795

## Day2

### [I Was Told There Would Be No Math](https://adventofcode.com/2015/day/2)

#### part-1

``` r
input <- readLines('input2.txt')

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
```

    ## [1] 1606483

#### part-2

``` r
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
```

    ## [1] 3842356

### Day 3

### [Perfectly Spherical Houses in a Vacuum](https://adventofcode.com/2015/day/3)

#### Part-1

``` r
input <- readLines('input3.txt')
```

    ## Warning in readLines("input3.txt"): incomplete final line found on 'input3.txt'

``` r
## part-1
answer3_1 <- function(input){
  input_vec <- input %>% 
    str_split('') %>% 
    unlist
  
  input_vec <- ifelse(input_vec== '>', 1, 
                      ifelse(input_vec == '<', -1, 
                             ifelse(input_vec == '^', 1i, -1i)))
  
  input_vec <- c(0+0i, input_vec)
  # answer
  length(unique(cumsum(input_vec)))
}

answer3_1(input)
```

    ## [1] 2572

#### Part-2

``` r
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
```

    ## [1] 2631

### Day-4

### [The Ideal Stocking Stuffer](https://adventofcode.com/2015/day/4)

#### Part-1

``` r
answer4_1 <- function(first_part){
  second_part <- 1
  while(!startsWith(digest(paste0(first_part, second_part), algo = 'md5', serialize = FALSE), '00000')){
    second_part <- second_part + 1
  }
  second_part
}

answer4_1('ckczppom')
```

    ## [1] 117946

#### Part-2

``` r
answer4_2 <- function(first_part){
  second_part <- 1
  while(!startsWith(digest(paste0(first_part, second_part), algo = 'md5', serialize = FALSE), '000000')){
    second_part <- second_part + 1
  }
  second_part
}

answer4_2('ckczppom')
```

    ## [1] 3938038

### Day 5

### [Doesn’t He Have Intern-Elves For This?](https://adventofcode.com/2015/day/5)

#### Part-1

``` r
input <- readLines('input5.txt')
answer5_1 <- function(input){
  # vowels
  rule1_st <- c('a', 'e', 'i', 'o', 'u')
  # check for at least 3 vowels
  rule1 <- map_lgl(str_split(input, '') , ~ sum(.x %in% rule1_st) >= 3)
  # check for repeated alphabets
  rule2 <- map_lgl(str_split(input, ''), function(.a)any(map_lgl(seq(length(.a)-1), ~.a[.x]==.a[.x+1])))
  # rule 3 exclusion strings
  rule3_st <- c('ab', 'cd', 'pq', 'xy')
  # check for exclusions
  rule3 <- !map_lgl(input, function(.a)any(map_lgl(rule3_st, ~str_detect(.a, .x))))
  # answer (nice strings)
  sum(rule1 & rule2 & rule3)
}

answer5_1(input)
```

    ## [1] 258
