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

### [— Day 1: No Time for a Taxicab —](https://adventofcode.com/2016/day/1)

#### Part-1

**Explanation** - I am doing it with the use of complex numbers. Complex
numbers can be interpreted as two dimensional numbers. So I will use
these as location of Santa. Needless to say real part will denote *x*
coordinate and imaginary part will denote *y* coordinate. Since
initially he is facing north, `.init` in `purrr::accumulate` has been
used as 0 + 1*i*.

``` r
input <- readLines('input1.txt')

answer1_1 <- function(input){
  # parse input in a more usable way
  dat <- str_split(input, ', ') %>% unlist
  
  # Define a helper function for purrr::accumulate
  my_fun <- function(.x, .y){
    steps <- as.numeric(gsub('R|L', '', .y))
    .x * if(startsWith(.y, 'R')){
      0-1i 
    } else {
      0+1i 
    }
  }
  # x vector will have directions only
  x <- accumulate(dat, .init = 0+1i, my_fun)
  
  # y vector will have distance travelled
  y <- c(0, as.numeric(gsub('R|L', '', dat)))
  
  # z is an intermediate value
  z <- reduce(x*y, `+`)
  
  # answer
  abs(Re(z))+abs(Im(z))
}
answer1_1(input)
```

    ## [1] 234

#### Part-2

**Explanation** - Here we need complete path travelled. We have
directions to travel, initial point and final points. Using these values
through `seq` of baser, I have build complete locations travelled using
`purrr::map` and `unlist`. Last part is then easy.

``` r
answer1_2 <- function(input){
  # parse input in a more usable way
  dat <- str_split(input, ', ') %>% unlist
  
  # Define a helper function for purrr::accumulate
  my_fun <- function(.x, .y){
    steps <- as.numeric(gsub('R|L', '', .y))
    .x * if(startsWith(.y, 'R')){
      0-1i 
    } else {
      0+1i 
    }
  }
  # x vector will have directions only
  x <- accumulate(dat, .init = 0+1i, my_fun)
  
  # y vector will have distance travelled
  y <- c(0, as.numeric(gsub('R|L', '', dat)))
  
  # locations travelled
  z <- cumsum(x*y)
  
  #complete itinerary
  complete_map <- unlist(map(seq_along(x), ~seq(to=z[.x], by=x[.x], length.out=y[.x])))
  
  # office location
  office <- complete_map[which.max(duplicated(complete_map))]
  
  # answer
  abs(Re(office))+abs(Im(office))
  
}
answer1_2(input)
```

    ## [1] 113

### Day-2

### [— Day 2: Bathroom Security —](https://adventofcode.com/2016/day/2)

#### Part-1

``` r
input <- read_lines('input2.txt')

answer2_1 <- function(input){
  # initial position
  init <- list(r=2, c=2)
  # keypad map
  keypad <- matrix(1:9, 3, byrow = TRUE)
  # parse data
  dat <- str_split(input, '')
  # we need the end positions
  pos <- map_int(dat, length)
  # unlist the dat completely
  dat <- dat %>% unlist
  # define helper function
  my_fun <- function(.x, .y){
    if(.y == 'U'){
      list(r=max(.x[['r']]-1, 1), c=.x[['c']])
    } else if(.y == 'D'){
      list(r=min(.x[['r']]+1, 3), c=.x[['c']])
    } else if(.y == 'R'){
      list(r=.x[['r']], c= min(.x[['c']]+1,3))
    } else {
      list(r=.x[['r']], c= max(.x[['c']]-1,1))
    }
  }
  # positions
  code <- accumulate(dat, .init = init, my_fun)[cumsum(pos) +1]
  # answers
  paste0(map_chr(code, ~keypad[.x$r, .x$c]), collapse = '') %>% 
    as.integer()
}

answer2_1(input)
```

    ## [1] 24862