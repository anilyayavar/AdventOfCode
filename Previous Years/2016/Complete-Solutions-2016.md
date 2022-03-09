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
library(caesar)
library(digest)
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

#### Part-2

### Day-3

### [— Day 3: Squares With Three Sides —](https://adventofcode.com/2016/day/3)

#### Part-1

``` r
input <- read_lines('input3.txt')
answer3_1 <- function(input){
  # parse
  dat <- map(str_extract_all(input, '\\d+'), ~ as.integer(.x) %>% sort)
  # answer
  map_lgl(dat, ~ .x[1] + .x[2] > .x[3]) %>% 
    sum
}
answer3_1(input)
```

    ## [1] 983

#### Part-2

``` r
answer3_2 <- function(input){
  # parse data differently
  dat <- map_df(str_extract_all(input, '\\d+'), ~set_names(as.integer(.x), c('num1', 'num2', 'num3')))
  # modify data structure
  dat2 <- dat %>% 
    mutate(r_no = 1 + ((row_number() - 1) %/% 3)) %>% 
    pivot_longer(!r_no) %>% 
    group_by(tri_n = paste(r_no, name, sep = '_')) %>% 
    summarise(points = list(value), .groups = 'drop')
  # answer (valid triangles)
  map(dat2$points, sort) %>% 
    map_lgl(~ .x[1] + .x[2] > .x[3]) %>% 
    sum
}

answer3_2(input)
```

    ## [1] 1836

### Day-4

### [— Day 4: Security Through Obscurity —](https://adventofcode.com/2016/day/4)

#### Part-1

``` r
input <- read_lines('input4.txt')

answer4_1 <- function(input){
  # pattern for unglue
  patterns <- c('{sectors=\\D*}-{sec}[{sec_name}]')
  # data format
  dat <- unglue::unglue_data(input, patterns = patterns, convert = TRUE)
  # answer
  dat$sec[map_lgl(seq_along(input),
                  ~ str_remove_all(dat$sectors[.x], '-') %>% 
                    str_split('') %>% 
                    table() %>% 
                    sort(decreasing = TRUE) %>% 
                    head(5) %>% 
                    names() %>% 
                    paste0(collapse = '') == dat$sec_name[.x])] %>% 
    sum()
}
answer4_1(input)
```

    ## [1] 185371

#### Part-2

``` r
library(caesar)

answer4_2 <- function(input){
  # patterns
  patterns <- c('{sectors=\\D*}-{sec}[{sec_name}]')
  
  dat <- unglue::unglue_data(input, patterns = patterns, convert = TRUE)
  # answer
  dat$sec[map_lgl(seq_along(input),
                  ~ caesar(dat$sectors[.x], 1 + ((dat$sec[.x] -1) %% 26)) %>% 
                    str_replace_all('\\W', ' ') %>% 
                    tolower() %>% 
                    str_detect('north'))]
}

answer4_2(input)
```

    ## [1] 984

### Day-5

### [— Day 5: How About a Nice Game of Chess? —](https://adventofcode.com/2016/day/5)

#### Part-1

``` r
answer5_1 <- function(input='reyedfim'){
  res <- ''
  counter <- 1
  for(i in 1:8){
    counter <- counter +1
    while(!startsWith(digest(paste0(input, as.character(counter)), algo = 'md5', serialize = FALSE), '00000')){
      counter <- counter +1
    }
    res <- append(res, substr(digest(paste0(input, as.character(counter)), algo = 'md5', serialize = FALSE), 6, 6))
  }
  paste0(res, collapse = '')
}
answer5_1()
```

    ## [1] "f97c354d"

### Part-2

``` r
answer5_2 <- function(input='reyedfim'){
  # some initial helper values 
  res <- rep('#', 8)
  names(res) <- as.character(0:7)
  pos <- vector()
  res_vec <- vector()
  counter <- 1
  # double while loop
  while('#' %in% res){
    
    while(!startsWith(digest(paste0(input, as.character(counter)), algo = 'md5', serialize = FALSE), '00000')){
      counter <- counter +1
    }
    hash <- digest(paste0(input, as.character(counter)), algo = 'md5', serialize = FALSE)
    if( substr(hash, 6,6) %in% as.character(0:7) ){
      res[substr(hash, 6, 6)] <- substr(hash, 7, 7)
      pos <- append(pos, substr(hash, 6, 6))
      res_vec <- append(res_vec, substr(hash, 7, 7))
    }
    counter <- counter +1
  }
  # final answer in 3 steps
  ans <- res_vec[!duplicated(pos)]
  names(ans) <- pos[!duplicated(pos)]
  paste0(ans[order(names(ans))], collapse = '')
  
}
answer5_2()
```

    ## [1] "863dde27"

### Day-6

### [](https://adventofcode.com/2016/day/6)

#### Part-1

``` r
input <- read_lines('input6.txt')

answer6_1 <- function(input){
  # helper function
  digit_no <- function(pos){
    map_chr(input, ~ substr(.x, pos, pos)) %>% 
      table() %>% 
      sort(decreasing = TRUE) %>% 
      head(1) %>% 
      names()
  }
  # answer
  map_chr(1:8, digit_no) %>% paste0(collapse = '')
}

answer6_1(input)
```

    ## [1] "ikerpcty"

### Part-2

``` r
answer6_2 <- function(input){
  # helper function modified slightly
  digit_no <- function(pos){
    map_chr(input, ~ substr(.x, pos, pos)) %>% 
      table() %>% 
      sort() %>% 
      head(1) %>% 
      names()
  }
  # answer
  map_chr(1:8, digit_no) %>% paste0(collapse = '')
}

answer6_2(input)
```

    ## [1] "uwpfaqrq"
