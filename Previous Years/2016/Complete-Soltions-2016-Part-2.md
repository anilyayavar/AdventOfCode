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

### Day 8

### [](https://adventofcode.com/2016/day/8)

#### Part-1

### Day-9

### [— Day 9: Explosives in Cyberspace —](https://adventofcode.com/2016/day/9)

#### Part-1

``` r
input <- read_lines('input9.txt')

answer9_1 <- function(input){
  x <- input
  res <- 0
  while(str_detect(x, '\\(')){
    res <- regexpr('\\(', x, perl = TRUE)[[1]] -1 + res
    x <- substr(x, regexpr('\\(', x, perl = TRUE), nchar(x))
    marker <- substr(x, regexpr('\\(', x, perl = TRUE), regexpr('\\)', x, perl = TRUE))
    marker_vals <- as.integer(str_extract_all(marker, '\\d+')[[1]])
    x <- str_remove(x, '\\(\\d+x\\d+\\)')
    res <- res + prod(marker_vals)
    x <- substr(x, marker_vals[1]+1, nchar(x))
  }
  res
}

answer9_1(input)
```

    ## [1] 138735

#### Part-2

``` r
answer9_2 <- function(input){
  x <- input
  res <- rep(1, nchar(x))
  counter <- 1
  # while loop
  while(str_detect(x, '\\(')){
    
    counter <- regexpr('\\(', x, perl = TRUE)[[1]] -1 + counter
    x <- substr(x, regexpr('\\(', x, perl = TRUE), nchar(x))
    marker <- substr(x, regexpr('\\(', x, perl = TRUE), regexpr('\\)', x, perl = TRUE))
    marker_vals <- as.integer(str_extract_all(marker, '\\d+')[[1]])
    x <- str_remove(x, '\\(\\d+x\\d+\\)')
    res[seq(counter, by =1, length.out=nchar(marker))] <- 0
    res[seq(counter+nchar(marker), 
            by = 1, 
            length.out = marker_vals[1])] <- res[seq(counter+nchar(marker), 
                                                     by = 1, 
                                                     length.out = marker_vals[1])] * marker_vals[2]
    counter <- counter + nchar(marker)
  }
  # answer
  sum(res)
}
answer9_2(input)
```

    ## [1] 11125026826
