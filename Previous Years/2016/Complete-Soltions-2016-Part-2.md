``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

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
