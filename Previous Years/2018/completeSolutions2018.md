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

### [— Day 1: Chronal Calibration —](https://adventofcode.com/2018/day/1)

#### Part-1

``` r
input <- read_lines('input1.txt')

answer1_1 <- function(input){
  input %>% 
    as.integer() %>% 
    sum()
}

answer1_1(input)
```

    ## [1] 569

#### Part-2

``` r
answer1_2 <- function(input){
  freq <- input %>% as.integer()
  # initiate while loop
  while(!any(duplicated(cumsum(freq)))){
    freq <- append(freq, freq)
  }
  # final answer
  cumsum(freq)[which.max(duplicated(cumsum(freq)))]
}

answer1_2(input)
```

    ## [1] 77666

### Day-2

### [— Day 2: Inventory Management System —](https://adventofcode.com/2018/day/2)

#### Part-1
