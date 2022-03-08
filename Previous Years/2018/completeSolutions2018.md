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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

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

``` r
input <- read_lines('input2.txt')

answer2_1 <- function(input){
  twos <- input %>% 
    str_split('') %>% 
    map_lgl(~ 2 %in% table(.x)) %>% 
    sum()
  
  threes <- input %>% 
    str_split('') %>% 
    map_lgl(~ 3 %in% table(.x)) %>% 
    sum() 
  
  twos * threes
}

answer2_1(input)
```

    ## [1] 4693

#### Part-2

``` r
answer2_2 <- function(input){
  # modify input
  input <- input %>% str_split('')
  # initiate while loop to find i
  i <- 1
    while(!any(map_int(input[-i], ~sum(.x == input[[i]])) == 25)){
    i <- i+1
  }
  my_char <- input[[i]]
  my_char2 <- input[-i][[which.max(map_int(input[-i], ~ sum(.x == input[[i]])))]]
  # answer
  paste0(my_char[my_char == my_char2], collapse = '')
}

answer2_2(input)
```

    ## [1] "pebjqsalrdnckzfihvtxysomg"

### Day-3

### [— Day 3: No Matter How You Slice It —](https://adventofcode.com/2018/day/3)

#### Part-1

#### Part-2

### Day-4

### [— Day 4: Repose Record –](https://adventofcode.com/2018/day/4)

#### Part-1

**Explanation** A solution using `dplyr` and `tidyr` strategy.
`data.table::rleid()` has also been used.

``` r
input <- read_lines('input4.txt')

answer4_1 <- function(input){
  # patterns
  patterns <- c('[{time}] Guard #{guard_no=\\d+} {pat}',
                '[{time}] {pat}')
  # unglue data
  dat <- unglue::unglue_data(input, patterns = patterns) %>% 
    mutate(time = ymd_hm(time))
  
  # modify data to intermediate usable format
  dat2 <- dat %>% 
    arrange(time) %>% 
    fill(guard_no, .direction= 'down') %>% 
    mutate(duty = rleid(guard_no)) %>% 
    group_by(duty, guard_no) %>% 
    complete(time = seq.POSIXt(from = min(time), to = max(time), by = 'min')) %>% 
    fill(pat, .direction = 'down') %>% 
    ungroup %>% 
    filter(pat == 'falls asleep') %>% 
    mutate(minute = minute(time))  
  
  # guard number (strategy-1)
  guard <- dat2 %>% 
    group_by(guard_no) %>% 
    summarise(asleep = n()) %>% 
    filter(asleep == max(asleep)) %>% 
    pull(guard_no) %>% 
    as.numeric()
  # desired minute 
  minute <- dat2 %>% 
    filter(guard_no == guard) %>% 
    group_by(minute) %>% 
    summarise(times = n()) %>% 
    filter(times == max(times)) %>% 
    pull(minute)
  # answer
  guard * minute
}
answer4_1(input)
```

    ## [1] 35623

#### Part-2

``` r
answer4_2 <- function(input){
  # patterns
  patterns <- c('[{time}] Guard #{guard_no=\\d+} {pat}',
                '[{time}] {pat}')
  # unglue data
  dat <- unglue::unglue_data(input, patterns = patterns) %>% 
    mutate(time = ymd_hm(time))
  
  # modify data to intermediate usable format
  dat2 <- dat %>% 
    arrange(time) %>% 
    fill(guard_no, .direction= 'down') %>% 
    mutate(duty = rleid(guard_no)) %>% 
    group_by(duty, guard_no) %>% 
    complete(time = seq.POSIXt(from = min(time), to = max(time), by = 'min')) %>% 
    fill(pat, .direction = 'down') %>% 
    ungroup %>% 
    filter(pat == 'falls asleep') %>% 
    mutate(minute = minute(time)) 
  
  # strategy-2 is easier
  dat2 %>% 
  group_by(guard_no, minute) %>% 
  summarise(times_asleep = n(), .groups = 'drop') %>% 
  filter(times_asleep == max(times_asleep)) %>% 
  {as.numeric(.$guard_no)*.$minute}
}

answer4_2(input)
```

    ## [1] 23037

### Day-5

### [— Day 5: Alchemical Reduction —](https://adventofcode.com/2018/day/5)

#### Part-1

``` r
input <- read_lines('input5.txt')

answer5_1 <- function(input){
  # starting polymer
  polymer <- input
  # reactive polymers
  reactive_polymers <- c(paste0(letters, LETTERS), paste0(LETTERS, letters))
  # while loop to reduce polymer to unreactive polymer
  while(any(map_lgl(reactive_polymers , ~str_detect(polymer, .x)))){
    polymer <- reduce(reactive_polymers, 
                      .init = polymer,
                      ~str_remove_all(.x, .y))
  }
  # length of unreactive polymer
  nchar(polymer)
}

answer5_1(input)
```

    ## [1] 11946

#### Part-2

### Day-6

### [](https://adventofcode.com/2018/day/6)

#### Part-1

#### Part-2