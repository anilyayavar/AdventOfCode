# load tidyverse
library(tidyverse)     # we are going to use a couple of purrr functions along with magrittr pipe

#lets define a function
day21_1 <- function(init){                                           # Only argument/input needed is initial positions of both players in a vector of 2 numeric positions
  n <- 1                                                             # First move
  p1 <- 0                                                            # player 1 initial score
  p2 <- 0                                                            # player 2 initial score
  
  while(p1 < 1000 && p2 < 1000){                                     # initiate a while loop
    moves <- (((6 + 9 * (seq(n)-1)) ) %% 10 )                        # create an intermediate vector of length of moves (n)
    score <- split(moves,                                            # split the above vector in two according to odd/even places for both players separately
                   seq(n) %% 2 == 0) %>% 
      map2(init,                                                     # purrr::map2 has been used to create positions first and score at each step
           ~ {p <- (.y + 1 + (cumsum(.x) -1 ) %% 10); 
           f <- (1 + (p -1) %% 10); 
           cumsum(f)}) %>% 
      reduce(rbind)
    if (n %% 2 == 0){
      p1 <- score[n]
    } else {
      p2 <- score[n]
    }
    if (p1 >= 1000 | p2 >= 1000) print(3 * n * score[n-1])           # for every move die is rolled thrice
    n <- n + 1
  }
}
# for sample
day21_1(c(4,8))

# for part-1
day21_1(c(8,2))
