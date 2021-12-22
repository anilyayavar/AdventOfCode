#lets define a function
day21_1 <- function(init){
  n <- 1
  
  p1 <- 0
  p2 <- 0
  
  while(p1 < 1000 && p2 < 1000){
    moves <- (((6 + 9 * (seq(n)-1)) ) %% 10 )
    score <- reduce(map2(split(moves, seq(n) %% 2 == 0), init, 
                         ~ {p <- (.y + 1 + (cumsum(.x) -1 ) %% 10); 
                         f <- (1 + (p -1) %% 10); 
                         cumsum(f) }), 
                    function(.a, .b) c(rbind(.a, .b)))
    if (n %% 2 == 0){
      p1 <- score[n]
    } else {
      p2 <- score[n]
    }
    if (p1 >= 1000 | p2 >= 1000) print(3 * n * score[n-1])
    n <- n + 1
  }
}
# for sample
day21_1(c(4,8))

# for part-1
day21_1(c(8,2))
