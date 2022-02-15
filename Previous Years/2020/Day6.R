samp <- readLines('Previous Years/2020/samp6.txt')

### part-1
answer6_1 <- function(x){
  
  i1 <- !nzchar(x)
  inp_l <- unname(split(x[!i1], cumsum(i1)[!i1]))
  
  sum(map_int(inp_l, ~reduce(str_split(.x, ''), union) %>% length))
}
input <- readLines('Previous Years/2020/input6.txt')

answer6_1(samp)
answer6_1(input)

### part-2
answer6_2 <- function(x){
  
  i1 <- !nzchar(x)
  inp_l <- unname(split(x[!i1], cumsum(i1)[!i1]))
  
  sum(map_int(inp_l, ~reduce(str_split(.x, ''), intersect) %>% length))
}

answer6_2(samp)
answer6_2(input)
