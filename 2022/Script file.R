library(tidyverse)
readLines('2022/input1.txt') |> 
  as.numeric() |> 
  as.data.frame() |> 
  set_names('input') |> 
  group_by(grp = cumsum(is.na(input))) |> 
  summarise(food = sum(input, na.rm = T)) |> 
  slice_max(order_by = food)

readLines('2022/input1.txt') |> 
  as.numeric() |> 
  as.data.frame() |> 
  set_names('input') |> 
  group_by(grp = cumsum(is.na(input))) |> 
  summarise(food = sum(input, na.rm = T)) |> 
  slice_max(order_by = food, n=3) |> 
  summarise(sum(food))

############### day-2


readLines('2022/input2.txt') |> 
  as_tibble() |> 
  set_names('turns') |> 
  separate(turns, into = c('Me', "You"))


fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

str_split('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg', '') |> 
  unlist() -> samp

for(i in seq_along(samp)){
  if(length(unique(samp[i:i+3]))==4){
    break
  }
}

input <- readLines('2022/input6.txt') |> str_split('') |> unlist()

i <- 1
while (length(unique(input[i:(i+13)])) < 14) {
  i <- i + 1  
}
i+13
