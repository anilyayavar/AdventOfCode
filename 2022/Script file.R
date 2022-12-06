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
