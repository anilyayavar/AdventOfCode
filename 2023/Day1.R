library(tidyverse)
str_extract_all("xabc1nhg2bv5", pattern = "\\d+")
str_c


input1 <- readLines('2023/data/input1.txt')

input1 %>% 
  as_tibble() %>% 
  set_names('calibration') %>% 
  mutate(val = map(calibration, ~ str_extract_all(.x, "\\d") %>% unlist()),
         val1 = map_chr(val, first),
         val2 = map_chr(val, last)) %>% 
  transmute(cal_val = as.integer(str_c(val1, val2))) %>% 
  summarise(ans = sum(cal_val)) %>% 
  pull(ans)

         