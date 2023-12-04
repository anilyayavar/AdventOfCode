library(tidyverse)
library(stringi)
input1 <- readLines('2023/data/input1.txt')
# Part-1
input1 %>% 
  as_tibble() %>% 
  set_names('calibration') %>% 
  mutate(val = map(calibration, ~ str_extract_all(.x, "\\d") %>% unlist()),
         val1 = map_chr(val, first),
         val2 = map_chr(val, last)) %>% 
  transmute(cal_val = as.integer(str_c(val1, val2))) %>% 
  summarise(ans = sum(cal_val)) %>% 
  pull(ans)

# Part-2
digits <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
digit <- c(set_names(1:9, digits), set_names(1:9, 1:9))

input1 %>% 
  as_tibble() %>% 
  set_names('calibration') %>% 
  mutate(val = map(calibration, ~ str_extract_all(.x, paste0(c(digits, "\\d"), collapse = "|")) %>% unlist()),
         val1 = digit[map_chr(val, first)],
         val = map(stri_reverse(calibration), ~ str_extract_all(.x, paste0(c(stri_reverse(digits), "\\d"), collapse = "|")) %>% unlist()),
         val2 = digit[stri_reverse(map_chr(val, first))]) %>% 
  mutate(cal_val = val1*10+val2) %>% 
  summarise(ans = sum(cal_val)) %>% 
  pull(ans)

