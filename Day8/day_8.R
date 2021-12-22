library(tidyverse)

input <- read_lines('Day8\\input8.txt')

input[1] %>% as_tibble() %>% 
  separate(value, into = c('inputs', 'outputs'), sep = ' \\| ') %>% 
  mutate(across(everything(), ~str_split(., ' '))) %>% 
  mutate(across(.fns =  ~ map(., function(.z) str_split(.z, ''))),
         across(everything(), ~ map(., function(.a) map(.a, function (.b) length(.b))), 
                .names = 'len_{.col}')) %>% str
