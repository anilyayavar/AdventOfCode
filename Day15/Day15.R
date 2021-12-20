# load libraries
library(tidyverse)
library(igraph)

# input data
input_mat <- read_csv('Day15\\data\\input.txt', col_names = F, col_types = 'c')

# initial array size
array_size = nrow(input_mat)

#Define edges
input_mat %>%   
  separate(X1, into = paste0('x_', 0:array_size), sep = '') %>% 
  select(-x_0) %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = 'col', values_to = 'weight', values_transform = list(weight = as.integer)) %>% 
  mutate(col = as.integer(sub('x_', '', col)),
         Ver_2 = complex(real = row, imaginary = col)) %>% 
  select(-(1:2)) -> edges

#row_folding

reduce(1:4, .init = edges,
       ~ edges %>% 
         mutate(Ver_2 = Ver_2 + complex(real = 0, imaginary = 100*.y), 
                weight = ((weight -1 + .y ) %% 9)+1) %>% 
         bind_rows(.x)) -> row_folded

# col folding
reduce(1:4, .init = row_folded,
       ~ row_folded %>% 
         mutate(Ver_2 = Ver_2 + complex(real = 100*.y, imaginary = 0), 
                weight = ((weight -1  + .y ) %% 9)+1) %>% 
         bind_rows(.x)) -> mod_edges

# modify array size to 5 times
array_size = array_size *5 

# new_graph
expand.grid(seq(array_size), seq(array_size)) %>% 
  as_tibble() %>% 
  mutate(Ver_1 = complex(real = Var1, imaginary = Var2),
         ed_1 = Ver_1 - Re(1),
         ed_2 = Ver_1 + Re(1),
         ed_3 = Ver_1 - complex(real = 0L, imaginary = 1L),
         ed_4 = Ver_1 + complex(real = 0L, imaginary = 1L)) %>% 
  select(-starts_with('Var')) %>%
  pivot_longer(-Ver_1, names_to = NULL, values_to = 'Ver_2') %>% 
  filter(Im(Ver_2) %in% seq(array_size), Re(Ver_2) %in% seq(array_size), Ver_1 != Ver_2) %>%
  left_join(mod_edges, by = 'Ver_2') %>% 
  graph_from_data_frame() -> my_g

#distance between first and last node
distances(my_g, v = V(my_g)[1], to = V(my_g)[250000], mode = 'out', weights = E(my_g)$weights)
