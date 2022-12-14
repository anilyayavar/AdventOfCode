library(tidyverse)
library(igraph)
library(magrittr)
library(visNetwork)

g <- make_ring(10)
distances(g)
shortest_paths(g, 5, 9) %$% vpath[[1]]
all_shortest_paths(g, 1, 6:8)
mean_distance(g)
plot(g)


############### day-12

input <- readLines('2022/input12.txt')
len <- nchar(input[1])
input %>% 
  str_split('') %>% 
  unlist() %>% 
  {seq_along(.) -1} %>% 
  {(. %% len)+(. %/% len)*(0+1i)} -> input_nodes

names(input_nodes) <- input %>% 
  str_split('') %>% 
  unlist()

input_nodes1 <- input_nodes
from_node <- input_nodes1[names(input_nodes1) == 'S']
to_node <- input_nodes1[names(input_nodes1) == 'E']
from_nodes <- input_nodes1[names(input_nodes1) == 'a']

names(input_nodes) <- ifelse(names(input_nodes) == 'S', 
                             'a', 
                             ifelse(names(input_nodes) == 'E', 
                                    'z',
                                    names(input_nodes)
                                    )
                             )

names(input_nodes) <- match(names(input_nodes), letters)

cross2(input_nodes, input_nodes, .filter = function(x, y) (Mod(x-y)>1 | x==y) ) %>% 
  map_df(setNames, c('from', 'to')) %>% 
  filter(as.integer(names(input_nodes)[match(to, input_nodes)]) - as.integer(names(input_nodes)[match(from, input_nodes)]) <= 1) %>% 
  graph_from_data_frame() -> gg

from_g <- V(gg) %>% names() %>% as.complex() %>% {which(. == from_node)}
to_g <- V(gg) %>% names() %>% as.complex() %>% {which(. == to_node)}

distances(gg, v = from_g, to_g, mode = 'out', algorithm = 'unweighted')

from_g2 <- V(gg) %>% names() %>% as.complex() %>% {which(. %in% from_nodes)}

distances(gg, v = from_g2, to_g, mode = 'out', algorithm = 'unweighted') %>% min()


