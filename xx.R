library(tidyverse)
library(unglue)
library(adagio)
library(igraph)
input <- read_lines('Previous Years/2015/input9.txt')
patterns <- '{V1} to {V2} = {dist}'
dat <- unglue_data(input, patterns = patterns, convert = TRUE)

g <- graph_from_data_frame(dat, directed = FALSE)
plot(g)
E(g)$weight <- dat$dist
hamiltonian(E(g), cycle = FALSE)
V(g)
input
patterns <- c('{var1=[a-z0-9]*} {oper=[A-Z]*} {var2=[a-z0-9]+} -> {assign_var=[a-z]+}', 
              '{oper=[A-Z]*} {var2=[a-z0-9]+} -> {assign_var=[a-z]+}',
              '{var2=[a-z0-9]+} -> {assign_var=[a-z]+}')

unglue_data(input, patterns = patterns)
input[7]


nchar("\\x27")

strwidth("aaa\"aaa")

install.packages('adagio')


hamiltonian()