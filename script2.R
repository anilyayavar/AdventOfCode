library(tidyverse)

vec <- c(1,2,-3,3,-2,0,4)

ind <- seq_along(vec)


cur <- which(ind == 1)

ind <- append(ind[-cur], ind[cur], after = (cur-1+vec[1]) %% length(vec))

vec[ind]
