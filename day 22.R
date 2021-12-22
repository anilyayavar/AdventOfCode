library(tidyverse)

# Part-1
# Read Data
data <- unglue::unglue_data(
  read_lines("C:\\Users\\Dell\\Documents\\new 1.txt"),
  "{cond} x={x_min}..{x_max},y={y_min}..{y_max},z={z_min}..{z_max}",
  convert = TRUE)
#data split into rows
split(data, seq(nrow(data))) -> data_sp 
#Answer
tail(data_sp, -1) %>% 
  reduce(.init = cross_df(list(x = seq(data_sp[[1]][['x_min']], data_sp[[1]][['x_max']], 1), 
                               y = seq(data_sp[[1]][['y_min']], data_sp[[1]][['y_max']], 1),
                               z = seq(data_sp[[1]][['z_min']], data_sp[[1]][['z_max']], 1)
                               )),
         ~ if (.y[['cond']] == 'on'){
           .x %>% 
           bind_rows(cross_df(list(x = seq(.y[['x_min']], .y[['x_max']], 1), 
                                   y = seq(.y[['y_min']], .y[['y_max']], 1),
                                   z = seq(.y[['z_min']], .y[['z_max']], 1)
                                   ))
                     ) %>% 
           distinct()}
         else {.x %>% 
           anti_join(cross_df(list(x = seq(.y[['x_min']], .y[['x_max']], 1), 
                                   y = seq(.y[['y_min']], .y[['y_max']], 1),
                                   z = seq(.y[['z_min']], .y[['z_max']], 1)
                                   )),
                     by = c('x', 'y', 'z'))}
         ) %>% 
  filter(x %in% -50:50, y %in% -50:50, z %in% -50:50) %>%                                           # this step may be redundant given that input has been taken for the first few rows only
  nrow()


