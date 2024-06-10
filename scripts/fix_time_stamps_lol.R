# clear environment
rm(list = ls()); gc()

# load necessary libraries
pacman::p_load(tidyverse, here, rio, lubridate)


# perhaps a tutorial on how to fix time stamps lol ------------------------

df <- data.frame(
  time = c('1104', '803', '110', '1959', '30'),
  hour = c('11', '8', '1', '19', '0')
)

df <- df %>% 
  mutate(hour_24 = case_when(
    nchar(time) == 4 ~ paste0(substr(time, start = 1, stop = 2), ':', substr(time, start = 3, stop = 4)),
    nchar(time) == 3 ~ paste0('0', substr(time, start = 1, stop = 1), ':', substr(time, start = 2, stop = 3)),
    nchar(time) == 2 ~ paste0('00:', substr(time, start = 1, stop = 2)),
    TRUE ~ time
  )) %>% 
  mutate(hour_24_fixed = hm(hour_24),
         hour_24_fixed2 = strptime(hour_24, format = '%H:%M'),
         hour_24_fixed3 = parse_time(hour_24))

temp = '1104'
temp2 = '803'
