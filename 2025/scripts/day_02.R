#########
# Program: 30DayChartChallenge,  Day 02 - Slope
# Date Created: 04/02/2025
# Author: Jessi Cacioppo
# Data Source: https://www.nielsen.com/news-center/2024/womens-college-basketball-championship-draws-record-breaking-18-9-million-viewers/
########

library(readr)
library(tidyverse)
library(janitor)

wbb <- read_delim(file = "C:/USers/Jessica/Documents/R Projects/30DayChartChallenge/2025/data/ncaawbb_viewership.txt")
glimpse(wbb)

wbb <- clean_names(wbb)

plot <- wbb %>% 
  ggplot() +
  geom_point(aes(x=year, y=total_viewers_p2, color=network_s))

plot
