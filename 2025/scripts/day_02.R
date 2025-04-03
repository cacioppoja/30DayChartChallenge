#########
# Program: 30DayChartChallenge,  Day 02 - Slope
# Date Created: 04/02/2025
# Author: Jessi Cacioppo
# Data Source: https://www.nielsen.com/news-center/2024/womens-college-basketball-championship-draws-record-breaking-18-9-million-viewers/
########

# Setup
library(readr)
library(tidyverse)
library(janitor)
library(showtext)
library(camcorder)
library(sysfonts)
library(colorspace)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Public Sans", "Public Sans")
showtext::showtext_auto()
gg_record(
  device = "png",
  width = 6,
  height = 4,
  units = "in",
  dpi = dpi
)

# Data input and cleaning
wbb <- read_delim(file = "C:/USers/Jessica/Documents/R Projects/30DayChartChallenge/2025/data/ncaawbb_viewership.txt")

wbb_cle <- clean_names(wbb) %>% 
  arrange(year) %>% 
  mutate(prev_year_viewers = lag(total_viewers_p2)) %>% 
  mutate(pct_year_change_viewers = (total_viewers_p2 - prev_year_viewers) / prev_year_viewers) %>% 
  mutate(network_tv = ifelse(str_detect(network_s, "ABC|CBS"), 1, 0))
  


# wbb_cle$slope <- 0
# wbb_cle$slope[1:nrow(wbb_cle)] <- 
#   wbb_cle$total_viewers_p2[1:nrow(wbb_cle)] - 
#   c(wbb_cle$total_viewers_p2[2:nrow(wbb_cle)], wbb_cle$total_viewers_p2[nrow(wbb_cle)])


# Set theme aspects
# colors <- data.frame(
#   color = c("blue_bracket", "orange", "black", "blue_logo", "white"),
#   hex_code = c("#0071BD", "#F3841A", "#000000", "#05A5E2", "#FFFFFF")
# )
colors <- c("#0071BD", "#F3841A", "#000000", "#05A5E2", "#FFFFFF")
names(colors) <- c("blue_bracket", "orange", "black", "blue_logo", "white")
colors["black_light"] <- lighten(colors["black"])

wbb_theme <- theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = colors["black_light"]),
    plot.background =  element_rect(fill = colors["black_light"]),
    text = element_text(
      family = "Public Sans",
      color = colors["white"])
  )

plot <- wbb_cle %>% 
  ggplot() +
  geom_line(
    data = wbb_cle[wbb_cle$year < 2020,],
    aes(x=year, y=total_viewers_p2),
    linetype = 1,
    color = colors["white"]
  ) +
  geom_line(
    data = wbb_cle[wbb_cle$year > 2020,],
    aes(x=year, y=total_viewers_p2),
    linetype = 1,
    color = colors["white"]
  ) +
  geom_line(
    data = wbb_cle[2019 <= wbb_cle$year & wbb_cle$year <= 2021,],
    aes(x=year, y=total_viewers_p2),
    linetype = 2,
    color = colors["white"]
  ) +
  geom_point(aes(x=year, y=total_viewers_p2, color=network_tv)) +
  wbb_theme +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 20000000),
    breaks = seq(0, 20000000, 5000000), 
    labels = scales::label_number(suffix = "M", scale = 1e-6),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(1994, 2025),
    breaks = seq(1995, 2025, 5),
    name = NULL
  ) +
  # scale_color_manual(values = c("1" = colors["blue_logo"], "0" = colors["orange"])) +
  guides(color = "none") + 
  labs(
    title = "NCAA Women's Basketball Championship Viewership"
  )

plot


# plot <- wbb_cle %>% 
#   ggplot() +
#   geom_point(aes(x=year, y=pct_year_change_viewers, color=network_s)) +
#   geom_line(aes(x=year, y=pct_year_change_viewers)) +
#   scale_y_continuous(labels = scales::label_percent())
