#########
# Program: 30DayChartChallenge,  Day 09 - Diverging
# Date Created: 04/08/2025
# Author: Jessi Cacioppo
# Data Source: https://www.baseball-reference.com/teams/CHC/index.shtml
########

# Setup
library(readr)
library(tidyverse)
library(janitor)
# library(stringr)
library(glue)
library(camcorder)
library(showtext)
library(ggtext)


dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Sacramento", "Sacramento")
sysfonts::font_add_google("Alice", "Alice")
showtext::showtext_auto()
plot_width <- 6
plot_height <- 8
plot_units <- "in"
gg_record(
  device = "png",
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

# Data input and cleaning
cubs <- read_delim(file = "2025/data/cubs_record.txt", delim = ",")

width <- 0.3
cubs_cle <- cubs %>% 
  clean_names() %>% 
  mutate(diff_500 = w_l_percent - 0.500) %>% 
  mutate(
    xmin = if_else(diff_500 < 0, diff_500, 0),
    xmax = if_else(diff_500 < 0, 0, diff_500),
    ymin = year - width,
    ymax = year + width,
    group = if_else(diff_500 <=0, "red", "blue"),
    playoff_type = str_extract(playoffs, ".*(NLCS|NLDS|NLWC|WS).*", group = 1),
    playoff_win = if_else(str_detect(playoffs, "(Tied in|Won) WS"), "Won", "Lost"),
    playoff_color = if_else(playoff_win == "Won", "#FFBF00", "#9D957E"),
    wildcard = if_else(year %in% c(2018, 2015, 1998), 1, 0),
    division = if_else(year %in% c(2020, 2017, 2007, 2003), 1, 0)
  ) %>% 
  filter(tm == "Chicago Cubs") %>% 
  filter(year <= 2024)

color_palette <- c("#0E3386", "#CC3433")
names(color_palette) <- c("blue", "red")

text_color <- c("#FFBF00", "#9D957E")
names(text_color) <- c("#FFBF00", "#9D957E")

cubs_cle %>% 
  ggplot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group)
  ) +
  geom_text(
    data = cubs_cle %>% filter(is.na(playoff_type) == FALSE),
    aes(x = 0.3, y = year, label = playoff_type, color = playoff_color),
    # position = position_jitter(width = 0.1),
    size = 6,
    size.unit = "pt"
  ) +
  geom_vline(
    xintercept = 0,
    color = "#000000",
    linewidth = 0.1
  ) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = text_color) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(1900, 2025)
  ) +
  guides(
    fill = "none",
    color = "none") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  )

