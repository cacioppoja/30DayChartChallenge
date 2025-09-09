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
library(ggthemes)
library(ggtext)
library(glue)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Public Sans", "Public Sans")
showtext::showtext_auto()
plot_width <- 6
plot_height <- 4
plot_units <- "in"
gg_record(
  device = "png",
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

# Data input and cleaning
wbb <- read_delim(file = "2025/data/ncaawbb_viewership.txt")

wbb_cle <- clean_names(wbb) %>% 
  arrange(year) %>% 
  mutate(prev_year_viewers = lag(total_viewers_p2)) %>% 
  mutate(pct_year_change_viewers = (total_viewers_p2 - prev_year_viewers) / prev_year_viewers) %>% 
  mutate(network_tv = ifelse(str_detect(network_s, "ABC|CBS"), "Network", "Cable"))
  
# Color scheme
colors <- c("#0071BD", "#F3841A", "#000000", "#05A5E2", "#FFFFFF")
names(colors) <- c("blue_bracket", "orange", "black", "blue_logo", "white")
colors["black_light"] <- "#0E0E0E"

colors2 <- colors[names(colors) %in% c("orange", "blue_logo")]
names(colors2) <- c("Network", "Cable")

# Theme
wbb_theme <- theme_stata() +
  theme(
    panel.grid = element_blank(),
    # panel.background = element_rect(fill = colors["white"]),
    # plot.background =  element_rect(fill = colors["white"]),
    text = element_text(
      family = "Public Sans",
      color = colors["black_light"]
    ),
    axis.text.y = element_text(
      angle = 0
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(2, 0, 4, 0),
      size = 10
    ),
    plot.caption = element_textbox_simple(
      margin = margin(4,0,0,0),
      size = 7,
      color = "#878787"
    )
  )

long_subtitle <- glue(
  "The largest single year changes in the past 30 years of the tournament ",
  "occured when the broadcast switched between",
  "<b style='color: {colors2['Network']};'> network</b> and ",
  "<b style='color: {colors2['Cable']};'> cable</b> tv."
)

source <- "https://www.nielsen.com/news-center/2024/womens-college-basketball-championship-draws-record-breaking-18-9-million-viewers/"
source_caption <- glue("Source: Nielsen (2024) <i>Women's College Basketball Championship Game draws record-breaking 18.9 million viewers</i>")

plot <- wbb_cle %>% 
  ggplot() +
  geom_line(
    data = wbb_cle[wbb_cle$year < 2020,],
    aes(x=year, y=total_viewers_p2),
    linetype = 1,
    color = colors["black_light"]
  ) +
  geom_line(
    data = wbb_cle[wbb_cle$year > 2020,],
    aes(x=year, y=total_viewers_p2),
    linetype = 1,
    color = colors["black_light"]
  ) +
  geom_line(
    data = wbb_cle[2019 <= wbb_cle$year & wbb_cle$year <= 2021,],
    aes(x=year, y=total_viewers_p2),
    linetype = 2,
    color = colors["black_light"]
  ) +
  geom_point(
    aes(
      x=year,
      y=total_viewers_p2,
      color=network_tv
    ),
    size = 2
  ) +
  annotate(
    "text",
    label = wbb_cle$network_s[wbb_cle$network_tv=="Network"],
    x = wbb_cle$year[wbb_cle$network_tv=="Network"] - 0.4,
    y = wbb_cle$total_viewers_p2[wbb_cle$network_tv=="Network"],
    color = colors2["Network"],
    size = 2.5,
    hjust="right",
  ) +
  annotate(
    "text",
    label = wbb_cle$network_s[wbb_cle$year == 2009],
    x = wbb_cle$year[wbb_cle$year == 2009],
    y = wbb_cle$total_viewers_p2[wbb_cle$year == 2009] - 1000000,
    color = colors2["Cable"],
    size = 3
  ) +
  annotate(
    "text",
    label = "Canceled due to Covid-19",
    x = 2020,
    y = wbb_cle$total_viewers_p2[wbb_cle$year == 2019] - 1500000,
    color = "#A5A5A5",
    size = 2,
    
  ) +
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
  scale_color_manual(values = colors2) +
  guides(color = "none") + 
  labs(
    title = "NCAA Women's Basketball Championship Viewership",
    subtitle = long_subtitle,
    caption = source_caption
  ) +
  wbb_theme

ggsave(
  filename = "2025/Images/day_02.png",
  plot = plot,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)
