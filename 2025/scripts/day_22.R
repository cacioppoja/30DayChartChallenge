#########
# Program: 30DayChartChallenge,  Day 22 - Stars
# Date Created: 04/22/2025
# Author: Jessi Cacioppo
# Data Source: https://aa.usno.navy.mil/data/RS_OneYear
########

# Setup
library(tidyverse)
library(janitor)
library(glue)
library(camcorder)
library(showtext)
library(ggHoriPlot)
# library(rnaturalearth)
library(ggtext)
library(cowplot)

dpi <- 600
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Source Sans 3", "source sans 3")
showtext::showtext_auto()
plot_width <- 8
plot_height <- 12
plot_units <- "in"
gg_record(
  device = "png",
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

bg_color <- "#FFFFFF"
text_color <- "#000000"

# Locations for plot
places <- data.frame(
  name = c(
    "Chicago, USA",
    "Mazatlan, Mexico",
    "Quito, Ecuador",
    "Sao Paulo, Brazil",
    "Buenos Aires, Argentina",
    "Nuuk, Greenland",
    "Esperanza, Antarctica"
  ),
  coords = c(
    "41.85, -87.68",
    "23.439, -106.463",
    "0, -78.428",
    "-23.455, -46.630",
    "-34.613, -58.377",
    "64.175, -51.737",
    "-63.397, -56.997"
  )
)

places2 <- places %>% 
  mutate(
    file = str_extract(name, "(.*),", group = 1),
    file = str_remove(tolower(file), " "),
    file = paste0("sunlight_", file, ".txt"),
    coordinates = coords
  ) %>% 
  separate_wider_delim(coords, delim = ", ", names = c("latitude", "longitude")) %>% 
  mutate(
    across(c(latitude, longitude), ~ as.numeric(.))
  )

allcities <- read_delim(file = glue("2025/data/{places2$file}"))
allcities$place <- rep(places2$name, each = 31)

sunlight <- allcities %>% 
  select(place, day, ends_with("set"), ends_with("rise")) %>% 
  mutate(
    across(day:dec_rise, ~ as.character(.))
  ) %>%
  pivot_longer(!c(place, day)) %>% 
  separate_wider_delim(name, delim = "_", names = c("month", "type")) %>% 
  filter(!is.na(value)) %>% 
  separate_wider_regex(
    value, 
    patterns = c(
      hours = "\\d\\d",
      minutes = "\\d\\d$"
    )
  ) %>% 
  mutate(
    time = hms(paste(hours, minutes, "00", sep=":"))
  ) %>% 
  select(-c("hours", "minutes")) %>% 
  pivot_wider(names_from = type, values_from = time) %>% 
  mutate(
    daylight_sec = ifelse(
      set > rise, 
      as.duration(set - rise),
      as.duration( (set + hms("24:00:00")  - rise))
    ),
    daylight_hrs = daylight_sec / (60*60),
    month_nm = match(month, tolower(month.abb)),
    date = mdy(paste(month_nm, day, 2025, sep = "/")),
    place2 = str_replace(place, ", ", ",\n")
  ) %>% 
  inner_join(places2, by = join_by(place == name)) %>% 
  arrange(desc(latitude), place2, date) %>% 
  mutate(place2 = fct_reorder(place2, desc(latitude)))

# Quito has values jumping back and forth which makes chart weird but
# the entire range is 2 minutes. For chart aesthetics, set all equal
sunlight$daylight_hrs[sunlight$place == "Quito, Ecuador"] <- 12.11667
sunlight$daylight_hrs[sunlight$place == "Quito, Ecuador" & sunlight$date >= mdy("12-15-2025")] <- 12.13333
sunlight$daylight_hrs[sunlight$place == "Quito, Ecuador" & sunlight$date <= mdy("01-07-2025")] <- 12.13333


# Colors for horizon plot
color_palette <- c(
  "#FCFFA4FF", "#F5DB4BFF", "#FCAD12FF", "#F78311FF", "#E65D2FFF", "#BB3754FF",
  "#56106EFF", "#000004FF"
)

color_palette_df <- data.frame(
  color = rev(color_palette),
  x = 1:length(color_palette), 
  y = rep(1, length(color_palette)),
  width = rep(1, length(color_palette)),
  height = rep(1, length(color_palette))
)

# Breaks in legend 
legend_text <- data.frame(
  x = 0:length(color_palette) + 0.55,
  y = rep(0.3, length(color_palette)+1),
  text = seq(0, 24, by = 3)
)

legend_palette <- color_palette_df$color
names(legend_palette) <- color_palette_df$color

# Legend plot
legendplot <- ggplot() +
  geom_tile(
    data = color_palette_df,
    aes(x = x, y = y, width = width, height = height, fill = color)
  ) +
  coord_equal() +
  ylim(c(-0.1, 2)) +
  scale_fill_manual(values = legend_palette) +
  geom_text(
    data = legend_text,
    aes(x = x, y = y, label = text),
    size = 10,
    size.unit = "pt",
    color = text_color
  ) +
  theme_void() +
  guides(fill = "none")

# Horizon plot - Offsetting the breaks from the displayed legend labels
#  for better readability. It has to be centered at Quito (12.11667 instead of 12)
horiplot <- sunlight %>% 
  ggplot() +
  geom_horizon(
    aes(x = date, y = daylight_hrs, fill = ..Cutpoints..),
    origin = 0,
    horizonscale = seq(3.11667, 24.11667, by=3)
  ) +
  facet_wrap(~place2, ncol = 1, strip.position = "left") +
  scale_fill_manual(
    values = color_palette
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    text = element_text(
      family = "source sans 3",
      color = text_color
    ),
    panel.spacing.y=unit(0, "lines"),
    strip.text.y.left = element_text(angle = 0, color = text_color, face = "bold"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      color = text_color
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = text_color),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b") +
  geom_vline(xintercept = mdy("03-20-2025"), linetype = 2, color = text_color) +
  geom_vline(xintercept = mdy("06-20-2025"), linetype = 2, color = text_color) +
  geom_vline(xintercept = mdy("09-22-2025"), linetype = 2, color = text_color) +
  geom_vline(xintercept = mdy("12-21-2025"), linetype = 2, color = text_color)
  

# Latitude labels
places3 <- places2 %>% 
  mutate(place2 = str_replace(name, ", ", ",\n")) %>% 
  mutate(place2 = fct_reorder(place2, desc(latitude))) %>% 
  mutate(
    north_south = ifelse(latitude >=0, "N", "S"),
    latitude2 = abs(latitude),
    lat = sprintf("%06.3f", latitude2),
    lat2 = paste(lat, north_south),
    lat2 = ifelse(lat2 == "00.000 N", "0.000 N", lat2)
  )

# Latitude plot
latplot <- ggplot(places3) +
  geom_text(
    aes(y = 0.5, x = 0, label = lat2),
    size = 10,
    size.unit = "pt",
    color = text_color
  ) + 
  theme_void() +
  facet_wrap(~place2, ncol = 1) +
  theme(
    strip.text.x = element_blank(),
    plot.margin = margin(0,0,0,0,unit="pt")
  ) 


# Assemble all plot components together:
#   Title + subtitle, legend, horizonplot + latitude


plot_margin <- 0.025
title <- "Daily Hours of Sunlight Around the Globe for 2025"
subtitle <- "The horizon plot shows the total hours of sunlight per day for several locations. The amount of sunlight in a geographic location depends on the latitude and time of year due to the 23.5° tilt of Earth. The hemisphere tilted toward the sun receives more hours of sunlight than the hemisphere tilted away from the sun. Sunlight per day varies greatly closer to the poles, where days can have 0 hours to 24 hours of sunlight. Near the equator, the days have around 12 hours of sunlight all year."
source <- "Source: Astronomical Applications Department of the U.S. Naval Observatory"

# background
canvas <- grid::rectGrob(
  x = 0, y = 0, 
  width = 12, height = 8,
  gp = grid::gpar(fill = bg_color, alpha = 1, col = bg_color)
)

# Same axis need to align
aligned <- plot_grid(horiplot, latplot, align = "h", axis = "b", rel_widths = c(2.5, 1))

fullplot <- ggdraw(
    ylim = c(0, 1),
    xlim = c(0, 1)
  ) +
  draw_grob(
    canvas,
    x = 0, y = 1,
    height = plot_height, width = plot_width,
    hjust = 0, vjust = 0
  ) +
  draw_plot(
    aligned,
    height = 0.65, y = plot_margin * 4
  ) +
  draw_plot(
    legendplot,
    x = 0.19,
    y = 0.75,
    height = 0.05,
    width = 0.5 - plot_margin * 2
  ) +
  draw_label(
    title,
    x = plot_margin * 2, y = 1 - plot_margin*2,
    size = 20,
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    str_wrap(subtitle, width = 87),
    x = plot_margin * 2, y = 1 - plot_margin * 4,
    size = 12,
    hjust = 0,
    vjust = 1,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    source,
    x = plot_margin * 2, y = plot_margin*2,
    size = 9,
    hjust = 0,
    vjust = 0,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "Vernal Equinox",
    x = 0.25, y = plot_margin * 3.5,
    siz = 9,
    hjust = 0.5,
    vjust = 0,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "Summer Solstice",
    x = 0.4, y = plot_margin * 3.5,
    siz = 9,
    hjust = 0.5,
    vjust = 0,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "Autmnal Equinox",
    x = 0.55, y = plot_margin * 3.5,
    siz = 9,
    hjust = 0.5,
    vjust = 0,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "Winter Solstice",
    x = 0.69, y = plot_margin * 3.5,
    siz = 9,
    hjust = 0.5,
    vjust = 0,
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  geom_segment(
    aes(x = 0.77, y = 0.47, xend = 0.77, yend = 0.72),
    arrow = arrow(type = "closed", length=unit(4, "mm")),
    linewidth = 1,
    color = text_color
  ) +
  geom_segment(
    aes(x = 0.77, y = 0.39, xend = 0.77, yend = 0.14),
    arrow = arrow(type = "closed", length=unit(4, "mm")),
    linewidth = 1,
    color = text_color
  ) +
  draw_label(
    "Equator",
    x = 0.77, y = 0.43,
    size = 10,
    hjust = 0.5,
    vjust = 0,
    fontface = "bold",
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "North Pole",
    x = 0.77, y = 0.73,
    size = 10,
    hjust = 0.5,
    vjust = 0,
    fontface = "bold",
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  ) +
  draw_label(
    "South Pole",
    x = 0.77, y = 0.12,
    size = 10,
    hjust = 0.5,
    vjust = 0,
    fontface = "bold",
    fontfamily = "source sans 3",
    color = text_color,
    lineheight = 1
  )

  
  
ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_22.png",
  plot = fullplot,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)
