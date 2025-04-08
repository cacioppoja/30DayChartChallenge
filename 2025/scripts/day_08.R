#########
# Program: 30DayChartChallenge,  Day 08 - Histogram
# Date Created: 04/08/2025
# Author: Jessi Cacioppo
# Data Source: https://grownative.org/native-plant-database/
########

# Setup
library(tidyverse)
library(glue)
library(camcorder)
library(showtext)
library(ggtext)


dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Sacramento", "Sacramento")
sysfonts::font_add_google("Alice", "Alice")
showtext::showtext_auto()
plot_width <- 8
plot_height <- 6
plot_units <- "in"
gg_record(
  device = "png",
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

# Data previously extracted and cleaned
load(paste0(getwd(), "/2025/data/mo_native_plant_cle.RData"))

# MO native plants palette
color_palette <- c("#7da948", "#D5E1BB", "#E2B842", "#672A65", "#413C58")
names(color_palette) <- c("asparagus", "tea", "gold", "violet", "navy")

source_caption <- glue(
  "Source: Native Plant Database *Grow Native! Missouri Prarie Foundation*"
  )
subtitle <- glue(
  "Native plants are species that occur naturally in a region and are best ",
  "adapted to the region's environment. Growing native plants benefits the environment ",
  "by providing food for birds and pollinators, improving soil health, and ",
  "conserving water. The data points for the histogram are heights of Missouri ",
  "native flowers, where the height is the midpoint of typical size range."
)


# Plot distribution of flower heights
histogram <- plant_df_cle %>% 
  filter(plant_type %in% c("Herbaceous Perennials", "Annuals", "Spring Ephemeral")) %>%
  filter(is.na(size_height_mid_feet) == FALSE) %>% 
  ggplot() +
  geom_histogram(
    aes(x = size_height_mid_inches),
    fill = color_palette["navy"],
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = color_palette["tea"], fill = color_palette["tea"]),
    text = element_text(
      family = "Alice",
      color = color_palette["navy"]
    ),
    plot.title = element_text(
      # color = color_palette["navy"],
      family = "Sacramento",
      size = rel(3),
      margin = margin(0.25, 0, 0.5, 0.25, "cm")
    ),
    plot.subtitle = element_textbox_simple(
      color = color_palette["navy"],
      size = rel(1)
    ),
    plot.caption = element_textbox_simple(
      color = color_palette["navy"],
      margin = margin(0.5, 0, 0, 0, "cm"),
      family = "serif"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 0.25, 1, "cm")
  ) + 
  labs(
    title = "Heights of Missouri Native Plants",
    subtitle = subtitle,
    caption = source_caption
  ) +
  scale_x_continuous(name = "Height (inches)") +
  scale_y_continuous(name = "Frequency")

ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_08.png",
  plot = histogram,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)





