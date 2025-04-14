#########
# Program: 30DayChartChallenge,  Day 13 - Clusters
# Date Created: 04/13/2025
# Author: Jessi Cacioppo
# Data Source: https://grownative.org/native-plant-database/
########

# Setup
library(tidyverse)
library(glue)
library(camcorder)
library(showtext)
library(ggtext)
library(ggh4x)
library(cowplot)
library(gghighlight)
library(ggfittext)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Montserrat", "Montserrat")
sysfonts::font_add_google("Waiting for the Sunrise", "wfts")
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

load(file = paste0(getwd(), "/2025/data/mo_native_plants_cle.RData"))

color_palette <- c(
  "#7DA948", "#E2B842", "#CD533B", "#51344D", "#515A47", "#f9f9f4", "#545F66"
)
names(color_palette) <- c(
  "Trees", "Grasses", "Flowers", "Vines", "Shrubs", "White", "Dark"
)


# Main plot with height vs spread for all plant types
plot_main <- native_plants %>% 
  filter(plant_type2 != "Ferns") %>%
  ggplot() +
  geom_abline(
    aes(slope = 1, intercept = 0),
    linetype = 2,
    color = unlist(ggpomological:::pomological_base)[4]
  ) +
  geom_point(
    aes(
      x = size_spread_mid_feet,
      y = size_height_mid_feet,
      color = plant_type2
    ),
    alpha = 0.75
  ) +
  coord_fixed() +
  guides(
    color = guide_legend(
      position = "bottom",
      title = NULL
    )
  ) +
  xlab("Spread (feet)") +
  ylab("Height (feet)")

# Theme
theme_natives <- function() {
  theme_minimal() +
  theme(
    panel.grid.major = element_line(
      linewidth = 0.5,
      linetype = "solid"
    ),
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0.5, 2, 0.5, 2, "cm"),
    strip.background = 
      element_rect(
        # fill = color_palette["White"],
        # color = color_palette["White"]
        fill = color_palette["Dark"],
        color = color_palette["Dark"]
      ),
    strip.text = 
      element_text(
        color = color_palette["White"],
        face = "bold"
      ),
    axis.title.x = element_text(
      margin = margin(8, 0, 0, 0),
      size = rel(0.75),
      color = color_palette["Dark"]
    ),
    axis.title.y = element_text(
      margin = margin(0, 8, 0, 0),
      size = rel(0.75),
      color = color_palette["Dark"]
    ),
    plot.caption = element_textbox_simple(
      color = ggpomological:::pomological_palette[6],
      size = rel(0.75),
      margin = margin(0.5, 0.25, 0, 0.25, "cm"),
      family = "serif"
    ),
    plot.title = element_text(
      hjust = 0,
      color = color_palette["Dark"],
      family = "Montserrat",
      face = "bold",
      size = rel(1.75),
      margin = margin(0.5, 0, 0.6, 0, "cm")
    ),
    plot.subtitle = element_textbox_simple()
  )
}

# Set the missing facet to R1 C3
design <- "
  AB#
  CDE
"

# Faceted plot for each plant type using gghighlight to show all points on each facet
plot_faceted <- plot_main + 
  gghighlight() +
  facet_manual(vars(plant_type2), design = design) +
  # facet_wrap(~ plant_type2) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(
    limits = c(0, 90)
  ) +
  scale_x_continuous(
    limits = c(0, 90)
  ) +
  theme_natives() +
  labs(
    title = "Height and Spread of Missouri Native Plants",
    caption =  "Source: Native Plant Database *Grow Native! Missouri Prarie Foundation*"
  )

# Text to go in the square in R1 C3
subtitle <- glue(
  "Plants have different height to spread ratios depending on plant type. ",
  "The clusters of plants above the dashed line are taller than they are wide, ",
  "particularly vines. Plants such as flowers and grasses tend to have a more ",
  "equal height to width ratio."
)

# Dataset to make the square
df <- data.frame(
  xmin = 0, ymin = 0, xmax = 83, ymax = 90, label = subtitle
)

# Square plus text for R1C3
plot_subtitle <- 
  ggplot(
    data = df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = subtitle)
  ) +
  geom_rect(
    fill = color_palette["Dark"],
    color = color_palette["Dark"]
  ) +
  geom_fit_text(
    # place = "left",
    reflow = TRUE,
    grow = TRUE,
    color = "white",
    family = "Montserrat",
    padding.x = unit(0.25, "cm"),
    padding.y = unit(0.25, "cm"),
    hjust = 0,
    vjust = 0.5
  ) +
  coord_fixed() +
  scale_y_continuous(
    limits = c(0, 90)
  ) +
  scale_x_continuous(
    limits = c(0, 83)
  ) +
  theme_void()
  
# Assemble the faceted plot and add the square with text
full_plot <-
  ggdraw(
    ylim = c(0,1),
    xlim = c(0,1)
  ) +
  draw_plot(plot_faceted) +
  draw_plot(
    plot_subtitle, scale = 0.38, x = 0.59, y = 0.503, halign = 0, valign = 0
  ) + 
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )

ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_13.png",
  plot = full_plot,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)


  