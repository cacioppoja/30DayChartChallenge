#########
# Program: 30DayChartChallenge,  Day 11 - Stripes
# Date Created: 04/11/2025
# Author: Jessi Cacioppo
# Data Source: https://grownative.org/native-plant-database/
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
# library(patchwork)
library(cowplot)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Oswald", "Oswald")
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

unique_nature <- plants_cle %>% 
  count(nature_attracting) %>% 
  filter(is.na(nature_attracting) == FALSE) %>% 
  select(nature_attracting) %>% 
  mutate(nature = str_split(nature_attracting, ", ")) %>%
  unnest(cols = c(nature)) %>% 
  select(nature) %>% 
  distinct() %>% 
  unlist()


unique_benefit <- plants_cle %>% 
  count(wildlife_benefit) %>% 
  filter(is.na(wildlife_benefit) == FALSE) %>% 
  select(wildlife_benefit) %>% 
  mutate(benefit = str_split(wildlife_benefit, ", ")) %>%
  unnest(cols = c(benefit)) %>% 
  select(benefit) %>% 
  distinct() %>% 
  unlist()


unique_environ <- plants_cle %>% 
  count(native_environment) %>% 
  filter(is.na(native_environment) == FALSE) %>% 
  select(native_environment) %>% 
  mutate(environ = str_split(native_environment, ", ")) %>%
  unnest(cols = c(environ)) %>% 
  select(environ) %>% 
  distinct() %>% 
  unlist()

plants_cle2 <- plants_cle %>% 
  mutate(
    benefit1 = as.numeric(str_detect(wildlife_benefit, unique_benefit[1])),
    benefit2 = as.numeric(str_detect(wildlife_benefit, unique_benefit[2])),
    benefit3 = as.numeric(str_detect(wildlife_benefit, unique_benefit[3])),
    benefit4 = as.numeric(str_detect(wildlife_benefit, unique_benefit[4])),
    benefit5 = as.numeric(str_detect(wildlife_benefit, unique_benefit[5])),
    benefit6 = as.numeric(str_detect(wildlife_benefit, unique_benefit[6])),
    benefit7 = as.numeric(str_detect(wildlife_benefit, unique_benefit[7]))
  ) %>% 
  mutate(
    nature1 = as.numeric(str_detect(nature_attracting, unique_nature[1])),
    nature2 = as.numeric(str_detect(nature_attracting, unique_nature[2])),
    nature3 = as.numeric(str_detect(nature_attracting, unique_nature[3])),
    nature4 = as.numeric(str_detect(nature_attracting, unique_nature[4]))
  ) %>% 
  mutate(
    environ1 = as.numeric(str_detect(native_environment, unique_environ[1])),
    environ2 = as.numeric(str_detect(native_environment, unique_environ[2])),
    environ3 = as.numeric(str_detect(native_environment, unique_environ[3])),
    environ4 = as.numeric(str_detect(native_environment, unique_environ[4])),
    environ5 = as.numeric(str_detect(native_environment, unique_environ[5])),
    environ6 = as.numeric(str_detect(native_environment, unique_environ[6])),
    environ7 = as.numeric(str_detect(native_environment, unique_environ[7])),
  ) %>% 
  mutate(
    across(benefit1:benefit7, ~ if_else(is.na(.x) == TRUE, 0, .x)),
    across(nature1:nature4, ~ if_else(is.na(.x) == TRUE, 0, .x)),
    across(environ1:environ7, ~ if_else(is.na(.x) == TRUE, 0, .x)),
  ) %>% 
  mutate(
    shade = if_else(str_detect(sun_exposure, "Shade"), 1, 0)
  ) %>% 
  mutate(
    ratio_ht_sp = size_height_mid_inches / size_spread_mid_inches
  ) %>% 
  mutate(
    color1 = str_remove(word(main_color), ",")
  )

plants_cle3 <- plants_cle2 %>% 
  filter(is.na(size_height_mid_inches) == FALSE &
           is.na(size_spread_mid_inches) == FALSE) %>% 
  mutate(plant_type2 = case_when(
    plant_type %in% c("Annuals", "Herbaceous Perennials", "Spring Ephemeral") ~ "Flowers",
    .default = word(plant_type))
  ) %>% 
  mutate(plant_type2 = 
           factor(plant_type2,
                  levels = c("Ferns", "Flowers", "Grasses", "Shrubs", "Vines", "Trees"))
         ) %>% 
  mutate(plant_type_nm = match(plant_type2, levels(plant_type2))) %>% 
  arrange(plant_type_nm, color1)

plants_cle3$ymin <- 0
plants_cle3$ymax <- plants_cle3$size_height_mid_inches
plants_cle3$xmin <- 1:dim(plants_cle3)[1]
plants_cle3$xmax <- 2:(dim(plants_cle3)[1] + 1)

plants_cle3 <- plants_cle3 %>%
  group_by(color1) %>%
  mutate(
    xmin2 = xmin - min(xmin),
    xmax2 = xmax - min(xmax) + 1,
  ) %>%
  ungroup()


# MO native plants palette
color_palette <- c("#7da948", "#D5E1BB", "#E2B842", "#672A65", "#413C58")
color_palette <- c("#5E8746", "#ac8498", "#F9E21F", "#f9f9f4", "#402D0A",
                   "#F0544F", "#b9b7dc", "#818fa0", "#ca6f6c")
names(color_palette) <- c("Green", "Purple", "Yellow", "White", "Brown",
                          "Red", "Lavender", "Blue", "Pink")

plot_stripes <- function(color){
  plot <- plants_cle3 %>% 
    filter(color1 == color) %>%
    filter(plant_type2 == "Flowers") %>%
    ggplot() +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = 50, fill = size_height_mid_inches)
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank()
    ) +
    guides(fill = "none")
    
  if (color == "White"){
    plot2 <- plot +
      scale_fill_gradient(high = "#B1B1B1", low = color_palette[color])
    } else{
      plot2 <- plot +
        scale_fill_gradient(low = "#F7F8F5", high = color_palette[color])
    }
  return(plot2)
}
yellow <- plot_stripes(color = "Yellow")
green <- plot_stripes(color = "Green")
lavender <- plot_stripes(color = "Lavender")
pink <- plot_stripes(color = "Pink")
purple <- plot_stripes(color = "Purple")
red <- plot_stripes(color = "Red")
blue <- plot_stripes(color = "Blue")
white <- plot_stripes(color = "White")

source_caption <- glue(
  "Source: Native Plant Database *Grow Native! Missouri Prarie Foundation*"
)


plot_yellow <- yellow +
  theme_void() +
  geom_curve(
    data = plants_cle3 %>% filter(color1 == "Yellow") %>% filter(plant_type2 == "Flowers") %>% 
      slice_max(size_height_mid_inches, n = 1),
    aes(x = xmin + 3, y = 60,
        xend = xmin + 0.5, yend = 51),
    arrow = grid::arrow(length = unit(0.5, 'lines')), 
    curvature = 0.5, angle = 100, ncp = 10,
    color = "#C6E6B2") +
  geom_curve(
    data = plants_cle3 %>% filter(color1 == "Yellow") %>% filter(plant_type2 == "Flowers") %>% 
      slice_min(size_height_mid_inches, n = 1),
    aes(x = xmin + 3, y = -9,
        xend = xmin + 0.5, yend = -1),
    arrow = grid::arrow(length = unit(0.5, 'lines')), 
    curvature = -0.5, angle = 100, ncp = 10,
    color = "#C6E6B2") +
  theme(
    plot.background = element_rect(color = "#383A45", fill = "#383A45"),
    text = element_text(
      # family = "Alice",
      color = "#F7F8F5"
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "Oswald",
      size = rel(3),
      margin = margin(0.25, 0, 0.5, 0.75, "cm")
    ),
    plot.subtitle = element_textbox_simple(
      # color = color_palette["navy"],
      margin = margin(0.15, 0.75, 0.5, 0.75, "cm")
    ),
    plot.caption = element_textbox_simple(
      # color = color_palette["navy"],
      size = rel(0.75),
      margin = margin(0.5, 0.25, 0, 0.25, "cm"),
      family = "serif"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 0.25, 1, "cm")
  ) + 
  labs(
    title = "Missouri Native Flowers",
    subtitle = "Each stripe represents a different Missouri native yellow flower, where darker yellow stripes are taller flowers and lighter yellow stripes are shorter flowers.",
    caption = source_caption
  )

plot_all <- ggdraw(ylim = c(0,1), # 0-1 bounds make it easy to place viz items on canvas
       xlim = c(0,1)) +
  draw_plot(plot_yellow) + 
  draw_label("Tallest : Cup Plant\nAvg. Height : 8.5 feet",
             fontfamily = "wfts",
             x = 0.3,
             y = 0.71,
             size = 12,
             hjust = 0,
             color = "#C6E6B2") +
  draw_label("Shortest : Barren Strawberry\nAvg. Height : 2.5 inches",
             fontfamily = "wfts",
             x = 0.18,
             y = 0.12,
             size = 12,
             hjust = 0,
             color = "#C6E6B2")


ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_11.png",
  plot = plot_all,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)


