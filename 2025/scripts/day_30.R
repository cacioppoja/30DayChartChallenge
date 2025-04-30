#########
# Program: 30DayChartChallenge,  Day 30 - National Geographic
# Date Created: 04/30/2025
# Author: Jessi Cacioppo
# Data Source: https://www.iucnredlist.org/resources/summary-statistics
########

library(tidyverse)
library(glue)
library(camcorder)
library(showtext)
library(scales)
library(ggtext)

dpi <- 400
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Alatsi", "Alatsi")
sysfonts::font_add_google("Roboto", "Roboto")
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

fontmajor <- "Alatsi"
fontminor <- "Roboto"
highlight <- "#FFD800"
lowlight <- "#F2F2F2"
dark <- "#000000"
light <- "#FFFFFF"

threatened <- data.frame(
    vertebrate = c("Mammals", "Birds", "Reptiles", "Amphibians", "Fishes", "Total\nVertebrates"),
    lower = c(0.23, 0.12, 0.18, 0.36, NA, 0.18),
    best = c(0.27, 0.12, 0.21, 0.41, NA, 0.21),
    upper = c(0.36, 0.12, 0.33, 0.47, NA, 0.31),
    described = c(0.89, 1, 0.83, 0.9, 0.78, 0.84)
) %>% 
  mutate(
    order = ifelse(is.na(best), 0, ifelse(vertebrate == "Total\nVertebrates", 1, best)),
    vertebrate = fct_reorder(vertebrate, order),
    best_pct = scales::percent(best, accuracy = 1, trim = FALSE, suffix = "")
  )

subtitle <- glue(
  "<span style = 'font-size: 13pt'><b>Estimated % threatened vertebrate species in 2025</b></span>",
  "<br><br>",
  "Species classified as <b>Threatened</b> are listed as <b>Critically Endangered</b>, ",
  "<b>Endangered</b>, or <b>Vulnerable</b>. At least 80% of species (",
  "<span style = 'color: {highlight}; font-size: 12px;'>",
  "<span style = 'font-family: \"Font Awesome 6\"'>&#xf219;</span>",
  "</span>",
  ") within a group must ",
  "be evaluated for a species to be classified. Groups with < 80% of species evaulated (",
  "<span style = 'color: {dark}; font-size: 12px;'>",
  "<span style = 'font-family: \"Font Awesome 6\"'>&#xf219;</span>",
  "</span>",
  ") cannot be classified because it is unknown if ",
  "Data Deficient (DD) species are threatened or not.<br><br>",
  "<span style = 'font-size: 9px;'>",
  "<b>Lower Est:</b> % if all DD species are not threatened ",
  "<span style = 'color: {light}'>__</span>",
  "<b>Best Est:</b> % if DD species are equally threatened as data sufficient species",
  "<span style = 'color: {light}'>++</span>",
  "<b>Upper Est:</b> % if all DD species are threatened",
  "</span>"
)

text_annotations <- threatened %>%
  filter(vertebrate == "Total\nVertebrates") %>% 
  pivot_longer(c(lower, best, upper)) %>% 
  mutate(
    y = ifelse(name == "best", nrow(threatened) + 0.12, nrow(threatened)),
    x = ifelse(name == "best", value + 0.012, value),
    name = glue("{str_to_title(name)}\n Est.")
  ) %>% 
  add_row(
    name = "> 80% evaluated",
    x = 0.86,
    y = nrow(threatened) + 0.15,
    vertebrate = "Total\nVertebrates"
  ) %>% 
  add_row(
    name = "< 80% evaluated",
    x = 0.74,
    y = 1,
    vertebrate = "Fishes"
  )

  
plot <- ggplot(data = threatened) +
  geom_vline(
    aes(xintercept = 0.8),
    color = colorspace::darken(lowlight, amount = 0.5),
    linetype = 2
  ) +
  geom_segment(
    aes(x = lower, xend = upper, y = vertebrate),
    color = highlight,
    linewidth = 3
  ) +
  geom_point(
    aes(y = vertebrate, x = best),
    size = 6,
    color = dark
  ) +
  geom_point(
    data = threatened %>% filter(is.na(best) == FALSE),
    aes(y = vertebrate, x = described),
    size = 3,
    color = dark,
    fill = highlight,
    shape = 23
  ) +
  geom_point(
    data = threatened %>% filter(is.na(best) == TRUE),
    aes(y = vertebrate, x = described),
    size = 3,
    color = dark,
    fill = dark,
    shape = 23
  ) +
  geom_text(
    aes(y = vertebrate, x = best, label = best_pct),
    color = light,
    family = fontminor,
    size = 3,
    fontface = "bold"
  ) +
  geom_text(
    data = text_annotations,
    aes(x = x, y = y, label = name),
    color = dark,
    nudge_x = 0,
    nudge_y = 0.32,
    size = 2
  ) +
  scale_y_discrete(
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0,1)
  ) +
  theme(
    text = element_text(
      size = rel(4),
      color = dark,
      family = fontminor,
      face = "bold"
    ),
    plot.background = element_rect(
      fill = light,
      color = highlight,
      linewidth = 5
    ),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(
      fill = lowlight,
      color = NA
    ),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = light
    ),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = fontmajor,
      color = dark,
      face = "bold",
      size = rel(6.5),
      margin = margin(0, 0, 0.5, 0, "cm")
    ),
    plot.subtitle = element_textbox_simple(
      family = fontminor,
      color = dark,
      face = "plain",
      size = rel(2.85),
      margin = margin(0, 0, 0.5, 0.1, "cm")
    ),
    plot.caption = element_text(
      family = fontmajor,
      color = dark,
      size = rel(1.5),
      hjust = 0,
      margin = margin(0.5, 0, -0.5, 0, "cm"),
    ),
  ) +
  labs(
    title = "Global Extinction Risk for Vertebrate Species",
    subtitle = subtitle,
    caption = "Source: IUCN (International Union for Conservation of Nature) 2025. https://www.iucnredlist.org/resources/summary-statistics"
  )

ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_30.png",
  plot = plot,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)
