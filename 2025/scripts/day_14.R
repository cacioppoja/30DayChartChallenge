#########
# Program: 30DayChartChallenge,  Day 14 - Kinship
# Date Created: 04/14/2025
# Author: Jessi Cacioppo
# Data Source: https://grownative.org/native-plant-database/,
#              https://plants.usda.gov/
########

# Setup
library(tidyverse)
library(glue)
library(camcorder)
library(showtext)
# library(ggflowchart)
library(igraph)
library(ggforce)
# library(ggtext)
# library(ggh4x)
library(cowplot)
# library(gghighlight)
# library(ggfittext)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Oswald", "Oswald")
sysfonts::font_add_google("Montserrat", "Montserrat")
# sysfonts::font_add_google("Waiting for the Sunrise", "wfts")
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

load(file = paste0(getwd(), "/2025/data/native_plants.RData"))

# color_palette <- c(
#   "#7DA948", "#E2B842", "#CD533B", "#51344D", "#515A47", "#f9f9f4", "#545F66"
# )
# names(color_palette) <- c(
#   "Trees", "Grasses", "Flowers", "Vines", "Shrubs", "White", "Dark"
# )

coneflower <- native_plants %>% 
  filter(
    str_detect(tolower(common_name), "coneflower") |
    str_detect(tolower(latin_name), "echinacea|rudbeckia|ratibida")
  )
  # separate(latin_name, c("Genus", "Species"))


names_taxonomy <- c(
  "Kindom", "Division", "Class", "Order", "Family", "Genus", "Species"
)

data_taxonomy <- c(
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Echinacea - purple coneflower",
  "Echinacea purpurea - eastern purple coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Echinacea  - purple coneflower",
  "Echinacea pallida  - pale purple coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae  - Aster family",
  "Rudbeckia  - coneflower",
  "Rudbeckia laciniata  - cutleaf coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae  - Aster family",
  "Echinacea - purple coneflower",
  "Echinacea simulata  - wavyleaf purple coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Rudbeckia - coneflower",
  "Rudbeckia fulgida - orange coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Echinacea  - purple coneflower",
  "Echinacea paradoxa - Bush's purple coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae  - Aster family",
  "Ratibida - prairie coneflower",
  "Ratibida pinnata - pinnate prairie coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Rudbeckia - coneflower",
  "Rudbeckia missouriensis - Missouri orange coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Rudbeckia - coneflower",
  "Rudbeckia subtomentosa - sweet coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Ratibida - prairie coneflower",
  "Ratibida columnifera  - upright prairie coneflower",
  "Plantae - Plants",
  "Magnoliophyta - Flowering plants",
  "Magnoliopsida - Dicotyledons",
  "Asterales",
  "Asteraceae - Aster family",
  "Rudbeckia - coneflower",
  "Rudbeckia hirta  - blackeyed Susan"
)

taxonomy <- as.data.frame(
  matrix(data_taxonomy, ncol = 7, byrow = T)
)
colnames(taxonomy) <- names_taxonomy


taxonomy <- taxonomy %>% 
  separate_wider_delim(cols = everything(), delim = "-", names_sep = "") %>% 
  rename_with(~ str_replace(., "2", "_common"), ends_with("2")) %>% 
  rename_with(~ str_remove(., "1"), ends_with("1")) %>% 
  mutate(across(names(taxonomy), ~ str_squish(.)))
  
coneflower2 <- coneflower %>%
  inner_join(taxonomy, by = join_by(latin_name == Species), keep = TRUE) %>% 
  arrange(Genus, Species)

coneflower_taxonomy <- coneflower2 %>% 
  select(c(names_taxonomy, common_name))

coneflower_genus <- coneflower_taxonomy %>% 
  select(Genus) %>% 
  distinct() %>% 
  unlist()

layout_genus <- function(genus) {
  outlist <- vector(mode = "list", length(genus))
  for (g in genus) {
    
    flow <- coneflower_taxonomy %>% 
      filter(Genus == g)

    flow <- data.frame(
      from = unlist(flow[,1:(length(names_taxonomy)-1)], use.names = FALSE),
      to = unlist(flow[,2:length(names_taxonomy)], use.names = FALSE)
    )
    flow_distinct <- flow %>%
      distinct()

    flow_distinct <- flow_distinct[6:dim(flow_distinct)[1],]
    
    star <- as.data.frame(
      layout_as_tree(graph_from_data_frame(flow_distinct))
    )
    
    colnames(star) <- c("x", "y")
    
    star$label <- flow_distinct %>% unlist() %>% unique()
    
    star$shape <- rep("petal", nrow(star))
    star$shape[1] <- "center"
    star$text <- paste0(star$shape, "_text")
    star$ytext <- star$y
    star$ytext[1] <- -1
    # star$angle <- star$x*45
    
    star2 <- star %>%
      mutate(
        angle = case_when(
          x == 0 & y == 1 ~ 0,
          x == 0 ~ 90,
          abs(x) == 1 ~ x*55,
          abs(x) == 2 ~ x*10,
          abs(x) == 0.5 ~ x*140,
          abs(x) == 1.5 ~ x*24
        ),
        label = case_when(
          label == g ~ label,
          .default = str_extract(label, ".* (.*)", group = TRUE)
        ) 
      )
    
    # outlist[[g]] <- flow_distinct
    outlist[[g]] <- star2
  }
  return(outlist)
}

genus_flow <- layout_genus(coneflower_genus)

# Color palette

yellow <- c("#577C3F", "#E4A904","#180F08", "#f9f9f4", "black", "white")
names(yellow) <- c("green", "petal", "center", "white", "petal_text", "center_text")
# yellow_text <- c("black", "white")
# names(yellow) <- c("petal", "center")
purple <- c("#577C3F", "#D99AC7", "#180F08", "#f9f9f4", "black", "white")
names(purple) <- c("green", "petal", "center", "white", "petal_text", "center_text")


rud <- genus_flow$Rudbeckia %>% 
  ggplot() +
  # coord_fixed() +
  # xlim(c(-5, 5)) +
  # ylim(c(-5, 5)) +
  geom_ellipse(
    data = genus_flow$Rudbeckia %>% filter(shape == "petal"),
    aes(
      x0 = x,
      y0 = y,
      b = 0.3,
      a = 0.95,
      angle = pi/2,
      color = shape,
      fill = shape
    )
  ) +
  geom_tile(
    data = genus_flow$Rudbeckia %>% filter(shape == "center"),
    aes(
      x = x,
      y = ytext,
      width = 10,
      height = 0.5,
      color = shape,
      fill = shape
    )
  ) +
  geom_text(
    aes(
      x = x,
      y = ytext,
      label = label,
      color = text,
      angle = angle
    ),
    fontface = "bold"
  ) +
  theme_void() +
  scale_color_manual(values = yellow) +
  scale_fill_manual(values = yellow) +
  guides(fill = "none", color = "none") +
  coord_polar()

  

ech <- genus_flow$Echinacea %>% 
  ggplot() +
  # coord_fixed() +
  # xlim(c(-5, 5)) +
  # ylim(c(-5, 5)) +
  geom_ellipse(
    data = genus_flow$Echinacea %>% filter(shape == "petal"),
    aes(
      x0 = x,
      y0 = y,
      b = 0.3,
      a = 0.95,
      angle = pi/2,
      color = shape,
      fill = shape
    )
  ) +
  geom_tile(
    data = genus_flow$Echinacea %>% filter(shape == "center"),
    aes(
      x = x,
      y = ytext,
      width = 10,
      height = 0.5,
      color = shape,
      fill = shape
    )
  ) +
  geom_text(
    aes(
      x = x,
      y = ytext,
      label = label,
      color = text,
      angle = angle
    ),
    fontface = "bold"
  ) +
  theme_void()  +
  scale_color_manual(values = purple) +
  scale_fill_manual(values = purple) +
  guides(fill = "none", color = "none") +
  coord_polar()

rat <- genus_flow$Ratibida %>% 
  ggplot() +
  # coord_fixed() +
  # xlim(c(-5, 5)) +
  # ylim(c(-5, 5)) +
  geom_ellipse(
    data = genus_flow$Ratibida %>% filter(shape == "petal"),
    aes(
      x0 = x,
      y0 = y,
      b = 0.3,
      a = 0.95,
      angle = pi/2,
      color = shape,
      fill = shape
    )
  ) +
  geom_tile(
    data = genus_flow$Ratibida %>% filter(shape == "center"),
    aes(
      x = x,
      y = ytext,
      width = 10,
      height = 0.5,
      color = shape,
      fill = shape
    )
  ) +
  geom_text(
    aes(
      x = x,
      y = ytext,
      label = label,
      color = text,
      angle = angle
    ),
    fontface = "bold"
  ) +
  theme_void() +
  scale_color_manual(values = yellow) +
  scale_fill_manual(values = yellow) +
  guides(fill = "none", color = "none") +
  coord_polar()

flowers <- 
  ggdraw(
    ylim = c(0,1.05),
    xlim = c(0,1.05)
  ) +
  draw_text(
    "Taxonomy of Coneflowers",
    x = 0.015, y = 1, size = 26, hjust = 0,
    color = purple["green"], fontface = "bold", family = "Oswald") +
  draw_text(
    "Flowers commonly known as coneflowers all belong to the Asteraceae family but can be from",
    x = 0.015, y = 0.94, size = 15, hjust = 0,
    color = purple["green"], family = "Oswald") +
  draw_text(
    "three different genera: Rudbeckia, Echinacea, and Ratibida. Echinacea species are typically",
    x = 0.015, y = 0.89, size = 15, hjust = 0,
    color = purple["green"], family = "Oswald") +
  draw_text(
    "purple, such as the purple coneflower (e. purpurea). Rudbeckia and Ratibida are shades of yellow.",
    x = 0.015, y = 0.84, size = 15, hjust = 0,
    color = purple["green"], family = "Oswald") +
  draw_text(
    "The petals on the flowers show Missouri native species for the three genera of coneflowers.",
    x = 0.015, y = 0.79, size = 15, hjust = 0,
    color = purple["green"], family = "Oswald") +
  
  draw_plot(rud, x = -0.2, y = 0.16) +
  draw_plot(ech, x = 0.1, y = -0.1) +
  draw_plot(rat, x = 0.4, y = 0.15) +
  draw_text(
    "Sources:",
    x = 0.015, y = 0.1, size = 8, hjust = 0, vjust = 0,
    color = "#5a5a5a", family = "sans"
  ) +
  draw_text(
    "Native Plant Database, Grow Native! Missouri Prarie Foundation",
    x = 0.015, y = 0.075, size = 8, hjust = 0, vjust = 0,
    color = "#5a5a5a", family = "sans"
  ) +
  draw_text(
    "USDA, NRCS. [2025]. The PLANTS Database",
    x = 0.015, y = 0.05, size = 8, hjust = 0, vjust = 0,
    color = "#5a5a5a", family = "sans"
  ) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )


ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_14.png",
  plot = flowers,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

