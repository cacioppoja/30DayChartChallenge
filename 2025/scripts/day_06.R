#########
# Program: 30DayChartChallenge,  Day 06 - Florence Nightingale
# Date Created: 04/06/2025
# Author: Jessi Cacioppo
# Data Source: https://ourworldindata.org/grapher/share-plastic-waste-recycled
#              https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEnvironment%20and%20climate%20change%23ENV%23%7CPlastics%23ENV_PLS%23&pg=0&fc=Topic&bp=true&snb=11&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_PW%40DF_PW&df[ag]=OECD.ENV.EEI&df[vs]=1.0&dq=..A..&isAvailabilityDisabled=false&pd=%2C&to[TIME_PERIOD]=false&lb=bt
########

# Setup
library(tidyverse)
library(OECD)
library(janitor)
library(glue)
library(camcorder)
library(showtext)
library(ggtext)
library(ggpomological)
library(ggimage)
library(ggh4x)


dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Homemade Apple", "Homemade Apple")
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


# Plastic waste end of life from OECD
dataset <- "OECD.ENV.EEI,DSD_PW@DF_PW,1.0"

df <- get_dataset(dataset, filter = NULL)

df_structure <- get_data_structure(dataset)

# Clean the data and pull in the labels
df_cle <- df %>% 
  clean_names() %>% 
  left_join(
    df_structure$CL_FREQ, 
    by = join_by(freq == id),
    keep = FALSE
  ) %>% 
  rename(freq_label = label) %>% 
  left_join(
    df_structure$CL_MEASURE, 
    by = join_by(measure == id),
    keep = FALSE
  ) %>% 
  rename(measure_label = label) %>% 
  left_join(
    df_structure$CL_PLASTIC_END_LIFE, 
    by = join_by(plastic_end_life == id),
    keep = FALSE
  ) %>% 
  rename(plastic_end_life_label = label) %>% 
  left_join(
    df_structure$CL_PLASTIC_RECYCLING, 
    by = join_by(plastic_recycling == id),
    keep = FALSE
  ) %>% 
  rename(plastic_recycling_label = label) %>% 
  left_join(
    df_structure$CL_AREA, 
    by = join_by(ref_area == id),
    keep = FALSE
  ) %>% 
  rename(ref_area_label = label) %>% 
  mutate(obs_value_numeric = as.numeric(obs_value)) %>% 
  mutate(time_period_numeric = as.numeric(time_period))
  
# Combine all ref_area to get the world totals at each time point and type
annual_world <- df_cle %>% 
  filter(measure == "PW_EL_REGION") %>% 
  filter(freq_label == "Annual") %>% 
  group_by(time_period_numeric, plastic_end_life_label) %>% 
  summarise(value = sum(obs_value_numeric)) %>% 
  mutate(ref_area_label = "World")

# Separate ref_areas but combine some groups:
#  Europe - EU in OECD, EU non-OECD, non-EU OECD
#  Asia (excl. China and India) - OECD Asia, Other Asia, non OECD Eurasia
#  America (excl. USA) - Other OECD America, Latin America, Canada
#  Add back in the overall world data
annual_ref_area <- df_cle %>%
  filter(measure == "PW_EL_REGION") %>%
  filter(freq_label == "Annual") %>%
  mutate(
    ref_area_label =
      if_else(str_detect(ref_area_label, "Europe"), "Europe",
        if_else(ref_area_label %in% c("OECD Asia", "Other Asia", "Other non-OECD Eurasia"), "Asia (excl. China & India)",
          if_else(ref_area_label %in% c("Other OECD America", "Latin America", "Canada"), "Americas (excl. USA)", ref_area_label)
        )
      )
  ) %>% 
  group_by(ref_area_label, time_period_numeric, plastic_end_life_label) %>% 
  summarise(value = sum(obs_value_numeric)) %>% 
  bind_rows(annual_world)

# Total for each ref_area
annual_ref_area_total <- annual_ref_area %>% 
  filter(plastic_end_life_label == "Total") %>% 
  rename(value_total = value) %>% 
  select(c(ref_area_label, time_period_numeric, value_total))

# Merge total back with all data to get percents
# Clean up final geographic area labels
annual_ref_area2 <- annual_ref_area %>% 
  left_join(
    annual_ref_area_total, 
    by = c("ref_area_label", "time_period_numeric"),
    keep = FALSE
  ) %>% 
  mutate(percent = value / value_total) %>% 
  mutate(
    ref_area_label = 
      if_else(ref_area_label == "Australia and New Zealand", "Oceania",
        if_else(ref_area_label == "Other Africa", "Sub-Saharan Africa",
          if_else(ref_area_label == "Middle East and North Africa", "Middle East & North Africa",
            if_else(str_detect(ref_area_label, "China \\(P"), "China", ref_area_label)
          )
        )
      )
    ) %>% 
  mutate(ref_area_label = str_wrap(ref_area_label, width =11)) %>% 
  mutate(ref_area_label = factor(ref_area_label, levels = str_wrap(c(
    "Americas (excl. USA)", "Europe", "China", "India", "Asia (excl. China & India)",
    "Middle East & North Africa", "Sub-Saharan Africa", "Oceania","United States",
    "World"), width = 11)
    )
  )

# Plotting

source_caption <- glue("Source: OECD (2024) Plastic Waste - Estimations from 1990 to 2019")

# To offset the 2 plots just enough so that labesl do not overlap
design <- "
  AA##
  AABB
  ##BB
"

# Radial bar plot for share of plastic waste recycled by geographic region
plot <- annual_ref_area2 %>%
  filter(ref_area_label != "World") %>%
  filter(plastic_end_life_label %in% c("Recycled")) %>%
  filter(time_period_numeric %in% c(2000, 2019)) %>%
  ggplot() +
  geom_col(
    aes(
      x = ref_area_label,
      y = percent,
      fill = as.character(time_period_numeric)
    )
  ) +
  scale_fill_manual(values = ggpomological:::pomological_palette[c(2,2)]) +
  scale_y_continuous(
    breaks = seq(0, 0.125, 0.025),
    labels = c("", "", scales::percent(seq(0.05, 0.125, 0.025))),
    expand = c(0,0)
  ) +
  guides(fill = "none") +
  theme_pomological(
    "Homemade Apple"
  ) +
  theme(
    panel.grid.major = element_line(
      linewidth = 0.5,
      linetype = "solid"
    ),
    panel.border = element_blank(),
    plot.margin = margin(0.5, 2, 0.5, 2, "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      color = ggpomological:::pomological_base$black
    ),
    plot.caption = element_text(
      color = ggpomological:::pomological_palette[6],
      hjust = -0.25,
      vjust = -0.25
    ),
    plot.title = element_text(
      hjust = 0.5
    )
  ) +
  coord_radial(r.axis.inside = TRUE, inner.radius = 0) +
  facet_manual(vars(as.character(time_period_numeric)), design = design) +
  labs(
    title = "Share of Plastic Waste that is Recycled in 2000 vs 2019",
    caption = source_caption
  )


plot2 <- ggbackground(plot, ggpomological:::pomological_images("background"))

ggsave(
  filename = "2025/Images/day_06.png",
  plot = plot2,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)

