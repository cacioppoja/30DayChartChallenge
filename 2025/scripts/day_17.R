#########
# Program: 30DayChartChallenge,  Day 17 - Birds
# Date Created: 04/15/2025
# Author: Jessi Cacioppo
# Data Source: https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/wild-birds
########

# Setup
library(tidyverse)
library(janitor)
library(glue)
library(camcorder)
library(showtext)
library(urbnmapr)
library(ggtext)

dpi <- 300
showtext_opts(dpi = dpi)
sysfonts::font_add_google("Montserrat", "Montserrat")
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

flu <- read_delim(file = "2025/data/HPAI Detections in Wild Birds.csv") %>% 
  clean_names()

glimpse(flu)
skimr::skim(flu)

# Review the 52 states
flu %>% count(state) %>% print(n=55)

# Correct issue of lower case vs proper case for michigan. There are 50 states + DC.
# 7 cases of date_detected > collection_date, unclear why so just assume they were flipped
# When collection date is missing use detection
flu2 <- flu %>% 
  mutate(
    state = tolower(state),
    county = tolower(county),
    collection_date = ifelse(collection_date == "null", NA, collection_date),
    date_collected = mdy(collection_date),
    date_detected = mdy(date_detected), 
    tmp_detected = date_detected
  ) %>% 
  mutate(
    date_detected = case_when(
      tmp_detected < date_collected ~ date_collected,
      .default = date_detected
    ),
    date_collected = case_when(
      tmp_detected < date_collected ~ tmp_detected,
      is.na(date_collected) ~ tmp_detected,
      .default = date_collected
    ),
    county = ifelse(county == "chugach", "valdez-cordova", county),
    county = ifelse(county == "wade hampton", "kusilvak", county),
    county = str_remove(county, ", city and  of"),
    county = str_remove(county, ", town and  of"),
    county = str_remove(county, ", consolidated municipality of"),
    county = str_remove(county, " parish"),
    county = case_when(
      state == "louisiana" & county == "lasalle" ~ "la salle",
      state == "kansas" & county == "pottawattamie" ~ "pottawatomie",
      state == "minnesota" & county == "le sneur" ~ "le sueur",
      state == "minnesota" & county == "mile lacs city" ~ "mile lacs",
      state == "new york" & county == "rensselear" ~ "rensselaer",
      state == "pennsylvania" & county == "bucks county" ~ "bucks",
      state == "virginia" & county == "matthews" ~ "mathews",
      state == "iowa" & county == "fulton" ~ "jackson",
      state == "virginia" & county == "norfolk city" ~ "norfolk",
      .default = county
    )
  ) %>% 
  select(-c(tmp_detected, collection_date))

flu_county <- flu2 %>% 
  count(state, county)

# Shape files for USA
states_sf <- get_urbn_map(map = "states", sf = TRUE)
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)

counties_sf2 <- counties_sf %>% 
  mutate(
    county_name = tolower(str_remove(county_name, " County")),
    state_name = tolower(state_name),
    county_name = case_when(
      county_name == "aleutians west census area" ~ "aleutians west",
      county_name == "anchorage municipality" ~ "anchorage",
      county_name == "bethel census area" ~ "bethel",
      county_name == "dillingham census area" ~ "dillingham",
      county_name == "haines borough" ~ "haines",
      county_name == "juneau city and borough" ~ "juneau",
      county_name == "valdez-cordova census area" ~ "valdez-cordova",
      county_name == "chesapeake city" ~ "chesapeake",
      county_name == "hampton city" ~ "hampton",
      county_name == "norfolk city" ~ "norfolk",
      county_name == "virginia beach city" ~ "virginia beach",
      county_name == "waynesboro city" ~ "waynesboro",
      .default = str_remove(county_name, " borough"),
    ),
    county_name = str_remove(county_name, " census area"),
    county_name = str_remove(county_name, " city and"),
    county_name = str_remove(county_name, " parish")
  )

# Join in the flu data
counties_flu <- counties_sf2 %>% 
  left_join(flu_county, by = join_by(state_name == state, county_name == county))
  
plot <- counties_flu %>%
  ggplot() +
  geom_sf(mapping = aes(fill = n), color = "white") +
  geom_sf(data = states_sf, fill = NA, color = "#404040", size = 0.25) +
  coord_sf(datum = NA) +   
  scale_fill_gradient(
    name = "Caes", 
    # trans = "log",
    low='#C6DBEF',
    high='#053472', 
    na.value="#FFFFFF",
    breaks = c(1, 100, 200, 300, 389),
    labels = c(1, 100, 200, 300, 390)
  )

flu2 %>% 
  count(bird_species) %>% 
  arrange(desc(n))

plot2<- plot + 
  theme_bw() + 
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position="bottom", 
    panel.border = element_blank(),
    text = element_text(family = "Montserrat"),
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold",
      size = rel(1.5),
      margin = margin(0.5, 0.5, 0.25, 0.5, "cm")
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0,
      margin = margin(0.1, -0.25, 0, 0.5, "cm")
    ),
    plot.caption = element_text(
      size = rel(0.75),
      hjust = 0,
      margin = margin(0.5, 0.25, 0, 0.25, "cm"),
    ),
  ) +
  labs(
    title = "Avian Influenza Cases in the United States",
    subtitle = glue("The map shows the cumulative number per county of bird flu cases detected in wild birds from January 2022 tp April 2025. Counties in white had no detected cases. A total of <b>{scales::comma(nrow(flu2))}</b> cases have been detected."),
    caption = "Source: USDA Detections of Highly Pathogenic Avian Influenza in Wild Birds"
  )

ggsave(
  filename = "C:/Users/Jessica/Documents/R Projects/30DayChartChallenge/2025/Images/day_17.png",
  plot = plot2,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = dpi
)
