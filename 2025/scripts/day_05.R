#########
# Program: 30DayChartChallenge,  Day 05 - Ranking
# Date Created: 04/03/2025
# Author: Jessi Cacioppo
# Data Source: https://grownative.org/native-plant-database/?_per_page=-1
########

library(tidyverse)
library(rvest)

source <- "https://grownative.org/native-plant-database/?_per_page=-1"

html <- read_html(source)

alllinks <- html %>% 
  # html_elements(".fwpl-item") %>% 
  html_nodes(".fwpl-item") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  unique()
  
page <- load(file = paste0(getwd(), "/2025/data/mo_native_plant.RData"))

# Ran code to get the data but commenting it out so that it is not accidentally run again
get_plant_database <- function(links){
  page <- vector(mode = "list", length(links))
  for (i in 1:length(links)) {
    page[[i]] <- read_html(links[i])
    Sys.sleep(120)
  }
  return(page)
}
# allpages <- get_plant_database(links=alllinks)
# save(allpages, file = "mo_native_plant_all.RData")

# Read in the saved data
load("2025/data/mo_native_plant.RData")

for (i in 35:40) {
  page[[i]] <- read_html(alllinks[i])
  Sys.sleep(120)
}

# alllinks_html <- read_html(alllinks[1])

# plant <- alllinks_html %>% 
#   html_nodes(".et_pb_text_inner") %>% 
#   html_text()


# basic_description <- alllinks_html %>% 
#   html_elements(".et_pb_post_content_0_tb_body p") %>% 
#   html_text()

clean_plant_database <- function(html_pages){
  
  df <- as.data.frame(matrix(nrow = length(page), ncol=23))
  
  for(j in 1:length(html_pages)){
    
    allinfo <- html_pages[[j]] %>% 
      html_elements(".et_pb_row") %>% 
      html_text2()
    
    # Header info
    headerinfo <- allinfo[str_detect(allinfo, "Plant Type")]
    common_name <- str_extract(headerinfo, "(.*)\n", group=TRUE)
    latin_name <- str_extract(headerinfo, ".*\n(.*)", group=TRUE)
    plant_type <- str_extract(headerinfo, "Plant Type: (.*)\n", group=TRUE)
    native_environment <- str_extract(headerinfo, "Native Environment: (.*)\n", group=TRUE)
    season_of_interest <- str_extract(headerinfo, "Season of Interest: (.*)\n", group=TRUE)
    main_color <-  str_extract(headerinfo, "Main Color: (.*)\n", group=TRUE)
    fall_color <-  str_extract(headerinfo, "Fall Color: (.*)\n", group=TRUE)
    
    # Environmental info (sun, soil, nature attracting, wildlife benefit, animal resis)
    envinfo <- allinfo[str_detect(allinfo, "Sun Exposure \n")]
    sun_exposure <- str_extract(envinfo, "Sun Exposure \n(.*)\nSoil", group = TRUE)
    soil_moisture <- str_extract(envinfo, "Soil\nMoisture\n(.*)\nNature", group = TRUE)
    nature_attracting <- str_extract(envinfo, "Nature Attracting\n(.*)\nWildlife", group = TRUE)
    wildlife_benefit <- str_extract(envinfo, "Wildlife Benefit\n(.*)\nAnimal", group = TRUE)
    animal_resistance <- str_extract(envinfo, "Animal\nResistance\n(.*)", group = TRUE)
    
    # Size info (height, spread)
    sizeinfo <- unique(allinfo[str_detect(allinfo, "Size") & str_detect(allinfo, "Spread")])
    size_height_min <- str_extract(sizeinfo, "Height: (.*) to", group = TRUE)
    # size_height_max <- str_extract(sizeinfo, "Height: .* to\n(.*)\n", group = TRUE)
    size_height_max <- str_extract(sizeinfo, "Height: .* to\n(\\d*)\n.*\nSpread", group = TRUE)
    # size_height_units <- str_extract(sizeinfo, "Height: .* to\n.*\n(.*)\n", group = TRUE)
    size_height_units <- str_extract(sizeinfo, "Height: .* to(\n\\d*)?\n(.*)\nSpread", group = 2)
    size_spread_min <- str_extract(sizeinfo, "Spread: (.*) to", group = TRUE)
    size_spread_max <- str_extract(sizeinfo, "Spread: .* to\n(.*)\n", group = TRUE)
    # size_spread_units <- str_extract(sizeinfo, "Spread: .* to\n.*\n(.*)", group = TRUE)
    size_spread_units <- str_extract(sizeinfo, "Spread: .* to(\n\\d*)?\n(.*)", group = 2)
    
    # Info Extras
    landscape_use <- allinfo[str_detect(allinfo, "Typical Landscape Use")] %>% 
      str_extract("Typical Landscape Use\n(.*)", group = TRUE)
    
    care_instructions <- allinfo[str_detect(allinfo, "Establishment and Care Instructions")] %>% 
      str_extract("Establishment and Care Instructions\n(.*)", group = TRUE)
    
    special_features <- allinfo[str_detect(allinfo, "Special Features")] %>% 
      str_extract("Special Features\n(.*)", group = TRUE)
    
    special_usage <- allinfo[str_detect(allinfo, "Special Usage")] %>% 
      str_extract("Special Usage\n(.*)", group = TRUE)
    
    basic_description <- allinfo[str_detect(allinfo, "Basic Description")] %>% 
      str_extract("Basic Description\n\n(.*)", group = TRUE)

    df[j, ] <- c(
      common_name, latin_name, plant_type, native_environment,
      season_of_interest, main_color, fall_color, sun_exposure,
      soil_moisture, nature_attracting, wildlife_benefit, animal_resistance,
      size_height_min, size_height_max, size_height_units,
      size_spread_min, size_spread_max, size_spread_units,
      landscape_use, care_instructions, special_features, special_usage,
      basic_description
    )
  }
  names(df) <- c(
    "common_name", "latin_name", "plant_type", "native_environment",
    "season_of_interest", "main_color", "fall_color", "sun_exposure",
    "soil_moisture", "nature_attracting", "wildlife_benefit", "animal_resistance",
    "size_height_min", "size_height_max", "size_height_units",
    "size_spread_min", "size_spread_max", "size_spread_units",
    "landscape_use", "care_instructions", "special_features", "special_usage",
    "basic_description"
  )
  return(df)
}

# plant_df <- clean_plant_database(allpages)
plants <- clean_plant_database(allpages)

plants_cle <- plants %>%
  mutate(
    size_height_min_inches =
      if_else(size_height_units == "inches", as.numeric(size_height_min),
        if_else(size_height_units == "feet", as.numeric(size_height_min) * 12, -9)
      ),
    size_height_max_inches =
      if_else(size_height_units == "inches", as.numeric(size_height_max),
              if_else(size_height_units == "feet", as.numeric(size_height_max) * 12, -9)
      ),
    size_height_min_feet =
      if_else(size_height_units == "feet", as.numeric(size_height_min),
              if_else(size_height_units == "inches", as.numeric(size_height_min) / 12, -9)
      ),
    size_height_max_feet =
      if_else(size_height_units == "feet", as.numeric(size_height_max),
              if_else(size_height_units == "inches", as.numeric(size_height_max) / 12, -9)
      ),
    size_spread_min_inches =
      if_else(size_spread_units == "inches", as.numeric(size_spread_min),
              if_else(size_spread_units == "feet", as.numeric(size_spread_min) * 12, -9)
      ),
    size_spread_max_inches =
      if_else(size_spread_units == "inches", as.numeric(size_spread_max),
              if_else(size_spread_units == "feet", as.numeric(size_spread_max) * 12, -9)
      ),
    size_spread_min_feet =
      if_else(size_spread_units == "feet", as.numeric(size_spread_min),
              if_else(size_spread_units == "inches", as.numeric(size_spread_min) / 12, -9)
      ),
    size_spread_max_feet =
      if_else(size_spread_units == "feet", as.numeric(size_spread_max),
              if_else(size_spread_units == "inches", as.numeric(size_spread_max) / 12, -9)
      )
  ) %>% 
  rowwise() %>% 
  mutate(
    size_height_mid_inches = mean(c(size_height_min_inches, size_height_max_inches), na.rm = TRUE),
    size_height_mid_feet = mean(c(size_height_min_feet, size_height_max_feet), na.rm = TRUE),
    size_spread_mid_inches = mean(c(size_spread_min_inches, size_spread_max_inches), na.rm = TRUE),
    size_spread_mid_feet = mean(c(size_spread_min_feet, size_spread_max_feet), na.rm = TRUE)
  ) %>% 
  ungroup()

save(plants_cle, file = "mo_native_plants_cle.RData")
# save(allpages, file = "mo_native_plant_all.RData")

