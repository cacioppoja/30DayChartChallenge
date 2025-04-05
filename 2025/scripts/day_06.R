#########
# Program: 30DayChartChallenge,  Day 06 - Florence Nightingale
# Date Created: 04/05/2025
# Author: Jessi Cacioppo
# Data Source: https://ourworldindata.org/grapher/share-plastic-waste-recycled
#              https://data-explorer.oecd.org/vis?tenant=archive&df[ds]=DisseminateArchiveDMZ&df[id]=DF_PLASTIC_WASTE_5&df[ag]=OECD&dq=.&pd=2000%2C&to[TIME_PERIOD]=false&vw=tb
########

library(tidyverse)
library(OECD)
library(janitor)

dataset <- "OECD.ENV.EEI,DSD_PW@DF_PW,1.0"
# filter <- "USA1."

df <- get_dataset(dataset, filter = NULL)

df_structure <- get_data_structure(dataset)

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
  mutate(obs_value_numeric = as.numeric(obs_value))
  
world_sum <- df_cle %>% 
  filter(measure == "PW_EL_REGION") %>% 
  filter(freq_label == "Annual") %>% 
  group_by(time_period, plastic_end_life_label) %>% 
  summarise(
    annual_total = sum(obs_value_numeric)
  )

tmp <- df_cle %>% 
  filter(ref_area == "USA") %>% 
  filter(measure == "PW_EL_REGION") %>% 
  filter(plastic_end_life %in% c("REC", "TOTAL"))
  