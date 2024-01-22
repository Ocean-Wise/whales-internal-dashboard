####~~~~~~~~~~~~~~~~~~~~~~Monthly Dashboard Metrics~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the production of metrics for the ELT dashboard. Should be able to be run by anyone in the team.
## Date written: 2024-01-19
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Run this section first
library(magrittr)

user = "AlexMitchell"

source(file = "./data-processing.R")


####~~~~~~~~~~~~~~~~~~~~~~# WRAS alerts vs 2023~~~~~~~~~~~~~~~~~~~~~~~####
### Goal...
## Send a total of 15,000 WRAS alerts (at least a 10% increase from 2023 baseline) in BC and WA waters to reduce 
## the risk of ship strike for at least 15,000 encounters with whales

## total alerts and percentage difference between 2023 and 2024
perc_diff = alert_clean %>% 
  dplyr::group_by(year = lubridate::year(sent_at), month = lubridate::month(sent_at)) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::filter(year == 2023 | year == 2024) %>% 
  tidyr::pivot_wider(names_from = year, values_from = count) %>% 
  dplyr::mutate(`2023` = cumsum(`2023`)) %>% 
  dplyr::mutate(`2024` = cumsum(`2024`)) %>% 
  dplyr::mutate(perc_diff = ((`2024`-`2023`)/`2024`)*100)


####~~~~~~~~~~~~~~~~~~~~~~total number of acartia alerts~~~~~~~~~~~~~~~~~~~~~~~####


acartia = detections_clean %>% 
  dplyr::filter(!sourceEntity == "JASCO" & is.na(sourceEntity) == F)

acartia_detections = acartia %>% 
  nrow()

acartia_alerts = alert_clean %>% 
  dplyr::filter(sighting_id %in% unique(acartia$id)) %>% 
  nrow()


####~~~~~~~~~~~~~~~~~~~~~~total number of hydrophone alerts~~~~~~~~~~~~~~~~~~~~~~~####

## hydrophone detections are grouped into detection events which is defined as a continuous string of 
## detections without 30mins of silence. 

## to get alerts we need to group detections sent by JASCO into events as one event = an alert

hydrophones = detections_clean %>% 
  dplyr::filter(sourceEntity == "JASCO") %>% 
  # Calculate the time difference between consecutive rows
  dplyr::mutate(time_difference = as.numeric(difftime(created_at, dplyr::lag(created_at)), "mins")) %>% 
  # Create a unique ID for each group where the time difference is less than 30 minutes
  dplyr::mutate(group_id = cumsum(ifelse(time_difference > 30 | is.na(time_difference), 1, 0)))

## this is total amount of **detection events**
no_hydrophone_detections = length(unique(hydrophones$group_id))

## filtering the alert data by detection id gives us the **number of alerts sent due to hydrophone detections**
no_hydrophone_alerts = alert_clean %>% 
  dplyr::filter(sighting_id %in% unique(hydrophones$id)) %>% 
  nrow()


