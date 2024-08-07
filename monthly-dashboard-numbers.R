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

## What data views do we want to see?
##    - number of alerts sent
##      - infrared, other data providers, hydrophones
##    - cumulative monthly split per year
##    - growth



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Run this section first
library(magrittr)

user = "AlexMitchell"

source(file = "./data-processing.R")


####~~~~~~~~~~~~~~~~~~~~~~# WRAS alerts vs 2023~~~~~~~~~~~~~~~~~~~~~~~####
### Goal...
## Send a total of 15,000 WRAS alerts (at least a 10% increase from 2023 baseline) in BC and WA waters to reduce 
## the risk of ship strike for at least 15,000 encounters with whales

## total alerts and percentage increase between 2023 and 2024
joined_tables = alert_clean %>% 
  dplyr::left_join(detections_clean, 
                   dplyr::join_by(sighting_id == id)
                   ) %>%
  janitor::clean_names() %>%
  dplyr::mutate(source_entity =
                  dplyr::case_when(
                    is.na(source_entity) == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Sightings Partner",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Sightings Partner",
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "IR Camera",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "Hydrophone",
                    TRUE ~ source_entity)
                )

overall_alerts = joined_tables %>% 
  dplyr::group_by(
    year = lubridate::year(sent_at), 
    month = lubridate::month(sent_at),
    source = source_entity
    ) %>% 
  dplyr::summarise(
    count = dplyr::n()) %>% 
  dplyr::filter(
    year == 2023 | year == 2024) %>% 
  dplyr::mutate(date = lubridate::as_date(paste0(year,"/",month,"/01"))) %>% 
  tidyr::pivot_wider(
    names_from = source,
    values_from = count
  ) %>% 
  dplyr::mutate(
    JASCO = tidyr::replace_na(Hydrophone, 0),
    `Sightings Partner` = tidyr::replace_na(`Sightings Partner`, 0),
    `IR Camera` = tidyr::replace_na(`IR Camera`, 0),
    SMRU = tidyr::replace_na(SMRUC, 0)
    ) %>% 
  dplyr::select(-c(Hydrophone, SMRUC)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(
    Total = cumsum(`Ocean Wise` + JASCO + `IR Camera` + `Sightings Partner` + SMRU),
    `Ocean Wise` = cumsum(`Ocean Wise`),
    `Sightings Partner` = cumsum(`Sightings Partner`),
    `IR Camera` = cumsum(`IR Camera`),
    JASCO = cumsum(JASCO),
    SMRU = cumsum(SMRU)
  ) %>% 
  # dplyr::select(year, month, total, 2:6) %>%
  dplyr::mutate(
    `Ocean Wise %` = (`Ocean Wise`/Total)*100,
    `Sightings Partner %` = (`Sightings Partner`/Total)*100,
    `JASCO %` = (JASCO/Total)*100,
    `IR Camera %` = (`IR Camera`/Total)*100,
    `SMRU %` = (SMRU/Total)*100
  )

## LOOK AT THIS
overall_alerts

perc_diff = overall_alerts %>% 
  dplyr::select(year, month, Total) %>%
  dplyr::group_by(year, month) %>% 
  tidyr::pivot_wider(names_from = year, values_from = Total) %>% 
  dplyr::mutate(perc_inc = ((`2024`-`2023`)/`2023`)*100)

## LOOK AT THIS
perc_diff


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sandbox ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# 
# ### Mapping 
# 
# ## Sightings
# sight_map = sightings_clean %>%
#   dplyr::filter(lubridate::year(date) == 2024 & lubridate::month(date) == 4) %>%
#   dplyr::mutate(col_palette =
#                   dplyr::case_when(
#                     species == "Harbour porpoise" ~ "#A569BD",
#                     species == "Killer whale" ~ "#17202A",
#                     species == "Humpback whale" ~ "#E74C3C",
#                     species == "Fin whale" ~ "#F4D03F",
#                     species == "Dall's porpoise" ~ "#566573",
#                     species == "Grey whale" ~ "#AAB7B8",
#                     species == "Pacific white-sided dolphin" ~ "#1ABC9C"
#                   )) %>%
#   dplyr::mutate(
#     popup_content = ifelse(
#       !is.na(ecotype),
#       paste("<b>Species:</b> ", species, "<b><br>Ecotype:</b> ", ecotype, "<b><br>Date:</b>", as.Date(date)),
#       paste("<b>Species:</b> ", species, "<b><br>Date:</b> ", as.Date(date))
#     )
#   ) %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
#   leaflet::addCircleMarkers(
#     lng = ~longitude,
#     lat = ~latitude,
#     radius = 3,
#     group = ~species,
#     color = ~col_palette,
#     fillOpacity = 0.8,
#     opacity = 0.8,
#     popup = ~popup_content
#   ) %>%
#   leaflet::addLegend(
#     "bottomright",
#     colors = c(unique(sight_map$col_palette)),
#     labels = c(unique(sight_map$species)),
#     opacity = 0.8)

# 
# ## Alerts
# 
# alert_map = alert_clean %>% 
#   dplyr::filter(lubridate::year(sent_at) == 2024 & lubridate::month(sent_at) == 4) %>% 
#   dplyr::left_join(detections_clean, 
#                    dplyr::join_by(sighting_id == id)) %>% 
#   janitor::clean_names() %>% 
#   dplyr::mutate(source_entity = 
#                   dplyr::case_when(
#                     is.na(source_entity) == T ~ "Ocean Wise",
#                     stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
#                     stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network via Conserve.io app",
#                     TRUE ~ source_entity 
#                   )) %>% 
#   dplyr::mutate(col_palette = 
#                   dplyr::case_when(
#                     stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A569BD",
#                     stringr::str_detect(source_entity, "Orca Network") == T ~ "#27AE60",
#                     stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#F5B041",
#                     stringr::str_detect(source_entity, "JASCO") == T ~ "#17202A"
#                   )) %>% 
#   dplyr::mutate(detection_method = 
#                   dplyr::case_when(
#                     stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "Infrared camera",
#                     stringr::str_detect(source_entity, "Orca Network") == T ~ "Partner sightings network",
#                     stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Whale report app",
#                     stringr::str_detect(source_entity, "JASCO") == T ~ "Hydrophone"
#                   )) %>% 
#   dplyr::mutate(
#     popup_content = 
#       paste("<b>Species:</b> ", name,
#             "<b><br>Source:</b> ", source_entity,
#             "<b><br>Detection method:</b>", detection_method,
#             "<b><br>Date:</b>", as.Date(sent_at)
#             ))
# 
# alert_map %>% 
#   leaflet::leaflet() %>%
#   leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>% 
#   leaflet::addCircleMarkers(
#     lng = ~longitude,
#     lat = ~latitude,
#     radius = 3,
#     color = ~col_palette,
#     fillOpacity = 0.6,
#     opacity = 0.6,
#     popup = ~popup_content
#   ) %>% 
#   leaflet::addLegend(
#     "bottomright",
#     colors = c(unique(alert_map$col_palette)),
#     labels = c(unique(alert_map$detection_method)),
#     opacity = 0.8
#   )
# 


## Number of whales avoided
# no_whales_averted = alert_clean %>% 
#   dplyr::left_join(
#     sightings_clean, by = dplyr::join_by(sighting_id == id)
#   ) 
# 
# 
# x = no_whales_averted %>% 
#   dplyr::mutate(number_of_animals = 
#                   dplyr::case_when(stringr::str_detect(number_of_animals, "~") ~ stringr::str_remove(number_of_animals, "~.*"),
#                                    stringr::str_detect(number_of_animals, "-") ~ "0",
#                                    TRUE ~ number_of_animals
#                   )) %>% 
#   dplyr::filter(is.na(number_of_animals) == F)
# 
# sum(as.numeric(x$number_of_animals))
# 
# 
# ## Users
# 
# users = readxl::read_xlsx(paste0("C:/Users/", user,
#                                                  "/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/WhaleReport Alert System/Participants/WRASUSERS_main.xlsx"),
#                                           sheet = "Authorized") %>%
#   janitor::clean_names() %>% 
#   dplyr::select(1:12) %>% 
#   dplyr::mutate(approval_date = as.Date(approval_date)) %>% 
#   dplyr::group_by(year = lubridate::year(approval_date)) %>% 
#   dplyr::summarise(count = dplyr::n()) %>% 
#   dplyr::mutate(cumsum = cumsum(count)) %>% 
#   dplyr::filter(!year == 2018 & is.na(year) == F) %>% 
#   dplyr::mutate(
#     yoy_growth = (cumsum - dplyr::lag(cumsum,1))/dplyr::lag(cumsum,1)*100
#   ) %>% 
#   dplyr::filter(is.na(yoy_growth) == F)



