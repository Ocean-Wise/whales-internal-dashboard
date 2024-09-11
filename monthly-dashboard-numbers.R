
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
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "WhaleSpotter",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "JASCO",
                    stringr::str_detect(source_entity, "SMRUC") == T ~ "SMRU",
                    TRUE ~ source_entity))

## EXTRA STEP AS LAPIS MESSED UP THE DB. - this will take information from sightings spreadsheet and populate missing sightings data for alerts
interim_sightings = sightings_clean %>% 
  dplyr::mutate(
    lat_new = latitude,
    lon_new = longitude
  ) %>% 
  dplyr::select(c(id, lat_new, lon_new))

interim_1 = joined_tables %>% 
  dplyr::filter(is.na(latitude))


interim_2 = joined_tables %>% 
  dplyr::filter(!is.na(latitude))

interim_1 = interim_1 %>% dplyr::left_join(
  interim_sightings,
  by = dplyr::join_by(sighting_id == id)) %>% 
  dplyr::mutate(
    latitude = ifelse(!is.na(lat_new), lat_new, latitude),  # Overwrite lat if lat_new is not NA
    longitude = ifelse(!is.na(lon_new), lon_new, longitude)   # Overwrite lon if lon_new is not NA
  ) %>%
  dplyr::select(-lat_new, -lon_new) %>% 
  dplyr::filter(!is.na(latitude))

## Missing 91 lat lons from 2024 data, 1 from 2023, and 556 from 2019-2021

joined_tables = dplyr::bind_rows(interim_1, interim_2)

# still a few NAs in lat but I just will have to filter these out. 

overall_alerts = joined_tables %>% 
  dplyr::group_by(
    year = lubridate::year(sent_at), 
    month = lubridate::month(sent_at),
    source = source_entity
    ) %>% 
  # dplyr::filter(month > 1) %>%
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
    `Orca Network` = tidyr::replace_na(`Orca Network`, 0),
    JASCO = tidyr::replace_na(JASCO, 0),
    `WhaleSpotter` = tidyr::replace_na(`WhaleSpotter`, 0),
    SMRU = tidyr::replace_na(SMRU, 0)
    ) %>% 
  # dplyr::select(-c(
  #   SMRUC)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(
    `Cumulative Ocean Wise` = cumsum(`Ocean Wise`),
    `Cumulative Orca Network` = cumsum(`Orca Network`),
    `Cumulative WhaleSpotter` = cumsum(`WhaleSpotter`),
    `Cumulative JASCO` = cumsum(JASCO),
    `Cumulative SMRU` = cumsum(SMRU),
    Total = cumsum(`Ocean Wise` + JASCO + `WhaleSpotter` + `Orca Network` + SMRU)
  ) %>% 
  dplyr::mutate(
    `Ocean Wise %` = (`Cumulative Ocean Wise`/Total)*100,
    `Orca Network %` = (`Cumulative Orca Network`/Total)*100,
    `JASCO %` = (`Cumulative JASCO`/Total)*100,
    `WhaleSpotter %` = (`Cumulative WhaleSpotter`/Total)*100,
    `SMRU %` = (`Cumulative SMRU`/Total)*100
  )

## LOOK AT THIS
overall_alerts

perc_diff = overall_alerts %>% 
  dplyr::select(year, month, Total) %>%
  dplyr::group_by(year, month) %>% 
  tidyr::pivot_wider(names_from = year, values_from = Total) %>% 
  dplyr::mutate(perc_inc = ((`2024`-`2023`)/`2023`)*100) %>%
  dplyr::mutate(dplyr::across(c(`2024`,perc_inc), ~tidyr::replace_na(.x, 0)))

## LOOK AT THIS
perc_diff


#### ~~~~~~~~~~~~~~~~ How many detections has each source made? ~~~~~~~~~~~~~~~~~~~~~~~ ####


## Sightings numbers

sights_pre = detections_clean %>% 
  janitor::clean_names() %>%
  dplyr::mutate(source_entity =
                  dplyr::case_when(
                    is.na(source_entity) == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "WhaleSpotter",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "JASCO",
                    stringr::str_detect(source_entity, "SMRUC") == T ~ "SMRU",
                    # add in whale alert alaska here
                    TRUE ~ source_entity)) 


sights = sights_pre %>%  
  dplyr::group_by(year_mon = zoo::as.yearmon(detections_clean$sighted_at), source_entity) %>% 
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::pivot_wider(names_from = source_entity,
                     values_from = n) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>% 
  dplyr::select(-c(`TEST - PLEASE IGNORE`, string)) %>% 
  dplyr::filter(lubridate::year(year_mon) != 2019)


#### ~~~~~~~~~~~~~~~~ Where are the automated detection methods? ~~~~~~~~~~~~~~~~~~~~~~~ ####

locations = tibble::tibble(
  station_name = c("Lime Kiln", "Boundary Pass", "Carmanah Lighthouse", "Active Pass North", "Active Pass South", "Saturna Island"),
  station_type = c("hydrophone","hydrophone","infrared camera","infrared camera","infrared camera","infrared camera"),
  latitude = c(48.515834, 48.773653, 48.611406, 48.877781, 48.857528, 48.792393),
  longitude = c(-123.152978,  -123.042226, -124.751156, -123.316408, -123.344047, -123.096821))


## Map of locations with icons

# icon_list <- iconList(
#   "hydrophone" = makeIcon(iconUrl = "./../../../Downloads/Picture4.png", iconWidth = 44, iconHeight = 44),
#   "infrared camera" = makeIcon(iconUrl = "./../../../Downloads/Picture3.png", iconWidth = 38, iconHeight = 38)
# )
# 
# leaflet::leaflet(data = locations) %>%
#   leaflet::addTiles() %>%
#   leaflet::addMarkers(
#     ~longitude, ~latitude, 
#     icon = ~icon_list[station_type], 
#     label = ~paste("<b>Station Name:</b>", station_name, "<br><b>Type:</b>", station_type),
#     labelOptions = labelOptions(noHide = FALSE, textsize = "12px", direction = "auto")
#   ) %>%
#   # leaflet::addTitle("Stations Map", 
#   #                   leaflet::titleOpts = list(textsize = "24px", textOnly = TRUE)) %>%
#   leaflet::addMiniMap(toggleDisplay = TRUE) %>%
#   leaflet::setView(lng = -123.1207, lat = 49.2827, zoom = 6)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sandbox ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
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



