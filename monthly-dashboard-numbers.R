####~~~~~~~~~~~~~~~~~~~~~~Monthly Dashboard Metrics~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the production of metrics for the ELT dashboard. Should be able to be run by anyone in the team.
## Date written: 2024-01-19

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

library(magrittr)

## UPDATE THIS TO YOUR USERNAME 
user = "AlexMitchell"


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Runs the data-processing.R file from the R Project - this saves you from opening the other file and means we set the user here. 
source(file = "./data-processing.R")

####~~~~~~~~~~~~~~~~~~~~~~# WRAS alerts vs 2023~~~~~~~~~~~~~~~~~~~~~~~####
### Goal...
## Send a total of 15,000 WRAS alerts (at least a 10% increase from 2024 baseline) in BC and WA waters to reduce 
## the risk of ship strike for at least 15,000 encounters with whales
## total alerts and percentage increase between 2024 and 2025

source_filter = c("Ocean Wise", "Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert")

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
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "Whale Alert",
                    TRUE ~ source_entity)) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(source_entity %in% source_filter)

## Create a filter to remove users who may be inflating the alert numbers - SIMRES / SMRU testers etc
## Filter
ignore_ids = 
  c(
    "auth0|6594b4cf033bf133ad3699cd", # Kathleen Durant - Tester SIMRES
    "auth0|668f2121ac3a7fb4523c3154", # Sam Tubbut - SMRU
    "auth0|60bea9676b3a6b00710e23dc", # Pauline Preston - Tester SIMRES
    "auth0|6594b52f8d750bdc3986a74e", # Chris Genovali - Tester SIMRES
    "auth0|62ec568fa194be1ada460ea8", # Jason Wood - SMRU
    "auth0|668469f2caa4d91f1f939ecf", # Paul King - SMRU
    "auth0|61d60319deb6b60069830256", # Patrick Gallagher
    "auth0|65fe005349ddc30bde015041" # Emma Laqua - Ocean Wise
  )
## EXTRA STEP AS LAPIS MESSED UP THE DB. - this will take information from sightings spreadsheet and populate missing sightings data for alerts
interim_sightings = sightings_clean %>% 
  dplyr::mutate(
    lat_new = latitude,
    lon_new = longitude
  ) %>% 
  dplyr::select(c(id, species, lat_new, lon_new))

interim_1 = joined_tables %>% 
  dplyr::filter(is.na(latitude)) %>% 
  dplyr::select(-species)

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

joined_tables = dplyr::bind_rows(interim_1, interim_2) %>%
  dplyr::filter(!auth_id %in% ignore_ids)

# still a few NAs in lat but I just will have to filter these out. 



overall_alerts = joined_tables %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(
    year = lubridate::year(sent_at), 
    month = lubridate::month(sent_at),
    source = source_entity
  ) %>% 
  dplyr::summarise(
    count = dplyr::n()) %>% 
  # dplyr::filter(
    # year == 2023 | year == 2024) %>%
  dplyr::mutate(date = lubridate::as_date(paste0(year,"/",month,"/01"))) %>% 
  tidyr::pivot_wider(
    names_from = source,
    values_from = count
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(), ~tidyr::replace_na(.x, 0))
  ) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(
    `Cumulative Ocean Wise` = cumsum(`Ocean Wise`),
    `Cumulative Orca Network` = cumsum(`Orca Network`),
    `Cumulative WhaleSpotter` = cumsum(`WhaleSpotter`),
    `Cumulative JASCO` = cumsum(JASCO),
    `Cumulative SMRU` = cumsum(SMRU),
    `Cumulative Whale Alert` = cumsum(`Whale Alert`),
    Total = cumsum(`Ocean Wise` + JASCO + `WhaleSpotter` + `Orca Network` + SMRU + `Whale Alert`)
  ) %>% 
  dplyr::mutate(
    `Ocean Wise %` = (`Cumulative Ocean Wise`/Total)*100,
    `Orca Network %` = (`Cumulative Orca Network`/Total)*100,
    `JASCO %` = (`Cumulative JASCO`/Total)*100,
    `WhaleSpotter %` = (`Cumulative WhaleSpotter`/Total)*100,
    `SMRU %` = (`Cumulative SMRU`/Total)*100,
    `Whale Alert %` = (`Cumulative Whale Alert`/Total)*100
  ) 
# %>% 
  # dplyr::filter(month < lubridate::month(Sys.Date())) ## This line removes the current months data
##  as reporting generally happens for the last month
## LOOK AT THIS
overall_alerts


perc_inc = overall_alerts %>% 
  dplyr::filter(year == 2024 | year == 2025) %>% 
  dplyr::select(year, month, Total) %>%
  dplyr::group_by(year, month) %>% 
  tidyr::pivot_wider(names_from = year, values_from = Total) %>% 
  dplyr::mutate(perc_inc = ((`2025`-`2024`)/`2024`)*100) %>%
  dplyr::mutate(dplyr::across(c(`2025`,perc_inc), ~tidyr::replace_na(.x, 0)))
## LOOK AT THIS

perc_inc

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
                    stringr::str_detect(source_entity, "BCHN/SWAG") == T ~ "Fin Island",
                    TRUE ~ source_entity)) %>%  
  dplyr::ungroup() %>% 
  dplyr::select(-c(created_at, id, code)) %>%  # do this to remove errors caused by bugs which led to duplicates sent at same time with different
  dplyr::distinct()                             # created_at values

detections = sights_pre %>%  
  dplyr::group_by(year_mon = zoo::as.yearmon(sighted_at), source_entity) %>% 
  dplyr::summarise(n = dplyr::n()) %>%
  tidyr::pivot_wider(names_from = source_entity,
                     values_from = n) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>% 
  dplyr::select(-c(`TEST - PLEASE IGNORE`, string, TEST)) %>% 
  dplyr::filter(lubridate::year(year_mon) != 2019) %>% 
  dplyr::group_by(year = lubridate::year(year_mon)) %>% 
  dplyr::mutate(
    `Cumulative Ocean Wise` = cumsum(`Ocean Wise`),
    `Cumulative Orca Network` = cumsum(`Orca Network`),
    `Cumulative WhaleSpotter` = cumsum(`WhaleSpotter`),
    `Cumulative JASCO` = cumsum(JASCO),
    `Cumulative SMRU` = cumsum(SMRU),
    `Cumulative Whale Alert` = cumsum(`Whale Alert Alaska`),
    `Cumulative Fin Island` = cumsum(`Fin Island`),
    Total = cumsum(`Ocean Wise` + JASCO + `WhaleSpotter` + `Orca Network` + SMRU + `Whale Alert Alaska` + `Fin Island`))

detections

#### ~~~~~~~~~~~~~~~~ Where are the automated detection methods? ~~~~~~~~~~~~~~~~~~~~~~~ ####
 locations = tibble::tibble(
   station_name = c("Fin Island", "Lime Kiln", "Boundary Pass", "Carmanah Lighthouse", "Active Pass North", "Active Pass South", "Saturna Island"),
   station_type = c("hydrophone", "hydrophone","hydrophone","infrared camera","infrared camera","infrared camera","infrared camera"),
   latitude = c(53.211, 48.515834, 48.773653, 48.611406, 48.877781, 48.857528, 48.792393),
   longitude = c(-129.498, -123.152978,  -123.042226, -124.751156, -123.316408, -123.344047, -123.096821))
 #
 #
 # ## Map of locations with icons
 #
 icon_list = leaflet::iconList(
   "hydrophone" = leaflet::makeIcon(iconUrl = "./../../../Downloads/Picture4.png", iconWidth = 60, iconHeight = 60),
   "infrared camera" = leaflet::makeIcon(iconUrl = "./../../../Downloads/Picture3.png", iconWidth = 38, iconHeight = 38)
 )
 #
 locations %>%
   # dplyr::filter(station_type == "infrared camera") %>%
   leaflet::leaflet() %>%
   leaflet::addTiles() %>%
   leaflet::addMarkers(
     ~longitude, ~latitude,
     icon = ~icon_list[station_type],
     label = ~paste("<b>Station Name:</b>", station_name, "<b>Type:</b>", station_type),
     leaflet::labelOptions(noHide = FALSE, textsize = "12px", direction = "auto")
   ) %>%
   # leaflet::addTitle("Stations Map",
   #                   leaflet::titleOpts = list(textsize = "24px", textOnly = TRUE)) %>%
   leaflet::addMiniMap(toggleDisplay = TRUE) %>%
   leaflet::setView(lng = -123.1207, lat = 49.2827, zoom = 6)
 #


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Sandbox ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

