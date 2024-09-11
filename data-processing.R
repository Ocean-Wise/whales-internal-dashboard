####~~~~~~~~~~~~~~~~~~~~~~Cleaning WRAS data~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the cleaning of WRAS data as far as possible to be used in future
##          analysis to test the efficacy of the WRAS, as well as any other data needs.
## Date written: 2023-12-21
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## Currently not able to automate this data process, so to download the data, ensure to change the
## "user" 
library(magrittr)

user = "AlexMitchell"

####~~~~~~~~~~~~~~~~~~~~~~Data Import~~~~~~~~~~~~~~~~~~~~~~~####

## API query to get sightings data from the sightings database without downloading the whole dataset... 

## Basic params
# base_url = "https://sightingsapi.ocean.org/sightings"
# 
# api_key = "rkcAx0oi7F9O0S7ZOOb482PMePhVCrk55jxpB60G"
# 
# params = list(
#   api_key = api_key,
#   limit = 10,
#   page = 1,
#   sourceType = "autonomous"
# )
# 
# # function to query API
# fetch_data <- function(base_url, params) {
#   response <- httr::GET(url = base_url, query = params, httr::add_headers(`x-api-key` = api_key))
#   content <- httr::content(response, as = "parsed")
#   data <- content$sightings
# 
#   return(data)
# }
# 
# all_data <- list()
# 
# repeat {
#   data <- fetch_data(base_url, params)
# 
#   # if (params$page == 10) {
#   #   break
#   # }
# 
#   if (length(data) == 0) {
#     break
#   }
# 
#   all_data <- append(all_data, list(data))
# 
#   params$page = params$page + 1
# }
# 
# combined_data = dplyr::bind_rows(all_data) %>%
#   dplyr::distinct() %>%
#   janitor::clean_names()

# 
# x = combined_data %>% 
#   dplyr::mutate(date_received == date_received) %>%
#   dplyr::filter(
#     !stringr::str_detect(comments, "(?i)test")) %>%
#   dplyr::filter(
#       !stringr::str_detect(location_desc, "(?i)test")) %>% 
#   dplyr::filter(
#       !stringr::str_detect(source_entity, "(?i)test")) %>% 
#   dplyr::filter(
#       !stringr::str_detect(reporter_email, "(?i)test")
#       ) 
# dplyr::filter(
#   !stringr::str_detect(reporter_email, "(?i)test")
# )


## Get a list of files in the directory which we want to get the data from. It is important that older files are overwritten, not added. 
file_list = list.files(paste0("C:/Users/", user, "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/dashboard/"), full.names = T) %>% 
  .[. != paste0("C:/Users/", user, "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/dashboard/historical_data")]
  


list_of_dfs = purrr::map(file_list, ~readr::read_csv(.x) %>% 
                           janitor::clean_names() %>% 
                           dplyr::filter(., dplyr::if_any(dplyr::starts_with("s") & dplyr::ends_with("_at"), ~ . >= as.Date("2019-01-01"))))

df_name = c("alert_raw", #alert data from database 
            "detection_recent", #contains sightings
            "user_raw")

named_dfs = setNames(list_of_dfs, df_name)

list2env(named_dfs, envir = .GlobalEnv)

rm(list = c("list_of_dfs", "named_dfs", "df_name", "file_list"))

sightings_spreadsheet = readxl::read_xlsx(paste0("C:/Users/", user,
                                                 "/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Sightings/Cheat Sheet Archive/BCCSN Sightings Master.xlsx"),
                                          sheet = "app") %>%
  janitor::clean_names()


####~~~~~~~~~~~~~~~~~~~~~~Data Clean~~~~~~~~~~~~~~~~~~~~~~~####

## alert cleaning
alert_clean = alert_raw %>% 
  dplyr::select(-location_id)
  
  
## detection cleaning
sightings_clean = sightings_spreadsheet %>% 
  dplyr::select(sub_date, sub_time, id = report_id, species = species_name, ecotype, species_category,
                species_category, latitude = latitude_dd, longitude = longitude_dd,
                date, time, number_of_animals, email = reporter_email, organization = db_org,
                confidence = id_confidence) %>% 
  dplyr::mutate(time = format(time, "%H:%M:%S"),
                date = lubridate::as_date(date)) %>% 
  dplyr::mutate(date = lubridate::as_datetime(paste(date, time))) %>% 
  dplyr::select(-time)

detections_clean = detection_recent %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(details = stringr::str_remove_all(details, "[\\\\\"{}]")) %>% 
  dplyr::mutate(details = stringr::str_remove(details, "^[^:]+:")) %>% 
  dplyr::mutate(details = stringr::str_remove(details, "sightingDistance:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "travelDirection:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "hydrophoneMeta:")) %>%
  tidyr::separate_rows(details, sep = ",") %>% 
  tidyr::separate(col = details, into = c("name", "value"), sep = ":") %>% 
  dplyr::mutate(name = stringr::str_replace(name, "id", "value")) %>% 
  tidyr::spread(name, value) %>% 
  dplyr::select(-c(V1, code))

  ## user cleaning
user_clean = user_raw %>%
  dplyr::select(c(id, auth_id, name, phonenumber, organization, vessel = vesselname)) %>% 
  dplyr::rename(tracking_id = id)


  ####~~~~~~~~~~~~~~~~~~~~~~Data tidy~~~~~~~~~~~~~~~~~~~~~~~####
 
  
  
  
  
  
#   ####~~~~~~~~~~~~~~~~~~~~~~Metric Sandbox~~~~~~~~~~~~~~~~~~~~~~~####


## sightings 2022 vs 2023
### Use a combination of the Detections df and the sightings spreadsheet to get update to date info on sightings
### 1. match cols between detections and sightings
### 2. rbind rows to merge datasets 
### 3. group per year
### 4. comparison per year (with and without autonomous detections?)

## As all of the sightings in the spreadsheet are not in real time (some submitted later via the app),
## I need to match the sub time vs sighting time to see what is in real time, I would count real time 
## as within an "Alert Period" of 30mins. Therefore, subtime - date < 30mins

# real_time_sightings = sightings_clean %>% 
  
  
#   
# sightings_match = sightings_clean %>% 
#   dplyr::select(id, species, 
#                 latitude, longitude, 
#                 date, number_of_animals,
#                 email)
# 
# detections_match = detections_clean %>% 
#   dplyr::select(c(date = sighted_at,
#                   latitude, longitude,
#                   number_of_animals = numberOfAnimals,
#                   email, id,
#                   species = name)) %>% 
#   dplyr::filter(!id %in% sightings_match$id)
# 
# sightings_combined = rbind(sightings_match, detections_match)
# 
# total_sightings = sightings_combined %>% 
#   dplyr::group_by(year = lubridate::year(date)) %>% 
#   dplyr::summarise(count = dplyr::n())
# 
# period_2022 = total_sightings[6,2]
# 
# period_2023 = total_sightings[7,2]


## number of north coast 2023 vs 2022
# 
# north_coast_sightings = sightings_combined %>% 
#   dplyr::filter(dplyr::between(latitude, 51, 56) | dplyr::between(longitude, 126, 135)) %>% 
#   dplyr::group_by(year = lubridate::year(date)) %>% 
#   dplyr::summarise(count = dplyr::n()) %>% 
#   dplyr::mutate(perc_diff = ((count-dplyr::lag(count)/dplyr::lag(count)))*100)


## BC Sightings
# Coordinates 
# 51°00.00’N to 56°00.00’N (with Stewart, BC) 
# 126°00.00’W to 135°00.00’W 


#   
#   ## Who recieved alerts? 
#   
#   alert_id = alert_clean %>% 
#     dplyr::left_join(user_clean, by = "tracking_id") %>% 
#     dplyr::mutate(vessel = stringr::str_remove_all(vessel, "mv |m v|MV|M/V ")) %>% 
#     ## The next mutates clean up some of the common spelling errors and bits the fuzzy matching misses. Repeated for vessel data.
#     dplyr::mutate(vessel = tolower(vessel)) %>% 
#     dplyr::mutate(vessel = dplyr::case_when(vessel == "charles hays amwaal" ~ "charles hays",
#                                             vessel == "amwaal" ~ "charles hays",
#                                             vessel == "cowichan" ~ "queen of cowichan",
#                                             vessel == "sobc" ~ "spirit of british columbia",
#                                             vessel == "qalb" ~ "queen of alberni",
#                                             vessel == "spirit of bc" ~ "spirit of british columbia",
#                                             vessel == "reliant" ~ "seaspan reliant",
#                                             vessel == "queen of newwest" ~ "queen of new westminster",
#                                             vessel == "q of alberni" ~ "queen of alberni",
#                                             vessel == "mazuru bishamon" ~ "maizuru bishamon",
#                                             vessel == "coastal renn" ~ "coastal renaissance",
#                                             vessel == "sovc" ~ "spirit of vancouver island",
#                                             vessel == "q of alberni" ~ "queen of alberni",
#                                             vessel == "qnw" ~ "queen of new westminster",
#                                             vessel == "laurier" ~ "sir wilfrid laurier",
#                                             vessel == "suquamish" ~ "wsf suquamish",
#                                             vessel == "howe sounbd queen" ~ "howe sound queen",
#                                             vessel == "inspiration" ~ "coastal inspiration",
#                                             vessel == "suquwamish" ~ "wsf suquamish",
#                                             vessel == "seapan zambizi" ~ "seaspan zambezi",
#                                             vessel == "sprit of british columbia" ~ "spirit of british columbia",
#                                             vessel == "roald almundsen" ~ "roald amundsen",
#                                             vessel == "ovean clio" ~ "ocean clio",
#                                             vessel == "coroleader ol" ~ "coreleader ol",
#                                             vessel == "zuidetdam" ~ "zuiderdam",
#                                             vessel == "seaspan anadonis" ~ "seaspan adonis",
#                                             vessel == "berge yotie" ~ "berge yotei",
#                                             vessel == "carnval splendor" ~ "carnival splendor",
#                                             vessel == "blackball" ~ "coho",
#                                             vessel == "zeta" ~ "star zeta",
#                                             vessel == "cma cgm rigalito" ~ "cma cgm rigoletto",
#                                             vessel == "gulf islands spirit" ~ "spirit of vancouver island",
#                                             vessel == "zeda" ~ "star zeta",
#                                             vessel == "zeta" ~ "star zeta",
#                                             TRUE ~ vessel))
# 
# bc_ferries_alerts = alert_id %>% 
#   dplyr::filter(stringr::str_detect(organization, "B[[:space:].]*C[[:space:].]*F")) %>% 
#   dplyr::mutate(vessel = dplyr::case_when(stringr::str_detect(vessel, "land") == T ~ "unknown",
#                                           stringr::str_detect(vessel, "on land") == T ~ "unknown",
#                                           stringr::str_detect(vessel, "(?i)BC\\s*Ferries?|Ferry") == T ~ "unknown",
#                                           vessel == "mix" ~ "unknown",
#                                           vessel == "shore" ~ "unknown",
#                                           vessel == "spirit of british colombia" ~ "spirit of british columbia",
#                                           vessel == "c falcon" ~ "centennial falcon",
#                                           vessel == "spirit of britis columbia" ~ "spirit of british columbia",
#                                           vessel == "howe soubd queen" ~ "howe sound queen",
#                                           vessel == "howe soubd queen" ~ "howe sound queen",
#                                           vessel == "sovi" ~ "spirit of vancouver island",
#                                           vessel == "cosstal inspiration" ~ "coastal inspiration",
#                                           vessel == "queen alberni" ~ "queen of alberni",
#                                           vessel == "norther adventure" ~ "northern adventure",
#                                           TRUE ~ vessel)) %>% 
#   dplyr::group_by(vessel) %>% 
#   dplyr::summarise(count = dplyr::n())
#   
# user_clean %>% 
#   dplyr::filter(tracking_id %in% c("208a89cd-daea-cd53-6e83-f49e4da9d3a1", 
#                                    "4deac481-edb3-c5bd-cfb9-3e4b3fa48161",
#                                    "864f93b8-ead2-cd5f-6278-e67dedfa8c1c"))
    

# file_list = list.files("C:/Users/alext/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/dashboard/", full.names = T)

# sightings_spreadsheet = readxl::read_xlsx(paste0("C:/Users/alext/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Sightings/Cheat Sheet Archive/BCCSN Sightings Master.xlsx"),
#                                           sheet = "app") %>%
#   janitor::clean_names()
