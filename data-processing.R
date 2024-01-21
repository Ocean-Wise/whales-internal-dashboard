####~~~~~~~~~~~~~~~~~~~~~~Cleaning WRAS data~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the cleaning of WRAS data as far as possible to be used in future
##          analysis to test the efficacy of the WRAS, as well as any other data needs.
## Date written: 2022-12-21
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## Currently not able to automate this data process, so to download the data, ensure to change the
## "user" 
library(magrittr)

user = "AlexMitchell"

####~~~~~~~~~~~~~~~~~~~~~~Data Import~~~~~~~~~~~~~~~~~~~~~~~####

## Get a list of files in the directory which we want to get the data from. It is important that older files are overwritten, not added. 
file_list = list.files(paste0("C:/Users/", user, "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/dashboard/"), full.names = T)

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
  dplyr::select(sub_date, sub_time, id = report_id, species_name, ecotype, species_category,
                species_category, latitude = latitude_dd, longitude = longitude_dd,
                date, time, number_of_animals, email = reporter_email, organization = db_org) %>% 
  dplyr::mutate(time = format(time, "%H:%M:%S"),
                date = lubridate::as_date(date)) %>% 
  dplyr::mutate(date = lubridate::as_datetime(paste(date, time))) %>% 
  dplyr::select(-time)

detections_clean = detection_recent %>% 
  dplyr::mutate(details = stringr::str_remove_all(details, "[\\\\\"{}]")) %>% 
  dplyr::mutate(details = stringr::str_remove(details, "^[^:]+:")) %>% 
  dplyr::mutate(details = stringr::str_remove(details, "sightingDistance:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "travelDirection:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "hydrophoneMeta:")) %>%
  tidyr::separate_rows(details, sep = ",") %>% 
  tidyr::separate(col = details, into = c("name", "value"), sep = ":") %>% 
  tidyr::spread(name, value) %>% 
  dplyr::select(-c(V1, code))

  ## user cleaning
  user_clean = user_raw %>%
    dplyr::select(c(id, auth_id, name, phonenumber, organization, vessel = vesselname)) %>% 
    dplyr::rename(tracking_id = id)
  

  ####~~~~~~~~~~~~~~~~~~~~~~Data tidy~~~~~~~~~~~~~~~~~~~~~~~####
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   ####~~~~~~~~~~~~~~~~~~~~~~Metric Sandbox~~~~~~~~~~~~~~~~~~~~~~~####
#   
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
    


