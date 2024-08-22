####~~~~~~~~~~~~~~~~~~~~~~Monthly Dashboard Metrics Visualization~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To visualize production of metrics for the ELT dashboard. Should be able to be run by anyone in the team.
## Date written: 2024-01-19
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project

## What data views do we want to see?

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Number of alerts overall
## Edit the filter depending on what data source you want. Although may need to be edited to make it look a little nicer when using a longer timeframe... 

vfpa_data = joined_tables %>% dplyr::filter(source_entity == "SMRUC") %>% 
  dplyr::mutate(day_night = 
                   dplyr::case_when(
                     dplyr::between(lubridate::hour(sent_at), 6, 20) ~ "day",
                     dplyr::between(lubridate::hour(sent_at), 21, 5) ~ "night")
      # lubridate::hour(sent_at) >= 6 & lubridate::hour(sent_at) <= 21 ~ "day",
      # lubridate::hour(sent_at) <= 5 & lubridate::hour(sent_at) >= 22 ~ "night"
      ) %>% 
  dplyr::mutate(day_night = tidyr::replace_na(day_night, "night")) %>% 
  dplyr::select(
    c(id, tracking_id, sighting_id, created_at, sent_at, sighted_at, day_night)
    ) %>% 
  # dplyr::group_by(day_night) %>% 
  # dplyr::summarise(n = dplyr::n())
  dplyr::group_by(date = lubridate::date(sent_at),day_night) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = count
  ) %>% 
  plotly::plot_ly(
    x = ~date,
    y = ~day,
    type = "bar",
    name = "Daylight hours",
    marker = list(color = "#F2B949") 
  ) %>% 
  plotly::add_trace(
    y = ~night,
    name = "Outside daylight\n hours",
    marker = list(color = "#6449F2")
  ) %>% 
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = list(text='No. Alerts', font = list(size = 16, family = 'Arial Black'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 barmode = "stack")
  
  


ggplot2::geomgeom_bar

X#### ~~~~~~~~~~~ Line graph of alerts ~~~~~~~~~~~ ####

# 
# plotly::plot_ly(overall_alerts,
#                   x = ~date,
#                   y = ~`Ocean Wise`,
#                   type = "scatter",
#                   mode = "lines") %>%
#   plotly::add_lines(y = ~`Ocean Wise`)
# 
# line_graph = overall_alerts %>%
#   dplyr::ungroup() %>%
#   dplyr::select(3:9) %>%
#   dplyr::filter(lubridate::year(date) == 2024) %>%
#   tidyr::pivot_longer(cols = 2:7, names_to = "source")
# 
# line_graph %>%
#   ggplot(aes(x = date,
#              y = value,
#              group = source,
#              colour = source)) +
#   geom_line(size = 1) +
#   scale_y_continuous(label = scales::comma) +
#   scale_colour_brewer(palette = "Dark2") +
# ggthemes::theme_hc() +
# ggthemes::scale_colour_hc() +
# hrbrthemes::theme_ipsum() +
# theme(panel.background = element_rect(fill = "white",
#                                       colour = "white",
#                                       size = 0.5, linetype = "solid"),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#       ) +
#   theme_minimal() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   ylab("Number of alerts generated")+
#   xlab("")
#   
# 
# 
# 
# library(plotly)






 ### Mapping

 ## Sightings
 sight_map = sightings_clean %>%
   dplyr::filter(lubridate::year(date) == 2024 & lubridate::month(date) == 6) %>%
   dplyr::mutate(species = 
                   dplyr::case_when(
                     stringr::str_detect(species, "dolphin") ~ "Dolphin/Porpoise species",
                     stringr::str_detect(species, "porpoise") ~ "Dolphin/Porpoise species",
                     .default = as.character(species)
                   )) %>% 
   dplyr::mutate(col_palette =
                  dplyr::case_when(
                    species == "Minke whale" ~ "#A569BD",
                    species == "Killer whale" ~ "#17202A",
                    species == "Humpback whale" ~ "#E74C3C",
                    species == "Fin whale" ~ "#F4D03F",
                    species == "Dolphin/Porpoise species" ~ "#566573",
                    species == "Grey whale" ~ "#AAB7B8",
                    species == "Dolphin species" ~ "#1ABC9C",
                    species == "Sperm whale" ~ "#C0392B",
                    species == "Unidentified whale" ~ "#B7950B"
                  )) %>%
  dplyr::mutate(
    popup_content = ifelse(
      !is.na(ecotype),
      paste("<b>Species:</b> ", species, "<b><br>Ecotype:</b> ", ecotype, "<b><br>Date:</b>", as.Date(date)),
      paste("<b>Species:</b> ", species, "<b><br>Date:</b> ", as.Date(date))
    )
  ) 

sight_map %>% 
 leaflet::leaflet() %>%
  leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    group = ~species,
    color = ~col_palette,
    fillOpacity = 0.8,
    opacity = 0.8,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(sight_map$col_palette)),
    labels = c(unique(sight_map$species)),
    opacity = 0.8
   )

## Alerts

alert_map = alert_clean %>%
  dplyr::filter(lubridate::year(sent_at) == 2024 & lubridate::month(sent_at) == 5) %>%
  dplyr::left_join(detections_clean,
                   dplyr::join_by(sighting_id == id)) %>%
  janitor::clean_names() %>%
  dplyr::mutate(source_entity =
                  dplyr::case_when(
                    is.na(source_entity) == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network via Conserve.io app",
                    TRUE ~ source_entity
                  )) %>%
  dplyr::mutate(col_palette =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A569BD",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "#27AE60",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#F5B041",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "#17202A"
                  )) %>%
  dplyr::mutate(detection_method =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "Infrared camera",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Partner sightings network",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Whale report app",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "Hydrophone"
                  )) %>%
  dplyr::mutate(
    popup_content =
      paste("<b>Species:</b> ", name,
            "<b><br>Source:</b> ", source_entity,
            "<b><br>Detection method:</b>", detection_method,
            "<b><br>Date:</b>", as.Date(sent_at)
            ))

alert_map %>%
  leaflet::leaflet() %>%
  leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = ~col_palette,
    fillOpacity = 0.6,
    opacity = 0.6,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(alert_map$col_palette)),
    labels = c(unique(alert_map$detection_method)),
    opacity = 0.8
  )

























