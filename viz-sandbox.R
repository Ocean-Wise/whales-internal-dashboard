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

#### ~~~~~~~~~~~~~~~~~~~~~~~ Sighting proportions ~~~~~~~~~~~~~~~~~~~~####

## All real-time sightings, not just sightings that led to alerts

sights_props = sights %>% 
  tidyr::pivot_longer(
    !year_mon,
    names_to = "source",
    values_to = "count"
  ) %>% 
  dplyr::filter(year_mon != "Aug 2024", source != "Whale Alert Alaska") %>% 
  dplyr::group_by(year = lubridate::year(year_mon), source) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::filter(year == 2024)
  # update the year to get different years, or remove to get a timeline
  # remove whale alert alaska when we get more data from them 
  
  

## Drop this into Excel and make a unit chart from the data
sights_species = sights_pre %>% 
  dplyr::mutate(species_id = dplyr::case_when(
    species_id == 1 ~ "killer whale",
    species_id == 2 ~ "humpback whale",
    species_id == 3 ~ "grey whale",
    species_id == 4 ~ "minke whale",
    species_id == 5 ~ "fin whale",
    species_id == 6 ~ "sperm whale",
    species_id == 7 ~ "blue whale",
    species_id == 8 ~ "sei whale",
    species_id == 9 ~ "north pacific right whale",
    species_id == 10 ~ "bairds beaked whale",
    species_id == 11 ~ "cuviers beaked whale",
    species_id == 12 ~ "other rare whale",
    species_id == 13 ~ "unidentified whale",
    species_id == 14 ~ "killer whale",
    species_id == 15 ~ "harbour porpoise",
    species_id == 16 ~ "dalls porpoise",
    species_id == 17 ~ "pacific white-sided dolphin",
    species_id == 18 ~ "rissos dolphin",
    species_id == 19 ~ "northern right whale dolphin",
    species_id == 20 ~ "false killer whale",
    species_id == 21 ~ "common dolphin",
    species_id == 22 ~ "unidentified dolphin or porpoise",
    species_id == 23 ~ "leatherback sea turtle",
    species_id == 24 ~ "green sea turtle",
    species_id == 25 ~ "olive ridley sea turtle",
    species_id == 26 ~ "loggerhead sea turtle",
    species_id == 27 ~ "unidentidied sea turtle"
  )) %>% 
  dplyr::group_by(year = lubridate::year(sighted_at), month = lubridate::month(sighted_at), species_id) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::filter(year == 2024 & month < 8) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(month) %>%
  dplyr::arrange(desc(count), .by_group = TRUE) %>% 
  dplyr::mutate(date = as.Date(paste0(year,"-", month, "-01"), format = "%Y-%m-%d")) %>% 
  dplyr::mutate(species = factor(species_id, levels = species_id[order(-count)]))

total_species = sights_species %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(species_id) %>% 
  dplyr::summarise(count = sum(count))
  


#### ~~~~~~~~~~~~~~~~~~~~~~~ Species detections proportions ~~~~~~~~~~~~~~~~~~~~####


plotly::plot_ly(sights_species, 
        x = ~date, 
        y = ~count, 
        color = ~species, 
        type = 'bar') %>% 
        # text = ~paste(Species, ": ", Count),
        # hoverinfo = 'text',
        # marker = list(line = list(color = 'rgba(0,0,0,1)', width = 1.5))) %>%
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickformat = "%b %Y",
                              # tickvals = ~year_mon,
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
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2                  # move legend below the plot
                 ),
                 barmode = "stack")


S#### ~~~~~~~~~~~~~~~~~~~ NIGHT VS DAY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
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
  
  




#### ~~~~~~~~~~~ Line graph of alerts ~~~~~~~~~~~ ####

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


#### ~~~~~~~~~~~ USER GROWTH ~~~~~~~~~~~ ####

### IMPORT DATA ###

users_overall = readxl::read_xlsx(
  "C:/Users/AlexMitchell/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/WhaleReport Alert System/Participants/WRASUSERS_main.xlsx",
  sheet = "Authorized"
) %>% 
  janitor::clean_names()
  
users_cumulative = users_overall %>% 
  dplyr::group_by(year_qtr = zoo::as.yearqtr(approval_date), org_type) %>%
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::group_by(org_type) %>% 
  dplyr::mutate(cum_count = cumsum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = org_type,
                     values_from = cum_count) %>% 
  tidyr::fill(`Marine Pilots`, Ferries, 
              Enforcement, Government, 
              Industry, Guardians, Developer,
              `Port Authorities`, Research,
              `Tug and Tow`, .direction = "down") %>% 
  dplyr::mutate(dplyr::across(.cols = 2:11, ~tidyr::replace_na(.x,0))) 
  
users_cumulative_total = 
  users_cumulative %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = sum(dplyr::across(2:11))) %>% 
  dplyr::select(year_qtr, total)



### PLOT ###

### Total
plotly::plot_ly(users_cumulative_total, 
                x = ~year_qtr, 
                y = ~total, 
                type = 'scatter', 
                mode = 'lines') %>%
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # # tickformat="%b %Y",
                              # ticktext = format("%b %Y"),
                              # dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   tickfont = list(family = 'Arial',
                                   size = 16,
                                   color = 'rgb(82, 82, 82)'),
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2                  # move legend below the plot
                 ))


### Total by orgs


plotly::plot_ly(users_cumulative, x = ~year_qtr) %>%
  plotly::add_trace(y = ~`Marine Pilots`, name = "Marine Pilots", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Ferries, name = "Ferries", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Enforcement, name = "Enforcement", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Government, name = "Government", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Guardians, name = "Guardians", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Industry, name = "Industry", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~`Port Authorities`, name = "Port Authorities", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Research, name = "Research", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~`Tug and Tow`, name = "Tug and Tow", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Developer, name = "Developer", type = 'scatter', mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # tickformat="%b %Y",
                              # ticktext = format("%b %Y"),
                              # dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2,                  # move legend below the plot
                   tickfont = list(family = 'Arial',
                                   size = 18,
                                   color = 'rgb(82, 82, 82)')))


 ####~~~~~~~~~~~~~~~~~~~~~~~~~~~ Mapping ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

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
  leaflet::addTiles() %>%
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







# # Extract the last row and reshape the data into long format
# prop_wras <- wras_info %>% 
#   dplyr::slice(dplyr::n()) %>% 
#   dplyr::select(-year, -month, -total) %>%
#   tidyr::pivot_longer(cols = dplyr::everything(), 
#                       names_to = "category", 
#                       values_to = "count") %>% 
#   dplyr::mutate(proportion = count / sum(count),
#                 category = forcats::fct_reorder(category, proportion, .desc = F))
# 
# # Create the stacked bar chart
# library(ggplot2)
# ggplot(prop_wras, aes(x = proportion, y = "", fill = category)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   # coord_polar(theta = "y") + ## This makes the plot a banned pie chart.
#   labs(title = "",
#        x = NULL, y = NULL) +
#   scale_fill_brewer(palette = "Dark2") +
#   scale_x_continuous(labels = scales::percent_format()) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank())
# 


















