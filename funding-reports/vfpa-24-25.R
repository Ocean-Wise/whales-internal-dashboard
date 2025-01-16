####~~~~~~~~~~~~~~~~~~~~~~VFPA Reporting~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the production of metrics for VFPA reporting.
## Date written: 2024-07-05
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project
##      3. Run the "monthly-dashboard-numbers.R" script first

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

source("./monthly-dashboard-numbers.R")


####~~~~~~~ Numbers for the text (specific reporting period makes the numbers annoying to get)~~~~~~####

## How many sightings in reporting period Feb -...?

period_detections = detections_clean %>% 
  dplyr::filter(sighted_at > lubridate::as_date("2024-02-01"))




#### ~~~~~~~~~~~~~~~~~~~ Plots ~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

## Map of alerts

alert_map = joined_tables %>%
  ## EDIT THIS FOR REPORTING PERIOD
  dplyr::filter(sent_at > as.Date("2024-06-30") & sent_at < as.Date("2024-10-01")) %>% 
  dplyr::filter(lubridate::year(sent_at) == 2024 & !lubridate::month(sent_at) == 1) %>%
  # dplyr::left_join(detections_clean,
  #                  dplyr::join_by(sighting_id == id)) %>%
  janitor::clean_names() %>%
  dplyr::mutate(col_palette =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A569BD",
                    stringr::str_detect(source_entity, "Orca Network|Alaska") == T ~ "#27AE60",
                    # stringr::str_detect(source_entity, "Alaska") == T ~ "#27AE60",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#F5B041",
                    stringr::str_detect(source_entity, "JASCO|SMRU") == T ~ "#36648b"
                  )) %>%
  dplyr::mutate(detection_method =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "Infrared camera",
                    stringr::str_detect(source_entity, "Orca Network|Alaska") == T ~ "Partner sightings network",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Whale report app",
                    stringr::str_detect(source_entity, "JASCO|SMRU") == T ~ "Hydrophone"
                  )) %>%
  dplyr::mutate(
    popup_content =
      paste("<b>Species:</b> ", species,
            "<b><br>Source:</b> ", source_entity,
            "<b><br>Detection method:</b>", detection_method,
            "<b><br>Date:</b>", as.Date(sent_at)
      ))


## Point Map
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
  

## Heat map
leaflet::leaflet(alert_map) %>% 
  leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  leaflet.extras::addHeatmap(
    lng = ~longitude,
    lat = ~latitude,
    blur = 10, 
    max = 0.7, 
    radius = 20)

## Line graph of alerts

overall_alerts %>% 
  dplyr::filter(year == 2024)  %>%
  plotly::plot_ly(
    y = ~`Cumulative Ocean Wise`,
    x = ~date,
    name = "WhaleReport",
    type = "scatter",
    mode = "lines",
    line = list(color = "#1f77b4"),  # Blue for WhaleReport
    fill = 'tonexty',
    fillcolor = '#1f77b42'# Transparent blue for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative SMRU`,
    name = "Lime Kiln",
    type = "scatter",
    mode = "lines",
    line = list(color = "#2ca02c"),  # Green for SMRU
    fill = 'tonexty',
    fillcolor = 'rgba(44, 160, 44, 0.3)' # Transparent green for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative JASCO`,
    name = "Boundary Pass",
    type = "scatter",
    mode = "lines",
    line = list(color = "#ff7f0e"),  # Orange for JASCO
    fill = 'tonexty',
    fillcolor = 'rgba(255, 127, 14, 0.3)' # Transparent orange for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative Orca Network`,
    name = "Partner Sightings Network",
    type = "scatter",
    mode = "lines",
    line = list(color = "#d62728"),  # Red for Partner Sightings Network
    fill = 'tonexty',
    fillcolor = 'rgba(214, 39, 40, 0.3)' # Transparent red for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative WhaleSpotter`,
    name = "IR Camera",
    type = "scatter",
    mode = "lines",
    line = list(color = "#9467bd"),  # Purple for IR Camera
    fill = 'tonexty',
    fillcolor = 'rgba(148, 103, 189, 0.3)' # Transparent purple for fill
  ) %>%
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
                              ticktext = format("%b %Y"),
                              dtick = "M1",
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
                   xanchor = "center",       # anchor legend at the centerS
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2                  # move legend below the plot
                 ))
legend = list(
  orientation = "h",        # horizontal legend
  xanchor = "center",       # anchor legend at the center
  x = 0.5,                  # set position to the center (horizontally)
  y = -0.2                  # move legend below the plot
)


## Specific hydrophone visuals

sights %>% 
  dplyr::ungroup() %>% 
  dplyr::select(c(year_mon, sightigns_jasco = JASCO, sightings_smru = SMRU)) %>% 
  dplyr::mutate(cum_sightings_smru = cumsum(sightings_smru),
                cum_sightings_jasco = cumsum(sightigns_jasco),
                date = lubridate::as_date(year_mon)) %>%
  dplyr::right_join(overall_alerts %>% 
                      dplyr::filter(year == 2024), by = dplyr::join_by(date)) %>% 
  dplyr::select(date, 
                sightings_smru, sightigns_jasco, cum_sightings_smru, cum_sightings_jasco,
                alert_jasco = JASCO, alert_smru = SMRU, cum_alert_jasco = `Cumulative JASCO`, cum_alert_smru =`Cumulative SMRU`) %>% 
  plotly::plot_ly(x = ~date) %>%
  plotly::add_lines(y = ~cum_sightings_smru, name = 'Detections Lime Kiln', line = list(color = '#2ca02c')) %>%
  plotly::add_lines(y = ~cum_sightings_jasco, name = 'Detections Boundary Pass', line = list(color = '#ff7f0e')) %>%
  plotly::add_lines(y = ~cum_alert_smru, name = 'Alerts Lime Kiln', line = list(color = '#2ca02c', dash = 'dash')) %>%
  plotly::add_lines(y = ~cum_alert_jasco, name = 'Alerts Boundary Pass', line = list(color = '#ff7f0e', dash = 'dash')) %>%
  plotly::layout(title = "",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

                