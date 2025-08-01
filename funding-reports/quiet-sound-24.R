###~~~ Quiet Sound Reporting ~~~######
## Author: Alex Mitchell
## Purpose: To produce monthly reports for Quiet Sound
## Date written: 2023-12-05
## Quality Assured: No

## Notes: metrics needed == 			
##        § Number of sightings into WRAS 
##        § Number of alerts


###~~~ Data import and basic cleaning ~~~######

start_date = as.Date("2024-01-01")
end_date = as.Date("2025-09-30")

ocean_wise_palette = c(
  "Sun"      = "#FFCE34",
  "Kelp"     = "#A2B427",
  "Coral"    = "#A8007E",
  "Anemone"  = "#354EB1",
  "Ocean"    = "#005A7C",
  "Tide"     = "#5FCBDA",
  "Black"    = "#000000",
  "White"    = "#FFFFFF",
  "Dolphin"  = "#B1B1B1"
)

## Import US economic exclusive zone.
US_EZZ = sf::read_sf(dsn = paste0("C:/Users/",
                                  user,
                                  "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/Shapefiles/World_EEZ_v11_20191118"), 
                     layer = "eez_v11") %>% 
  dplyr::filter(SOVEREIGN1 == "United States", MRGID == 8456)


##~~~ Data manipulation ~~~######

# US Sightings total
alerts_detections %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_join(x = .,
              y = US_EZZ,
              join = sf::st_within) %>%
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(sighting_id,sent_at) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::filter(
    ., dplyr::between(year_mon,
                      zoo::as.yearmon("Jan 2024"),
                      zoo::as.yearmon("Sep 2025"))
  )

##
alert_us = alerts_detections %>% 
  dplyr::filter(is.na(longitude) == F) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone" & is.na(code) == F)

detections_us = detections_pre %>% 
  dplyr::filter(is.na(longitude) == F) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone")

## Total year monthly detections

detections_us %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(id,sighted_at) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year_mon = zoo::as.yearmon(sighted_at)) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::filter(dplyr::between(year_mon,
                               zoo::as.yearmon(start_date),
                               zoo::as.yearmon(end_date)))
  

### Year on year change in alerts and sightings

n_sightings = alert_us %>%
 sf::st_drop_geometry() %>%
 # dplyr::group_by(year = lubridate::year(sent_at)) %>%
 dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
 dplyr::summarize(count_sightings = dplyr::n_distinct(sighting_id))


n_alerts = alert_us %>%
 sf::st_drop_geometry() %>%
 # dplyr::group_by(year = lubridate::year(sent_at)) %>%
 dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
 dplyr::summarize(count_alerts = dplyr::n())

counts = dplyr::left_join(n_sightings, n_alerts) %>%
 dplyr::filter(is.na(year_mon) == F)
# Drop this into excel and we can make a quick and easy graph that way for year on year growth
## Continuous change in alerts and sightings
high_alerts_list = alert_us %>%
 dplyr::filter(sent_at > as.Date("2024-07-31") & sent_at < as.Date("2025-01-01")) %>%
 dplyr::group_by(tracking_id) %>%
 sf::st_drop_geometry() %>%
 dplyr::summarise(number_of_alerts = dplyr::n()) %>%
 dplyr::left_join(., user_clean) %>%
 dplyr::filter(is.na(auth_id) == T)


monthly_detections = detections_us %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sighted_at)) %>% 
  dplyr::summarize(count_detections = dplyr::n()) %>% 
  dplyr::filter(year_month < zoo::as.yearmon(Sys.Date()))

monthly_alerts = alert_us %>% 
  sf::st_drop_geometry() %>%
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_alerts = dplyr::n()) 

cont_counts = dplyr::left_join(monthly_detections, monthly_alerts) %>% 
  dplyr::mutate(year_month = zoo::as.Date(year_month)) %>% 
  dplyr::filter(year_month < end_date)


##~~~ Data Viz ~~~####

vline <- list(type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = "2023-10-01",
              x1 = "2023-10-01",
              line = list(color = "B1B1B1", dash="dot")
)


vline2 <- list(type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = "2024-05-01",
              x1 = "2024-05-01",
              line = list(color = "B1B1B1", dash="dot")
)



vline3 <- list(type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = "2024-08-01",
              x1 = "2024-08-01",
              line = list(color = "B1B1B1", dash="dot")
)

# vline4 <- list(type = "line",
#                y0 = 0,
#                y1 = 1,
#                yref = "paper",
#                x0 = "2024-03-01",
#                x1 = "2024-03-01",
#                line = list(color = "B1B1B1", dash="dot")
# )


highlight_rect <- list(
  type = "rect",
  x0 = "2024-10-01",
  x1 = "2025-01-31",
  xref = "x",
  y0 = 0,
  y1 = 1,
  yref = "paper",
  fillcolor = "rgba(168, 0, 126, 0.15)",  # matches detection line color with 15% opacity
  line = list(width = 0)
)

## Detections and alerts combined 
plotly::plot_ly(data = cont_counts,
                y = ~count_detections,
                x = ~as.character(year_month),
                name = "Detections",
                type = "scatter",
                mode = "lines",
                line = list(color = "A8007E")) %>% 
  plotly::add_trace(y = ~count_alerts,
                    name = "Alerts",
                    type = "scatter",
                    mode = "lines",
                    line = list(color = "005580")) %>% 
  plotly::layout(shapes = list(vline, 
                               vline2, 
                               vline3, 
                               # vline4
                               highlight_rect
                               ),
                 xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              ticktext = format("%b %Y"),
                              # dtick = "M6",
                              # tickvals = c(as.character(cont_counts$year_month)[seq(1, length(cont_counts$year_month), by = 2)]),
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)'))) %>% 
  plotly::add_text(showlegend = FALSE, 
                   x = "2023-08-01", y = 2000,
                   text = "Acartia \nintegrated") %>%  
  plotly::add_text(showlegend = FALSE,
                   x = "2024-06-01", y = 4000,
                   text = "Orca Network \nsummer contract\n expired") %>% 
  # plotly::add_text(showlegend = FALSE,
  #                  x = "2024-02-01", y = 5000,
  #                  text = "Cetacean Desk \noperational") %>% 
  plotly::add_text(showlegend = FALSE,
                   x = "2024-10-01", y = 2400,
                   text = "Orca Network \n contract renewed")
  



## Combine formatting with initial graph to create cleaned plot
# cumulative_alert_plot_cleaned = cumulative_alert_plot %>% 
#   plotly::layout(xaxis = cumulative_xaxis, 
#                  yaxis = cumulative_yaxis,
#                  margin = cumulative_margin,
#                  showlegend = F)

us_map = alert_us %>% 
  dplyr::filter(lubridate::year(sighted_at) == 2025 | lubridate::year(sighted_at) == 2025) %>%
  dplyr::mutate(species = 
                  dplyr::case_when(
                    stringr::str_detect(species, "dolphin") ~ "Dolphin/Porpoise species",
                    stringr::str_detect(species, "porpoise") ~ "Dolphin/Porpoise species",
                    stringr::str_detect(species, "turtle") ~ "Potential Turtle species",
                    stringr::str_detect(species, "False") ~ "Dolphin/Porpoise species",
                    stringr::str_detect(species, "Sei") ~ "Unidentified whale",
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
                    species == "Sperm whale" ~ "blue",
                    species == "Unidentified whale" ~ "#B7950B",
                    species == "Potential Turtle species" ~ "darkgreen" 
                  ))
  
us_map %>% 
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addCircleMarkers(
      radius = 2,
      group = ~species,
      color = ~col_palette,
      fillOpacity = 0.8,
      opacity = 0.8
      # popup = ~popup_content
    ) %>%
    leaflet::addLegend(
      "bottomright",
      colors = c(unique(us_map$col_palette)),
      labels = c(unique(us_map$species)),
      opacity = 0.8
    ) %>% 
    leaflet::addMiniMap(toggleDisplay = TRUE)
    # htmltools::save_html(., paste0("C:/Users/", 
    #                                user, 
    #                                "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
    #                                "sightings-map-",
    #                                Sys.Date(),
    #                                ".html"))


#### Heat map ####
us_map %>% 
  leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet.extras::addHeatmap(
    blur = 23,
    max = 0.05,
    radius = 15
  ) %>% 
  leaflet::addControl(
    html = "<div style='background:white;padding:8px;border-radius:5px;box-shadow:0 0 5px rgba(0,0,0,0.3);'>
              <b>Heatmap Intensity</b><br>
              <span style='color:#2CAED8;'>&#9632;</span> Low<br>
              <span style='color:#FFEDA0;'>&#9632;</span> Medium<br>
              <span style='color:#F03B20;'>&#9632;</span> High
            </div>",
    position = "bottomright")





#### Detection bar graph ####
plot_data = detections_us %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(year_month = lubridate::floor_date(sighted_at, unit = "month")) %>%
  # UPDATE THE DATE HERE!!!!!
  dplyr::filter(year_month > as.Date("2023-01-01") & year_month < as.Date("2025-07-31")) %>% 
  dplyr::filter(!is.na(source_entity)) %>%
  dplyr::group_by(year_month, source_entity) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
# Match source_entity to colors (recycling if needed)
color_map = rep(ocean_wise_palette, length.out = dplyr::n_distinct(plot_data$source_entity))
names(color_map) = unique(plot_data$source_entity)  

plotly::plot_ly(
  plot_data,
    x = ~year_month,
    y = ~count,
    color = ~source_entity,
    colors = color_map,
    type = 'bar'
  ) %>%
  plotly::layout(
    barmode = 'stack',
    xaxis = list(title = ''),
    yaxis = list(title = 'Detections'),
    title = '',
    xaxis = list(title = "",
                 showline = TRUE,
                 showgrid = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklength = 5,
                 ticktext = format("%b %Y"),
                 # dtick = "M6",
                 # tickvals = c(as.character(cont_counts$year_month)[seq(1, length(cont_counts$year_month), by = 2)]),
                 tickfont = list(family = 'Arial',
                                 size = 16,
                                 color = 'rgb(82, 82, 82)')),
    yaxis = list(title = "",
                 showgrid = T,
                 zeroline = FALSE,
                 showline = FALSE,
                 tickfont = list(family = 'Arial',
                                 size = 16,
                                 color = 'rgb(82, 82, 82)')))
  





## Alert bar graph ####
plot_data = alert_us %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(year_month = lubridate::floor_date(sighted_at, unit = "month")) %>%
  ## UPDATE THIS DATE!!!!
  dplyr::filter(year_month > as.Date("2023-01-01") & year_month < as.Date("2025-07-31")) %>% 
  dplyr::filter(!is.na(source_entity)) %>%
  dplyr::group_by(year_month, source_entity) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

# Match source_entity to colors (recycling if needed)
color_map = rep(ocean_wise_palette, length.out = dplyr::n_distinct(plot_data$source_entity))
names(color_map) = unique(plot_data$source_entity)  

plotly::plot_ly(
  plot_data,
  x = ~year_month,
  y = ~count,
  color = ~source_entity,
  colors = color_map,
  type = 'bar'
) %>%
  plotly::layout(
    barmode = 'stack',
    xaxis = list(title = ''),
    yaxis = list(title = 'Alerts'),
    title = '',
    xaxis = list(title = "",
                 showline = TRUE,
                 showgrid = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklength = 5,
                 ticktext = format("%b %Y"),
                 # dtick = "M6",
                 # tickvals = c(as.character(cont_counts$year_month)[seq(1, length(cont_counts$year_month), by = 2)]),
                 tickfont = list(family = 'Arial',
                                 size = 16,
                                 color = 'rgb(82, 82, 82)')),
    yaxis = list(title = "",
                 showgrid = T,
                 zeroline = FALSE,
                 showline = FALSE,
                 tickfont = list(family = 'Arial',
                                 size = 16,
                                 color = 'rgb(82, 82, 82)')))

## Day and Night Whale Spotter ####
## Calculate sunrise and sunset for each date in your dataset
x = detections_clean %>%
  dplyr::ungroup() %>% 
  dplyr::filter(as.Date(sighted_at) >= as.Date("2025/01/01") & as.Date(sighted_at) <= as.Date("2025/07/31")) %>%
  dplyr::filter(stringr::str_detect(source_entity, "WhaleSpotter") & org_name == "Quiet Sound") %>%  
  # & stringr::str_detect(org_name, "Quiet Sound")) %>% 
  dplyr::mutate(
    sun_times = suncalc::getSunlightTimes(as.Date(sighted_at),
                                          lat = 48.51566, 
                                          lon = -123.1528)) %>% 
  dplyr::mutate(sunrise = lubridate::ymd_hms(.$sun_times$sunrise, tz = "UTC") - lubridate::hours(7),
                sunset = lubridate::ymd_hms(.$sun_times$sunset, tz = "UTC") - lubridate::hours(7)) %>% 
  dplyr::select(id, sighted_at, latitude, longitude, 
                species, source_entity, sunrise, sunset) %>% 
  dplyr::mutate(day_night = 
                  dplyr::case_when(dplyr::between(sighted_at, sunrise, sunset)  ~ "day",
                                   TRUE  ~ "night"))

openxlsx::write.xlsx(x,paste0("C:/Users/", user, "/Downloads/whalespotter-quietsound", Sys.Date(),".xlsx"))

x  %>% 
  dplyr::group_by(month = lubridate::month(sighted_at),day_night) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = count
  ) %>% 
  dplyr::mutate(month_name = factor(month.name[month], levels = month.name)) %>% 
  plotly::plot_ly(
    x = ~month_name,
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
                 yaxis = list(title = list(text='No. detections', font = list(size = 16, family = 'Arial'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 barmode = "stack")



## User Growth ####

users_overall = readxl::read_xlsx(
  "C:/Users/AlexMitchell/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Whale Report Alert System/Participants/WRASUSERS_main.xlsx",
  sheet = "Authorized"
) %>% 
  janitor::clean_names() %>% 
  # dplyr::mutate(approval_date = janitor::excel_numeric_to_date(as.numeric(approval_date))) %>% 
  dplyr::filter(region_clean == "USA") %>%
  dplyr::group_by(year_qtr = zoo::as.yearqtr(approval_date), org_type) %>%
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::group_by(org_type) %>% 
  dplyr::mutate(cum_count = cumsum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = org_type,
                     values_from = cum_count) 

dates = seq(from = floor(min(users_overall$year_qtr)),
            to = max(users_overall$year_qtr),
            by = 1/4) %>% 
  tibble::as_tibble(.)

users_cumulative = dates %>% 
  dplyr::left_join(users_overall, by = dplyr::join_by(value == year_qtr)) %>% 
  tidyr::fill(2:11,.direction = "down") %>% 
  dplyr::rename(year_qtr = value) %>% 
  dplyr::mutate(dplyr::across(.cols = 2:11, ~tidyr::replace_na(.x,0)))

### PLOT ###
### Stacked bar chart 

users_cumulative %>% 
  tidyr::pivot_longer(cols = -year_qtr, 
                      names_to = "catagory",
                      values_to = "value") %>% 
  plotly::plot_ly(x = ~year_qtr,
                  y = ~value,
                  color = ~catagory,
                  type = "bar") %>% 
  plotly::layout(barmode = 'stack',
                 xaxis = list(title = "",
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
                 tickfont = list(family = 'Arial',
                                 size = 18,
                                 color = 'rgb(82, 82, 82)'))

## Who is recieving alerts? ####

high_alerts_list = alert_us %>% 
  dplyr::filter(sent_at > as.Date("2025-01-01") & zoo::as.yearmon(sent_at) < zoo::as.yearmon(Sys.Date())) %>%
  dplyr::group_by(tracking_id) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(number_of_alerts = dplyr::n()) %>% 
  dplyr::left_join(., user_clean) %>% 
  dplyr::filter(is.na(auth_id) == F)



## DATA EXPORTS
export = alert_us %>% 
  dplyr::filter(sent_at > as.Date("2024-12-31")) %>% 
  dplyr::as_tibble() %>%
  dplyr::mutate(
    longitude = sf::st_coordinates(geometry)[, 1],
    latitude = sf::st_coordinates(geometry)[, 2]
  ) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-geometry)

openxlsx::write.xlsx(export, paste0("C:/Users/", user, "/Downloads/us-alerts-quietsound-", Sys.Date(),".xlsx"))

export = detections_us %>% 
  dplyr::filter(sighted_at > as.Date("2024-12-31")) %>% 
  dplyr::as_tibble() %>%
  dplyr::mutate(
    longitude = sf::st_coordinates(geometry)[, 1],
    latitude = sf::st_coordinates(geometry)[, 2]
  ) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-geometry)

openxlsx::write.xlsx(export, paste0("C:/Users/", user, "/Downloads/us-detections-quietsound-", Sys.Date(),".xlsx"))

rm(export)


##~~~~~~~~~~~~~~~~~~~~~ Sandbox ~~~~~~~~~~~~~~~~~~~~~##


## US Coast Guard Numbers

# detections_pre %>% 
#   dplyr::filter(is.na(longitude) == F) %>% 
#   # sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   # sf::st_make_valid() %>% 
#   # sf::st_join(x = ., 
#   #             y = US_EZZ,
#   #             join = sf::st_within) %>% 
#   # dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>% 
#   # sf::st_drop_geometry() %>% 
#   dplyr::filter(stringr::str_detect(email, "(?i)uscg.mil\\b")) %>%  
#   # dplyr::filter(lubridate::year(sighted_at) == 2024) %>%
#   dplyr::mutate(
#     Month = lubridate::month(sighted_at, label = TRUE, abbr = TRUE),
#     Source = dplyr::if_else(
#       tolower(email) == "whales@uscg.mil", 
#       "Cetacean Desk", 
#       "Coast Guard Resources"
#     )
#   ) %>%
#   dplyr::count(Source, Month, name = "Sightings") %>%
#   tidyr::pivot_wider(
#     names_from = Month,
#     values_from = Sightings,
#     values_fill = 0
#   ) %>%
#   gt::gt() %>%
#   gt::tab_header(
#     title = "USCG Sightings by Source and Month (2024)"
#   ) %>%
#   gt::cols_label(
#     Source = "Source"
#   )
#   
# x = detections_pre %>% 
#   dplyr::filter(is.na(longitude) == F) %>% 
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   sf::st_make_valid() %>% 
#   sf::st_join(x = ., 
#               y = US_EZZ,
#               join = sf::st_within) %>% 
#   dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>% 
#   sf::st_drop_geometry() %>% 
#   dplyr::filter(stringr::str_detect(email, "(?i)uscg.mil\\b")) %>%  
#   dplyr::filter(lubridate::year(sighted_at) == 2024) %>%
#   dplyr::mutate(
#     Month = lubridate::month(sighted_at, label = TRUE, abbr = TRUE),
#     Source = dplyr::if_else(
#       tolower(email) == "whales@uscg.mil", 
#       "Cetacean Desk", 
#       "Coast Guard Resources"
#     )
#   ) %>% 
#   dplyr::select(sighted_at, id, Source, species, number_of_animals) %>% 
#   dplyr::left_join(alert_clean, by = dplyr::join_by(id == sighting_id)) %>% 
#   dplyr::mutate(
#     alert_sent = dplyr::case_when(
#       is.na(sent_at) == T ~ "no",
#       TRUE ~ "yes"
#     )
#   )%>%
#   dplyr::mutate(
#     year_month = lubridate::floor_date(as.POSIXct(sighted_at), unit = "month")
#   ) %>%
#   dplyr::group_by(year_month, Source, alert_sent) %>%
#   dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
#   tidyr::pivot_wider(
#     names_from = alert_sent,
#     values_from = count,
#     values_fill = 0,
#     names_prefix = "alert_"
#   )
# 
# 
# x = detections_pre %>% 
#   dplyr::filter(is.na(longitude) == F) %>% 
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
#   sf::st_make_valid() %>% 
#   sf::st_join(x = ., 
#               y = US_EZZ,
#               join = sf::st_within) %>% 
#   dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>% 
#   sf::st_drop_geometry() %>% 
#   dplyr::filter(stringr::str_detect(email, "(?i)uscg.mil\\b")) %>%  
#   dplyr::filter(lubridate::year(sighted_at) == 2024) %>%
#   dplyr::mutate(
#     Month = lubridate::month(sighted_at, label = TRUE, abbr = TRUE),
#     Source = dplyr::if_else(
#       tolower(email) == "whales@uscg.mil", 
#       "Cetacean Desk", 
#       "Coast Guard Resources"
#     )
#   ) %>% 
#   dplyr::select(sighted_at, id, Source, species, number_of_animals) %>% 
#   dplyr::left_join(alert_clean, by = dplyr::join_by(id == sighting_id)) %>% 
#   dplyr::mutate(
#     alert_sent = dplyr::case_when(
#       is.na(sent_at) == T ~ "no",
#       TRUE ~ "yes"
#     )
#   )%>%
#   dplyr::mutate(
#     year_month = lubridate::floor_date(as.POSIXct(sighted_at), unit = "month")
#   ) %>%
#   dplyr::filter(alert_sent == "yes")
# 
# x %>% 
#   dplyr::mutate(number_of_animals = dplyr::case_when(
#     grepl("~", number_of_animals) ~ as.numeric(sub(".*~([0-9]+)$", "\\1", number_of_animals)),
#     TRUE ~ as.numeric(number_of_animals)
#   )) %>% 
#   dplyr::group_by(species) %>%
#   dplyr::select(species, number_of_animals) %>% 
#   dplyr::summarise(count = sum(number_of_animals), .groups = "drop") %>%
#   tidyr::pivot_wider(
#     names_from = alert_sent,
#     values_from = count,
#     values_fill = 0,
#     names_prefix = "alert_"
#   )
# 
# 
# detections_us %>% 
#   dplyr::filter(stringr::str_detect(email, "(?i)uscg.mil\\b")) %>%  
#   dplyr::mutate(Source = dplyr::if_else(
#     tolower(email) == "whales@uscg.mil", 
#     "Cetacean Desk", 
#     "Coast Guard Resources"
#   )) %>%
#   dplyr::count(Source, name = "Sightings") %>%
#   gt::gt() %>%
#   gt::tab_header(
#     title = "Summary of USCG Sightings by Source"
#   ) %>%
#   gt::cols_label(
#     Source = "Source",
#     Sightings = "Number of Sightings"
#   )
# 
# summary_table
