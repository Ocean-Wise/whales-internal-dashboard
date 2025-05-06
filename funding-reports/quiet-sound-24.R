#####~~~~~~~~~~~~~~~~~~~~ Quiet Sound Reporting ~~~~~~~~~~~~~~~~~~~~~~~~######
## Author: Alex Mitchell
## Purpose: To produce monthly reports for Quiet Sound
## Date written: 2023-12-05
## Quality Assured: No

## Notes: metrics needed == 			
##        § Number of sightings into WRAS 
##        § Number of alerts


#####~~~~~~~~~~~~~~~~~~~~ Data import and basic cleaning ~~~~~~~~~~~~~~~~~~~~~~~~######

# source("./monthly-dashboard-numbers.R")


#####~~~~~~~~~~~~~~~~~~~~ Data manipulation ~~~~~~~~~~~~~~~~~~~~~~~~######

### Create US based sightings

## Import US economic exclusive zone.
US_EZZ = sf::read_sf(dsn = paste0("C:/Users/",
                                  user,
                                  "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/Shapefiles/World_EEZ_v11_20191118"), 
                     layer = "eez_v11") %>% 
  dplyr::filter(SOVEREIGN1 == "United States", MRGID == 8456)


## US Sightings total
joined_tables %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  # dplyr::filter(lubridate::month(sighted_at) == 4 & lubridate::year(sighted_at) == 2024) %>%
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


sighting_alert_us = joined_tables %>% 
  dplyr::filter(is.na(longitude) == F) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone" & is.na(code) == F)

### Year on year change in alerts and sightings
# 
# n_sightings = sighting_alert_us %>% 
#   sf::st_drop_geometry() %>% 
#   # dplyr::group_by(year = lubridate::year(sent_at)) %>% 
#   dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
#   dplyr::summarize(count_sightings = dplyr::n_distinct(sighting_id)) 
# 
# 
# n_alerts = sighting_alert_us %>% 
#   sf::st_drop_geometry() %>% 
#   # dplyr::group_by(year = lubridate::year(sent_at)) %>%
#   dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
#   dplyr::summarize(count_alerts = dplyr::n())
# 
# counts = dplyr::left_join(n_sightings, n_alerts) %>% 
#   dplyr::filter(is.na(year) == F)

## Drop this into excel and we can make a quick and easy graph that way for year on year growth

### Continuous change in alerts and sightings

# high_alerts_list = sighting_alert_us %>% 
#   # dplyr::filter(sent_at > as.Date("2024-07-31") & sent_at < as.Date("2025-01-01")) %>% 
#   dplyr::group_by(tracking_id) %>% 
#   sf::st_drop_geometry() %>% 
#   dplyr::summarise(number_of_alerts = dplyr::n()) %>% 
#   dplyr::left_join(., user_clean) %>% 
#   dplyr::filter(is.na(auth_id) == T)



cont_data = joined_tables %>%
  dplyr::filter(dplyr::case_when(zoo::as.yearmon(sent_at) == zoo::as.yearmon(Sys.Date()) ~ F,
                                 zoo::as.yearmon(sent_at) != zoo::as.yearmon(Sys.Date()) ~ T)) 

monthly_sightings = cont_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_detections = dplyr::n_distinct(sighting_id)) 

monthly_alerts = cont_data %>% 
  sf::st_drop_geometry() %>% 
  # dplyr::filter(!tracking_id %in% high_alerts_list$tracking_id) %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_alerts = dplyr::n()) 

cont_counts = dplyr::left_join(monthly_sightings, monthly_alerts) %>% 
  dplyr::mutate(year_month = zoo::as.Date(year_month)) %>% 
  dplyr::filter(year_month > "2022-12-01")


## Data Viz 

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
  plotly::layout(shapes = list(vline, vline2, vline3, highlight_rect),
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
                              dtick = "M6",
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
                   x = "2024-03-01", y = 2100,
                   text = "Orca Network \nsummer contract\n expired") %>% 
  plotly::add_text(showlegend = FALSE,
                   x = "2024-07-01", y = 1900,
                   text = "Orca Network \n contract renewed")



## Combine formatting with initial graph to create cleaned plot
# cumulative_alert_plot_cleaned = cumulative_alert_plot %>% 
#   plotly::layout(xaxis = cumulative_xaxis, 
#                  yaxis = cumulative_yaxis,
#                  margin = cumulative_margin,
#                  showlegend = F)

us_map = sighting_alert_us %>% 
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
    leaflet::addTiles() %>%
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





#### line graph alert source ####
detections_source = joined_tables %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(c(species, source_entity, sighted_at)) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(sighted_at > as.Date("2024-01-01") &  zoo::as.yearmon(sighted_at) < zoo::as.yearmon(Sys.Date())) %>% 
  dplyr::group_by(source_entity, yearmon = zoo::as.yearmon(sighted_at)) %>% 
  dplyr::summarise(detections = dplyr::n())

alerts_source = joined_tables %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(c(species, source_entity, sent_at)) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(sent_at > as.Date("2024-01-01") &  zoo::as.yearmon(sent_at) < zoo::as.yearmon(Sys.Date())) %>% 
  dplyr::group_by(source_entity, yearmon = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarise(alerts = dplyr::n())

source_colors = c(
  "Ocean Wise" = "#1f77a4",   # blue
  "Orca Network" = "#9ab11e", # green
  "SMRU" = "#ff3aff"          # Purple
)

source_details = detections_source %>% 
  dplyr::left_join(alerts_source) %>% 
  dplyr::mutate(yearmon = lubridate::my(yearmon),
                year = lubridate::year(yearmon),
                month = lubridate::month(yearmon, label = TRUE, abbr = TRUE)) %>% 
  dplyr::mutate(line_dash = ifelse(year == 2024, "dash", "solid"),
                opacity = ifelse(year == 2024, 0.5, 1),
                line_group = paste(source_entity, year) # so we can group correctly
                )



plotly::plot_ly() %>%
  plotly::add_trace(
    data = source_details,
    x = ~month,
    y = ~detections,
    type = "line",
    split = ~line_group,
    opacity = ~opacity
    # text = ~paste(source_entity, "<br>Year:", year, "<br>Month:", month, "<br>Detections:", detections),
    # hoverinfo = "text"
    # marker = list(color = source_colors[source_entity])
  ) %>%
  plotly::layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Detections"),
    barmode = "group",
    legend = list(title = list(text = "<b>Source</b>"))
  )

## Detections
plotly::plot_ly() %>%
  plotly::add_trace(
    data = source_details,
    x = ~month,
    y = ~detections,
    # color = ~source_entity,
    # colors = ~source_colors,
    line = ~list(
      dash = ifelse(year == 2024, "dash", "solid"),
      width = 2,
      color = source_colors[source_entity]
    ),
    mode = "line",
    split = ~line_group,
    opacity = ~opacity,
    text = ~paste(source_entity, "<br>Year:", year, "<br>Month:", month, "<br>Detections:", detections),
    hoverinfo = "text",
    type = "scatter"
  ) %>%
  plotly::layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Detections"),
    legend = list(title = list(text = "<b>Source</b>"))
  )


## Alerts
plotly::plot_ly() %>%
  plotly::add_trace(
    data = source_details,
    x = ~month,
    y = ~alerts,
    # color = ~source_entity,
    # colors = ~source_colors,
    line = ~list(
      dash = ifelse(year == 2024, "dash", "solid"),
      width = 2,
      color = source_colors[source_entity]
    ),
    mode = "lines",
    split = ~line_group,
    opacity = ~opacity,
    text = ~paste(source_entity, "<br>Year:", year, "<br>Month:", month, "<br>Detections:", detections, "<br>Alerts:", alerts),
    hoverinfo = "text",
    type = "scatter"
  ) %>%
  plotly::layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Alerts"),
    legend = list(title = list(text = "<b>Source</b>"))
  )



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
  dplyr::mutate(dplyr::across(.cols = 2:11, ~tidyr::replace_na(.x,0))) %>% 
  dplyr::select(-`NA`)

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

high_alerts_list = sighting_alert_us %>% 
  dplyr::filter(sent_at > as.Date("2025-01-01") & zoo::as.yearmon(sent_at) < zoo::as.yearmon(Sys.Date())) %>%
  dplyr::group_by(tracking_id) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::summarise(number_of_alerts = dplyr::n()) %>% 
  dplyr::left_join(., user_clean) %>% 
  dplyr::filter(is.na(auth_id) == F)


