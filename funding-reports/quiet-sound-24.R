#####~~~~~~~~~~~~~~~~~~~~ Quiet Sound Reporting ~~~~~~~~~~~~~~~~~~~~~~~~######
## Author: Alex Mitchell
## Purpose: To produce monthly reports for Quiet Sound
## Date written: 2023-12-05
## Quality Assured: No

## Notes: metrics needed == 			
##        § Number of sightings into WRAS 
##        § Number of alerts


#####~~~~~~~~~~~~~~~~~~~~ Data import and basic cleaning ~~~~~~~~~~~~~~~~~~~~~~~~######

source("./monthly-dashboard-numbers.R")


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
                      zoo::as.yearmon("Sep 2023"), 
                      zoo::as.yearmon("Aug 2024"))
  )
  
  # nrow()

## Acartia Sightings total
joined_tables %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  # sf::st_join(x = ., 
  #             y = US_EZZ,
  #             join = sf::st_within) %>% 
  # dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  dplyr::filter(source_entity == "Orca Network") %>% 
  # dplyr::filter(lubridate::month(sighted_at) == 3 & lubridate::year(sighted_at) == 2024) %>%
  # leaflet::leaflet() %>% 
  # leaflet::addTiles() %>% 
  # leaflet::addCircleMarkers()
  # sf::st_drop_geometry() %>% 
  dplyr::select(sighting_id,sent_at,source_entity) %>% 
  dplyr::distinct() %>% 
  nrow()


sighting_alert_us = joined_tables %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone")


### Year on year change in alerts and sightings

n_sightings = sighting_alert_us %>% 
  sf::st_drop_geometry() %>% 
  # dplyr::group_by(year = lubridate::year(sent_at)) %>% 
  dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
  dplyr::summarize(count_sightings = dplyr::n_distinct(sighting_id)) 


n_alerts = sighting_alert_us %>% 
  sf::st_drop_geometry() %>% 
  # dplyr::group_by(year = lubridate::year(sent_at)) %>%
  dplyr::group_by(year_mon = zoo::as.yearmon(sent_at)) %>%
  dplyr::summarize(count_alerts = dplyr::n())

counts = dplyr::left_join(n_sightings, n_alerts) %>% 
  dplyr::filter(is.na(year) == F)

## Drop this into excel and we can make a quick and easy graph that way for year on year growth

### Continuous change in alerts and sightings

cont_data = sighting_alert_us %>%
  dplyr::filter(dplyr::case_when(zoo::as.yearmon(sent_at) == zoo::as.yearmon(Sys.Date()) ~ F,
                                 zoo::as.yearmon(sent_at) != zoo::as.yearmon(Sys.Date()) ~ T))

monthly_sightings = cont_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_sightings = dplyr::n_distinct(sighting_id)) 

monthly_alerts = cont_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_alerts = dplyr::n()) 

cont_counts = dplyr::left_join(monthly_sightings, monthly_alerts) %>% 
  dplyr::mutate(year_month = zoo::as.Date(year_month)) %>% 
  dplyr::filter(year_month < "2024-04-01")


## Data Viz 

vline <- list(type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = "2023-10-01",
              x1 = "2023-10-01",
              line = list(color = "B1B1B1", dash="dot")
)


# vline2 <- list(type = "line",
#               y0 = 0,
#               y1 = 1,
#               yref = "paper",
#               x0 = "2024-05-01",
#               x1 = "2024-05-01",
#               line = list(color = "B1B1B1", dash="dot")
# )



# vline3 <- list(type = "line",
#               y0 = 0,
#               y1 = 1,
#               yref = "paper",
#               x0 = "2024-08-01",
#               x1 = "2024-08-01",
#               line = list(color = "B1B1B1", dash="dot")
# )



plotly::plot_ly(data = cont_counts,
                y = ~count_sightings,
                x = ~as.character(year_month),
                name = "Sightings",
                type = "scatter",
                mode = "lines",
                line = list(color = "A8007E")) %>% 
  plotly::add_trace(y = ~count_alerts,
                    name = "Alerts",
                    type = "scatter",
                    mode = "lines",
                    line = list(color = "005580")) %>% 
  plotly::layout(shapes = list(vline),
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
                   x = "2023-07-01", y = 2000,
                   text = "Acartia \nintegrated") 
  # plotly::add_text(showlegend = FALSE, 
  #                  x = "2024-03-01", y = 2100,
  #                  text = "Orca Network \nsummer contract\n expired")
  # plotly::add_text(showlegend = FALSE, 
  #                  x = "2024-11-01", y = 1900,
  #                  text = "Orca Network \n contract renewed")


## Combine formatting with initial graph to create cleaned plot
# cumulative_alert_plot_cleaned = cumulative_alert_plot %>% 
#   plotly::layout(xaxis = cumulative_xaxis, 
#                  yaxis = cumulative_yaxis,
#                  margin = cumulative_margin,
#                  showlegend = F)

sighting_alert_us = sighting_import %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>% 
  dplyr::filter(lubridate::year(sighted_at) == 2023 | lubridate::year(sighted_at) == 2024) %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addCircleMarkers(radius = 1, col = "black") 


sighting_alert_us %>% 
  htmlwidgets::saveWidget(., file = "../../../../Downloads/sightings_us_23_24.html")



increased_alerts = sighting_alert_us %>%
  sf::st_drop_geometry() %>% 
  dplyr::mutate(group = 
                  dplyr::case_when(
                    dplyr::between(sent_at, 1as.Date("2022-10-01"), as.Date("2023-04-26")) ~ 1,
                    dplyr::between(sent_at, as.Date("2023-10-01"), as.Date("2024-04-26")) ~ 2,
                    TRUE ~ 3
                  )) %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise(row_count = dplyr::n())
