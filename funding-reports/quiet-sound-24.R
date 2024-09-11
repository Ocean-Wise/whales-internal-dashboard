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
joined %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  # dplyr::filter(lubridate::month(sighted_at) == 4 & lubridate::year(sighted_at) == 2024) %>% 
  nrow()

## Acartia Sightings total
sighting_import %>% 
  dplyr::filter(stringr::str_detect(details, "Acartia")) %>% 
  # dplyr::filter(lubridate::month(sighted_at) == 4 & lubridate::year(sighted_at) == 2024) %>%
  nrow()


sighting_alert_us = sighting_alert %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone")


### Year on year change in alerts and sightings

n_sightings = sighting_alert_us %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year = lubridate::year(sent_at)) %>% 
  dplyr::summarize(count_sightings = dplyr::n_distinct(id)) 


n_alerts = sighting_alert_us %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year = lubridate::year(sent_at)) %>% 
  dplyr::summarize(count_alerts = dplyr::n())

counts = dplyr::left_join(n_sightings, n_alerts) %>% 
  dplyr::filter(!year == 2018 & is.na(year) == F)

## Drop this into excel and we can make a quick and easy graph that way for year on year growth

### Continuous change in alerts and sightings

cont_data = sighting_alert_us %>%
  dplyr::filter(dplyr::case_when(zoo::as.yearmon(sent_at) == zoo::as.yearmon(Sys.Date()) ~ F,
                                 zoo::as.yearmon(sent_at) != zoo::as.yearmon(Sys.Date()) ~ T))

monthly_sightings = cont_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_sightings = dplyr::n_distinct(id)) 

monthly_alerts = cont_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
  dplyr::summarize(count_alerts = dplyr::n()) 

cont_counts = dplyr::left_join(monthly_sightings, monthly_alerts)


## Data Viz 

vline <- list(type = "line",
              y0 = 0,
              y1 = 1,
              yref = "paper",
              x0 = "Oct 2023",
              x1 = "Oct 2023",
              line = list(color = "B1B1B1", dash="dot")
)



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
                              tickvals = c(as.character(cont_counts$year_month)[seq(1, length(cont_counts$year_month), by = 2)]),
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
                   x = "Aug 2023", y = 900,
                   text = "Acartia integrated")


SS## Combine formatting with initial graph to create cleaned plot
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
  dplyr::mutate(group = 
                  dplyr::case_when(
                    dplyr::between(sent_at, as.Date("2022-10-01"), as.Date("2023-04-26")) ~ 1,
                    dplyr::between(sent_at, as.Date("2023-10-01"), as.Date("2024-04-26")) ~ 2,
                    TRUE ~ 3
                  )) %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise(row_count = dplyr::n())
