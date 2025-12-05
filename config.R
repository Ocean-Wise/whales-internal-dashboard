####~~~~~~~~~~~~~~~~~~~~~~Configuration~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Database connection and global configuration variables
## Date written: 2025-10-29

####~~~~~~~~~~~~~~~~~~~~~~Packages~~~~~~~~~~~~~~~~~~~~~~~####
library(magrittr)

####~~~~~~~~~~~~~~~~~~~~~~Database Connection~~~~~~~~~~~~~~~~~~~~~~~####
## Connect to the read-only database instance
connect = DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = 3306,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  ssl.ca = Sys.getenv("SSL_CA")
)

## Note: Connection must be closed at end of session with DBI::dbDisconnect(connect)

####~~~~~~~~~~~~~~~~~~~~~~Global Variables~~~~~~~~~~~~~~~~~~~~~~~####

## Date range for analysis (easily configurable)
start_date = lubridate::as_date("2019-01-01")
end_date = lubridate::today()

## Source filter - which data providers to include
source_filter = c("Ocean Wise", "Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert")

## Create a regex pattern that matches any of these to filter for data we are allowed to share. 
ocean_wise_data_only = paste(c("Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert", "testing", "BCHN/SWAG"), collapse = "|")



## Test user filter - users to exclude from impact metrics
## (to be populated after data exploration)
test_user_ids = c()

####~~~~~~~~~~~~~~~~~~~~~~Color Palette~~~~~~~~~~~~~~~~~~~~~~~####
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

## Function to get colors for plotting
get_ocean_wise_colors = function(n) {
  if (n > length(ocean_wise_palette)) {
    warning("Not enough Ocean Wise colors — some colors will be reused.")
  }
  rep(ocean_wise_palette, length.out = n)
}

####~~~~~~~~~~~~~~~~~~~~~~Helper Functions~~~~~~~~~~~~~~~~~~~~~~~####

## Function to extract latitude from MySQL POINT type
extract_latitude = function(point_column) {
  DBI::dbGetQuery(connect, paste0("SELECT ST_Y(", point_column, ") as lat"))$lat
}

## Function to extract longitude from MySQL POINT type
extract_longitude = function(point_column) {
  DBI::dbGetQuery(connect, paste0("SELECT ST_X(", point_column, ") as lon"))$lon
}
