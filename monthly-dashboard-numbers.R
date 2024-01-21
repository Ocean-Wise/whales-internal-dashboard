####~~~~~~~~~~~~~~~~~~~~~~Monthly Dashboard Metrics~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the production of metrics for the ELT dashboard. Should be able to be run by anyone in the team.
## Date written: 2024-01-19
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## Run this section first
library(magrittr)

user = "AlexMitchell"

source(file = "./data-processing.R")


####~~~~~~~~~~~~~~~~~~~~~~# WRAS alerts vs 2023~~~~~~~~~~~~~~~~~~~~~~~####
### Goal...
## Send a total of 15,000 WRAS alerts (at least a 10% increase from 2023 baseline) in BC and WA waters to reduce 
## the risk of ship strike for at least 15,000 encounters with whales




####~~~~~~~~~~~~~~~~~~~~~~total number of acartia alerts~~~~~~~~~~~~~~~~~~~~~~~####

####~~~~~~~~~~~~~~~~~~~~~~total number of hydrophone alerts~~~~~~~~~~~~~~~~~~~~~~~####

