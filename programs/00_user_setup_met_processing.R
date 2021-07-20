#!/usr/bin/env Rscript

# Author: KM Hall
# 
# 
# Purpose: 
# Setup user information in order to process hourly meteorological data for a given year

library(stringr)

# USER: list the 4-digit year to process ---- 
# this should be a numeric value - this needs to 
# be changed each time you are processing a different year of raw hourly met data.
year_to_process <- 2021


# USER: set the working directory to the top directory for your project ---- 
# the other programs navigate the file structure based on setting this directory properly -
# this may only need to be done once if you run the programs from the same directory each time.
setwd("~/Documents/SEV/Projects/Meteorological_Data_Processing/")




# add other variables that are used in other programs - the user does not need
# to do anything here
ytp <- as.character(year_to_process)
year_to_process_last2digits <- str_sub(ytp, 3, 4)


