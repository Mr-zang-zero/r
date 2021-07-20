#!/usr/bin/env Rscript

# Author: KM Hall
#
# 
#
# Purpose: 
# To process the hourly raw data created in 01_raw_hourly_met_processing.R.  Creates date 
# variables, filters out of range records for variables, creates a log of out of range records 
# that are reset to missing (-999 or NA), and produces to data sets.
# One data set include -999 (missing) & -888 (not collected).  The other data set includes NA
# in place of -999 or -888.  
#
# NOTE TO USER: You should have run program 00_user_setup_met_processing.R and
#   program 01_raw_hourly_met_processing.R prior to running this program


# setup ----
# clear any datasets that are loaded in R session
rm(list = ls(all=TRUE))

# load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)


source("./programs/00_user_setup_met_processing.R")




# read in hourly_raw from year of interest
met <- read_csv(paste("./data_output_files/hourly_raw_", year_to_process_last2digits, ".csv", sep = ""))

met <- unique(met)

head(met)
names(met)
summary(met)


##### Creating datetime (dt) variable --------------------------------------------------------------------------------
met <- met %>% mutate(hr_minus_1 = (time / 100) - 1,
                       min_value = "00") %>% 
  unite(date, day, year, sep = "-", remove = FALSE) 
   # Note: the reason for the hr_minus_1 variable is that R uses 0:00:00 as midnight, not 24:00:00.
   # The dataloggers collect data so that, for example, 200 is the data from 1 am to 2 am.  So, it is
   # for the previous hour in the hourly data.  
   #
   # Note: According to Doug, all times are Mountain Standard Time.  They are not adjusted for Mountain Daylight Time.
   #
   # Note:  This is more of a reminder for the minute-rain data.  A time of 101 really means 1:01 am according to Doug.



met <- met %>% unite(hr, hr_minus_1, min_value, sep = ":")

# converts from julian day to yyyy-mm-dd date format
met$date <- as.Date(met$date, format = "%j-%Y")

met <- met %>% unite(dt, date, hr, sep = " ", remove = FALSE)
met$dt <- as.character(met$dt)

met <- met %>% mutate(dt = ymd_hm(met$dt))

names(met)
str(met)


# cleaning up some time variables
str(met)
met <- met %>% select(-time) %>% separate(hr, into=c("hr", "min")) %>% select(-min) %>% 
  rename(hour =  hr, day_of_year = day) %>% mutate(month = month(date), 
                                                   day_of_month = day(date),
                                                   hour = str_sub(hour, 1, 2))  


met <- met %>% select(sta, dt, date, year, month, day_of_month, day_of_year, hour, airt:pan)


met$sta <- as.factor(met$sta)





##### Filter out bad data using previously used filters -------------------------------------------------------------------------------

variable <- c("airt", "maxair", "minair", "rh", "ms", "vms", "dir", "sddir",
              "maxw", "minw", "cm1", "cm10", "vp", "ppt", "sm10", "sm30", "sol", 
              "maxsol", "minsol","evap", "th2o", "bar")
oor_max <- c(50, 50, 50, 110, 50, 50, 365, 200, 70, 50, 80, 50, 30, 100, 
             20, 20, 500, 800, 400, 15, 60, 1030)
oor_min <- c(-40, -40, -40, 0, 0, 0, -5, 0, 0, 0, -30, -10, 0, 0, 0.1, 0.1, 
             -5, -5, -5, 0, -10, 0)


variable_ranges <- data.frame(variable, oor_max, oor_min)


# convert data from wide to long, and subset datasets based on filter criteria
met_long <- met %>% 
  gather(airt:pan, key = "variable", value = "value")

met_long <- met_long %>% 
  left_join(variable_ranges)


# separate out variables that do not have out of range adjustments - bat, pan.  Want to keep correct values.
met_long_bat_pan <- met_long %>% 
  filter(variable == "bat" | variable == "pan") %>% 
  select(-c(oor_max, oor_min))

met_long <- met_long %>% 
  filter(variable != "bat" & variable != "pan")


# create an out of range log to keep track of bad values
# oor_log <- met_long %>% 
#   filter((value != -999 & value != -888) & (value > oor_max | value < oor_min)) %>% 
#   select(1:9) %>% mutate(error_log = "out of range - reset to -999")
oor_flag <- met_long %>% 
  filter((value != -999 & value != -888) & (value > oor_max | value < oor_min)) %>% 
  select(1:10) %>% mutate(out_of_range_flag = 1,
                          missing_flag = 1,
                          not_collected_flag = 0)
missing_flag <- met_long %>% 
  filter(value == -999) %>% 
  select(1:10) %>% mutate(out_of_range_flag = 0,
                          missing_flag = 1,
                          not_collected_flag = 0)
not_collected_flag <- met_long %>% 
  filter(value == -888) %>% 
  select(1:10) %>% mutate(out_of_range_flag = 0,
                          missing_flag = 0,
                          not_collected_flag = 1)


summary(oor_flag)
summary(missing_flag)
summary(not_collected_flag)

error_log <- rbind(oor_flag, missing_flag, not_collected_flag)
summary(error_log)



# assign -999 to out of range variables
met_long_value_corrected <- met_long %>% 
  mutate(value_corrected = ifelse((value != -999 & value != -888) & (value > oor_max | value < oor_min), -999, value)) 

met_long <- met_long_value_corrected %>% select(-c(value, oor_max, oor_min)) %>% 
  rename(value = value_corrected)

met_long <- rbind(met_long, met_long_bat_pan)

# back to wide with -999s and -888s included
met_filtered_neg999888 <- met_long %>% 
  spread(key = variable, value = value) %>% 
  select(sta, dt, date, year, month, day_of_month, day_of_year, hour, airt, maxair, minair, rh, ms, vms, dir, sddir,
         maxw, minw, cm1, cm10, vp, ppt, sm10, sm30, sol, maxsol, minsol,
         evap, th2o, bar, bat, pan)

summary(met_filtered_neg999888)

met_filtered_neg999888[is.na(met_filtered_neg999888)] <- -999  # NAs sometimes sneak in


# back to wide - change -999s and -888s to NA for further analysis in R

met_long$value <- ifelse((met_long$value == -999 | met_long$value == -888), NA, met_long$value)


met_filtered <- met_long %>% 
  spread(key = variable, value = value) %>% 
  select(sta, dt, date, year, month, day_of_month, day_of_year, hour, airt, maxair, minair, rh, ms, vms, dir, sddir,
         maxw, minw, cm1, cm10, vp, ppt, sm10, sm30, sol, maxsol, minsol,
         evap, th2o, bar, bat, pan)



summary(met_filtered)



##### Graph data to look for any obvious outliers ------------------------------------------------------------

# function draws a time series plot for a specified variable
ts_fxn <- function(met_filtered, met_var) {
  ggplot(data = met_filtered, aes_string(x = "dt", y = met_var, color = "sta")) +
    geom_line(size = .5) + facet_wrap(~sta)
}


# NOTE TO USER: It might be useful to take some notes (by variable and station) as you look at these graphs of
#  data points that look problematic.  The notes can be used for you to further clean up the data.  
# ts_fxn(met_filtered, "airt")
# ts_fxn(met_filtered, "maxair")
# ts_fxn(met_filtered, "minair")
# ts_fxn(met_filtered, "rh")
# ts_fxn(met_filtered, "ms")
# ts_fxn(met_filtered, "vms")
# ts_fxn(met_filtered, "dir")
# ts_fxn(met_filtered, "sddir")
# ts_fxn(met_filtered, "maxw")
# ts_fxn(met_filtered, "minw")
# ts_fxn(met_filtered, "cm1")
# ts_fxn(met_filtered, "cm10")
# ts_fxn(met_filtered, "vp")
# ts_fxn(met_filtered, "ppt")
# ts_fxn(met_filtered, "sm10")
# ts_fxn(met_filtered, "sm30")
# ts_fxn(met_filtered, "sol")
# ts_fxn(met_filtered, "maxsol")
# ts_fxn(met_filtered, "minsol")
# ts_fxn(met_filtered, "evap")
# ts_fxn(met_filtered, "th2o")
# ts_fxn(met_filtered, "bar")
# ts_fxn(met_filtered, "bat")
# ts_fxn(met_filtered, "pan")


# TODO: need better filters for bat and bar!!!!
met_filtered %>% filter(sta != 42) %>% 
ggplot(., aes_string(x = "dt", y = "bar", color = "sta")) +
  geom_line(size = .5) + facet_wrap(~sta)
met_filtered %>% select(bar, sta) %>% group_by(sta) %>% summarize(bar_min = min(bar, na.rm = TRUE),
                                                                  bar_max = max(bar, na.rm = TRUE),
                                                                  bar_median = median(bar, na.rm = TRUE),
                                                                  bar_mean = mean(bar, na.rm = TRUE))



##### Write out files ----------------------------------------------------------------------------------------------

# filtered file, but contains -999 and -888 instead of NA
write_csv(met_filtered_neg999888, paste("./data_output_files/hourly_filtered_neg999888_", year_to_process_last2digits, ".csv", sep = ""))

# filtered file containing NA for missing values
write_csv(met_filtered, paste("./data_output_files/hourly_filtered_", year_to_process_last2digits, ".csv", sep = ""))    # this will be the easiest datatset to work with 

# Out of range log - not writing this out because it can be recreated from hourly raw data and this program
# write_csv(oor_log, paste("./data_output_files/oor_log_", year_to_process_last2digits, ".csv", sep = ""))


##### Variable information ----------------------------------------------------------------------------------------
# Variables in filtered data:  
# sta - Meteorological Station ID  
# dt - Date Time (YYYY-MM-DDTHH:MM:SSZ)  
# date - Date (YYYY-MM-DD)  
# year - Year  
# month - Month of Year  
# day_of_month -  Day of the Month  
# day_of_year - Julian Day of the Year  
# hour - Hour of the Day  
# airt - Mean Air Temperature (Celsius)  
# maxair - Maximum Air Temperature (C)  
# minair - Minimum Air Temperature (C)  
# rh - Relative Humidity (%)  
# ms - Mean Wind Speed (meters per second)  
# vms - Mean Vectored Wind Speed (m/s)  
# dir - Wind Direction (0-360 degrees)  
# sddir - Standard Deviation of the Wind  Direction (degrees)  
# maxw - Maximum Wind Speed (m/s)  
# minw - Minimum Wind Speed (m/s)  
# cm1 - Soil Temperature @ 1 cm (C)  
# cm10 - Soil Temperature @ 10 cm (C)  
# vp - Mean Vapor Pressure (mbars)  
# ppt - Precipitation (mm)  
# sm10 - Soil Moisture Potential @ 10 cm (bars)  
# sm30 - Soil Moisture Potential @ 30 cm (bars)  
# sol - Mean Solar Flux (joules/cm2/sec)  
# maxsol - Maximum Solar Flux (joules/cm2/sec)  
# minsol - Minimum Solar Flux (joules/cm2/sec)  
# evap - not used  
# th2o - not used  
# bar - not used  
# bat - Battery Voltage (Volts)  
# pan - not used  





