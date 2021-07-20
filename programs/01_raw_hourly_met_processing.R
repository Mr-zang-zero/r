#!/usr/bin/env Rscript

# Author: KM Hall
#
# 
# Purpose: 
# Process the raw hourly meteorological data that is collected by 
# the 10 full meteorological stations at the SEV.  
#
# 
#
# READ THIS!!!!!
# 
# NOTES TO USER (IMPORTANT!!!): 
# The program assumes that you have a directory structure for this project as follows:
# - A main directory folder for the project - this is what you will set your working directory to
#   at the beginning of the code below.
# - Under the main directory, you should have a "data_raw_hourly" subdirectory
#   and a "data_output_files" subdirectory.  Place the raw meteorological .dat files for
#   all 10 met stations in the "data_raw_hourly" folder - this is where the program 
#   will look for the files.  The program combines the data from all of the stations
#   and outputs files that contain data for all stations into the "data_output_files" folder.
# 
#   Folder structure:
#   Meteorological_Data_Processing
#   |_ data_raw_hourly
#   |_ data_output_files
#   
#
# You need to run program 00_user_setup_met_processing.R before running this program
# (End of notes to user)





########################## Begin Program ##########################


##### Program setup -----------------------------------------------
# clear any datasets that are loaded
rm(list=ls(all=TRUE)) 

# sources program 00_user_setup_met_processing.R - needs to be run before this program
source("./programs/00_user_setup_met_processing.R")



library(tidyverse)
library(stringr)


paste0("You are processing data for the year: ", year_to_process)
paste0("Your working directory is: ", getwd())
paste0("Output file for this program will be located here: ", getwd(), "/data_output_files")


##### Read in and combine the raw hourly files ---------------------
file_list <- list.files("./data_raw_hourly", full.names = TRUE)
file_list 

# pull out station 42 data from the loop that reads in data.  see below.
if (year_to_process_last2digits >= 19) {
  file_list <- file_list[file_list != "./data_raw_hourly/met42.dat"]
  met_42_file <- "./data_raw_hourly/met42.dat"
}

#The following raw files will be used as input - there should be one file per station
file_list




for (file in file_list) {
  # if the merged dataset doesn't exist, create it - don't run this twice in a session.
  if (!exists("all_raw")) {
    all_raw <- read_csv(file, col_names = FALSE)
  } else {
    temp_dataset <- read_csv(file, col_names = FALSE)
    all_raw <- rbind(all_raw, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merges dataset does exist, append to it
  # if (exists("all_raw")) {
  #   temp_dataset <- read_csv(file, col_names = FALSE)
  #   all_raw <- rbind(all_raw, temp_dataset)
  #   rm(temp_dataset)
  # }
}


# need to handle station 42 for 2019 forward separately.  Only has 27 vars - missing last var - this throws off the above statement.
if (year_to_process_last2digits >= 19) {
  met_42 <- read_csv(met_42_file, col_names=FALSE)

  # add the missing column at the end
  met_42 <- met_42 %>%
    mutate(X28 = NA)
}

# merge 42 with other data
all_raw <- rbind(all_raw, met_42)



# Note to user:
# The variables do not yet have proper header names. They have "Xn" assignments, where n is 1-28.  
#
# all_raw generates errors because the data contain embedded 1-minute rows of data when it rains. 
# It expects 28 columns of data, but only 5 exist for 1-minute rain data.  This is dealt with below.  
#
# Not adding column/header names until the end of this program because
# 1-minute rain data contains ppt in column 5 and not airt like the 
# hourly data.




##### Filter data to year of interest, deduplicate, separate minute rain data from hourly data -------------

# examine data
str(all_raw)      # Note: all_raw should contain 28 variables
head(all_raw)
summary(all_raw)
# get rid of duplicate rows
notdeduped <- nrow(all_raw)
all_raw <- unique(all_raw)
deduped <- nrow(all_raw)

notdeduped
deduped
notdeduped - deduped

paste0("There were ", notdeduped, " rows. ", (notdeduped - deduped), " duplicate rows were removed. The deduplicated dataset contains ", deduped, " rows.")


# summarize raw data by year before filtering to the year of interest
all_raw %>% 
  group_by(X2) %>% 
  summarize(n())

# filter the dataset to only contain the year of data that you specified at the beginning 
# of the program
all_raw <- all_raw %>% 
  group_by(X2) %>% 
  filter(X2 == year_to_process)

all_raw %>% 
  group_by(X2) %>% 
  summarize(n())

# summary by station - to see whether any stations have unexpectedly high/low record counts.
all_raw %>% 
  group_by(X1) %>% 
  summarize(n())


# get rid of any non-numeric data
summary(all_raw)
all_raw <- as_tibble(sapply(all_raw, as.numeric))
summary(all_raw)

# separate 1-minute rain data from hourly data.
# pulls out data for rain data records that only populates the first 5 column
rain_raw <- all_raw %>% 
  filter(is.na(X6) & is.na(X7) & is.na(X8) & 
           is.na(X9) & is.na(X10) & is.na(X11) &
           is.na(X12) & is.na(X13) & is.na(X14) &
           is.na(X15) & is.na(X16) & is.na(X17) &
           is.na(X18) & is.na(X19) & is.na(X20) & 
           is.na(X21) & is.na(X22) & is.na(X23) &
           is.na(X24) & is.na(X25) & is.na(X26) &
           is.na(X27) & is.na(X28)) 


head(rain_raw)

rain_raw %>% 
  group_by(X2) %>% 
  summarize(n())

summary(rain_raw)


# only keep first five variables of 1-minute data.  The other columns are all NA.
rain_raw <- rain_raw %>% 
  select(1:5)




# hourly data - without 1-minute rain data
hourly_raw <- all_raw %>% 
  anti_join(rain_raw)

head(hourly_raw)
hourly_raw %>% 
  group_by(X2) %>% 
  summarize(n())

# this provides a better summary by station after rain data is removed
hourly_raw %>% 
  group_by(X1) %>% 
  summarize(n())

summary(hourly_raw)

# Some NAs may be generated, and not filled with -999 or -888.  Need to fix these
# Note: uncomment code to see records that generate NAs

# View(hourly_raw %>% filter(is.na(X1) | is.na(X2) | is.na(X3) | is.na(X4) |
#                              is.na(X5) | is.na(X6) | is.na(X7) | is.na(X8) | 
#                              is.na(X9) | is.na(X10) | is.na(X11) |
#                              is.na(X12) | is.na(X13) | is.na(X14) |
#                              is.na(X15) | is.na(X16) | is.na(X17) |
#                              is.na(X18) | is.na(X19) | is.na(X20) | 
#                              is.na(X21) | is.na(X22) | is.na(X23) |
#                              is.na(X24) | is.na(X25) | is.na(X26) |
#                              is.na(X27) | is.na(X28))) 


hourly_raw[is.na(hourly_raw)] <- -999



# verify that records in new datasets add up to the original
nrow(all_raw)
nrow(rain_raw) + nrow(hourly_raw)

paste0("There were ", nrow(all_raw), " rows in all_raw. rain_raw and hourly_raw together contain ", (nrow(rain_raw) + nrow(hourly_raw)), " rows of data. These numbers should be equal.")
paste0("Are the rows equal? ", ifelse(nrow(all_raw) == (nrow(rain_raw) + nrow(hourly_raw)), 'Yes', 'No'))


##### Calibrate hourly data for some variables and stations, create logs of -999s and -888s ----------------------------

# Note (201807) that the following are old calibrations according to Doug's SAS program.  Marcy plans to 
# recalibrate the guages at some point, and then new calibration values would need to be
# used.

# recalibrate X21(sol), X22(maxsol), X23(minsol)
hourly_raw$X21 <- ifelse(hourly_raw$X1 == 1, hourly_raw$X21/1.023, 
                         ifelse(hourly_raw$X1 == 40, hourly_raw$X21/1.0117, 
                                ifelse(hourly_raw$X1 == 41, hourly_raw$X21/1.06395, 
                                       ifelse(hourly_raw$X1 == 42, hourly_raw$X21/1.014, 
                                              ifelse(hourly_raw$X1 == 43, hourly_raw$X21/0.9936,
                                                     ifelse(hourly_raw$X1 == 44, hourly_raw$X21/1.003, 
                                                            ifelse(hourly_raw$X1 == 45, hourly_raw$X21/0.9954, 
                                                                   ifelse(hourly_raw$X1 == 50, hourly_raw$X21/2.777, hourly_raw$X21))))))))


hourly_raw$X22 <- ifelse(hourly_raw$X1 == 1, hourly_raw$X22/1.023, 
                            ifelse(hourly_raw$X1 == 40, hourly_raw$X22/1.0117, 
                                   ifelse(hourly_raw$X1 == 41, hourly_raw$X22/1.06395, 
                                          ifelse(hourly_raw$X1 == 42, hourly_raw$X22/1.014, 
                                                 ifelse(hourly_raw$X1 == 43, hourly_raw$X22/0.9936,
                                                        ifelse(hourly_raw$X1 == 44, hourly_raw$X22/1.003, 
                                                               ifelse(hourly_raw$X1 == 45, hourly_raw$X22/0.9954, 
                                                                      ifelse(hourly_raw$X1 == 50, hourly_raw$X22/2.777, hourly_raw$X22))))))))


hourly_raw$X23 <- ifelse(hourly_raw$X1 == 1, hourly_raw$X23/1.023, 
                            ifelse(hourly_raw$X1 == 40, hourly_raw$X23/1.0117, 
                                   ifelse(hourly_raw$X1 == 41, hourly_raw$X23/1.06395, 
                                          ifelse(hourly_raw$X1 == 42, hourly_raw$X23/1.014, 
                                                 ifelse(hourly_raw$X1 == 43, hourly_raw$X23/0.9936,
                                                        ifelse(hourly_raw$X1 == 44, hourly_raw$X23/1.003, 
                                                               ifelse(hourly_raw$X1 == 45, hourly_raw$X23/0.9954, 
                                                                      ifelse(hourly_raw$X1 == 50, hourly_raw$X23/2.777, hourly_raw$X23))))))))




# recalibrate X18(ppt)
hourly_raw$X18 <- ifelse(hourly_raw$X1 == 1, hourly_raw$X18/0.922, 
                         ifelse(hourly_raw$X1 == 40, hourly_raw$X18/0.975,
                                ifelse(hourly_raw$X1 == 41, hourly_raw$X18/1.006, 
                                       ifelse(hourly_raw$X1 == 42, hourly_raw$X18/0.960,
                                              ifelse(hourly_raw$X1 == 44, hourly_raw$X18/1.016,
                                                     ifelse(hourly_raw$X1 == 45, hourly_raw$X18/0.903,
                                                            ifelse(hourly_raw$X1 == 48, hourly_raw$X18/0.94,
                                                                   ifelse(hourly_raw$X1 == 49, hourly_raw$X18/0.94, hourly_raw$X18))))))))







# create a log of -999 and -888 values for later use
summary(hourly_raw)

# first, delete any records where X1(sta), X3(day) or X4(time) are -999, -888, or NA 
# by selecting a year of data at the beginning of the program, any NA for year also filtered out.
# you probably won't find these in the data, but this is just to be safe.
nrow(hourly_raw)  # number of rows before filtering

hourly_raw <- hourly_raw %>% filter(X1 != -999 & X1 != -888 & !is.na(X1) & X3 != -999 & X3 != -888 &
                        !is.na(X3) & X4 != -999 & X4 != -888 & !is.na(X4))

nrow(hourly_raw)  # number of rows after filtering - usually won't be different than before filtering

# make sure rows of data are unique
nrow(hourly_raw)
hourly_raw <- unique(hourly_raw)
nrow(hourly_raw)


hourly_raw <- hourly_raw %>% distinct(X1, X2, X3, X4, .keep_all = TRUE)


# Check for accurate hourly data and correct 
# make sure that there are only records that are on the hour (e.g. - 100, 200, ..., 1100, 1200)
table(hourly_raw$X4)

# there may be some full records that are not on the hour and are not 1-minute rain data (i.e - have full record).  
# delete these records, as per Marcy.
hourly_raw$X4 <- as.character(hourly_raw$X4)

# uncomment next line to inspect bad data
# View(met_hourly %>% filter(str_sub(time, -2, -1) != "00"))

hourly_raw <- hourly_raw %>% 
  filter(str_sub(X4, -2, -1) == "00")

hourly_raw$X4 <- as.numeric(hourly_raw$X4)

table(hourly_raw$X4)


# create a long format of the dataset
hourly_raw_long <- hourly_raw %>% gather(5:28, key = "variable", value = "value")

# create datasets that document -999 and -888 separately - -999 is missing, -888 is not collected
hourly_raw_neg999_log <- hourly_raw %>% gather(5:28, key = "variable", value = "value") %>% 
  filter(value == -999)
hourly_raw_neg888_log <- hourly_raw %>% gather(5:28, key = "variable", value = "value") %>% 
  filter(value == -888)

# add human interpretable variable names
x_header <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12",
              "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20", "X21", "X22",
              "X23", "X24", "X25", "X26", "X27", "X28")
variable_name <- c("sta", "year", "day", "time", "airt", "maxair",
                   "minair", "rh", "ms", "vms", "dir", "sddir",
                   "maxw", "minw", "cm1", "cm10", "vp", "ppt", 
                   "sm10", "sm30", "sol", "maxsol", "minsol",
                   "evap", "th2o", "bar", "bat", "pan")

x_to_name <- tibble(x_header, variable_name)

hourly_raw_neg999_log <- hourly_raw_neg999_log %>% left_join(x_to_name, by = c("variable" = "x_header"))
hourly_raw_neg888_log <- hourly_raw_neg888_log %>% left_join(x_to_name, by = c("variable" = "x_header"))



##### Saving files -----------------------------------------------------------------------------------------------------

# write logs to file
getwd()

# write logs of records with variables containing -999 and -888
write_csv(hourly_raw_neg999_log, paste("./data_output_files/hourly_raw_neg999_log_", 
                                       year_to_process_last2digits, ".csv", sep = ""))
write_csv(hourly_raw_neg888_log, paste("./data_output_files/hourly_raw_neg888_log_", 
                                       year_to_process_last2digits, ".csv", sep = ""))



# write out hourly file without header
hourly_raw %>% select(-X27, -X28) %>% write_csv(., paste("./data_output_files/hourly_raw_no_header_", 
                            year_to_process_last2digits, ".csv", sep = ""), col_names = FALSE)

  


# write out hourly and minute rain files with readable header names to use in other programs
names(hourly_raw) <- c("sta", "year", "day", "time", "airt", "maxair",
                       "minair", "rh", "ms", "vms", "dir", "sddir",
                       "maxw", "minw", "cm1", "cm10", "vp", "ppt", 
                       "sm10", "sm30", "sol", "maxsol", "minsol",
                       "evap", "th2o", "bar", "bat", "pan")
names(rain_raw) <- c("sta", "year", "day", "time", "ppt")


write_csv(hourly_raw, paste("./data_output_files/hourly_raw_", year_to_process_last2digits, ".csv", sep = ""))
write_csv(rain_raw, paste("./data_output_files/rain_raw_", year_to_process_last2digits, ".csv", sep = ""))

# These files need more processing before they are ready to use - see program 
# 02_filtering_hourly_met_data.R for further filtering


