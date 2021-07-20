#!/bin/bash

# KM Hall
# This EXAMPLE script retrieves met data from the socorro server and processes it through the R met scripts.
# The actual script has the same name without _EXAMPLE

# retrieves data from server
cd /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/data_raw_hourly

echo "Local working directory: "
pwd

echo "Connecting to SFTP"
sftp -i ~/.ssh/id_rsa_sev username@servername.unm.edu << EOF
# this assumes you have a security key set up. Enter your information to connect to server

cd /net/ladron/export/db/work/wireless/met/socorro
get *.dat
bye
EOF

cd /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/

# Runs the R scripts
echo "Running R program 00"
Rscript /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/00_user_setup_met_processing.R
#Rscript 00_user_setup_met_processing.R

echo "Running R program 01"
Rscript /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/01_raw_hourly_met_processing.R 
#Rscript 01_raw_hourly_met_processing.R 

echo "Running R program 02"
Rscript /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/02_filtering_hourly_met_data.R 
#Rscript 02_filtering_hourly_met_data.R

# This chunk of code produces an RMarkdown report for met50_daily_precipitation.
echo "Running RMarkdown met50_daily_precipitation"
# R -e "rmarkdown::render('/Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/met50_daily_precip_report.Rmd', output_file='test.html')"
R -e "rmarkdown::render('/Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/met50_daily_precip_report.Rmd', output_file='met50_daily_precip_report.html')"

# This chunk of code produces an RMarkdown report for met49_50_daily_wind_speed.
echo "Running RMarkdown met49_50_daily_wind_speed"
R -e "rmarkdown::render('/Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/met49_50_daily_wind_speed_report.Rmd', output_file='met49_50_daily_wind_speed_report.html')"


cd /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs

# Program emails met50_daily_precip report
python /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/send_mail_met50_daily_precip.py

# Program emails met49_50_daily_precip report
python /Users/kris/Documents/SEV/Projects/Meteorological_Data_Processing/programs/send_mail_met49_50_daily_wind_speed.py