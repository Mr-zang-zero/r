SEV Hourly Meteorological Data Processing  

This GitHub repository provides the programs that are used to process raw hourly meteorological data from the server, produce reports that are emailed to researchers and field crew, scripts for emailing the reports, and a bash script that automates the entire process.  

Programs for processing and filtering the raw hourly meteorological data for a given year:  
- 00_user_setup_met_processing.R  
- 01_raw_hourly_met_processing.R  
- 02_filtering_hourly_met_data.R  

R Markdown programs that produce reports that are emailed to researchers - they produce html output:  
- met49_50_daily_wind_speed_report.Rmd  
- met50_daily_precip_report.Rmd  

Python programs that email the html output from the Rmd reports to researchers:  
- send_mail_met49_50_daily_wind_speed_EXAMPLE.py  
- send_mail_met50_daily_precip_EXAMPLE.py  

There is a bash shell script, met_retrieval_EXAMPLE.sh, that downloads raw hourly data from the server and then runs all of the programs listed above.  

The _EXAMPLE scripts are examples of the actual scripts that are not posted on GitHub.  

Click to access [SEV Hourly Meteorological Data](https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sev&identifier=1) through the Environmental Data Initiative (EDI) Data Repository.  

