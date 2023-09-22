# AQ_environmental_data
## A collection of environmental data from Hurricane Island's aquaculture lease site
**Repository Overview:** This repository is a platform for organizing, archiving, and analyzing environmental data collected on our 3.2 acre aquaculture lease site and also provides a forum for scientific collaboration, transparency, and methods standardization.

**Project Objectives:** This project aims to collect and share site-specfifc environmental data from our monitoring devices: Tilt-current meter (TCM), Water Probe, and HOBO loggers. These devices provide data on a variety of abiotic features of our site including: temperature, current speed/direction, salinity, turbidity, dissolved oxygen, and pH. 

**Tilt-Current Meter Data (TCM_enviro):** 

TCM_raw - all raw data from TCM device. Current variables include: ISO 8601 Time, speed, heading, velocity-north, velocity, east. Temperature variables ISO 8601 Time, temperature (degrees celsius) 

TCM_tidy - cleaned .csv for average/maximum/minimum daily current data and 30-minute-interval temperature data for corresponding data collection dates

TCM.Rmd - R markdown file of code used for TCM data cleaning and organization
TCM_vis.Rmd - R markdown file of code combining data sheets and simple visualization, to be used as more data is collected

**Manta 35+ Water Probe Data (Manta_enviro):**

old_plot - R file code with code used to make early figures, not annotated by might be useful in the future

20192022.csv - csv file with all HI, NH, and ST farm data collected using Eureka Manta 35+ water probe. Variables include: date, site, gear type, average GSI, average temperature, GSI SE, GSI SD, median GSI, type (wild/aquaculture),  chlorophyll (RFU), conductivity, dissolved oxygen, pressure, salinity, turbidity, pH.

Manta.Rmd - R markdown file of 20192022.csv data analysis. Code is pulled from old_plot. 
