# loading libraries
library(tidyverse) # metapackage with lots of helpful functions
library(lubridate) # for working with dates
library(plotly) # for interactive plots
library(janitor)
library(leaflet)
library(colorRamps)
library(proj4)
library(validate)

crime <- read_csv("../crimedata.csv") %>%
  clean_names()
loc <- crime[,"Location"]
loc_sep <- separate(crime, "location", into = c("wr_lon","wr_lat"), sep = ", ", remove = FALSE,  convert = FALSE, extra = "warn", fill = "warn")
lon <- extract_numeric(clean2$wr_lon)
lat <- extract_numeric(clean2$wr_lat)
clean <- crime[,c("dr_number", "date_reported", "area_name", "crime_code_description","victim_age", "victim_sex")]
clean <- cbind(clean,lon,lat)
write.csv(clean,file = "crime_clean.csv", row.names = FALSE)