# script that cleans your data
# Set up --------------------------
library(here)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(styler)
library(assertr)

#test where the top level of the project directory is 
here::here()


# Reads data into R, sets path, and finds sheet names
seabird_data_xls <- here::here("raw_data/seabirds.xls")
seabird_data_xls_sheets <- readxl::excel_sheets(path = seabird_data_xls)


#1 Imports sheet for .xls file
seabird_ship_data <- readxl::read_xls(path = seabird_data_xls, sheet = "Ship data by record ID")

seabird_bird_data <- readxl::read_xls(path = seabird_data_xls, sheet = "Bird data by record ID")

seabird_ship_data_codes <- readxl::read_xls(path = seabird_data_xls, sheet = "Ship data codes")

seabird_bird_data_codes <- readxl::read_xls(path = seabird_data_xls, sheet = "Bird data codes")

# Cleaning data set --------------------------

#2 Changes the names of the variables to follow naming standards.

# create new data-frame clean the headers
meteorite_data_clean <- clean_names(meteorite_data)

# ensure dataset follows naming standards by lowering case and removing punctuation
meteorite_data_clean <- meteorite_data_clean %>% 
  mutate(# Clean name column
    name = str_to_lower(name),
    name = str_replace_all(name, "\\(|\\)", ""),
    name = str_replace_all(name, "',-`", "_"),
    name = str_replace_all(name, " ", "_"),
    # clean fall column
    fall = str_to_lower(fall),
    # clean GeoLocation column
    geo_location = str_replace_all(
      geo_location, "\\(|\\)", "")   
  )



# Wrangling Data  --------------------------

#3 Splits column GeoLocation into latitude and longitude.
#         The new latitude & longitude columns are numeric.
# Separate Geo location by its sub-variables: latitude, longitude 
meteorite_data_clean <- meteorite_data_clean %>%
  separate(geo_location, c("latitude", "longitude"), sep = ",") %>% 
  mutate(latitude  = as.numeric(latitude),
         longitude = as.numeric(longitude))



#4 Replaces any missing values in latitude and longitude with zeros.
#
# This code is not used as it fails verification
#   i.e. contains data on longitude & latitude  that are outside of parameters
#   e.g. Latitude beyond -90 & 90 degrees, longitude beyond -180 & 180
#   
# meteorite_data_clean_null <- meteorite_data_clean %>%
#  mutate(longitude = str_replace_na(longitude, replacement = 0),
#         latitude  = str_replace_na(latitude, replacement  = 0))

# To ensure an accurate dataset, these rows should be removed and not given
# the Null Island geo-location. 
meteorite_data_clean_geo <- meteorite_data_clean %>% 
  drop_na(longitude,latitude) 

# Remove unknown years
meteorite_data_clean_geo <- meteorite_data_clean %>% 
  drop_na(year) 

#5 Removes meteorites less than 1000g in weight from the data.
meteorite_data_clean_geo <- meteorite_data_clean_geo %>% 
  filter(mass_g > 1000) %>% 
  arrange(mass_g)


# "...clean_null" not used as results inaccurate dataset 
# meteorite_data_clean_null <- meteorite_data_clean_null %>% 
filter(mass_g > 1000) %>% 
  arrange(mass_g)



#6 Orders the data by the year of discovery.
meteorite_data_clean_geo <- meteorite_data_clean_geo %>%
  arrange(year)

# "...clean_null" not used as results inaccurate dataset
# meteorite_data_clean_null <- meteorite_data_clean_null %>%
#  arrange(year) 


# Create output of the clean data 
write_csv(meteorite_data_clean_geo, "data/meteorite_landings_clean_geo.csv")

# "...clean_null" not used as results inaccurate dataset
#write_csv(meteorite_data_clean_null, "data/meteorite_landings_clean_null.csv")
