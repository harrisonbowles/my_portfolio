################################################################################
# TC Model Visualization Project
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 9/29/26
#
# This script will read in and clean best track and model data for
# Hurricane Ian, 2022. Data provided by the NHC.
# 
# Best Track: https://ftp.nhc.noaa.gov/atcf/btk/
# Model: https://ftp.nhc.noaa.gov/atcf/archive/2022/
#
################################################################################  

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(sf)

## Load data -------------------------------------------------------------------
best_track <- read.csv("data/raw/Observed/Ian_2022_HURDAT.csv",
                       strip.white = T) |> 
  as_tibble() #Read in Ian Best Track data, and convert to a tibble

model_data <- read.csv("data/raw/Model/ian_model.dat/ian_model.dat",
                       strip.white = T) |> 
  as_tibble() #Read in Ian model data, and convert to a tibble

# PROCESSING ###################################################################
best_track <- best_track |>                                     #BEST TRACK
  
  #Remove columns that indicate wind speed at a certain radii
  select(-(starts_with("X."))) |>       
  
  #Remove first row
  filter(Year.Month.Day != "AL092022") |>    
  
  #Create our datetime object
  rename("time" = "Model.Init.Time") |>         #Rename time
  mutate(valid_time = paste0(Year.Month.Day, str_sub(time, 1, 2))) |> 
  mutate(valid_time = parse_date_time(as.character(valid_time),
                                      orders = "YmdH",
                                      tz = "UTC")) |> 
  
  #Convert Lat/Lons to non-char
  mutate(
    Lat = parse_number(Lat) * if_else(str_detect(Lat, "S"), -1, 1), 
    Lon = parse_number(Lon) * if_else(str_detect(Lon, "W"), -1, 1)
  ) |> 
  st_as_sf(coords = c("Lon", "Lat"))



model_data <- model_data |>                                     #MODEL DATA
  
  #Doing lots of renaming. The data did not come with headers
  rename("model" = "CARQ") |>
  rename("year_month_day_hour" = "X2022092012") |> 
  rename("forecast_hour" = "X.24") |>
  rename("lat" = "X99N") |> 
  rename("long" = "X466W") |>
  rename("vmax" = "X20") |> 
  rename("min_pressure" = "X0") |> 
  rename("storm_type" = "DB") |> 
  
  #Collapse first two columns together
  unite(storm_id, matches("AL|09"), sep = "", remove = TRUE) |> 
  
  #Remove unused rows
  filter_out(str_detect(storm_id, "0|NEQ45|NEQ75")) |> 
  
  #Convert lat/lons to non-char
  mutate(
        lat = parse_number(lat) / 10 * if_else(str_detect(lat, "S"), -1, 1), 
        long = parse_number(long) / 10 * if_else(str_detect(long, "W"), -1, 1)     
         ) |> 
  
  #Remove unnecessary variables
  select(-(starts_with("X0.") |
           starts_with("X34") |
           starts_with("AAA"))) |>  
  
  #Create our datetime object out of existing col "year_month_day_hour"
  mutate(datetime = parse_date_time(as.character(year_month_day_hour),
                                    orders = "YmdH",
                                    tz = "UTC")) |> 
  mutate(valid_time = datetime + hours(forecast_hour))

#join the model data to the best track data by date
#model_bt <- left_join(best_track,
#                      model_data,
#                      by = )



#hafs_a <- model_data |> 
#  filter(model == " HFA2") |>  #Save all HAFS-A model runs
#  mutate(
#    lat = parse_number(lat) / 10 * if_else(str_detect(lat, "S"), -1, 1), 
#    long = parse_number(long) / 10 * if_else(str_detect(long, "W"), -1, 1)     #Convert lat/lons to non-char
# )
#day_one_hafs <- hafs_a |> 
#  filter(year_month_day_hour == 2022092112) #To save only one HAFS-A run for Ian
#
#ships <- model_data |> 
#  filter(model == " SHIP") |> #Save all SHIPS model runs
#  mutate(
#    lat = parse_number(lat) / 10 * if_else(str_detect(lat, "S"), -1, 1), 
#    long = parse_number(long) / 10 * if_else(str_detect(long, "W"), -1, 1)     #Convert lat/lons to non-char
#  )
#day_one_ships <- ships |> 
#  filter(year_month_day_hour == 2022092112) #To save only one SHIPS run for Ian


# EXPORT #######################################################################


write_rds(best_track,                                      #Export the data to be visualized
          file = "data/processed/best_track.rds")          #Best Track

#write_sf(best_track,                                  
#          dsn = "data/processed/best_track.shp")
#
#write_sf(model_data,
#         dsn = "data/processed/model_data.shp")

#write_rds(hafs_a,
#          file = "data/processed/hafs_a.rds")         #HAFS-A
#write_rds(day_one_hafs,
#          file = "data/processed/hafs_a_1.rds")
#
#write_rds(ships,
#          file = "data/processed/ships.rds")          #SHIPS
#write_rds(day_one_ships,
#          file = "data/processed/ships_1.rds")