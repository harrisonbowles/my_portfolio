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
  rename("obs_vmax" = "Max.Wind..kts.") |> 
  rename("obs_min_pres" = "Min.Cen.Pressure") |> 
  mutate(obs_min_pres = as.numeric(obs_min_pres)) |> 
  mutate(valid_time = paste0(Year.Month.Day, str_sub(time, 1, 4))) |> 
  mutate(valid_time = parse_date_time(as.character(valid_time),
                                      orders = "YmdHM",
                                      tz = "UTC")) |> 
  
  #Convert Lat/Lons to non-char, and make a geometry object out of them
  mutate(
    obs_lat = parse_number(Lat) * if_else(str_detect(Lat, "S"), -1, 1), 
    obs_lon = parse_number(Lon) * if_else(str_detect(Lon, "W"), -1, 1)
  )



model_data <- model_data |>                                     #MODEL DATA
  
  #Doing lots of renaming. The data did not come with headers
  rename("model" = "CARQ") |>
  rename("year_month_day_hour" = "X2022092012") |> 
  rename("forecast_hour" = "X.24") |>
  mutate(forecast_hour = as.numeric(forecast_hour)) |> 
  rename("model_lat" = "X99N") |> 
  rename("model_lon" = "X466W") |>
  rename("model_vmax" = "X20") |> 
  rename("model_min_pressure" = "X0") |> 
  mutate(model_min_pressure = as.numeric(model_min_pressure)) |> 
  rename("storm_type" = "DB") |> 
  
  #Collapse first two columns together
  unite(storm_id, matches("AL|09"), sep = "", remove = TRUE) |> 
  
  #Remove unused rows
  filter_out(str_detect(storm_id, "0|NEQ45|NEQ75")) |> 
  
  #Convert lat/lons to non-char, and make a geometry object out of them
  mutate(
        model_lat = parse_number(model_lat) / 10 * if_else(str_detect(model_lat, "S"), -1, 1), 
        model_lon = parse_number(model_lon) / 10 * if_else(str_detect(model_lon, "W"), -1, 1)     
         ) |> 
  
  #Remove unnecessary variables
  select(-(starts_with("X0.") |
           starts_with("X34") |
           starts_with("AAA"))) |>  
  
  #Create our datetime object out of existing col "year_month_day_hour"
  mutate(datetime = parse_date_time(as.character(year_month_day_hour),
                                    orders = "YmdH",
                                    tz = "UTC")) |> 
  mutate(valid_time = datetime + hours(as.numeric(forecast_hour)))

#join the model data to the best track data by date
model_bt <- left_join(best_track,
                      model_data,
                      by = join_by(valid_time),
                      relationship = "one-to-many") |> 
  select(-(starts_with("X.x") |
           starts_with("X.y") |
           starts_with("X.2") |
           starts_with("X.1")
           )) |> 
  st_as_sf(coords = c("obs_lon", "obs_lat"), 
           crs = 4326,
           remove = FALSE) |> 
  mutate(lat_error = abs(obs_lat - model_lat)) |> 
  mutate(lon_error = abs(obs_lon - model_lon)) |> 
  mutate(xy_err = sqrt((lat_error)^2 + (lon_error)^2 )) |> 
  mutate(vmax_err = abs(model_vmax - obs_vmax)) |> 
  mutate(pres_err = abs(model_min_pressure - obs_min_pres))


# EXPORT #######################################################################

#Export the data to be visualized
write_rds(model_bt,
          file = "data/processed/model_bt.rds")

