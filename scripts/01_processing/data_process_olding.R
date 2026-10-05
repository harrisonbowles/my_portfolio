################################################################################
# TC Model Visualization Project
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 9/29/26
#
# This project will read in, clean, and visualize best track and model data for
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
library(rnaturalearth)
library(mapview)

## Load data -------------------------------------------------------------------
best_track <- read.csv("data/raw/Observed/Ian_2022_HURDAT.csv") |> 
  as_tibble() #Read in Ian Best Track data, and convert to a tibble

model_data <- read.csv("data/raw/Model/ian_model.dat/ian_model.dat") |> 
  as_tibble() #Read in Ian model data, and convert to a tibble

# PROCESSING ###################################################################
best_track <- best_track |> 
  select(-(starts_with("X."))) |>            #Remove columns that indicate wind speed at a certain radii
  filter(Year.Month.Day != "AL092022") |>    #Remove first row
  mutate(
    Lat = parse_number(Lat) * if_else(str_detect(Lat, "S"), -1, 1), #Convert Lat/Lons to non-char
    Lon = parse_number(Lon) * if_else(str_detect(Lon, "W"), -1, 1)
  ) |> 
  st_as_sf(coords = c("Lon", "Lat"))

#Clean model dataset
model_data <- model_data |>                            #Doing lots of renaming. The data did not come with headers
  rename("model" = "CARQ") |>
  rename("year_month_day_hour" = "X2022092012") |> 
  rename("forecast_hour" = "X.24") |>
  rename("lat" = "X99N") |> 
  rename("long" = "X466W") |>
  rename("vmax" = "X20") |> 
  rename("min_pressure" = "X0") |> 
  rename("storm_type" = "DB") |> 
  unite(storm_id, matches("AL|09"), sep = "", remove = TRUE) |> #Collapse first two columns together
  select(-(starts_with("X0.") |
           starts_with("X34") |
           starts_with("AAA"))) |> #Remove unnecessary columns
  mutate(yr = str_sub(year_month_day_hour, start = 1, end = 4)) |>    #Convert date time numeric into
  mutate(mth = str_sub(year_month_day_hour, start = 5, end = 6)) |>   #individual columns, isolating 
  mutate(day = str_sub(year_month_day_hour, start = 7, end = 8)) |>   #initialization time
  mutate(init_time = str_sub(year_month_day_hour, start = 9, end = 10)) 

#Save all HAFS-A model runs
hafs_a <- model_data |> 
  filter(model == " HFA2") |>  
  mutate(
    lat = parse_number(lat) / 10 * if_else(str_detect(lat, "S"), -1, 1), 
    long = parse_number(long) / 10 * if_else(str_detect(long, "W"), -1, 1)     #Convert lat/lons to non-char
        )

#To display only one HAFS-A run for Ian, to de-clutter
day_one_hafs <- hafs_a |> 
  filter(year_month_day_hour == 2022092112) 
  
#Save all SHIPS model runs
ships <- model_data |> 
  filter(model == " SHIP") |> 
  mutate(
    lat = parse_number(lat) / 10 * if_else(str_detect(lat, "S"), -1, 1), 
    long = parse_number(long) / 10 * if_else(str_detect(long, "W"), -1, 1)     #Convert lat/lons to non-char
  )

#To display only one SHIPS run for Ian, to de-clutter
day_one_ships <- ships |> 
  filter(year_month_day_hour == 2022092112) 
  
## Some step -------------------------------------------------------------------


# VISUALIZE ####################################################################
#coast <- rnaturalearth::ne_coastline(scale = 50)
USA <- rnaturalearth::ne_countries(scale = 20, geounit = "United States of America")

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "Esri.WorldImagery", "CartoDB.Positron"))

mapview(USA) +
mapview(best_track, xcol = "Lon", ycol = 'Lat',
        legend = TRUE, color = 'red') +
mapview(day_one_hafs, xcol = "long", ycol = 'lat',
        legend = TRUE, color = "yellow") +
mapview(day_one_ships, xcol = "long", ycol = 'lat',
        legend = TRUE, color = "lightgreen")
## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------