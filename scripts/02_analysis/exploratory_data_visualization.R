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

# VISUALIZE ####################################################################

p <- ggplot(data = USA) +
  geom_sf(fill = "gray90")

p



#coast <- rnaturalearth::ne_coastline(scale = 50)
#USA <- rnaturalearth::ne_states(geounit = "United States of America")

#mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "Esri.WorldImagery", "CartoDB.Positron"))
#ggplot(USA)
  #ggplot(best_track, xcol = "Lon", ycol = 'Lat', crs = 4326, legend = TRUE, color = 'red') +
  #ggplot(day_one_hafs, xcol = "long", ycol = 'lat', crs = 4326, legend = TRUE, color = "yellow") +
  #ggplot(day_one_ships, xcol = "long", ycol = 'lat', crs = 4326, legend = TRUE, color = "lightgreen")
  
  

## Some step -------------------------------------------------------------------





## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
# VISUALIZE ####################################################################
coast <- rnaturalearth::ne_coastline(scale = 50)
USA <- rnaturalearth::ne_states(geounit = "United States of America")

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "Esri.WorldImagery", "CartoDB.Positron"))

ggplot(USA) +
  ggplot(best_track, xcol = "Lon", ycol = 'Lat', crs = 4326, legend = TRUE, color = 'red') +
  ggplot(day_one_hafs, xcol = "long", ycol = 'lat', crs = 4326, legend = TRUE, color = "yellow") 
ggplot(day_one_ships, xcol = "long", ycol = 'lat', crs = 4326, legend = TRUE, color = "lightgreen")