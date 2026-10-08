################################################################################
# TC Model Visualization Project
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 10/5/2026
#
# In this script I will use the data produced by data_processing.R to generate
# some maps of my data, as well as some statistical analyses.
#
#
# Data provided by the NHC.
#
# Best Track: https://ftp.nhc.noaa.gov/atcf/btk/
# Model: https://ftp.nhc.noaa.gov/atcf/archive/2022/
#
################################################################################  

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(rnaturalearth)
library(ggplot2)
library(raster)
library(mapview)

## Load data -------------------------------------------------------------------
#Read in model and best track data
model_bt <- read_rds("data/processed/model_bt.rds")


# PROCESSING ###################################################################
#Set a run time
t = as.POSIXct("2022-09-25 12:00", tz = "UTC") 

#Create an object for observed track with geometry
obs <- model_bt |> 
  st_drop_geometry() |> 
  distinct(valid_time, obs_lon, obs_lat) |> 
  st_as_sf(coords = c("obs_lon", "obs_lat"),
           crs = 4326, remove = FALSE)

#Create an object for model runs with no geometry  
model_runs <- model_bt |> 
  st_drop_geometry() |> 
  filter(datetime == t) |> 
  distinct(model, datetime, forecast_hour, model_lon, model_lat,
           model_vmax, model_min_pressure)

#lat lon error calculation
#xy_err <- abs(pointDistance(c(model_bt$obs_lon, model_bt$obs_lat), 
#                            c(model_bt$model_lon, model_bt$model_lat),
#                        lonlat = TRUE))
  

# VISUALIZE ####################################################################
#Let's create our "Basemap"
USA_states <- ne_states(geounit = "United States of America")
cuba <- ne_countries(scale = 110L, country = "Cuba")
mexico <- ne_countries(scale = 110L, country = "Mexico")
coastline <- ne_coastline(scale = 110L)

#Make our plot
p <- ggplot(data = USA_states) +
  geom_sf(data = cuba) +
  geom_sf(data = mexico) +
  geom_sf(data = coastline) +
  geom_sf(fill = "gray80") +
  
  geom_path(data = model_runs,      #paths for model runs
          aes(x= model_lon,
              y = model_lat,
              color = model)) +
  geom_sf(data = obs,               #points for observed track
          aes(fill = "black",
              size = 7)) +
  
  coord_sf(xlim = c(-90, -74), ylim = c(15, 33)) + #Limit lat/lon boundaries 
  
  labs(                                            #Labels
    x = "Longitude",
    y = "Latitude",
    title = t
  ) 
  
mapview(USA_States)
mapview(obs)

  #theme(legend.position = "none")                  #Remove legend (its HUGE)

#p


#Create plots of error

e1 <- ggplot(data = model_bt,                       #vmax
             aes(x = valid_time)) +
    geom_smooth(aes(y=vmax_err, color = model), se = FALSE) +
    geom_smooth(aes, y = model_bt$obs_vmax, 
                     fill = "black", 
                     linetype = "dashed",
                     size = 10
                     ) +
    #geom_vline(xintercept = ())
    theme(legend.position = "none") +
    labs(
      x = "Date",
      y = "Error in Vmax (kts)"
  )

e1

e1 <- ggplot(data = model_bt, 
             aes(x = valid_time)) +
  geom_smooth(aes(y=vmax_err, color = model, se = FALSE, alpha = 0.1)) +
  theme(legend.position = "none") +
  labs(
    x = "Date",
    y = "Error in Vmax (kts)"
  )



e3 <- ggplot(data = model_bt, 
             aes(x = valid_time)) +
  geom_smooth(aes(y=press_err, color = model, alpha = 0.1), , se = FALSE) +
  theme(legend.position = "none") +
  labs(
    x = "Date",
    y = "Error in Vmax (kts)"
  )

#e3



## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################


# EXPORT #######################################################################


print("All Done!")