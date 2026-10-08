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
library(sf)
library(cowplot)

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
              color = model,
              show.legend = FALSE)) +
  geom_sf(data = obs,               #points for observed track
          aes(fill = "black",
              size = 7)) +
  
  coord_sf(xlim = c(-90, -74), ylim = c(15, 33)) + #Limit lat/lon boundaries 
  
  labs(                                            #Labels
    x = "Longitude",
    y = "Latitude",
    title = t,
    caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/"
  ) +

  theme(legend.position = "none")                  #Remove legend (its HUGE)

p

  
  

###Create plots of error

#Create plots of obs to show storm strength
obs_wind <- ggplot(data = model_bt,          #Wind
              aes(x = valid_time)) +
  geom_line(aes(y = obs_vmax,
                color = "blue"),
            show.legend = FALSE) +
  theme_bw() +
  labs(x = NULL,
       y = "Max Wind Speed\n(kts)") +
  theme(axis.title.y = element_text(size = 9))

obs_pres <- ggplot(data = model_bt,         #Pressure
                   aes(x = valid_time)) +
  geom_line(aes(y = obs_min_pres,
                color = "red"),
            show.legend = FALSE) +
  theme_bw() +
  labs(x = "Date",
       y = "Minimum Pressure\n(hPa)",
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  theme(axis.title.y = element_text(size = 9))


e1 <- ggplot(data = model_bt,                       #vmax
             aes(x = valid_time)) +
    geom_smooth(aes(y=vmax_err, color = model,
                    ), show.legend = FALSE,
                se = FALSE, alpha = 0.5) +
    theme_bw() +
    #geom_vline(xintercept = )+
    theme(legend.position = "none") +
    labs(
      x = NULL,
      y = "Error in Vmax\n(kts)",
      title = "Error in Max Wind (kts), with Observed Min Pressure (hPa)\nand Observed Max Wind (kts)",
      caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/"
  ) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold"))

e2 <- ggplot(data = model_bt,                       #Min Pressure
             aes(x = valid_time)) +
  geom_smooth(aes(y = pres_err, color = model),
               se = FALSE) +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = "Error in Pressure (hPa)",
    title = "Error in Min Pressure (hPa), with Observed Min Pressure (hPa)\nand Observed Max Wind (kts)",
    caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/"
  ) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold"))


e3 <- ggplot(data = model_bt,                       #latlon
             aes(x = valid_time)) +
  geom_smooth(aes(y=xy_err, color = model, alpha = 0.1), , se = FALSE) +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = "Error in Position",
    title = "Error in Position, with Observed Min Pressure (hPa)\nand Observed Max Wind (kts)"
  ) +
  theme(plot.title = element_text(size = 14,
                                  face = "bold"))



#Cow plots
e1_cow = plot_grid(e1,
                   obs_wind,                    #Wind error
                   obs_pres,
                   e1,
                   ncol = 1,
                   rel_heights = c(1.75, 1, 1))
e1_cow

e2_cow = plot_grid(e2,
                   obs_wind,                    #Pres Error
                   obs_pres,
                   e2,
                   ncol = 1,
                   rel_heights = c(1.75, 1, 1))
e2_cow
                   
e3_cow = plot_grid(e3,
                   obs_wind,                    #latlon Error
                   obs_pres,
                   ncol = 1,
                   rel_heights = c(1.75, 1, 1))
e3_cow

# EXPORT #######################################################################
ggsave(p,
       filename = "results/img/plot.png") #Save spatial plot

ggsave(e1_cow,
       filename = "results/img/wind_cow.png") #Save wind error plot
ggsave(e2_cow,
       filename = "results/img/pres_cow.png") #Save pressure error plot
ggsave(e3_cow,
       filename = "results/img/pos_cow.png") #Save position error plot

ggsave(e1,
       filename = "results/img/wind_error.png") #Save original error plots
ggsave(e2,                                     #for funsies
       filename = "results/img/pres_error.png")
ggsave(e3,
       filename = "results/img/pos_error.png")

print("All Done!")