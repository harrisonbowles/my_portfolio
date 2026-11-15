################################################################################
# spatial_data_visualization
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 11/13/26
#
# This script will visualize spatial data from ida_processing.R
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(rnaturalearth)
library(ggplot2)
library(sf)
library(cowplot)
library(maps)
library(ggnewscale)
library(ggspatial)

#Select a run time
t = as.POSIXct("2021-08-29 00:00:00", tz = "UTC")

## Load data -------------------------------------------------------------------
model_bt <- read_rds("data/processed/model_bt_ida.rds")

obs_data <- read_rds("data/processed/obs_data_ida.rds")

by_fhour <- read_rds("data/processed/by_fhour_ida.rds")

# PROCESSING ###################################################################
#Get the current location of the storm at t
current_loc <- obs_data |> 
  filter(valid_time == t)

# Filter into groups by category, family, etc
ensembles_all <- model_bt |> 
  filter(category == "ensemble") |>
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
ensembles <- ensembles_all |> 
  filter(datetime == t)

ens_mean_all <- model_bt |> 
  filter(category == "ensemble") |> 
  filter(member == FALSE) |> 
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
ens_mean <- ens_mean_all |> 
  filter(datetime == t)

gfs_all <- model_bt |> 
  filter(family == "GFS",
         member == F) |> 
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
gfs <- gfs_all |> 
  filter(datetime == t)

statistical_all <- model_bt |> 
  filter(category == "statistical",
         member == F) |> 
  select(-c(obs_lat, obs_lon)) |>
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
statistical <- statistical_all |> 
  filter(datetime == t)

globals_all <- model_bt |> 
  filter(category == "global",
         member == F) |> 
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
globals <- globals_all |> 
  filter(datetime == t)

hurricanes_all <- model_bt |> 
  filter(category == "hurricane",
         member == F) |>
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
hurricanes <- hurricanes_all |> 
  filter(datetime == t)

coupled_all <- model_bt |> 
  filter(category == "coupled",
         member == F) |> 
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
coupled <- coupled_all |> 
  filter(datetime == t)

ria_all <- model_bt |> 
  filter(category == "RI",
         member == F) |> 
  select(-c(obs_lat, obs_lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
ria <- ria_all |> 
  filter(datetime == t)

USA <- rnaturalearth::ne_states(geounit = "United States of America")
MEX <- rnaturalearth::ne_countries(country = "Mexico")
CAN <- rnaturalearth::ne_countries(country = "Canada")
CARIB <- rnaturalearth::ne_countries(
  country = c("Cuba", "Jamaica", "Haiti", "Dominican Republic", "Puerto Rico",
              "Cayman Islands", "The Bahamas", "Turks and Caicos Islands", "Bermuda",
              "United States Virgin Islands", "British Virgin Islands",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis",
              "Montserrat", "Sint Maarten", "Saint Martin", "Saint Barthelemy",
              "Dominica", "Saint Lucia", "Saint Vincent and the Grenadines",
              "Barbados", "Grenada", "Trinidad and Tobago", "Aruba", "Curaçao"))

################################################################################
# VISUALIZE
################################################################################
p_globals <- ggplot(USA)+                #Create basemap
  geom_sf(fill = "gray50") +
  geom_sf(data = MEX,
          fill = "gray50") +
  geom_sf(data = CAN) +
  geom_sf(data = CARIB,
          fill = "gray50") +
  
  #Create paths for global models
  geom_path(data = globals,
            aes(x = model_lon, y = model_lat,
                group = interaction(model, datetime),
                color = family),
            alpha = 0.8,
            linewidth = 0.8) +
  scale_color_viridis_d(name = "Model",
                        na.value = "grey70") +
  
  #Create an outline for the observed track
  geom_path(data = obs_data,
            aes(x = obs_lon, y = obs_lat),
            color = "black", linewidth = 3,
            lineend = "round") +
  new_scale_color() +
  
  #Create path for observed track on top of outline
  geom_path(data = obs_data, 
            aes(x = obs_lon,
                y = obs_lat,
                color = vmax_kt),
            linewidth = 2) +
  scale_color_gradientn(name = "Observed\nWind (kt)",
                        colors = hcl.colors(256, "Burg")) +
  
  #Create a point for the current storm location
  geom_point(data = current_loc,
             aes(x = obs_lon, y = obs_lat),
             shape = 7,
             color = "black",
             size = 5,
             stroke = 2) +
  
  #Limit lat/lon scope
  lims(x = c(-95, -80),
       y = c(15, 35)) +
  
  #Labels and themeing
  labs(x = "Longitude", y = "Latitude",
       title = "HURDAT Best Track and Global Models\nHURRICANE IDA",
       subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  # Add a north arrow from ggspatial
  annotation_north_arrow(location = "tl") +
  # Add a scalebar from ggspatial
  annotation_scale(location = "bl",
                   plot_unit = "mi") +
  theme_grey(base_family = "serif", 
             base_size = 12) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray75"),
        legend.title = element_text(face = "bold")
  )

p_globals
ggsave(p_globals,
       filename = "results/img/p_globals.png",
       width = 9.6,
       height = 7.6)


p_hurricanes <- ggplot(USA)+              
  geom_sf(fill = "gray50") +
  geom_sf(data = MEX,
          fill = "gray50") +
  geom_sf(data = CAN) +
  geom_sf(data = CARIB,
          fill = "gray50") +
  geom_path(data = hurricanes,
            aes(x = model_lon, y = model_lat,
                group = interaction(model, datetime),
                color = family),
            alpha = 0.8,
            linewidth = 0.8) +
  scale_color_viridis_d(name = "Model",
                        na.value = "grey70") +
  geom_path(data = obs_data,
            aes(x = obs_lon, y = obs_lat),
            color = "black", linewidth = 3,
            lineend = "round") +
  new_scale_color() +
  geom_path(data = obs_data, 
            aes(x = obs_lon,
                y = obs_lat,
                color = vmax_kt),
            linewidth = 2) +
  scale_color_gradientn(name = "Observed\nWind (kt)",
                        colors = hcl.colors(256, "Burg")) +
  geom_point(data = current_loc,
             aes(x = obs_lon, y = obs_lat),
             shape = 7,
             color = "black",
             size = 5,
             stroke = 2) +
  lims(x = c(-95, -80),
       y = c(15, 35)) +
  labs(x = "Longitude", y = "Latitude",
       title = "HURDAT Best Track and Hurricane Models\nHURRICANE IDA",
       subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  # Add a north arrow from ggspatial
  annotation_north_arrow(location = "tl") +
  # Add a scalebar from ggspatial
  annotation_scale(location = "bl",
                   plot_unit = "mi") +
  theme_grey(base_family = "serif", 
             base_size = 12) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray75"),
        legend.title = element_text(face = "bold")
  )

p_hurricanes
ggsave(p_hurricanes,
       filename = "results/img/p_hurricanes.png",
       width = 10.2,
       height = 7)


p_coupled <- ggplot(USA)+
  geom_sf(fill = "gray50") +
  geom_sf(data = MEX,
          fill = "gray50") +
  geom_sf(data = CAN) +
  geom_sf(data = CARIB,
          fill = "gray50") +
  geom_path(data = coupled,
            aes(x = model_lon, y = model_lat,
                group = interaction(model, datetime),
                color = desc),
            alpha = 0.8,
            linewidth = 0.8) +
  scale_color_viridis_d(name = "Model",
                        na.value = "grey70") +
  geom_path(data = obs_data,
            aes(x = obs_lon, y = obs_lat),
            color = "black", linewidth = 3,
            lineend = "round") +
  new_scale_color() +
  geom_path(data = obs_data, 
            aes(x = obs_lon,
                y = obs_lat,
                color = vmax_kt),
            linewidth = 2) +
  scale_color_gradientn(name = "Observed\nWind (kt)",
                        colors = hcl.colors(256, "Burg")) +
  geom_point(data = current_loc,
             aes(x = obs_lon, y = obs_lat),
             shape = 7,
             color = "black",
             size = 5,
             stroke = 2) +
  lims(x = c(-95, -80),
       y = c(15, 35)) +
  labs(x = "Longitude", y = "Latitude",
       title = "HURDAT Best Track and Coupled Models\nHURRICANE IDA",
       subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  # Add a north arrow from ggspatial
  annotation_north_arrow(location = "tl") +
  # Add a scalebar from ggspatial
  annotation_scale(location = "bl",
                   plot_unit = "mi") +
  theme_grey(base_family = "serif", 
             base_size = 12) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray75"),
        legend.title = element_text(face = "bold")
  )

p_coupled
ggsave(p_coupled,
       filename = "results/img/p_coupled.png",
       width = 9.7,
       height = 7)

p_ens_mean <- ggplot(USA)+            
  geom_sf(fill = "gray50") +
  geom_sf(data = MEX,
          fill = "gray50") +
  geom_sf(data = CAN) +
  geom_sf(data = CARIB,
          fill = "gray50") +
  geom_path(data = ens_mean,
            aes(x = model_lon, y = model_lat,
                group = interaction(model, datetime),
                color = family),
            alpha = 0.8,
            linewidth = 0.8) +
  scale_color_viridis_d(name = "Model",
                        na.value = "grey70") +
  geom_path(data = obs_data,
            aes(x = obs_lon, y = obs_lat),
            color = "black", linewidth = 3,
            lineend = "round") +
  new_scale_color() +
  geom_path(data = obs_data, 
            aes(x = obs_lon,
                y = obs_lat,
                color = vmax_kt),
            linewidth = 2) +
  scale_color_gradientn(name = "Observed\nWind (kt)",
                        colors = hcl.colors(256, "Burg")) +
  geom_point(data = current_loc,
             aes(x = obs_lon, y = obs_lat),
             shape = 7,
             color = "black",
             size = 5,
             stroke = 2) +
  lims(x = c(-95, -80),
       y = c(15, 35)) +
  labs(x = "Longitude", y = "Latitude",
       title = "HURDAT Best Track and Model Ensemble Mean\nHURRICANE IDA",
       subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  # Add a north arrow from ggspatial
  annotation_north_arrow(location = "tl") +
  # Add a scalebar from ggspatial
  annotation_scale(location = "bl",
                   plot_unit = "mi") +
  theme_grey(base_family = "serif", 
             base_size = 12) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray75"),
        legend.title = element_text(face = "bold")
  )

p_ens_mean
ggsave(p_ens_mean,
       filename = "results/img/p_ens_mean.png",
       width = 9.7,
       height = 7)


p_ens <- ggplot(USA)+            
  geom_sf(fill = "gray50") +
  geom_sf(data = MEX,
          fill = "gray50") +
  geom_sf(data = CAN) +
  geom_sf(data = CARIB,
          fill = "gray50") +
  geom_path(data = ensembles,
            aes(x = model_lon, y = model_lat,
                group = interaction(model, datetime),
                color = family),
            alpha = 0.25,
            linewidth = 0.8) +
  scale_color_viridis_d(name = "model",
                        na.value = "grey70") +
  geom_path(data = obs_data,
            aes(x = obs_lon, y = obs_lat),
            color = "black", linewidth = 3,
            lineend = "round") +
  new_scale_color() +
  geom_path(data = obs_data, 
            aes(x = obs_lon,
                y = obs_lat,
                color = vmax_kt),
            linewidth = 2) +
  scale_color_gradientn(name = "Observed\nWind (kt)",
                        colors = hcl.colors(256, "Burg")) +
  geom_point(data = current_loc,
             aes(x = obs_lon, y = obs_lat),
             shape = 7,
             color = "black",
             size = 5,
             stroke = 2) +
  lims(x = c(-95, -80),
       y = c(15, 40)) +
  labs(x = "Longitude", y = "Latitude",
       title = "HURDAT Best Track and Model Ensembles\nHURRICANE IDA",
       subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
       caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
  # Add a north arrow from ggspatial
  annotation_north_arrow(location = "tl") +
  # Add a scalebar from ggspatial
  annotation_scale(location = "bl",
                   plot_unit = "mi") +
  theme_grey(base_family = "serif", 
             base_size = 12) +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray75"),
        legend.title = element_text(face = "bold")
  )

p_ens
ggsave(p_ens,
       filename = "results/img/p_ens.png",
       width = 7.6,
       height = 7)

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
