################################################################################
# TC Model Error Visualization
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 11/24/26
#
# This script will visualize the processed data from data_analysis.R
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

#Designate landfall time
landfall <- as.POSIXct("2021-08-29 16:55:00", tz = "UTC")

## Load data -------------------------------------------------------------------
model_bt <- read_rds("data/output/model_bt.rds")

obs_data <- read_rds("data/output/obs.rds")

by_fhour <- read_rds("data/output/by_fhour.rds")

# PROCESSING ###################################################################
#Get the current location of the storm at t
current_loc <- obs_data |> 
  filter(valid_time == t)

# Filter into groups by category, family, etc
ensembles_all <- model_bt |> 
  filter(category == "ensemble") |>
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
ensembles <- ensembles_all |> 
  filter(datetime == t)

ens_mean_all <- model_bt |> 
  filter(category == "ensemble") |> 
  filter(member == FALSE) |> 
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
ens_mean <- ens_mean_all |> 
  filter(datetime == t)

gfs_all <- model_bt |> 
  filter(family == "GFS",
         member == F) |> 
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
gfs <- gfs_all |> 
  filter(datetime == t)

statistical_all <- model_bt |> 
  filter(category == "statistical",
         member == F) |> 
  select(-c(lat, lon)) |>
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
statistical <- statistical_all |> 
  filter(datetime == t)

globals_all <- model_bt |> 
  filter(category == "global",
         member == F) |> 
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
globals <- globals_all |> 
  filter(datetime == t)

hurricanes_all <- model_bt |> 
  filter(category == "hurricane",
         member == F) |>
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
hurricanes <- hurricanes_all |> 
  filter(datetime == t)

coupled_all <- model_bt |> 
  filter(category == "coupled",
         member == F) |> 
  select(-c(lat, lon)) |> 
  st_drop_geometry() |> 
  st_as_sf(coords = c("model_lon", "model_lat"), 
           crs = 4326,
           remove = FALSE)
coupled <- coupled_all |> 
  filter(datetime == t)

ria_all <- model_bt |> 
  filter(category == "RI",
         member == F) |> 
  select(-c(lat, lon)) |> 
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

by_fhour_globals <- by_fhour |> 
  filter(category == "global")

by_fhour_hur <- by_fhour |> 
  filter(category == "hurricanes")

by_fhour_ens_mean <- by_fhour |> 
  filter(category == "ensemble",
         member == FALSE)

track_err_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_err = mean(track_err, na.rm = TRUE),
            n = n(),
            .groups = "drop") |> 
  filter(n >= 5) 

vmax_err_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_err = mean(vmax_mae, na.rm = TRUE),
            n = n(),
            .groups = "drop") |> 
  filter(n >= 5) 

vmax_bias_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_bias = mean(vmax_mbe, na.rm = TRUE), n = n(), .groups = "drop") |> 
  filter(n >= 5)

pres_err_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_err = mean(pres_mae, na.rm = TRUE),
            n = n(),
            .groups = "drop") |> 
  filter(n >= 5) 

pres_bias_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_bias = mean(pres_mbe, na.rm = TRUE),
            n = n(),
            .groups = "drop") |> 
  filter(n >= 5) 

tot_err_summary <- by_fhour_globals |> 
  group_by(family, f_hour) |> 
  summarize(mean_err = mean(mean_err_score, na.rm = TRUE),
            n = n(),
            .groups = "drop") |> 
  filter(n >= 5) 

# VISUALIZE ####################################################################
# Create functions for the different visualizations we will create

#Maps

map_xlim <- c(-96, -79)
map_ylim <- c(15, 36)

# Start with our theme
theme_storm <- function() {
  theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.background = element_rect(fill = "#d6e6f0", color = NA),   # ocean
      panel.grid       = element_line(color = "white", linewidth = 0.3),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.title       = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle    = element_text(size = 10, hjust = 0.5),
      plot.caption     = element_text(size = 7, hjust = 0),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.title     = element_text(face = "bold"),
      axis.title       = element_blank(),
      plot.margin      = margin(8, 8, 8, 8))
}

map_fun <- function(this_data) {
  this_plot <- ggplot(USA)+                #Create basemap
    geom_sf(fill = "gray50") +
    geom_sf(data = MEX,
            fill = "gray50") +
    geom_sf(data = CAN) +
    geom_sf(data = CARIB,
            fill = "gray50") +
    
    # Create paths for global models
    geom_path(data = this_data,
              aes(x = model_lon, y = model_lat,
                  group = interaction(model, datetime),
                  color = family),
              alpha = 0.8,
              linewidth = 0.8) +
    scale_color_viridis_d(name = "Model",
                          na.value = "grey70") +
    
    # Create an outline for the observed track
    geom_path(data = obs,
              aes(x = lon, y = lat),
              color = "black", linewidth = 3,
              lineend = "round") +
    new_scale_color() +
    
    # Create path for observed track on top of outline
    geom_path(data = obs, 
              aes(x = lon,
                  y = lat,
                  color = vmax_kt),
              linewidth = 2) +
    scale_color_gradientn(name = "Observed\nWind (kt)",
                          colors = hcl.colors(256, "Burg")) +
    
    # Create a point for the current storm location
    geom_point(data = current_loc,
               aes(x = lon, y = lat),
               shape = 7,
               color = "black",
               size = 5,
               stroke = 2) +
    
    # Limit lat/lon scope
    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) + 
    
    # Add our theme
    theme_storm() +
    
    # Labels
    labs(x = "Longitude", y = "Latitude",
         title = paste0("HURDAT Best Track and ",str_to_title(as.character(this_data$category[1]))," Models\nHURRICANE IDA"),
         subtitle = format(t, "%Y-%m-%d %H:%M UTC"),
         caption = "Data Sources: https://ftp.nhc.noaa.gov/atcf/btk/\nhttps://ftp.nhc.noaa.gov/atcf/archive/2022/") +
    # Add a north arrow from ggspatial
    annotation_north_arrow(location = "tl") +
    # Add a scalebar from ggspatial
    annotation_scale(location = "bl",
                     plot_unit = "mi")
  
  this_plot
  
  ggsave(plot = this_plot,
         paste0("results/img/maps/plot_",as.character(this_data$category),".png"),
         width = 7,
         height = 10)
}

# Run these to generate the plots

map_fun(globals)
map_fun(ensembles)
map_fun(hurricanes)
map_fun(coupled)
map_fun(ria)

err_fun <- function(y_var, ylab, label, y1, y2,
                    plot_data = by_fhour_globals, min_n = 5) {
  
  data_summary <- plot_data |> 
    group_by(family, f_hour) |> 
    summarize(mean_err = mean(.data[[y_var]], na.rm = TRUE),
              n = sum(!is.na(.data[[y_var]])),
              .groups = "drop") |> 
    filter(n >= min_n)
  
  this_plot <- ggplot(plot_data, aes(f_hour, .data[[y_var]], color = family)) +
    geom_line(aes(group = interaction(model, datetime)), alpha = 0.15) +
    geom_line(data = data_summary, aes(y = mean_err), linewidth = 1.2) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    coord_cartesian(xlim = c(0, 168), ylim = c(y1, y2), expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    facet_wrap(~ family) +
    labs(x = "Forecast Hour", y = ylab, color = "Model Family",
         title = paste0("Model ", label, " for Hurricane IDA by Forecast Hour")) +
    theme(panel.grid.major = element_line(color = "grey70", linewidth = 0.4),
          panel.grid.minor = element_line(color = "grey85", linewidth = 0.2,
                                          linetype = "dotted"))
  
  ggsave(filename = paste0("results/img/error/", str_to_snake(label), ".png"),
         plot = this_plot, width = 10, height = 10, dpi = 300, bg = "white")
  this_plot
}

specs <- tibble(
  y_var = c("track_err", "vmax_mae", "vmax_mbe", "pres_mae", "pres_mbe", "mean_err_score"),
  ylab  = c("Track Error (nmi)", "Max Wind Error (kt)", "Max Wind Bias (kt)",
            "Pressure Error (mb)", "Pressure Bias (mb)", "Total Error"),
  label = c("Track Error", "Max Wind Error", "Max Wind Bias",
            "Pressure Error", "Pressure Bias", "Average of Vmax, Pressure, and Track Error"),
  y1    = c(0, 0, -50, 0, -50, 0),
  y2    = c(1200, 100, 50, 50, 50, 400)
)

pwalk(specs, err_fun)


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
