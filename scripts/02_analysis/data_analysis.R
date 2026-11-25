################################################################################
# TITLE HERE
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# DATE HERE
#
# This script reads in model and observed tropical cyclone datasets, joins them,
# and performs error calculations for visualization and analysis
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(janitor)
library(tidyverse)
library(EVR628tools)
library(sf)

storm_id <- "al092021" #Hard-coded for now; meant to be queried by the user

## Load data -------------------------------------------------------------------
# Function to read in data given storm ID
read_obs <- function(id) {
  obs <- read_rds(paste0("data/processed/storm_obs/",str_to_lower(id),".rds"))
  return(obs)
}

obs <- read_obs(storm_id)

read_mod <- function(id) {
  model <- read_rds(paste0("data/processed/storm_model/",id,".rds"))
  return(model)
}

model <- read_mod(storm_id)

ref <- read.csv(file = "data/raw/model_ref.csv",
                strip.white = T) |> 
  as_tibble()

# PROCESSING ###################################################################
distance_nmi <- function(lat1, lon1, lat2, lon2) {
  r <- 3440.065                                  # Earth radius, nautical miles
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
    cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * r * asin(sqrt(a))
}

# Join model data with ref sheet
model_join <- left_join(model,
                        ref,
                        by = join_by(model),
                        relationship = "many-to-one")

# Join model_join with observed data so we can perform error calculations
model_bt <- left_join(obs, model_join, by = join_by(valid_time),
                      relationship = "one-to-many") |> 
  filter(!is.na(model)) |> 
  mutate(track_err_nmi = distance_nmi(lat, lon, model_lat, model_lon),
         vmax_bias = model_vmax - vmax_kt,
         vmax_err  = abs(vmax_bias),
         pres_bias = model_min_pressure - mslp_mb,
         pres_err  = abs(pres_bias))

# Calculate track, vmax, and pressure error by forecast hour
# The goal is to assess how short range predictability correlates to mid-long
# range predictability
by_fhour <- model_bt |> 
  group_by(family, category, model, f_hour, datetime, valid_time, member) |> 
  summarize(n_cases   = n(),
            track_err = mean(track_err_nmi, na.rm = TRUE),
            vmax_mae  = mean(vmax_err, na.rm = TRUE),
            vmax_mbe  = mean(vmax_bias, na.rm = TRUE),
            pres_mae  = mean(pres_err, na.rm = TRUE),
            pres_mbe  = mean(pres_bias, na.rm = TRUE),
            mean_err_score = ((vmax_mae + pres_mae + track_err)/3),
            .groups = "drop")

# EXPORT #######################################################################
write_rds(model_bt,
          "data/output/model_bt.rds")

write_rds(by_fhour,
          "data/output/by_fhour.rds")

write_rds(obs,
          "data/output/obs.rds")