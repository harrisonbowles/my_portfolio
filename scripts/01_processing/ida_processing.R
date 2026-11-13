################################################################################
# TITLE HERE
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 11/11/26
#
# In this script I will process model and observed data for Hurricane Ida, 2021
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(janitor)
library(tidyverse)
library(EVR628tools)
library(sf)

## Load data -------------------------------------------------------------------
#Best track data from HURDAT2
bt <- read_rds("data/raw/Ida/ida_bt.rds") |> 
  as_tibble() |> 
  select(-(n_expected)) |> 
  rename(valid_time = time_utc) |> 
  rename(obs_lon = lon,
         obs_lat = lat)

model <- read.csv(file = "data/raw/Ida/ida_model.dat",
                       header = FALSE,
                       strip.white = T,
                       col.names = c("basin", "storm_num", "yearmonthdayhour",
                                "", "model", "f_hour", "model_lat", "model_lon",
                                "model_vmax", "model_min_pressure", "storm_type", "", "",
                                "", "", "", "", "", "", "", "", "", "", "", "",
                                "", "", "", "", "", "", "", "", "", "", "", "",
                                "", "", "", "", "", "", "", "", ""),
                       fill = T) |> 
  as_tibble()

ref <- read.csv(file = "data/raw/Ida/model_ref.csv") |> 
  as_tibble()
  
# PROCESSING ###################################################################
model <- model |> 
  #Make number chars into numeric
  mutate(f_hour = as.numeric(f_hour)) |> 
  mutate(model_min_pressure = as.numeric(model_min_pressure)) |> 
  
  #Save relevant cols
  select(basin, storm_num, yearmonthdayhour, model, f_hour,
         model_lat, model_lon, model_vmax, model_min_pressure, storm_type) |> 
  
  #Convert lat/lons to non-char
  mutate(
    model_lat = parse_number(model_lat) / 10 * if_else(str_detect(model_lat, "S"), -1, 1), 
    model_lon = parse_number(model_lon) / 10 * if_else(str_detect(model_lon, "W"), -1, 1)
  ) |> 
  
  #Create datetime and valid_time
  mutate(datetime = parse_date_time(as.character(yearmonthdayhour),
                                    orders = "YmdH",
                                    tz = "UTC")) |> 
  mutate(valid_time = datetime + hours(as.numeric(f_hour))) |> 
  mutate(init_time = str_sub(yearmonthdayhour, -2, -1),
         init_time = as.numeric(init_time)) |> 
  select(-yearmonthdayhour) |> 
  #Move to the front
  relocate(valid_time) |> 
  filter(model_min_pressure != 0) |> 
  filter(model_vmax != 0)

model_join <- left_join(model,
                        ref,
                        by = join_by(model),
                        relationship = "many-to-one")



model_bt <- left_join(bt,
                      model_join,
                      by = join_by(valid_time),
                      relationship = "one-to-many") |> 
  mutate(lat_error = abs(obs_lat - model_lat)) |>                  #Calculate error for relevant values
  mutate(lon_error = abs(obs_lon - model_lon)) |> 
  mutate(xy_err = sqrt((lat_error)^2 + (lon_error)^2 )) |> 
  mutate(vmax_err = abs(model_vmax - vmax_kt)) |> 
  mutate(pres_err = abs(model_min_pressure - mslp_mb))

by_fhour <- model_bt |> 
  group_by(f_hour, model, valid_time, init_time) |> 
  summarize(xy_err = mean(xy_err),
            vmax_err = mean(vmax_err),
            pres_err = mean(pres_err)) 
by_fhour <- left_join(by_fhour,
                      ref,
                      by = join_by(model),
                      relationship = "many-to-one")

# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------
