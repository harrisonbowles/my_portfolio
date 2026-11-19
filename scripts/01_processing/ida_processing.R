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
         obs_lat = lat) |> 
  write_rds("data/processed/obs_data_ida.rds")

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

ref <- read.csv(file = "data/raw/Ida/model_ref.csv",
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
  filter(f_hour >= 0, model != "CARQ") |> 
  mutate(across(c(model_vmax, model_min_pressure), ~ na_if(.x, 0))) |> 
  distinct(model, datetime, f_hour, .keep_all = TRUE)

model_join <- left_join(model,
                        ref,
                        by = join_by(model),
                        relationship = "many-to-one")

model_bt <- left_join(bt, model_join, by = join_by(valid_time),
                      relationship = "one-to-many") |> 
  filter(!is.na(model)) |> 
  mutate(track_err_nmi = distance_nmi(obs_lat, obs_lon, model_lat, model_lon),
         vmax_bias = model_vmax - vmax_kt,
         vmax_err  = abs(vmax_bias),
         pres_bias = model_min_pressure - mslp_mb,
         pres_err  = abs(pres_bias))

by_fhour <- model_bt |> 
  group_by(family, category, model, f_hour, datetime, valid_time, member) |> 
  summarize(n_cases   = n(),
            track_err = mean(track_err_nmi, na.rm = TRUE),
            vmax_mae  = mean(vmax_err, na.rm = TRUE),
            vmax_mbe  = mean(vmax_bias, na.rm = TRUE),
            pres_mae  = mean(pres_err, na.rm = TRUE),
            mean_err_score = ((vmax_mae + pres_mae + track_err)/3),
            .groups = "drop")

# EXPORT #######################################################################
#Begin extracting by relevant characteristics
write_rds(model_bt,
          file = "data/processed/model_bt_ida.rds")

write_rds(by_fhour,
          file = "data/processed/by_fhour_ida.rds")