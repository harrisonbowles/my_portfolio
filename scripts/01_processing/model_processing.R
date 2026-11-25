################################################################################
# Model Processing
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 11/24/26
#
# This script automatically pulls, cleans, and exports model data given a year
# and a tropical cyclone ID 
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)

## Load data -------------------------------------------------------------------
atcf_cols <- c("basin", "storm_num", "yearmonthdayhour", "", "model", "f_hour",
               "model_lat", "model_lon", "model_vmax", "model_min_pressure",
               "storm_type", rep("", 35))              # 46 columns total

fetch_model <- function(year, id) {
  url <- str_to_lower(paste0("https://ftp.nhc.noaa.gov/atcf/archive/",
                             year, "/a", id, ".dat.gz"))
  tmp <- tempfile(fileext = ".dat.gz")
  on.exit(unlink(tmp))
  
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  
  model <- read.csv(gzfile(tmp),
            header = FALSE,
            strip.white = TRUE,
            fill = TRUE,
            col.names = atcf_cols) |> 
  as_tibble() |> 
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
  
  write_rds(model,
            paste0("data/processed/storm_model/",str_to_lower(id),".rds"))
}