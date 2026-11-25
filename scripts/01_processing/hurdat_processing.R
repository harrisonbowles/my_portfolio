################################################################################
# HURDAT2 Processing
################################################################################
#
# Harrison Woodson Bowles
# hxw831@miami.edu
# 11/24/26
#
# This script parses through the NOAA HURDAT2 database to isolate and export
# observed best track and relevant variables for single tropical cyclones
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)

storm_id <- "al092021" #Hard-coded for now; meant to be queried by the user

## Load data -------------------------------------------------------------------
url <- "https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2025-092326.txt"

raw <- read_lines(url)

# PROCESSING ###################################################################
# Tag lines meant to be headers
lines <- tibble(text = raw) |> 
  mutate(is_header = str_detect(text, "^[A-Z]{2}[0-9]{6},")) |> 
  # Copy header info down storm's entire data line
  mutate(
    storm_id = if_else(is_header, str_sub(text, 1, 8), NA),
    name = if_else(is_header, str_trim(str_split_i(text, ",", 2)), NA),
    n_expected = as.integer(if_else(is_header, str_split_i(text, ",", 3), NA))
  ) |> 
  fill(storm_id, name, n_expected) |> 
  filter(!is_header)

# Separate data at commas
track <- lines |> 
  separate_wider_delim(
    text, delim = ",",
    names = c("date", "time", "record", "status",
              "lat", "lon", "vmax_kt", "mslp_mb"),
    too_many = "drop"
  ) |> 
  mutate(across(date:mslp_mb, str_trim)) |> 
  # Correct the data types, set missing to NA
  mutate(
    valid_time = ymd_hm(paste(date, time), tz = "UTC"),
    season = as.integer(str_sub(storm_id, 5, 8)),
    lat = parse_number(lat) * if_else(str_ends(lat, "S"), -1, 1),
    lon = parse_number(lon) * if_else(str_ends(lon, "W"), -1, 1),
    vmax_kt = as.integer(vmax_kt),
    mslp_mb = as.integer(mslp_mb),
    vmax_kt = if_else(vmax_kt < 0, NA, vmax_kt),
    mslp_mb = if_else(mslp_mb < 0, NA , mslp_mb)
  ) |> 
  select(storm_id, name, season, valid_time, time, record, status, lat, lon,
         vmax_kt, mslp_mb, n_expected)

# Make a function to isolate a storm by id, and save as rds
save_storm <- function(id) {
  this_storm <- track |> 
    filter(storm_id == str_to_upper(id))
 write_rds(this_storm,
           paste0("data/processed/storm_obs/",str_to_lower(id),".rds"))
}

save_storm(storm_id)
