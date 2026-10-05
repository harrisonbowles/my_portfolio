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

## Load data -------------------------------------------------------------------
#Read in Ian Best Track data, and convert to a tibble
best_track <- read.csv("data/processed/best_track.rds") |> 
  as_tibble() 

#Read in Ian model data, and convert to a tibble
hafs_a <- read.csv("data/processed/hafs_a.rds") |>      #HAFS-A
  as_tibble() 
hafs_a_1 <- read.csv("data/processed/hafs_a_1.rds")

ships <- read.csv("data/processed/ships.rds") |>        #SHIPS
  as_tibble()
ships_1 <- read.csv("data/processed/ships_1.rds") |> 
  as_tibble()


# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------

# ANALYSIS #####################################################################

## Almost last step ------------------------------------------------------------


# EXPORT #######################################################################

## The final step --------------------------------------------------------------