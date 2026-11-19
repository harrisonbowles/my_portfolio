# my_portfolio
My repository for work done in EVR628

# Author
Harrison Woodson Bowles; hxw831@miami.edu; harrisonbowles

# Description
This repo will be populated with work done for EVR628. It performs two main operations.
The first is taking model and best track data for Hurricane Ian, 2022. It cleans 
and processes the data to be visualized. It plots the model and observed track
for a given timestep, and calculates the vmax, pressure, and track error, and creates
plots of these errors. This was a prototype script that I used as a framework to
generate the second operation

The second operation it completes is reading in model and best track data for 
Hurricane Ida, 2021. It cleans and processes it, categorizes it by model family,
category, etc, generates vmax, pressure, and track error, as well as a composite
error of all three. It then plots the observed and model tracks at a given
timestep for a given family/category. It also makes plots of the error organized
by forecast hour. The goal is to analyze a correlation between short-range and
mid-long range predictability in TC modelling, as well as to identify which models
handle a given storm the best.

## Data Sources
Best Track: https://www.nhc.noaa.gov/data/#hurdat

Model: https://ftp.nhc.noaa.gov/atcf/archive/2022/

## Project Structure
data/ : This will house all data used for the project

  raw/: untouched raw data
  
  processed/: data that has been tidied/cleaned
  
  
scripts/ : This will house all R scripts used for analysis

  01_processing/: This will house scripts to clean and tidy datasets for analysis
  
  03_content/: This will house scripts to visualize and analyze processed data
  
results/ : This will house all output figures and tables