# my_portfolio
My repository for work done in EVR628

# Author
Harrison Woodson Bowles; hxw831@miami.edu; harrisonbowles

# Description
This repo will be populated with work done for EVR628. It reads in model and best
track data for Hurricanes Ian and Ida. For Ida, it creates visualizations of the
forecast track and the observed best track, broken into relevant categories and
families as a proof of concept.

The goal of this project is to plot both model and observed hurricane track and
intensity data, and create an error score from the two datasets. The goal is to
attempt to better understand model accuracy and what is associated with it.

## Data Sources
Best Track: https://ftp.nhc.noaa.gov/atcf/btk/

Model: https://ftp.nhc.noaa.gov/atcf/archive/2022/

## Project Structure
data/ : This will house all data used for the project

  raw/: untouched raw data
  
  processed/: data that has been tidied/cleaned
  
  
scripts/ : This will house all R scripts used for analysis

  01_processing/: This will house scripts to clean and tidy datasets for analysis
  
  03_content/: This will house scripts to visualize and analyze processed data
  
results/ : This will house all output figures and tables