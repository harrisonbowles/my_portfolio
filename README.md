# my_portfolio
My repository for work done in EVR628

## Author
Harrison Woodson Bowles; hxw831@miami.edu; harrisonbowles

# Description
This repo will be populated with work done for EVR628. It currently reads in
best track and model data, provided by the NHC. It then cleans and tidies that
data, including creating a valid time for each data point,
and exports it to be visualized and analyzed. It currently can create a basic
plot of model tracks and best track data given a date and time, as well as time
series plots of error in max wind.

The goal of this project is to plot both model and observed hurricane track and
intensity data, and create an error score from the two datasets. The goal is to
attempt to better understand model accuracy and what is associated with it.

Best Track: https://ftp.nhc.noaa.gov/atcf/btk/
Model: https://ftp.nhc.noaa.gov/atcf/archive/2022/

# Project Structure
data/ : This will house all data used for the project
  raw/: untouched raw data
  processed/: data that has been tidied/cleaned
scripts/ : This will house all R scripts used for analysis
  01_processing/: This will house scripts to clean and tidy datasets for analysis
  02_analysis/: This will house scripts to visuazlie and analyze processed data
results/ : This will house all output figures and tables
