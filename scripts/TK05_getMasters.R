################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 5. TK05_getMasters
#####' 
#####' This script creates a master list of canaper output and predictor 
#####' variables for each region. It takes derived diversity metrics from 
#####' 02_canaper_analyses and 03_add_propGeoPD and merges them with provided
#####' raster data of environmental predictor variables into a data frame for 
#####' downstream regression modeling. Enter names of input for all regions.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### Set up

#### Load libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(terra)
library(dplyr)

#### Get predictor variables from environmental rasters
### Set list of desired variables (layer names should be meaningful for each var)
vars_list <- list.files("vars_mol", full.names = T)
### Create a raster stack of above variables
vars <- rast(vars_list)

#### Get response variables from canaper output files
### Enter file appendage names for each region here
names <- c("Aus_ver1", "Cal_ver1", "Chi_ver1", "Med_ver1", "SAf_ver1")
### Set response variables here
response <- c("pd_obs", "pd_obs_z", "rpd_obs_z", "gpPD", "SR", "gpSR")
### Get files
can_files <- paste0("output/canaper/Biodiverse_rand_results_", names, ".csv")
### Create a directory for these datasets if there is not already one
if (!dir.exists("Master_datasets")){
  dir.create("Master_datasets")
}

##### Create a master dataset for each canaper output file

#### Function to create the master datasets
getMaster <- function(sphy, vars, response = c("pd_obs", "pd_obs_z", "rpd_obs_z"),
                      factor_vars = NULL){
  ### Select desired columns from sphy object and rasterize
  sphy2 <- sphy[, c("x", "y", response)]
  sphy2 <- rast(sphy2)
  ## Update CRS for sphy raster stack
  crs(sphy2) <- crs(vars)
  
  ### Reproject and mask predictor variables to match the sphy raster
  vars <- project(vars, sphy2)
  vars <- mask(vars, sphy2)
  ## Round factor variables
  if (!is.null(factor_vars)){
    vars[[which(names(vars) == factor_vars)]] <- round(vars[[which(names(vars) == factor_vars)]], 0)
  }
  
  ### Combine the two raster stacks and output a data frame
  master <- c(sphy2, vars)
  master <- as.data.frame(master, xy = T, na.rm = NA)
  rownames(master) <- rownames(sphy)
  master
}

#### Create master data frame for each region and output 
for (file in can_files){
  fh <- sub("output/canaper/Biodiverse_rand_results", "", file)
  sphy <- read.csv(file, row.names = 1)
  master <- getMaster(sphy, vars, factor_vars = c("med_clim"), 
                      response = response)
  write.csv(master, paste0("Master_datasets/", "Master", fh), row.names = T)
  print(paste0("Master_datasets/", "Master", fh, " is available"))
}

