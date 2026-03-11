################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 2. TK02_canaper_analyses
#####' 
#####' This script takes input data prepared in 01_canaper_setup and executes 
#####' the canaper funtions, cpr_rand_test and cpr_classify_endem. These 
#####' functions are based off Biodiverse by Shawn Laffan. Set arguments and run 
#####' separately for each region.
#####' 
#####' Adapted and expanded from Joel Nitta vignettes for canaper 
#####' https://docs.ropensci.org/canaper/articles/canape.html
#####' Written by Taliesin (Tal) Kinser in 2023
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

############################# Part 1: Set up ###################################

##### ***Set the arguments here***
#### Input directory of the spatial data. (character)
in_dir <- "input/Aus" 
#### Set the number of replicates for the randomization function (integer)
reps <- 499
#### Set the number of processors (CPUs)
cpu = 5
### Set an output handle (including version number if desired) for output files.
## This should match the handle of the input files.
name <- "Aus_ver1"

##### Set working directory (if needed) and load libraries and functions
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(tictoc)
library(canaper)
library(tidyverse)
library(ape)
library(phytools)
library(future)

##### Retrieve the input files

### Retrieve the tree file
tree <- read.tree(paste0(in_dir, "/Phy_", name, ".tre"))
print(paste0("Phylogenetic tree file is ", paste0(in_dir, "/Phy_", name, ".tre")))

### Retrieve Basedata
print(paste0("Uploading Basedata from ", paste0(in_dir, "/BaseData_", name, ".csv")))
BD <- read.csv(paste0(in_dir, "/BaseData_", name, ".csv"), row.names = 1)
## Get x and y coordinates with the site #s (rownames) and remove from Basedata
sites <- BD[,1:2]
BD <- BD[,-c(1:2)]

############################# Part 2: Execute ##################################

##### Set the paramaters to run canaper randomizations
## Allow future to use larger objects
options(future.globals.maxSize= 1048576000) # May need to increase or set to +Inf
## Set a parallel back-end, with the chosen number of CPUs running simultaneously
plan(multisession, workers = cpu)
## Set a random number generator seed so we get the same results if this is run again
set.seed(071421)

##### Run the randomization test
### Run
print(paste0("Running canaper randomizations with ", reps, " replicates. This will take a while"))
tic()
rand_out <- cpr_rand_test(comm = BD, phy = tree, null_model = "curveball", 
                          n_reps = reps, n_iterations = 500000, tbl_out = T)
toc()
  # currently set to run all metrics
## Switch back to sequential (non-parallel) mode
plan(sequential)
## Return x and y coordinates
rand_out <- rand_out %>% remove_rownames %>% column_to_rownames(var="site")
rand_out <- cbind(sites, rand_out)
print("Randomizations are completed.")

##### Classify significance for sesPD and sesRPD
print("Classifying randomization results. This may take a few minutes")
tic()
rand_out <-
  cpr_classify_signif(rand_out, "pd") |>
  cpr_classify_signif("rpd") 
toc()
## Save output
# Create output directory if necessary
if (!dir.exists("output/canaper/")) dir.create("output/canaper/", recursive = T)
write.csv(rand_out, paste0("output/canaper/Biodiverse_rand_results_", name, ".csv"), 
          row.names = T)
print(paste0("Biodiverse is completed. Output stored as ", 
             paste0("output/canaper/Biodiverse_rand_results_", name, ".csv")))
