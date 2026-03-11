################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 10. TK10_beta_perman
#####' 
#####' This script uses the beta dissimilarity matrices from 08_beta_phyloregion
#####' and the master data sets from 05_getMasters to perform a PERMANOVA on the
#####' turnover and nestedness across the core / buffer zone delimitation for 
#####' all monocots and for geophytes only. Set arguments and run separately for
#####' each region and for taxic and phylogenetic beta (10 total).
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

#### Set up

### Set arguments and get libraries
## Get the file name
name <- "Aus_ver1" # Cal and Med should be agg2 (see 08_beta_phyloregion)
## Set phylo to TRUE if using phylobeta. Set to FALSE for taxic beta
phylo = T
## Set cores for running in parallel (make sure enough resources are available / requested)
numcores = 10
## Libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(vegan)
library(parallel)
library(tictoc)

### Get data
## Master data sets
mast <- read.csv(paste0("Master_datasets/Master_", name, ".csv"),row.names = 1)
mast <- subset(mast, !is.na(med_clim))
## Dissimilarity matrices (for all monocots and geophytes only)
indir <- paste0("output/Beta/", gsub("_.*", "", name), "/")
if (phylo == T) {fh = "phylobeta"} else {fh = "taxicbeta"}
pb_all <- readRDS(paste0(indir, fh, "_all_", name, ".Rdata"))
pb_geo <- readRDS(paste0(indir, fh, "_geo_", name, ".Rdata"))

#### PERMANOVAs

### Prepare data for permanova
## Make med_clim a factor and remove NAs
mast$med_clim <- factor(mast$med_clim, levels = c(0, 1), labels = c("Buffer","Core"))
# Create subset that only include sites with geophytes
mast2 <- subset(mast, gpPD > 0)
## Get the simpson matrix, reduce decimals, and use only sites in mast
pb_all_sim <- as.matrix(round(pb_all[[1]],5))
pb_all_sim <- as.dist(pb_all_sim[rownames(mast),rownames(mast)])
## Same for sne matrix
pb_all_sne <- as.matrix(round(pb_all[[2]],5))
pb_all_sne <- as.dist(pb_all_sne[rownames(mast),rownames(mast)])
## Same for geophyte matrices
pb_geo_sim <- as.matrix(round(pb_geo[[1]],5))
pb_geo_sim <- as.dist(pb_geo_sim[rownames(mast2),rownames(mast2)])
pb_geo_sne <- as.matrix(round(pb_geo[[2]],5))
pb_geo_sne <- as.dist(pb_geo_sne[rownames(mast2),rownames(mast2)])

### Perform permanova
## All species, simpson
print("Performing permanova on all species Simpson")
tic()
test_all_sim <- adonis2(pb_all_sim ~ med_clim, mast, permutations = 99, parallel = numcores)
toc()
print("complete")
## All species, sne
print("Performing permanova on all species SNE")
tic()
test_all_sne <- adonis2(pb_all_sne ~ med_clim, mast, permutations = 99, parallel = numcores)
toc()
print("complete")
## Geophytes, simpson
print("Performing permanova on geophytes Simpson")
tic()
test_geo_sim <- adonis2(pb_geo_sim ~ med_clim, mast2, permutations = 99, parallel = numcores)
toc()
print("complete")
## Geophytes, sne
print("Performing permanova on geophytes SNE")
tic()
test_geo_sne <- adonis2(pb_geo_sne ~ med_clim, mast2, permutations = 99, parallel = numcores)
toc()
print("complete")

### Save
test_ls <- list(test_all_sim, test_all_sne, test_geo_sim, test_geo_sne)
names(test_ls) <- c("all_sim", "all_sne", "geo_sim", "geo_sne")
saveRDS(test_ls, file=paste0("output/med_clim/", fh, "_permanova_results_", name, ".csv"))
