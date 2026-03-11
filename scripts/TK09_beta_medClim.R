################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 9. TK09_beta_medClim
#####' 
#####' This script uses the beta dissimilarity matrices from 08_beta_phyloregion
#####' and the master data sets from 05_getMasters to calculate the turnover
#####' and nestedness (phylogenetic or taxic) between each site in the core zone
#####' and every other core zone site (within) or every buffer zone site (among)
#####' for geophytes only and for all taxa. Set arguments and run separately for
#####' each region, for taxic and phylogenetic beta, and for turnover (Simpson)
#####' and nestedness (SNE) (20 total for 40 output files).
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

#### ***Set the arguments here***
#setwd("enter/working_dir/here/") # all code assumes a common working directory
### Set whether to use turnover/Simpson ("sim") or nestedness/SNE ("sne")
type = "sim"
### Get the file name
name <- "Aus_ver1" # Cal and Med should be agg2 (see 08_beta_phyloregion)
### Set phylo to TRUE if using phylobeta. Set to FALSE for taxic beta
phylo = T

#### Get data
### Master data sets
mast <- read.csv(paste0("Master_datasets/Master_", name, ".csv"),row.names = 1)
mast <- subset(mast, !is.na(med_clim))
## Create a subset that only include sites with geophytes
mast2 <- subset(mast, gpPD > 0)
### Dissimilarity matrices (for all monocots and geophytes only)
indir <- paste0("output/Beta/", gsub("_.*", "", name), "/")
if (phylo == T) {fh = "phylobeta"} else {fh = "taxicbeta"}
pb_all <- readRDS(paste0(indir, fh, "_all_", name, ".Rdata"))
pb_geo <- readRDS(paste0(indir, fh, "_geo_", name, ".Rdata"))

### Get core/buffer zone identity 
## All monocots
buff <- rownames(subset(mast, med_clim==0))
core <- rownames(subset(mast, med_clim==1))
## Geophytes
buff2 <- rownames(subset(mast2, med_clim==0))
core2 <- rownames(subset(mast2, med_clim==1))

### Get the Simpson or SNE matrix for all and for geophytes
pb_all <- round(pb_all[[grep(type, names(pb_all))]],5)
pb_all <- as.matrix(pb_all)[core,rownames(mast)]
pb_geo <- round(pb_geo[[grep(type, names(pb_geo))]],5)
pb_geo <- as.matrix(pb_geo)[core2,rownames(mast2)]

#### Get within and among turnover/nestedness for every cell
### All monocots
dx <- data.frame(grids=rownames(pb_all), all_w=NA, all_a=NA)
for (i in 1:nrow(dx)){
  dx[i,"all_w"] = mean(pb_all[i,core[-i]])
  dx[i,"all_a"] = mean(pb_all[i,buff])
}
### Geophytes
dx2 <- data.frame(grids=rownames(pb_geo), geo_w=NA, geo_a=NA)
for (i in 1:nrow(dx2)){
  dx2[i,"geo_w"] = mean(pb_geo[i,core2[-i]])
  dx2[i,"geo_a"] = mean(pb_geo[i,buff2])
}

#### Save
## Set output directory and create if necessary
outdir <- "output/med_clim/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = T)
write.csv(dx, paste0(outdir, fh, "Core_", type, "_all_", name, ".csv"), row.names = F)
write.csv(dx2, paste0(outdir, fh, "Core_", type, "_geo_", name, ".csv"), row.names = F)
