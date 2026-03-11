################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 1. TK01_canaper_setup
#####' 
#####' This script prepares the input data for the canaper analyses: Basedata 
#####' (site x species matrix) and the input tree. The script takes a raster 
#####' stack with layers for all species, a shapefile for the region, and a tree 
#####' that includes species from the raster stack. The resulting files will 
#####' have the same species with any name corrections for proper input into 
#####' canaper analyses. Set arguments and run separately for each region.
#####'
#####' Written by Taliesin (Tal) Kinser in 2023
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### ***Set the arguments here***
### File handle (path) for storing the SDM raster stack output
fh_rstack <- "input/Aus/aus_stack.tif"
### File handle (path) of the shape file
fh_shp <- "input/Aus/Australia_mol.shp"
### File handle (path) of the phylogenetic tree. This script will output a new 
## tree that matches the spatial data (character)
fh_tree <- "input/ultrametric_mean_nex.tre"
### Remove family names from tree tip labels. Set as T if needed (boolean)
clean_tree <- T
### Lump infraspecific taxa to the species rank (merge presence data; boolean) 
lump_infra <- T
### If you would like to aggregate the rasters before creating the spatial data, 
## provide an aggregation factor. Otherwise, set as NA (integer or NA)
agg <- NA # or 2 (Cal and Med need an agg factor of 2 for beta analyses)
### Set an output directory for out files
dir_out = "input/Aus"
### Set an output handle (including version number if desired) for output files.
out_name <- "Aus_ver1"

##### Set working directory (if needed) and load libraries and functions
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(ape)
library(phytools)
library(terra)

##### Retrieve the input files

### Upload raster stack
rstack <- rast(fh_rstack)
### Upload the region's shapefile
shp <- vect(fh_shp)
### Upload the tree file
tree <- read.tree(fh_tree)
# Remove family names to match raster names (if needed)
if (clean_tree == T){
  tree$tip.label <- gsub("^[A-Z][a-z]*aceae_", "", tree$tip.label)
}

##### Prepare the CANAPE input

#### Prepare the raster stack

if (lump_infra == T){
  ### Remove infraspecific names on rasters and store updated names
  names(rstack) <- gsub("(^[A-Z][a-z]+_.+)_[a-z]+$", "\\1", names(rstack))
}
## Subset raster stack to only species that are in tree
rstack <- terra::subset(rstack, which(names(rstack) %in% tree$tip.label))
### Aggregate raster stack (optional)
if (!is.na(agg)){
  print(paste0("Aggregating raster stack by factor of ", agg))
  rstack <- terra::aggregate(rstack, fact=agg, fun=mean) # currently using mean
  # Round to 0 or 1
  rstack <- round(rstack, 0) 
}

#### Create the Basedata for CANAPE

print("Creating Basedata. This may take seconds to minutes")
### Crop and mask raster stack to match shapefile
rstack <- crop(rstack, ext(shp))
rstack <- mask(rstack, shp)
### Convert masked raster stack to site X taxon dataframe for Basedata
BD <- terra::as.data.frame(rstack, xy = T, na.rm = NA)
BD[is.na(BD)] <- 0
### Make any necessary fixes
## Combine any duplicate species (e.g., infraspecific ranks with same species name)
# Separate out the x and y cols from the rest first
temp <- BD[,1:2]
if (any(duplicated(colnames(BD)))){
  temp2 <- as.data.frame(t(rowsum(t(BD[,-c(1:2)]), group = colnames(BD)[-c(1:2)])))
  temp2[temp2 > 1] <- 1
  BD <- cbind(temp, temp2)
}
BD <- BD[,-c(1:2)]
## Remove any species with no presences and site with no species
BD <- BD[rowSums(BD) > 0, colSums(BD) > 0]
## Return x and y coords
BD <- cbind(subset(temp, rownames(temp) %in% rownames(BD)), BD)
## Save
write.csv(BD, paste0(dir_out, "/BaseData_", out_name, ".csv"), row.names = T)
print(paste0("Basedata is created and stored as ", 
             paste0(dir_out, "/BaseData_", out_name, ".csv")))

### Prune tree to only species in Basedata
tree<-drop.tip(tree,tree$tip.label[!(tree$tip.label %in% colnames(BD)[-c(1:2)])])
## Save tree
write.tree(tree, paste0(paste0(dir_out, "/Phy_", out_name, ".tre")))
print(paste0("Phylogenetic tree is created and stored as ", 
             paste0(dir_out, "/Phy_", out_name, ".csv")))
