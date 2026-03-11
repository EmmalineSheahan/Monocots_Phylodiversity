################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 3. TK03_add_propGeoPD
#####' 
#####' This script takes output data from 02_canaper_analyses and data on 
#####' geophytic habit to calculate the proportion of PD represented by 
#####' geophytes in each region. It also calculates species richness (SR) and 
#####' gets geophytic proportion of SR, and it updates the species list. Enter 
#####' names of input for all regions.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

#### Load libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(phytools)
library(phyloregion)

#### Set up geophyte data
geosc <- read.csv("input/wcvp_names_accepted_subset_geophytescored.csv")
## Separate out geophytes from non-geophytes
geo <- subset(geosc, geophyte == 1)
geo_names <- gsub(" ", "_", geo$taxon_name)
nogeo <- subset(geosc, geophyte == 0)
nogeo_names <- gsub(" ", "_", nogeo$taxon_name)

#### Calculate geophytic PD and proportion for each region
names <- c("Aus_ver1", "Cal_ver1", "Chi_ver1", "Med_ver1", "SAf_ver1")
for (name in names){
  ### Read in the files
  indir <- paste0("input/", gsub("_.*", "", name), "/")
  # Biodiverse output
  sphy <- read.csv(paste0("output/canaper/Biodiverse_rand_results_", name, ".csv"), row.names = 1)
  # Phylogenetic tree and Base Data
  tree <- read.tree(paste0(indir, "Phy_", name, ".tre"))
  bd <- read.csv(paste0(indir, "BaseData_", name, ".csv"), row.names = 1)
  bd <- phyloregion::dense2sparse(bd[,-c(1:2)]) # make sparse
  ## Get SR (for all, including sp. with NA for geophytic trait info)
  sphy$SR <- Matrix::rowSums(bd)
  
  ### Prep the data
  ## Standardize edge length and trim tree
  # Should only include species checked for a geophytic habit
  tree <- drop.tip(tree, tree$tip.label[which(!(tree$tip.label %in% c(geo_names,nogeo_names)))])
  tree$edge.length <- tree$edge.length / sum(tree$edge.length)
  ## Now separate out geo and non-geo
  tree_geo <- drop.tip(tree, tree$tip.label[which(!(tree$tip.label %in% geo_names))])
  tree_nogeo <- drop.tip(tree, tree$tip.label[which(!(tree$tip.label %in% nogeo_names))])
  ## Trim base data and set up
  bd <- bd[rownames(sphy),tree$tip.label]
  bd_geo <- phyloregion::dense2sparse(bd[,which(colnames(bd) %in% geo_names)])
  bd_nogeo <- phyloregion::dense2sparse(bd[,which(colnames(bd) %in% nogeo_names)])
  
  ### Calculate PD/SR for geophytes and for nongeophytes
  ## Taxic
  SR <- Matrix::rowSums(bd)
  geoSR <- Matrix::rowSums(bd_geo)
  nogeoSR <- Matrix::rowSums(bd_nogeo)
  ## Phylogenetic
  PD <- phyloregion::PD(bd, tree)
  geoPD <- phyloregion::PD(bd_geo, tree_geo)
  nogeoPD <- phyloregion::PD(bd_nogeo, tree_nogeo)
  ### Add in geo PD and SR
  sphy$geoSR <- geoSR
  sphy$nogeoSR <- nogeoSR
  sphy$geoPD <- geoPD
  sphy$nogeoPD <- nogeoPD
  ## Calculate proportion SR/PD
  sphy$gpSR <- sphy$geoSR / SR
  sphy$nogpSR <- sphy$nogeoSR / SR
  sphy$gpPD <- sphy$geoPD / PD
  sphy$nogpPD <- sphy$nogeoPD / PD
  ### Save
  write.csv(sphy, paste0("output/canaper/Biodiverse_rand_results_", name, ".csv"), row.names = T)
  print(paste(name, "complete"))
}
