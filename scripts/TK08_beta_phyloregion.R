################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 8. TK08_beta_phyloregion
#####' 
#####' This script uses the phyloregion package to create beta (Sorensen) taxic 
#####' and phylo dissimilarity matrices from the site x tip (spp.) matrix and 
#####' corresponding phylogenetic tree produced in 01_canaper_setup for each 
#####' region. Dissimilarity matrices are also provided for the components of
#####' Sorensen beta: turnover (Simpson) and nestedness (SNE). Set arguments and
#####' run separately for each region, for taxic and phylogenetic beta, and for
#####' all monocots and geophytes only (20 total). Note that, due to their 
#####' sizes, Cal and Med should be aggregated by a factor of 2 for beta output.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2023
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### Set up

#### ***Set the arguments here***
### Input directory of the spatial data. (character)
in_dir <- "input/Aus" 
### Set a version number or other file appendage for in and output files
name <- "Aus_ver1"
### Set phylo to TRUE if using phylogenetic tree. Set to FALSE for taxic beta
phylo = T
### Set geo to TRUE if calculating for geophytes only; set FALSE for full data
geo = F

#### Load libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(ape)
library(dplyr)

#### Retrieve the input files

### Retrieve the tree file (if using)
if (phylo == T){
  tree <- read.tree(paste0(in_dir, "/Phy_", name, ".tre"))
  print(paste0("Phylogenetic tree file is ", paste0(in_dir, "/Phy_", name, ".tre")))
}

### Retrieve Basedata and set up for beta calculation
print(paste0("Uploading Basedata from ", paste0(in_dir, "/BaseData_", name, ".csv")))
BD <- read.csv(paste0(in_dir, "/BaseData_", name, ".csv"), row.names = 1)
BD <- BD[,-c(1:2)]
BD <- phyloregion::dense2sparse(BD)

### Subset to geophytes (if desired)
if (geo == T){
  ## Geophyte data
  geo_df <- read.csv("input/wcvp_names_accepted_subset_geophytescored.csv")
  geo_df <- subset(geo_df, geophyte == 1)
  geo_names <- gsub(" ", "_", geo_df$taxon_name)
  ## Subset input
  if (phylo == T){tree <- 
    drop.tip(tree, tree$tip.label[which(!(tree$tip.label %in% geo_names))])}
  BD <- BD[,which(colnames(BD) %in% geo_names)]
}

##### Produce dissimilarity matrices

#### Perform analysis
if (phylo == T){
  pb <- phyloregion::phylobeta(BD, tree)
  fh = "phylobeta_all_"
} else{
  pb <- phyloregion::beta_diss(BD)
  fh = "taxicbeta_all_"
}

#### Write output
### Set output directory
out_dir <- paste0("output/Beta/", sub("input/", "", in_dir), "/")
# Create output directory if necessary and finalize file handle
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = T)
if (geo == T) fh <- sub("all", "geo", fh)
saveRDS(pb, file = paste0(out_dir, fh, name, ".Rdata"))
