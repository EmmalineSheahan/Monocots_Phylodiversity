# creating PAMs and trees for biodiverse

source('./scripts/row_remove_for_pams.R')

library(ape)
library(dplyr)
library(doParallel)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(reshape2)

# region shapefiles
Australia <- st_read(dsn = './masterShpFiles/', layer = 'Australia_mol')
California <- st_read(dsn = './masterShpFiles/', layer = 'California_mol')
Chile <- st_read(dsn = './masterShpFiles/', layer = 'Chile_mol')
Med <- st_read(dsn = './masterShpFiles/', layer = 'Med_mol')
South_Africa <- st_read(dsn = './masterShpFiles/', layer = 'South_Africa_mol')

# tree_maker function to create trees pruned by region
tree_maker <- function(wanted_region) {
  whole_tree <- read.tree('./data/v2_undated_monocot_renamed.tre')
  clean_names <- gsub("'", '', whole_tree$tip.label)
  clean_names <- gsub(' ', '_', clean_names)
  whole_tree$tip.label <- clean_names
  
  reg_names <- list.files(paste0('./regions/', wanted_region))
  reg_names <- gsub('.tif', '', reg_names)
  
  wanted_nam <- which(reg_names %in% whole_tree$tip.label)
  wanted_nam <- reg_names[wanted_nam]
  drop <- which(!(whole_tree$tip.label %in% wanted_nam))
  
  wanted_tree <- drop.tip(whole_tree, drop)
  return(wanted_tree)
}

# Chile
chi_tree <- tree_maker("Chile")
plot.phylo(chi_tree, use.edge.length = F, cex = 0.3)

write.nexus(chi_tree, file = './data/chi_tree.nex')

# South Africa
saf_tree <- tree_maker("South_Africa")
plot.phylo(saf_tree, use.edge.length = F, cex = 0.3)

write.nexus(saf_tree, file = './data/saf_tree.nex')

# California
cal_tree <- tree_maker("California")
plot.phylo(cal_tree, use.edge.length = F, cex = 0.3)

write.nexus(cal_tree, file = './data/cal_tree.nex')

# Australia
aus_tree <- tree_maker("Australia")
plot.phylo(aus_tree, use.edge.length = F, cex = 0.3)

write.nexus(aus_tree, file = './data/aus_tree.nex')

# Med
med_tree <- tree_maker("Med")
plot.phylo(med_tree, use.edge.length = F, cex = 0.3)

write.nexus(med_tree, file = './data/med_tree.nex')

# initial pams
# load in stacks

aus_stack <- rast('./data/aus_stack.tif')
aus_stack <- crop(aus_stack, Australia)
aus_stack <- mask(aus_stack, Australia)

cal_stack <- rast('./data/cal_stack.tif')
cal_stack <- crop(cal_stack, California)
cal_stack <- mask(cal_stack, California)

chi_stack <- rast('./data/chi_stack.tif')
chi_stack <- crop(chi_stack, Chile)
chi_stack <- mask(chi_stack, Chile)

med_stack <- rast('./data/med_stack.tif')
med_stack <- crop(med_stack, Med)
med_stack <- mask(med_stack, Med)

saf_stack <- rast('./data/saf_stack.tif')
saf_stack <- crop(saf_stack, South_Africa)
saf_stack <- mask(saf_stack, South_Africa)

# presence absence matrix function
pam_maker <- function(wanted_stack, wanted_region) {
  mat <- matrix(NA, ncol = dim(wanted_stack)[3], 
                nrow = ncell(wanted_stack[[1]]) + 1)
  for (i in 1:dim(wanted_stack)[3]) {
    sname <- names(wanted_stack[[i]])
    mat[1,i] <- sname
    mat[2:nrow(mat),i] <- as.numeric(values(wanted_stack[[i]]))
  }
  mat <- data.frame(mat)
  names(mat) <- lapply(mat[1, ], as.character)
  mat <- mat[-1,]
  return(mat)
}

# cordinates matrix function
coord_maker <- function(wanted_stack) {
  mat <- matrix(NA, ncol = 2, 
                nrow = ncell(wanted_stack[[1]]) + 1)
  lay <- wanted_stack[[1]]
  rclmat <- matrix(c(NA, 0), ncol = 2, byrow = T)
  lay <- reclassify(lay, rclmat)
  pts <- rasterToPoints(lay, spatial = T)
  dim(pts@coords)
  mat[2:nrow(mat),1] <- as.numeric(pts@coords[,1])
  mat[2:nrow(mat),2] <- as.numeric(pts@coords[,2])
  mat[1,1] <- "lon"
  mat[1,2] <- "lat"
  mat <- data.frame(mat)
  names(mat) <- lapply(mat[1, ], as.character)
  mat <- mat[-1,]
  return(mat)
}

# pams per region
aus_pam <- pam_maker(aus_stack, "Australia")

cal_pam <- pam_maker(cal_stack, "California")

med_pam <- pam_maker(med_stack, "Med")

chi_pam <- pam_maker(chi_stack, "Chile")

saf_pam <- pam_maker(saf_stack, "South_Africa")

# coord matrices per region
aus_coords <- coord_maker(aus_stack)

cal_coords <- coord_maker(cal_stack)

med_coords <- coord_maker(med_stack)

chi_coords <- coord_maker(chi_stack)

saf_coords <- coord_maker(saf_stack)

# matching pam to tree with pam_matcher function
pam_matcher <- function(wanted_pam, wanted_tree, 
                        wanted_coords, wanted_row_remove) {
  wanted_pam1 <- wanted_pam %>% dplyr::select(wanted_tree$tip.label)
  
  for (i in 1:ncol(wanted_pam1)) {
    wanted_pam1[,i] <- as.numeric(wanted_pam1[,i])
  }
  
  for (i in 1:ncol(wanted_coords)) {
    wanted_coords[,i] <- as.numeric(wanted_coords[,i])
  }
  
  wanted_pam1clean <- wanted_pam1[-wanted_row_remove,]
  wanted_coordsclean <- wanted_coords[-wanted_row_remove,]
  
  wanted_pam2 <- wanted_pam1clean %>% replace(is.na(.), 0)
  wanted_pam_new <- cbind(wanted_coordsclean, wanted_pam2)
  
  spsumsrow <- rowSums(wanted_pam_new[,3:ncol(wanted_pam_new)])
  droprow <- which(spsumsrow == 0)
  if (length(droprow) > 0) {
    wanted_pam_final1 <- wanted_pam_new[-droprow,]
  } else {
    wanted_pam_final1 <- wanted_pam_new
  }
  
  spsumscol <- colSums(wanted_pam_final1)
  dropcol <- which(spsumscol == 0)
  if (length(dropcol) > 0) {
    wanted_pam_final <- wanted_pam_final1[,-dropcol]
  } else {
    wanted_pam_final <- wanted_pam_final1
  }
  wanted_pam_final_melt <- melt(wanted_pam_final, id = c("lon", "lat"))
  wanted_pam_final_melt <- wanted_pam_final_melt %>% filter(value == 1)
  return(wanted_pam_final_melt)
}

# Australia
aus_pam_final <- pam_matcher(aus_pam, aus_tree, aus_coords, aus_row_remove)
write.csv(aus_pam_final, file = './data/aus_pam_final.csv')

# California
cal_pam_final <- pam_matcher(cal_pam, cal_tree, cal_coords, cal_row_remove)
write.csv(cal_pam_final, file = './data/cal_pam_final.csv')

# Chile
chi_pam_final <- pam_matcher(chi_pam, chi_tree, chi_coords, chi_row_remove)
write.csv(chi_pam_final, file = './data/chi_pam_final.csv')

# Med
med_pam_final <- pam_matcher(med_pam, med_tree, med_coords, med_row_remove)
write.csv(med_pam_final, file = './data/med_pam_final.csv')

# South Africa
saf_pam_final <- pam_matcher(saf_pam, saf_tree, saf_coords, saf_row_remove)
write.csv(saf_pam_final, file = './data/saf_pam_final.csv')

