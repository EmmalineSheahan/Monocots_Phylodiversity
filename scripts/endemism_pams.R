# pams for endemism

library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(doParallel)
library(ape)
library(reshape2)

moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)
base_raster <- raster(ext = extent(land_poly), crs = moll_crs, 
                      resolution = 10000)

# function to create raster base using largest range
base_builder <- function(wanted_region) {
  want_species <- list.files(paste0('./regions/', wanted_region))
  file_vec <- vector(length = length(want_species))
  for (i in 1:length(want_species)) {
    t <- file.info(paste0('./rasters/', want_species[i]))$size
    file_vec[i] <- t
  }

  largest_range <- which(file_vec == max(file_vec))
  want_base <- raster(paste0('./rasters/', want_species[largest_range]))
  test_points <- rasterToPoints(want_base)
  test_points <- data.frame(test_points)
  colnames(test_points) <- c("x", "y", "z")
  test_points_sp <- test_points
  coordinates(test_points_sp) <- ~x+y
  proj4string(test_points_sp) <- "+proj=longlat +ellps=WGS84 +no_defs"
  test_points_moll <- spTransform(test_points_sp, CRSobj = moll_crs)
  cropped_base <- crop(base_raster, test_points_moll)
  new_raster <- rasterize(test_points_moll, cropped_base, 
                          field = test_points$z)
  return(new_raster)
}

saf_base <- base_builder("South_Africa")
cal_base <- base_builder("California")
aus_base <- base_builder("Australia")
chi_base <- base_builder("Chile")
med_base <- base_builder("Med")

# Function for row removal of false NA's
row_remove <- function(wanted_base) {
  land_raster <- rasterize(land_poly, base_raster, field = 1)
  land_raster <- crop(land_raster, wanted_base)
  totalrow_remove <- which(is.na(values(land_raster)))
  return(totalrow_remove)
}

saf_remove <- row_remove(saf_base)
cal_remove <- row_remove(cal_base)
aus_remove <- row_remove(aus_base)
chi_remove <- row_remove(chi_base)
med_remove <- row_remove(med_base)

# end_pam function to create preliminary pams by region for endemism
end_pam <- function(wanted_region, wanted_base, wanted_rowremove, filename) {
  want_species <- list.files(paste0('./regions/', wanted_region))
  mat_want <- matrix(NA, ncol = length(want_species), 
              nrow = (ncell(wanted_base) + 1) - length(wanted_rowremove))
  for (i in 1:length(want_species)) {
    test_raster <- raster(paste0('./rasters/', want_species[i]))
    test_vals_sum <- sum(values(test_raster), na.rm = T)
    if (test_vals_sum == 0) {
      values(test_raster)[which(values(test_raster == 0))] <- 1
    }
    test_points <- rasterToPoints(test_raster)
    test_points <- data.frame(test_points)
    colnames(test_points) <- c("x", "y", "z")
    test_points_sp <- test_points
    coordinates(test_points_sp) <- ~x+y
    proj4string(test_points_sp) <- "+proj=longlat +ellps=WGS84 +no_defs"
    test_points_moll <- spTransform(test_points_sp, CRSobj = moll_crs)
    new_raster <- rasterize(test_points_moll, wanted_base, 
                          field = test_points$z)
    sname <- want_species[i]
    sname <- gsub('.tif', '', sname)
    mat_want[1,i] <- sname
    vals <- as.numeric(values(new_raster))
    mat_want[2:nrow(mat_want),i] <- vals[-wanted_rowremove]
  }
  mat_want <- data.frame(mat_want)
  names(mat_want) <- lapply(mat_want[1, ], as.character)
  mat_want <- mat_want[-1,]

  save(mat_want, file = paste0('./data/', filename, '.Rdata'))
  
  return(mat_want) 
}

no_cores <- 10  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

saf_pam <- end_pam("South_Africa", saf_base, saf_remove, "saf_pam")
cal_pam <- end_pam("California", cal_base, cal_remove, "cal_pam")
aus_pam <- end_pam("Australia", aus_base, aus_remove, "aus_pam")
chi_pam <- end_pam("Chile", chi_base, chi_remove, "chi_pam")
med_pam <- end_pam("Med", med_base, med_remove, "med_pam")

stopCluster(cl)

# these have to be run one at a time for memory purposes, wiping the environment
# between each
load('./data/med_pam.Rdata')
med_pam <- mat_want

load('./data/saf_pam.Rdata')
saf_pam <- mat_want

load('./data/chi_pam.Rdata')
chi_pam <- mat_want

load('./data/aus_pam.Rdata')
aus_pam <- mat_want

load('./data/cal_pam.Rdata')
cal_pam <- mat_want

# cordinates matrix function
coord_maker_end <- function(region_base, region_row_remove) {
  mat_coord <- matrix(NA, ncol = 2, 
                nrow = (ncell(region_base) + 1) - length(region_row_remove))
  rclmat <- matrix(c(NA, 0), ncol = 2, byrow = T)
  lay <- reclassify(region_base, rclmat)
  pts <- rasterToPoints(lay, spatial = T)
  dim(pts@coords)
  mat_coord[2:nrow(mat_coord),1] <- 
    as.numeric(pts@coords[,1][-region_row_remove])
  mat_coord[2:nrow(mat_coord),2] <- 
    as.numeric(pts@coords[,2][-region_row_remove])
  mat_coord[1,1] <- "lon"
  mat_coord[1,2] <- "lat"
  mat_coord <- data.frame(mat_coord)
  names(mat_coord) <- lapply(mat_coord[1, ], as.character)
  mat_coord <- mat_coord[-1,]
  return(mat_coord)
}

saf_end_coord <- coord_maker_end(saf_base, saf_remove)
save(saf_end_coord, file = './data/saf_end_coord.Rdata')

cal_end_coord <- coord_maker_end(cal_base, cal_remove)
save(cal_end_coord, file = './data/cal_end_coord.Rdata')

aus_end_coord <- coord_maker_end(aus_base, aus_remove)
save(aus_end_coord, file = './data/aus_end_coord.Rdata')

chi_end_coord <- coord_maker_end(chi_base, chi_remove)
save(chi_end_coord, file = './data/chi_end_coord.Rdata')

med_end_coord <- coord_maker_end(med_base, med_remove)
save(med_end_coord, file = './data/med_end_coord.Rdata')

# reading in trees
saf_tree <- read.nexus('./data/saf_tree.nex')
cal_tree <- read.nexus('./data/cal_tree.nex')
aus_tree <- read.nexus('./data/aus_tree.nex')
chi_tree <- read.nexus('./data/chi_tree.nex')
med_tree <- read.nexus('./data/med_tree.nex')

# matching pam to tree with pam_matcher function
pam_matcher_end <- function(wanted_pam, wanted_tree, 
                        wanted_coords) {
  wanted_pam1 <- wanted_pam %>% dplyr::select(wanted_tree$tip.label)
  
  for (i in 1:ncol(wanted_pam1)) {
    wanted_pam1[,i] <- as.numeric(wanted_pam1[,i])
  }
  
  for (i in 1:ncol(wanted_coords)) {
    wanted_coords[,i] <- as.numeric(wanted_coords[,i])
  }
  
  wanted_pam2 <- wanted_pam1 %>% replace(is.na(.), 0)
  wanted_pam_new <- cbind(wanted_coords, wanted_pam2)
  
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

saf_pam_final_end <- pam_matcher_end(saf_pam, saf_tree, saf_end_coord)
write.csv(saf_pam_final_end, file = './data/saf_pam_final_end.csv')

cal_pam_final_end <- pam_matcher_end(cal_pam, cal_tree, cal_end_coord)
write.csv(cal_pam_final_end, file = './data/cal_pam_final_end.csv')

aus_pam_final_end <- pam_matcher_end(aus_pam, aus_tree, aus_end_coord)
write.csv(aus_pam_final_end, file = './data/aus_pam_final_end.csv')

chi_pam_final_end <- pam_matcher_end(chi_pam, chi_tree, chi_end_coord)
write.csv(chi_pam_final_end, file = './data/chi_pam_final_end.csv')

med_pam_final_end <- pam_matcher_end(med_pam, med_tree, med_end_coord)
write.csv(med_pam_final_end, file = './data/med_pam_final_end.csv')
