# separating species into their own region specific directories and
# transforming their CRS to Mollweide

library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(doParallel)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]

# creating list of species
species_list <- list.files('./rasters')
species_list <- gsub('.tif.aux.xml', '', species_list)
species_list <- gsub('.tif', '', species_list)
species_list <- unique(species_list)

# removing failed species
failed_species <- read.csv('./data/failed_buffer_plants.csv')
failed_species <- failed_species[,1]
remove_plants <- which(species_list %in% failed_species)
species_list <- species_list[-remove_plants]
species_listT <- gsub('_', ' ', species_list)

# reading in region shape files
Australia <- shapefile('./masterShpFiles/AU_Buffer.shp')
California <- shapefile('./masterShpFiles/CA_Buffer.shp')
Chile <- shapefile('./masterShpFiles/CL_Buffer.shp')
Med <- shapefile('./masterShpFiles/ME_Buffer.shp')
South_Africa <- shapefile('./masterShpFiles/SA_Buffer.shp')

# transforming shapefiles to Mollweide projection
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
Australia <- spTransform(Australia, CRSobj = moll_crs)
California <- spTransform(California, CRSobj = moll_crs)
Chile <- spTransform(Chile, CRSobj = moll_crs)
Med <- spTransform(Med, CRSobj = moll_crs)
South_Africa <- spTransform(South_Africa, CRSobj = moll_crs)

# saving transformed shapefiles
writeOGR(Australia, dsn = './masterShpFiles/', layer = 'Australia_mol',
         driver = 'ESRI Shapefile', overwrite = T)
writeOGR(California, dsn = './masterShpFiles/', layer = 'California_mol',
         driver = 'ESRI Shapefile', overwrite = T)
writeOGR(Chile, dsn = './masterShpFiles/', layer = 'Chile_mol',
         driver = 'ESRI Shapefile', overwrite = T)
writeOGR(Med, dsn = './masterShpFiles/', layer = 'Med_mol',
         driver = 'ESRI Shapefile', overwrite = T)
writeOGR(South_Africa, dsn = './masterShpFiles/', 
         layer = 'South_Africa_mol',
         driver = 'ESRI Shapefile', overwrite = T)

# create separate directories per region
dir.create('./regions/Australia')
dir.create('./regions/California')
dir.create('./regions/Chile')
dir.create('./regions/Med')
dir.create('./regions/South_Africa')

# finding species with weird list files error
flag <- integer()
for (i in 1:length(species_list)) {
  tryCatch(
    {
      test <- raster(paste0('./rasters/', species_list[i], '.tif'))
    },
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      flag <<-c(flag,i)
    }
  )
}
missed <- species_list[flag]
missedT <- gsub("_", " ", missed)
write.table(missedT, './data/missed_species.txt', row.names = F, col.names = F)

species_list_use <- species_list[-flag]

# creating standard rasters to resample to for each region
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)
base_raster <- raster(ext = extent(land_poly), crs = moll_crs, 
                      resolution = 5000)

aus_ras1 <- rasterize(Australia, base_raster, field = 1)
aus_ras <- crop(aus_ras1, Australia)
writeRaster(aus_ras, filename = './data/aus_ras.tif', format = "GTiff",
            overwrite = T)

cal_ras1 <- rasterize(California, base_raster, field = 1)
cal_ras <- crop(cal_ras1, California)
writeRaster(cal_ras, filename = './data/cal_ras.tif', format = "GTiff",
            overwrite = T)

chi_ras1 <- rasterize(Chile, base_raster, field = 1)
chi_ras <- crop(chi_ras1, Chile)
writeRaster(chi_ras, filename = './data/chi_ras.tif', format = "GTiff",
            overwrite = T)

med_ras1 <- rasterize(Med, base_raster, field = 1)
med_ras <- crop(med_ras1, Med)
writeRaster(med_ras, filename = './data/med_ras.tif', format = "GTiff",
            overwrite = T)

saf_ras1 <- rasterize(South_Africa, base_raster, field = 1)
saf_ras <- crop(saf_ras1, South_Africa)
writeRaster(saf_ras, filename = './data/saf_ras.tif', format = "GTiff",
            overwrite = T)

# dir_sep function to separate the rasters into directories by region
# reproject and align their extents
dir_sep <- function(wanted_dir_name, wanted_ras) {
  for (i in 1:length(species_list_use)) {
    test_raster <- raster(paste0('./rasters/', species_list_use[i], '.tif'))
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
    rassum <- sum(extract(wanted_ras, test_points_moll), na.rm = T)
    if(rassum > 0) {
      new_raster <- rasterize(test_points_moll, base_raster, 
                              field = test_points$z)
      names(new_raster) <- species_list_use[i]
      writeRaster(new_raster, filename = paste0('./regions/', 
                                                wanted_dir_name, '/',
                                                species_list_use[i], '.tif'),
                  format = "GTiff", overwrite = T)
    }
  }
}

no_cores <- 10  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

dir_sep("Australia", aus_ras)
dir_sep("California", cal_ras)
dir_sep("Chile", chi_ras)
dir_sep("Med", med_ras)
dir_sep("South_Africa", saf_ras)

stopCluster(cl)

# removing species manually checked for errors
delete_chi <- c("Bromus_mango.tif", "Calamagrostis_diemii.tif", 
                "Festuca_acanthophylla_scabriuscula.tif", 
                "Luzula_ruiz−lealii.tif", "Traubia_modesta.tif", 
                "Trisetum_longiglume.tif", "Zephyranthes_ananuca.tif")
for (i in seq_along(delete_chi)) {
  unlink(paste0('./regions/Chile/', delete_chi[i]))
}

delete_cal <- c("Brodiaea_californica_leptandra.tif", 
                "Bromus_hordeaceus_hordeaceus.tif", "Scirpus_cyperinus.tif")
for (i in seq_along(delete_cal)) {
  unlink(paste0('./regions/California/', delete_cal[i]))
}

delete_aus <- c("Halophila_spinulosa.tif", "Pterostylis_microglossa.tif", 
                "Pterostylis_meridionalis.tif")
for (i in seq_along(delete_aus)) {
  unlink(paste0('./regions/Australia/', delete_aus[i]))
}

delete_saf <- c("Eulalia_villosa.tif", "Moorochloa_eruciformis.tif", 
                "Moraea_simulans.tif", "Restio_rottboellioides.tif",
                "Satyrium_hallackii_hallackii.tif", 
                "Schoenoplectiella_leucantha.tif")
for (i in seq_along(delete_saf)) {
  unlink(paste0('./regions/South_Africa/', delete_saf[i]))
}

delete_med <- c("Allium_fuscum.tif", "Allium_noeanum.tif", 
                "Bromus_hordeaceus_hordeaceus.tif", "Bromus_moeszii.tif",
                "Bromus_pulchellus.tif", "Bromus_sewerzowii.tif", 
                "Bromus_sipyleus.tif", "Colchicum_kotschyi.tif", 
                "Cyperus_digitatus_auricomus.tif", "Festuca_elwendiana.tif",
                "Gagea_uliginosa.tif", "Leymus_cappadocicus.tif",
                "Piptatherum_molinioides.tif", 
                "Setaria_obtusifolia.tif", 
                "Stipagrostis_hirtigluma_hirtigluma.tif", 
                "Zagrosia_persica.tif")
for (i in seq_along(delete_med)) {
  unlink(paste0('./regions/Med/', delete_med[i]))
}

# creating lists of filenames to stack
species_aus <- list.files('./regions/Australia')
for (i in 1:length(species_aus)) {
  species_aus[i] <- paste0('./regions/Australia/', species_aus[i])
}
aus_list <- as.list(species_aus)

species_cal <- list.files('./regions/California')
for (i in 1:length(species_cal)) {
  species_cal[i] <- paste0('./regions/California/', species_cal[i])
}
cal_list <- as.list(species_cal)

species_chi <- list.files('./regions/Chile')
for (i in 1:length(species_chi)) {
  species_chi[i] <- paste0('./regions/Chile/', species_chi[i])
}
chi_list <- as.list(species_chi)

species_med <- list.files('./regions/Med')
for (i in 1:length(species_med)) {
  species_med[i] <- paste0('./regions/Med/', species_med[i])
}
med_list <- as.list(species_med)

species_saf <- list.files('./regions/South_Africa')
for (i in 1:length(species_saf)) {
  species_saf[i] <- paste0('./regions/South_Africa/', species_saf[i])
}
saf_list <- as.list(species_saf)

# creating stacks per region
aus_stack <- stack(aus_list)
for (i in 1:dim(aus_stack)[3]) {
  names(aus_stack[[i]]) <- gsub("/srv/mybook/esheahan/Monocots/regions/Australia/",
  '', aus_stack[[i]]@file@name)
  names(aus_stack[[i]]) <- gsub('.tif', '', names(aus_stack[[i]]))
}
aus_spatrast <- rast(aus_stack)
writeRaster(aus_spatrast, filename = './data/aus_stack.tif',
            overwrite = T)

cal_stack <- stack(cal_list)
for (i in 1:dim(cal_stack)[3]) {
  names(cal_stack[[i]]) <- gsub("/srv/mybook/esheahan/Monocots/regions/California/",
                                '', cal_stack[[i]]@file@name)
  names(cal_stack[[i]]) <- gsub('.tif', '', names(cal_stack[[i]]))
}
cal_spatrast <- rast(cal_stack)
writeRaster(cal_spatrast, filename = './data/cal_stack.tif',
            overwrite = T)

chi_stack <- stack(chi_list)
for (i in 1:dim(chi_stack)[3]) {
  names(chi_stack[[i]]) <- gsub("/srv/mybook/esheahan/Monocots/regions/Chile/",
                                '', chi_stack[[i]]@file@name)
  names(chi_stack[[i]]) <- gsub('.tif', '', names(chi_stack[[i]]))
}
chi_spatrast <- rast(chi_stack)
writeRaster(chi_spatrast, filename = './data/chi_stack.tif',
            overwrite = T)

med_stack <- stack(med_list)
for (i in 1:dim(med_stack)[3]) {
  names(med_stack[[i]]) <- gsub("/srv/mybook/esheahan/Monocots/regions/Med/",
                                '', med_stack[[i]]@file@name)
  names(med_stack[[i]]) <- gsub('.tif', '', names(med_stack[[i]]))
}
med_spatrast <- rast(med_stack)
writeRaster(med_spatrast, filename = './data/med_stack.tif',
            overwrite = T)

saf_stack <- stack(saf_list)
for (i in 1:dim(saf_stack)[3]) {
  names(saf_stack[[i]]) <- 
    gsub("/srv/mybook/esheahan/Monocots/regions/South_Africa/",
                                '', saf_stack[[i]]@file@name)
  names(saf_stack[[i]]) <- gsub('.tif', '', names(saf_stack[[i]]))
}
saf_spatrast <- rast(saf_stack)
writeRaster(saf_spatrast, filename = './data/saf_stack.tif',
            overwrite = T)