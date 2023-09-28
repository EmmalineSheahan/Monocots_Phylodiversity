# species richness and PAMs

library(sf)
library(terra)
library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(doParallel)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)

# reading in region shape files
Australia <- st_read(dsn = './masterShpFiles/', layer = 'Australia_mol')
California <- st_read(dsn = './masterShpFiles/', layer = 'California_mol')
Chile <- st_read(dsn = './masterShpFiles/', layer = 'Chile_mol')
Med <- st_read(dsn = './masterShpFiles/', layer = 'Med_mol')
South_Africa <- st_read(dsn = './masterShpFiles/', layer = 'South_Africa_mol')

# reading in stacks and transforming them to rasters
chi_stack <- rast('./data/chi_stack.tif')
chi_stack <- crop(chi_stack, Chile)
chi_stack <- mask(chi_stack, Chile)
chi_list <- vector("list", length = dim(chi_stack)[3])
for (i in 1:dim(chi_stack)[3]) {
  chi_list[[i]] <- raster(chi_stack[[i]])
}
chi_stack <- stack(chi_list)

cal_stack <- rast('./data/cal_stack.tif')
cal_stack <- crop(cal_stack, California)
cal_stack <- mask(cal_stack, California)
cal_list <- vector("list", length = dim(cal_stack)[3])
for (i in 1:dim(cal_stack)[3]) {
  cal_list[[i]] <- raster(cal_stack[[i]])
}
cal_stack <- stack(cal_list)

aus_stack <- rast('./data/aus_stack.tif')
aus_stack <- crop(aus_stack, Australia)
aus_stack <- mask(aus_stack, Australia)
aus_list <- vector("list", length = dim(aus_stack)[3])
for (i in 1:dim(aus_stack)[3]) {
  aus_list[[i]] <- raster(aus_stack[[i]])
}
aus_stack <- stack(aus_list)

saf_stack <- rast('./data/saf_stack.tif')
saf_stack <- crop(saf_stack, South_Africa)
saf_stack <- mask(saf_stack, South_Africa)
saf_list <- vector("list", length = dim(saf_stack)[3])
for (i in 1:dim(saf_stack)[3]) {
  saf_list[[i]] <- raster(saf_stack[[i]])
}
saf_stack <- stack(saf_list)

med_stack <- rast('./data/med_stack.tif')
med_stack <- crop(med_stack, Med)
med_stack <- mask(med_stack, Med)
med_list <- vector("list", length = dim(med_stack)[3])
for (i in 1:dim(med_stack)[3]) {
  med_list[[i]] <- raster(med_stack[[i]])
}
med_stack <- stack(med_list)

# richness plots
aus_richness <- calc(aus_stack, fun = sum, na.rm = T)
aus_richness <- mask(aus_richness, land_poly)
writeRaster(aus_richness, filename = './data/aus_richness.tif', format = "GTiff",
            overwrite = T)

cal_richness <- calc(cal_stack, fun = sum, na.rm = T)
cal_richness <- mask(cal_richness, land_poly)
writeRaster(cal_richness, filename = './data/cal_richness.tif', format = "GTiff",
            overwrite = T)

chi_richness <- calc(chi_stack, fun = sum, na.rm = T)
chi_richness <- mask(chi_richness, land_poly)
writeRaster(chi_richness, filename = './data/chi_richness.tif', format = "GTiff",
            overwrite = T)

med_richness <- calc(med_stack, fun = sum, na.rm = T)
med_richness <- mask(med_richness, land_poly)
writeRaster(med_richness, filename = './data/chi_richness.tif', format = "GTiff",
            overwrite = T)

saf_richness <- calc(saf_stack, fun = sum, na.rm = T)
saf_richness <- mask(saf_richness, land_poly)
writeRaster(saf_richness, filename = './data/chi_richness.tif', format = "GTiff",
            overwrite = T)

pdf('./plots/richness_Australia.pdf')
plot(aus_richness, main = "Australia")
plot(land_poly, add = T, col = NA)
dev.off()

pdf('./plots/richness_California.pdf')
plot(cal_richness, main = "California")
plot(land_poly, add = T, col = NA)
dev.off()

pdf('./plots/richness_Chile.pdf')
plot(chi_richness, main = "Chile")
plot(land_poly, add = T, col = NA)
dev.off()

pdf('./plots/richness_Mediterranean.pdf')
plot(med_richness, main = "Mediterranean")
plot(land_poly, add = T, col = NA)
dev.off()

pdf('./plots/richness_South_Africa.pdf')
plot(saf_richness, main = "South Africa")
plot(land_poly, add = T, col = NA)
dev.off()

# write region lists
aus_species <- list.files('./regions/Australia')
aus_species <- gsub('.tif', '', aus_species)
aus_species <- gsub('_', ' ', aus_species)
write.table(aus_species, './data/aus_species.txt', row.names = F, 
            col.names = F)

cal_species <- list.files('./regions/California')
cal_species <- gsub('.tif', '', cal_species)
cal_species <- gsub('_', ' ', cal_species)
write.table(cal_species, './data/cal_species.txt', row.names = F, 
            col.names = F)

med_species <- list.files('./regions/Med')
med_species <- gsub('.tif', '', med_species)
med_species <- gsub('_', ' ', med_species)
write.table(med_species, './data/med_species.txt', row.names = F, 
            col.names = F)

chi_species <- list.files('./regions/Chile')
chi_species <- gsub('.tif', '', chi_species)
chi_species <- gsub('_', ' ', chi_species)
write.table(chi_species, './data/chi_species.txt', row.names = F, 
            col.names = F)

saf_species <- list.files('./regions/South_Africa')
saf_species <- gsub('.tif', '', saf_species)
saf_species <- gsub('_', ' ', saf_species)
write.table(saf_species, './data/saf_species.txt', row.names = F, 
            col.names = F)

