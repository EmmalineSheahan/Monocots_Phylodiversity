# finding the proportion of geophytes

library(dplyr)
library(raster)
library(sp)
library(rnaturalearth)
library(sf)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"

aus_richness <- raster('./data/aus_richness.tif')
cal_richness <- raster('./data/cal_richness.tif')
chi_richness <- raster('./data/chi_richness.tif')
med_richness <- raster('./data/med_richness.tif')
saf_richness <- raster('./data/saf_richness.tif')

# reading in region shape files
Australia <- st_read(dsn = './masterShpFiles/', layer = 'Australia_mol')
California <- st_read(dsn = './masterShpFiles/', layer = 'California_mol')
Chile <- st_read(dsn = './masterShpFiles/', layer = 'Chile_mol')
Med <- st_read(dsn = './masterShpFiles/', layer = 'Med_mol')
South_Africa <- st_read(dsn = './masterShpFiles/', layer = 'South_Africa_mol')

# reading in geophyte scored csv
geophyte <- read.csv('./data/wcvp_names_accepted_subset_geophytescored.csv')
wanted_name_match <- vector(length = nrow(geophyte))
for (i in 1:nrow(geophyte)) {
  wanted_name_match[i] <- paste0(geophyte$genus[i], '_', geophyte$species[i],
                                 '.tif')
}

geophyte <- cbind(geophyte, wanted_name_match)
remove <- which(is.na(geophyte$geophyte))
geophyte <- geophyte[-remove,]

# creating function to write geophyte specific rasters
geo_fun <- function(wanted_reg) {
  want_list <- list.files(paste0('./regions/', wanted_reg))
  dir.create(paste0('./regions/geophyte_', wanted_reg))
  for (i in seq_along(geophyte$wanted_name_match)) {
    if (geophyte$wanted_name_match[i] %in% want_list == T) {
      tmp <- raster(paste0('./regions/', wanted_reg, '/', 
                           geophyte$wanted_name_match[i]))
      if (geophyte$geophyte[i] == 0) {
        rclmat <- matrix(c(0,1,0), ncol = 3, byrow = T)
        tmp <- reclassify(tmp, rclmat)
        writeRaster(tmp, filename = paste0('./regions/geophyte_', 
                                           wanted_reg, '/', 
                                           geophyte$wanted_name_match[i]), 
        overwrite = T)
      } else {
        writeRaster(tmp, filename = paste0('./regions/geophyte_', 
                                           wanted_reg, '/', 
                                       geophyte$wanted_name_match[i]), 
                    overwrite = T)
      }
    } else {
      print("not in region")
    }
  }
}


geo_fun("Australia")
geo_fun("California")
geo_fun("Chile")
geo_fun("Med")
geo_fun("South_Africa")

# creating lists of filenames to stack
species_aus_geo <- list.files('./regions/geophyte_Australia')
for (i in 1:length(species_aus_geo)) {
  species_aus_geo[i] <- paste0('./regions/geophyte_Australia/', 
                           species_aus_geo[i])
}
aus_list_geo <- as.list(species_aus_geo)

species_cal_geo <- list.files('./regions/geophyte_California')
for (i in 1:length(species_cal_geo)) {
  species_cal_geo[i] <- paste0('./regions/geophyte_California/', 
                           species_cal_geo[i])
}
cal_list_geo <- as.list(species_cal_geo)

species_chi_geo <- list.files('./regions/geophyte_Chile')
for (i in 1:length(species_chi_geo)) {
  species_chi_geo[i] <- paste0('./regions/geophyte_Chile/', 
                               species_chi_geo[i])
}
chi_list_geo <- as.list(species_chi_geo)

species_med_geo <- list.files('./regions/geophyte_Med')
for (i in 1:length(species_med_geo)) {
  species_med_geo[i] <- paste0('./regions/geophyte_Med/', 
                               species_med_geo[i])
}
med_list_geo <- as.list(species_med_geo)

species_saf_geo <- list.files('./regions/geophyte_South_Africa')
for (i in 1:length(species_saf_geo)) {
  species_saf_geo[i] <- paste0('./regions/geophyte_South_Africa/',
                               species_saf_geo[i])
}
saf_list_geo <- as.list(species_saf_geo)

# creating stacks per region
aus_stack_geo <- stack(aus_list_geo)
save(aus_stack_geo, file = './data/aus_stack_geo.Rdata')

cal_stack_geo <- stack(cal_list_geo)
save(cal_stack_geo, file = './data/cal_stack_geo.Rdata')

chi_stack_geo <- stack(chi_list_geo)
save(chi_stack_geo, file = './data/chi_stack_geo.Rdata')

med_stack_geo <- stack(med_list_geo)
save(med_stack_geo, file = './data/med_stack_geo.Rdata')

saf_stack_geo <- stack(saf_list_geo)
save(saf_stack_geo, file = './data/saf_stack_geo.Rdata')

land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)

# richness plots
aus_richness_geo <- calc(aus_stack_geo, fun = sum, na.rm = T)
aus_richness_geo <- crop(aus_stack_geo, as_Spatial(Australia))
aus_richness_geo <- mask(aus_richness_geo, land_poly)
save(aus_richness_geo, file = './data/aus_richness_geo.Rdata')

cal_richness_geo <- calc(cal_stack_geo, fun = sum, na.rm = T)
cal_richness_geo <- crop(cal_stack_geo, as_Spatial(California))
cal_richness_geo <- mask(cal_richness_geo, land_poly)
save(cal_richness_geo, file = './data/cal_richness_geo.Rdata')

chi_richness_geo <- calc(chi_stack_geo, fun = sum, na.rm = T)
chi_richness_geo <- crop(chi_stack_geo, as_Spatial(Chile))
chi_richness_geo <- mask(chi_richness_geo, land_poly)
save(chi_richness_geo, file = './data/chi_richness_geo.Rdata')

med_richness_geo <- calc(med_stack_geo, fun = sum, na.rm = T)
med_richness_geo <- crop(med_stack_geo, as_Spatial(Med))
med_richness_geo <- mask(med_richness_geo, land_poly)
save(med_richness_geo, file = './data/med_richness_geo.Rdata')

saf_richness_geo <- calc(saf_stack_geo, fun = sum, na.rm = T)
saf_richness_geo <- crop(saf_stack_geo, as_Spatial(South_Africa))
saf_richness_geo <- mask(saf_richness_geo, land_poly)
save(saf_richness_geo, file = './data/saf_richness_geo.Rdata')

# dividing geophyte richness by total richness and plotting
aus_geo_prop <- aus_richness_geo/aus_richness
pdf('./plots/Australia/geophyte_proportion_Australia.pdf')
plot(aus_geo_prop, main = "Proportion of geophytes in Australia")
plot(land_poly, add = T, col = NA)
dev.off()
writeRaster(aus_geo_prop, filename = './data/aus_geo_prop.tif', format = "GTiff")

cal_geo_prop <- cal_richness_geo/cal_richness
pdf('./plots/California/geophyte_proportion_California.pdf')
plot(cal_geo_prop, main = "Proportion of geophytes in California")
plot(land_poly, add = T, col = NA)
dev.off()
writeRaster(cal_geo_prop, filename = './data/cal_geo_prop.tif', format = "GTiff")

chi_geo_prop <- chi_richness_geo/chi_richness
pdf('./plots/Chile/geophyte_proportion_Chile.pdf')
plot(chi_geo_prop, main = "Proportion of geophytes in Chile")
plot(land_poly, add = T, col = NA)
dev.off()
writeRaster(chi_geo_prop, filename = './data/chi_geo_prop.tif', format = "GTiff")

med_geo_prop <- med_richness_geo/med_richness
pdf('./plots/Med/geophyte_proportion_Med.pdf')
plot(med_geo_prop, main = "Proportion of geophytes in the Mediterranean")
plot(land_poly, add = T, col = NA)
dev.off()
writeRaster(med_geo_prop, filename = './data/med_geo_prop.tif', format = "GTiff")

saf_geo_prop <- saf_richness_geo/saf_richness
pdf('./plots/South_Africa/geophyte_proportion_South_Africa.pdf')
plot(saf_geo_prop, main = "Proportion of geophytes in South Africa")
plot(land_poly, add = T, col = NA)
dev.off()
writeRaster(saf_geo_prop, filename = './data/saf_geo_prop.tif', format = "GTiff")

