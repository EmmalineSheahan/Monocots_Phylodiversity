# taxonomic endemism

library(dplyr)
library(raster)
library(sf)
library(rnaturalearth)
library(terra)

# creating land
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)

# reading in richness
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

# cal_endemism function to calculate an dplot the taxonomic endemism in each region
calc_endemism <- function(wanted_region, reg_shape) {
  species <- list.files(paste0('./regions/', wanted_region))
  endemism_ras_list <- vector("list", length = length(species))
  for (i in 1:length(species)) {
    ras <- raster(paste0('./regions/', wanted_region, '/', species[i]))
    ras <- ras %>% rast()
    end_ras <- terra::focal(ras, w=3, fun = sum,
                            na.rm = T)
    end_ras <- end_ras/sum(values(ras), na.rm = T)
    end_ras <- raster(end_ras)
    endemism_ras_list[[i]] <- end_ras
  }
  end_stack <- stack(endemism_ras_list)
  endemism <- calc(end_stack, fun = sum, na.rm = T)
  endemism <- mask(endemism, land_poly)
  return(endemism)
}

aus_endemism <- calc_endemism("Australia")
aus_endemism <- crop(aus_endemism, as_Spatial(Australia))
aus_weighted_endemism <- aus_endemism/aus_richness
save(aus_weighted_endemism, file = './data/aus_weighted_endemism.Rdata')
pdf('./plots/aus_weighted_endemism.pdf')
plot(log(aus_weighted_endemism), 
     main = "Australia Corrected Weighted Endemism")
plot(land_poly, add = T, col = NA)
dev.off()

cal_endemism <- calc_endemism("California")
cal_endemism <- crop(cal_endemism, as_Spatial(California))
cal_weighted_endemism <- cal_endemism/cal_richness
writeRaster(cal_weighted_endemism, filename = './data/cal_weighted_endemism.tif',
            format = "GTiff")
pdf('./plots/cal_weighted_endemism.pdf')
plot(log(cal_weighted_endemism), 
     main = "California Corrected Weighted Endemism")
plot(land_poly, add = T, col = NA)
dev.off()

med_endemism <- calc_endemism("Med")
med_endemism <- crop(med_endemism, as_Spatial(Med))
med_weighted_endemism <- med_endemism/med_richness
writeRaster(med_weighted_endemism, filename = './data/med_weighted_endemism.tif',
            format = "GTiff")
pdf('./plots/med_weighted_endemism.pdf')
plot(log(med_weighted_endemism), 
     main = "Mediterranean Corrected Weighted Endemism")
plot(land_poly, add = T, col = NA)
dev.off()

chi_endemism <- calc_endemism("Chile")
chi_endemism <- crop(chi_endemism, as_Spatial(Chile))
chi_weighted_endemism <- chi_endemism/chi_richness
writeRaster(chi_weighted_endemism, file = './data/chi_weighted_endemism.tif', 
     format = "GTiff")
pdf('./plots/chi_weighted_endemism.pdf')
plot(log(chi_weighted_endemism), 
     main = "Chile Corrected Weighted Endemism")
plot(land_poly, add = T, col = NA)
dev.off()

saf_endemism <- calc_endemism("South_Africa")
saf_endemism <- crop(saf_endemism, as_Spatial(South_Africa))
saf_weighted_endemism <- saf_endemism/saf_richness
writeRaster(saf_weighted_endemism, file = './data/saf_weighted_endemism.tif',
            format = "GTiff")
pdf('./plots/saf_weighted_endemism.pdf')
plot(log(saf_weighted_endemism), 
     main = "South Africa Corrected Weighted Endemism")
plot(land_poly, add = T, col = NA)
dev.off()
