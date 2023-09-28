library(dplyr)
library(raster)
library(sf)
library(rgdal)
library(rnaturalearth)
library(CoordinateCleaner)
library(rangeBuilder)
library(doParallel)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
a_ionica <- read.csv('./Acis_ionica.csv')
a_ionica_cl <- cc_outl(a_ionica, lon="decimalLongitude", lat="decimalLatitude", method = "quantile")
a_ionica_sdf<- st_as_sf(x = a_ionica_cl,
                        coords = c("decimalLongitude", "decimalLatitude"))
a_ionica_range <- getDynamicAlphaHull(a_ionica_cl, partCount=0, coordHeaders=c('decimalLongitude','decimalLatitude'), clipToCoast = 'terrestrial')
a_ionica_range_sf <- st_as_sf(a_ionica_range[[1]])
a_ionica_range_cr <- st_transform(a_ionica_range_sf,crs=st_crs(land))

# plotting
par(mar = c(1, 1, 1, 1))
plot(land, col="none")
plot(a_ionica_range_cr, col=transparentColor('dark green', 0.5), add=T, 
     border = NA)
plot(st_geometry(a_ionica_sdf), add = T, col = "red")

plot(st_geometry(a_ionica_sdf), col = "red")
plot(a_ionica_range_cr, col=transparentColor('dark green', 0.5), add=T, 
     border = NA)
plot(land, col = NA, add = T)

# loop to create buffers
t <- raster('./Ruscus_hypoglossum_SDM.tif')
plot(t)
wanted_res <- res(t)

plants <- list.files('./species/forBuffer')
plants_fnames <- gsub('.csv', '', plants)
plants_tnames <- gsub('_', ' ', plants_fnames)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
landpoly <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")[1]
landpoly <- spTransform(landpoly, CRSobj = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
small_list <- vector(length = length(plants))

no_cores <- detectCores() - 6  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

flag <- integer()
for(i in 1:length(plants)) {
  tryCatch(
    {
  wanted_species <- read.csv(paste0('./species/forBuffer/', plants[i]))
  wanted_species <- wanted_species %>% filter(!(is.na(decimalLatitude)))
  wanted_species <- wanted_species %>% filter(!(is.na(decimalLongitude)))
  if (length(wanted_species$decimalLatitude) < 7) {
    small_list[i] <- plants_tnames[i]
     } else {
    wanted_species_cl <- cc_outl(wanted_species, lon="decimalLongitude", 
                               lat="decimalLatitude", method = "quantile")
    wanted_species_sdf<- st_as_sf(x = wanted_species_cl,
                          coords = c("decimalLongitude", "decimalLatitude"))
    wanted_species_range <- getDynamicAlphaHull(wanted_species_cl, partCount=0, 
                                                coordHeaders=c('decimalLongitude',
                                                               'decimalLatitude'), 
                                                clipToCoast = 'terrestrial')
    wanted_species_range_sf <- st_as_sf(wanted_species_range[[1]])
    wanted_species_range_cr <- st_transform(wanted_species_range_sf,
                                          crs=st_crs(land)) 
    pdf(paste0('./plots/', plants_fnames[i], '.pdf'))
    plot(st_geometry(wanted_species_sdf), col = "red", main = plants_tnames[i])
    plot(wanted_species_range_cr, col=transparentColor('dark green', 0.5), add=T, 
       border = NA)
    plot(land, col = NA, add = T)
    dev.off()
    shapefile(wanted_species_range[[1]], filename = paste0('./polygons/', 
                      plants_fnames[i], '.shp'), overwrite = T)
    occ <- 1
    occ <- data.frame(occ)
    spdf <- SpatialPolygonsDataFrame(Sr = wanted_species_range[[1]], 
                                     data = occ)
    base_raster <- raster(landpoly, resolution = wanted_res)
    base_raster <- crop(base_raster, extent(spdf))
    landpolycrop <- crop(landpoly, extent(spdf))
    ras <- raster::rasterize(spdf, base_raster, field = 'occ')
    finalras <- raster::mask(ras, landpolycrop)
    writeRaster(finalras, filename = paste0('./rasters/', 
                      plants_fnames[i], '.tif'), format = "GTiff",
                overwrite = T)
     }
     },
     error=function(err){
     message('On iteration ',i, ' there was an error: ',err)
     flag <<-c(flag,i)
     }
  )
}

stopCluster(cl)

small_list <- small_list[which(small_list != FALSE)]

flag
small_list

# pipeline with cleaned failed species
plants <- list.files('./species/cleaned_species')
plants_fnames <- gsub('.csv', '', plants)
plants_fnames <- gsub('_cleaned', '', plants_fnames)
plants_tnames <- gsub('_', ' ', plants_fnames)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
landpoly <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")[1]
landpoly <- spTransform(landpoly, CRSobj = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
small_list <- vector(length = length(plants))

no_cores <- detectCores() - 6  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

flag <- integer()
for(i in 1:length(plants)) {
  tryCatch(
    {
      wanted_species <- read.csv(paste0('./species/cleaned_species/', plants[i]))
      wanted_species <- wanted_species %>% filter(!(is.na(decimalLatitude)))
      wanted_species <- wanted_species %>% filter(!(is.na(decimalLongitude)))
      if (length(wanted_species$decimalLatitude) < 5) {
        small_list[i] <- plants_tnames[i]
      } else {
        #wanted_species_cl <- cc_outl(wanted_species, lon="decimalLongitude", 
                                     #lat="decimalLatitude", method = "quantile")
        wanted_species_sdf<- st_as_sf(x = wanted_species,
                                      coords = c("decimalLongitude", "decimalLatitude"))
        wanted_species_range <- getDynamicAlphaHull(wanted_species, partCount=0, 
                                                    coordHeaders=c('decimalLongitude',
                                                                   'decimalLatitude'), 
                                                    clipToCoast = 'terrestrial')
        wanted_species_range_sf <- st_as_sf(wanted_species_range[[1]])
        wanted_species_range_cr <- st_transform(wanted_species_range_sf,
                                                crs=st_crs(land)) 
        pdf(paste0('./plots/cleaned/', plants_fnames[i], '.pdf'))
        plot(st_geometry(wanted_species_sdf), col = "red", main = plants_tnames[i])
        plot(wanted_species_range_cr, col=transparentColor('dark green', 0.5), add=T, 
             border = NA)
        plot(land, col = NA, add = T)
        dev.off()
        shapefile(wanted_species_range[[1]], filename = paste0('./polygons/cleaned/', 
                                                               plants_fnames[i], '.shp'), overwrite = T)
        occ <- 1
        occ <- data.frame(occ)
        spdf <- SpatialPolygonsDataFrame(Sr = wanted_species_range[[1]], 
                                         data = occ)
        base_raster <- raster(landpoly, resolution = wanted_res)
        base_raster <- crop(base_raster, extent(spdf))
        landpolycrop <- crop(landpoly, extent(spdf))
        ras <- raster::rasterize(spdf, base_raster, field = 'occ')
        finalras <- raster::mask(ras, landpolycrop)
        writeRaster(finalras, filename = paste0('./rasters/cleaned/', 
                                                plants_fnames[i], '.tif'), format = "GTiff",
                    overwrite = T)
      }
    },
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      flag <<-c(flag,i)
    }
  )
}

stopCluster(cl)

small_list <- small_list[which(small_list != FALSE)]

flag
small_list
