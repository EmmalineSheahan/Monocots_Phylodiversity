library(raster)
library(sf)

# creating land
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)

# creating pdfs of every species in each region for final checking
cal_plants <- list.files('./regions/California')
pdf('./plots/investigate_california.pdf')
for (i in 1:length(cal_plants)) {
  temp <- raster(paste0('./regions/California/', cal_plants[i]))
  cal_names <- gsub('.tif', '', cal_plants)
  cal_names <- gsub('_', ' ', cal_names)
  plot(temp, main = cal_names[i])
  plot(land_poly, add = T)
}
dev.off()

aus_plants <- list.files('./regions/Australia')
pdf('./plots/investigate_australia.pdf')
for (i in 1:length(aus_plants)) {
  temp <- raster(paste0('./regions/Australia/', aus_plants[i]))
  aus_names <- gsub('.tif', '', aus_plants)
  aus_names <- gsub('_', ' ', aus_names)
  plot(temp, main = aus_names[i])
  plot(land_poly, add = T)
}
dev.off()

chi_plants <- list.files('./regions/Chile')
pdf('./plots/investigate_chile.pdf')
for (i in 1:length(chi_plants)) {
  temp <- raster(paste0('./regions/Chile/', chi_plants[i]))
  chi_names <- gsub('.tif', '', chi_plants)
  chi_names <- gsub('_', ' ', chi_names)
  plot(temp, main = chi_names[i])
  plot(land_poly, add = T)
}
dev.off()

saf_plants <- list.files('./regions/South_Africa')
pdf('./plots/investigate_south_africa.pdf')
for (i in 1:length(saf_plants)) {
  temp <- raster(paste0('./regions/South_Africa/', saf_plants[i]))
  saf_names <- gsub('.tif', '', saf_plants)
  saf_names <- gsub('_', ' ', saf_names)
  plot(temp, main = saf_names[i])
  plot(land_poly, add = T)
}
dev.off()

med_plants <- list.files('./regions/Med')
pdf('./plots/investigate_med.pdf')
for (i in 1:length(med_plants)) {
  temp <- raster(paste0('./regions/Med/', med_plants[i]))
  med_names <- gsub('.tif', '', med_plants)
  med_names <- gsub('_', ' ', med_names)
  plot(temp, main = med_names[i])
  plot(land_poly, add = T)
}
dev.off()