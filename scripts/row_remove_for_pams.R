# find coordinates of false NA's

library(raster)
library(dplyr)

# reading in richness for base raster
aus_ras <- raster('./data/aus_richness.tif')
cal_ras <- raster('./data/cal_richness.tif')
chi_ras <- raster('./data/chi_richness.tif')
med_ras <- raster('./data/med_richness.tif')
saf_ras <- raster('./data/saf_richness.tif')

# creating full polygon raster
cal_row_remove <- which(is.na(values(cal_ras)))

med_row_remove <- which(is.na(values(med_ras)))

chi_row_remove <- which(is.na(values(chi_ras)))

saf_row_remove <- which(is.na(values(saf_ras)))

aus_row_remove <- which(is.na(values(aus_ras)))