# plotting CANAPE

library(dplyr)
library(ggplot2)
library(raster)
library(canaper)
library(rnaturalearth)
library(sf)

moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)
cols <- colorRampPalette(colors = c('#56B4E9', '#D55E00', '#009E73', "grey"))

# canape_plot function to plot CANAPE results per region
canape_plot <- function(wanted_region, region_short) {
  pe_orig <- raster(paste0('./data/', wanted_region, '_endemism/', region_short,
                           '_rand_C_PE_WE_P.tif'))
  pe_alt <- raster(paste0('./data/', wanted_region, '_endemism/', region_short,
                          '_rand_C_PHYLO_RPE_NULL2.tif'))
  rpe <- raster(paste0('./data/', wanted_region, '_endemism/', region_short,
                '_rand_C_PHYLO_RPE2.tif'))
  coord_mat <- coordinates(rpe)
  rpe_vals <- values(rpe)
  pe_orig_vals <- values(pe_orig)
  pe_alt_vals <- values(pe_alt)
  class_mat <- cbind(coord_mat, rpe_vals, pe_orig_vals, pe_alt_vals)
  class_df <- data.frame(class_mat)
  endemism_class <- vector(length = length(class_df$x))
  endemism_num <- vector(length = length(class_df$x))
  # for loop to code type of endemism
  # numeric code:
  # Paleo Endemism = 1
  # Neo Endemism = 2
  # Mixed Endemism = 3
  # not significant = 4
  for (i in 1:length(class_df$x)) {
    if (is.na(class_df$pe_orig_vals[i] || class_df$pe_alt_vals[i])){
      endemism_class[i] <- NA
      endemism_num[i] <- NA
    } else {
    if (class_df$pe_orig_vals[i] > 243.75 || class_df$pe_alt_vals[i] > 243.75) {
      if (class_df$rpe_vals[i] > 243.75) {
        endemism_class[i] <- "Paleo Endemism"
        endemism_num[i] <- 1
      } else if (class_df$rpe_vals[i] < 6.25) {
        endemism_class[i] <- "Neo Endemism"
        endemism_num[i] <- 2
      } else {
        endemism_class[i] <- "Mixed Endemism"
        endemism_num[i] <- 3
        }
      } else {
      endemism_class[i] <- "Not Significant"
      endemism_num[i] <- 4
    }
    }
  }
  xyzdf <- data.frame(class_df$x, class_df$y, endemism_num)
  colnames(xyzdf) <- c("x", "y", "endemism_num")
  canape_ras <- rasterFromXYZ(xyz = xyzdf, res = c(10000,10000), crs = moll_crs)
  
  pdf(paste0('./plots/', wanted_region, '/', region_short, '_CANAPE.pdf'), 
      width = 10)
  plot(canape_ras, main = paste0('CANAPE for ', wanted_region),
       col = cols(4),
       breaks = c(0, 1, 2, 3, 4),
       legend = F)
  plot(land_poly, add = T, col = NA)
  legend("bottomleft", 
         legend = c("Paleo Endemism", "Neo Endemism", 
                    "Mixed Endemism", "Not Significant"), 
         fill = cols(4),
         cex = 1,
         horiz = F, bg = "white")
  dev.off()
}

canape_plot("Chile", "chi")
canape_plot("California", "cal")
canape_plot("Australia", "aus")
canape_plot("South_Africa", "saf")
canape_plot("Med", "med")

# check on California
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

buff_plants <- list.files('./species/forBuffer')
buff_plants <- gsub('.csv', '.tif', buff_plants)
all_plants <- list.files('./rasters')
buff_plants <- buff_plants[which(buff_plants %in% all_plants)]

pdf('./plots/all_buffer_plants.pdf')
for (i in 1:length(cleaned_plants)) {
  temp <- raster(paste0('./rasters/', cleaned_plants[i]))
  buff_names <- gsub('.tif', '', cleaned_plants)
  buff_names <- gsub('_', ' ', buff_names)
  plot(temp, main = buff_names[i])
  plot(land, add = T, col = NA)
}
dev.off()

