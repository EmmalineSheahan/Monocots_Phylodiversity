# plotting biodiverse results

library(raster)
library(rnaturalearth)
library(sf)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
moll_crs <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
land_poly <- as_Spatial(land)
land_poly <- spTransform(land_poly, CRSobj = moll_crs)
cols <- colorRampPalette(colors = c('#03254c', 'white', '#800020'))

# PD_plot function
PD_plot <- function(wanted_region, region_short) {
  pd_ras <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                          '_justspatial_PD.tif'))
  pd.p_ras <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                            '_justspatial_PD_per_taxon.tif'))
  rpd_ras <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                           '_justspatial_PHYLO_RPD2.tif'))
  pd_sig <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                          '_signif_PD.tif'))
  pd.p_sig <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                            '_signif_PD_per_taxon.tif'))
  rpd_sig <- raster(paste0('./data/', wanted_region, '_PD/', region_short, 
                           '_signif_PHYLO_RPD2.tif'))
  pdf(paste0('./plots/', wanted_region, '/', region_short, '_PD_figures.pdf'))
  
  plot(pd_ras, main = paste0(wanted_region, ' PD'))
  plot(land_poly, col = NA, add = T)
  
  plot(rpd_ras, main = paste0(wanted_region, ' RPD'))
  plot(land_poly, col = NA, add = T)

  plot(pd.p_ras, main = paste0(wanted_region, ' PD Per Taxon'))
  plot(land_poly, col = NA, add = T)
  
  plot(pd_sig, main = paste0(wanted_region, ' PD Significance'), col = cols(3), 
       breaks = c(0, .025, .975, 1))
  plot(land_poly, col = NA, add = T)
  
  plot(rpd_sig, main = paste0(wanted_region, ' RPD Significance'), 
       col = cols(3), 
       breaks = c(0, .025, .975, 1))
  plot(land_poly, col = NA, add = T)
  
  plot(pd.p_sig, main = paste0(wanted_region, ' PD Per Taxon Significance'),
       col = cols(3), 
       breaks = c(0, .025, .975, 1))
  plot(land_poly, col = NA, add = T)
  dev.off()
}

PD_plot("Chile", "chi")
PD_plot("Australia", "aus")
PD_plot("South_Africa", "saf")
PD_plot("Med", "med")
PD_plot("California", "cal")

# PE_plot
PE_plot <- function(wanted_region, region_short) {
  pe_ras <- raster(paste0('./data/', wanted_region, '_endemism/', region_short, 
                          '_justspatial_PE_WE.tif'))
  rpe_ras <- raster(paste0('./data/', wanted_region, '_endemism/', region_short, 
                           '_justspatial_PHYLO_RPE2.tif'))
  pe_sig <- raster(paste0('./data/', wanted_region, '_endemism/', region_short, 
                         '_signif_PE_WE.tif'))
  rpe_sig <- raster(paste0('./data/', wanted_region, '_endemism/', region_short, 
                           '_signif_PHYLO_RPE2.tif'))
  pdf(paste0('./plots/', wanted_region, '/', region_short, '_PE_figures.pdf'))
  plot(pe_ras, main = paste0(wanted_region, ' PE'))
  plot(land_poly, col = NA, add = T)

  plot(rpe_ras, main = paste0(wanted_region, ' RPE'))
  plot(land_poly, col = NA, add = T)

  plot(pe_sig, main = paste0(wanted_region, ' PE Significance'),
     col = cols(3), 
     breaks = c(0, .025, .975, 1))
  plot(land_poly, col = NA, add = T)

  plot(rpe_sig, main = paste0(wanted_region, ' RPE Significance'),
     col = cols(3), 
     breaks = c(0, .025, .975, 1))
  plot(land_poly, col = NA, add = T)
  dev.off()
}

PE_plot("South_Africa", "saf")
PE_plot("Australia", "aus")
PE_plot("California", "cal")
PE_plot("Chile", "chi")
PE_plot("Med", "med")
