################ This script will create a figure to visualize the models

### Load libraries
library(ggplot2)
library(egg)

create_sdmFigure <- function(world = world, spp, r, r_pa, occ_df){
  spp = gsub("_", " ", spp)
  
############### This is from clean coordinate
  # world <- ne_countries(scale = "medium", returnclass = "sf")
  # us <- ne_countries(continent = "North America", returnclass = "sf")
  # spname = gsub(".csv", "", basename(curFile))
  # 
  # a <- ggplot() +
  #   geom_sf(world, mapping = aes()) +
  #   geom_point(cs2, mapping = aes(x = decimalLongitude, y = decimalLatitude),
  #              color = "purple") + 
  #   coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
  #            ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
  #   ggtitle(paste(spname, " Uncleaned", sep = "")) + 
  #   theme_bw()
  # # "navy" , "olivedrab3"
  # # "blue", "palegreen2"
  # cols <- c("TRUE" = "olivedrab3", "FALSE" = "blue2")
  # b <- ggplot() +
  #   geom_sf(world, mapping = aes()) +
  #   geom_point(cs2, mapping = aes(x = decimalLongitude, y = decimalLatitude,
  #                                 color = .summary))  +
  #   # scale_color_manual(values = c("blue2", "olivedrab3")) +
  #   scale_color_manual(values = cols) +
  #   coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
  #            ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
  #   labs(color = "Not flagged") +
  #   ggtitle(paste(spname, " Cleaned", sep = "")) +
  #   theme_bw()
  # 
  # cp <- cowplot::plot_grid(a, b, nrow = 2, ncol = 1)
  # 
  # ## Save the data and save the plot
  # write.csv(x = cs2, 
  #           file = paste("./GlobalDownloadedData/cleanedCoordinates/", basename(curFile), sep = ""),
  #           row.names = FALSE)
  # 
  # 
  # ggsave(plot = cp, 
  #        filename = paste("./GlobalDownloadedData/cleanedCoordinates/", gsub(".csv", ".png", basename(curFile)), sep = ""),
  #        width = 8, height = 8)
  # 

########## Till here.  
  
  q1 <- ggplot() +
    
    geom_raster(data = r , aes(x = x, y = y, fill = ClogLog))
  
  # 
  # 
  # ## Create a plot for the model
   p1 <- ggplot() +
     geom_sf(world, mapping = aes(), fill = NA) +
     geom_tile(as.data.frame(r, xy = T) %>% na.omit() %>% rename(ClogLog = 3),
               mapping = aes(x = x, y = y, fill = ClogLog)) + 
     geom_point(occ_df, mapping = aes(x = x, y = y), color = 'black', shape = 21, alpha = 0.6) #+
  #   coord_sf(xlim = c(min(occ_df$x) - 500, max(occ_df$x) + 500),
  #            ylim = c(min(occ_df$y) - 500, max(occ_df$y) + 500)) +
  #   scale_fill_viridis_c() +
  #   ggtitle(paste0(spp, " SDM", " (# coords = ", nrow(occ_df), ")")) +
  #   theme(legend.position = "none") +
  #   theme_classic()
  # 
  # ## Create a plot for the presence-absence model
  # p2 <- ggplot() +
  #   geom_sf(world, mapping = aes()) +
  #   geom_tile(as.data.frame(r_pa,xy = T) %>% na.omit() %>% rename(ClogLog = 3),
  #             mapping = aes(x = x, y = y, fill = as.character(ClogLog))) + 
  #   geom_point(occ_df, mapping = aes(x = x, y = y), color = 'black', shape = 21, alpha = 0.6) +
  #   coord_sf(xlim = c(min(occ_df$x) - 500, max(occ_df$x) + 500),
  #            ylim = c(min(occ_df$y) - 500, max(occ_df$y) + 500)) +
  #   labs(fill = "Presence") +
  #   scale_fill_viridis_d() +
  #   theme(legend.position = "none") +
  #   theme_classic()
  # 
  # ## Combine these two plots into one figure
  # e <- egg::ggarrange(p1, p2, nrow = 1)
  # 
  # return(e)
}













