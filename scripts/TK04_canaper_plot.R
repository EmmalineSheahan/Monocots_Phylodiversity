################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 4. TK04_canaper_plot
#####' 
#####' This script creates maps of the canaper output (from 02_canaper_analyses
#####' and 03_add_propGeoPD): PD, gpPD, sesPD, significant PD, sesRPD, 
#####' significant RPD, geoPD, nogeoPD, SR, geoSR, and gpSR. Respectively, these 
#####' maps are displayed in Figures 1, 4, and S2-10. Set arguments and run 
#####' separately for each region.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### Set up

#### ***Set the arguments here***
### File handle (path) of the shape file
fh_shp <- "input/Aus/Australia_mol.shp"
## Med shape
fh_shp2 <- "coreMedZones/coreMedZones.shp"
### Set a CRS to transform World object
target_crs = "+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=1"
### Set a version number or other file appendage for in and output files
name <- "Aus_ver1"
### Set the output directory
out_dir <- "plots/canaper"

#### Load in all the libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(sf)
library(terra)
library(tidyterra)
library(viridis)
library(data.table)
library(ggplot2)

##### Retrieve the input files

### Set up the region outline
shp <- vect(fh_shp)

### Set up world plot
World <- ne_countries(scale = "medium", returnclass = "sf") %>% st_make_valid()
## Change projection (and rotate) 
lon_0 <- unlist(strsplit(target_crs, "\\s"))
lon_0 <- as.numeric(gsub("\\+lon_0=", "", lon_0[grep("+lon_0", lon_0)]))
offset <- 180 - lon_0
rem_lon <- -0.0001 - offset

## Create narrow polygon at longitude origin
rem <- polygon <- st_polygon(x = list(rbind(c(rem_lon, 90),c(0 - offset, 90),
                                            c(0 - offset, -90),c(rem_lon, -90),
                                            c(rem_lon, 90)))) %>% st_sfc() %>% 
  st_set_crs(4326) 

## Modify world data set by clipping new polygon and project to Mollweide
World <- World %>% st_difference(rem) %>% 
  st_transform(crs = st_crs(target_crs)) %>% st_make_valid()

### Core shape
shp2 <- vect(fh_shp2)
shp2 <- project(shp2, target_crs)
shp2 <- aggregate(shp2)

### More
extent = ext(shp)
if (any(str_detect(name,c("Cal","SAf"),))){
  states <- c("united states of america","south africa")
  shp <- project(vect(ne_states(country = states)), target_crs)
} else {
  shp <- vect(World)
}

### Get data set
sphy <- fread(paste0("output/canaper/Biodiverse_rand_results_", name, ".csv"))
sphy <- as.data.frame(sphy)

###### Plot

### Set plotting scale
ratio <- as.numeric((extent[4]-extent[3]) / (extent[2]-extent[1]))
if (ratio >= 1){
  width = 5
  height = width * ratio
} else{
  height = 5
  width = height * (1/ratio)
}
height = height + 0.35

### Set plot theme 
plot_theme <- 
  theme(legend.position = "bottom", legend.key.width = unit(3,"line"),
        legend.key.height = unit(0.2,"line"), axis.title=element_blank(), 
        legend.title=element_blank(), legend.margin = margin(-4, 0, -6, 0),
        panel.grid = element_line(alpha("black", 0.5), linewidth =0.1), 
        panel.background = element_rect(fill=alpha("#D5E3D9",0.6)), text = element_text(size=16),
        panel.border = element_rect(fill=NA, color="black", linewidth = 0.1), 
        plot.background = element_rect(fill="transparent", color=NA), 
        legend.background = element_rect(fill = "transparent"), legend.key = element_rect(color="black"),
        legend.box.background = element_rect(fill = "transparent", color = NA))
### Create output directory if necessary
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = T)

#### Plot the continuous variables
cont_vars <- c("pd_obs", "pd_obs_z", "rpd_obs_z", "geoPD", "nogeoPD", "gpPD",
               "SR", "geoSR", "gpSR")
for (i in 1:length(cont_vars)){
  metric = cont_vars[i]
  cont_plot <- ggplot() +
    geom_spatvector(World, mapping=aes(), fill = alpha("#EEDD99",0.5)) +
    geom_tile(sphy, mapping = aes(x = x, y = y, fill = get(metric), colour = get(metric))) +
    geom_spatvector(data = shp, fill=NA, color=alpha("black",0.4), lwd = 0.1) +
    geom_spatvector(data = shp2, fill=NA, color="black", lwd = 0.5) +
    coord_sf(xlim = extent[1:2], ylim = extent[3:4]) +
    scale_fill_viridis(option="turbo") +
    scale_color_viridis(option="turbo") + plot_theme
  ggsave(cont_plot, filename = paste0(out_dir, "/", metric, "_", name, ".png"), 
         bg = "transparent", width = width, height = height, units = "in", dpi = "retina")
}
 
#### Plot the discrete (significance) variables
### Set colors
turbo_cols <- scales::viridis_pal(option="turbo")(1000)
group.colors <- c("< 0.01" = turbo_cols[25], "< 0.025" = turbo_cols[100], "not significant" = "#FAFAFA", "> 0.975" = turbo_cols[900], "> 0.99" = turbo_cols[975])

### Plot
for (metric in c("pd_signif", "rpd_signif")){
  sphy[,metric] <- factor(sphy[,metric], 
                          levels = c("< 0.01", "< 0.025", "not significant", "> 0.975", "> 0.99"))
  disc_plot <- ggplot() +
    geom_spatvector(World, mapping=aes(), fill = alpha("#EEDD99",0.5)) +
    geom_tile(sphy, mapping = aes(x = x, y = y, fill = get(metric), colour = get(metric))) +
    geom_spatvector(data = shp, fill=NA, color=alpha("black",0.4), lwd = 0.1) +
    geom_spatvector(data = shp2, fill=NA, color="black", lwd = 0.5) +
    coord_sf(xlim = extent[1:2], ylim = extent[3:4]) +
    scale_fill_manual(values=group.colors) +
    scale_color_manual(values=group.colors) +
    plot_theme + guides(fill = guide_legend(label.position = "bottom"))
  ggsave(disc_plot, filename = paste0(out_dir, "/", metric, "_", name, ".png"), 
         bg = "transparent", width = width, height = height, units = "in", dpi = "retina")
}
