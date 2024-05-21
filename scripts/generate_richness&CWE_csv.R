library(terra)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(data.table)
library(sf)

unixtools::set.tempdir("tmp/")
tmp_dir <- unixtools::set.tempdir("tmp/")
raster::rasterOptions(tmpdir="tmp")
files <- list.files(tmp_dir, full.names = T,  all.files = T, recursive = T)
file.remove(files)
gc()

# list files with tifs
tifs <- list.files("Endemism_tifs/", recursive = T, full.names = T)

## read in global extent
world <- ne_countries(scale = 10, returnclass = "sp")

# make it global
bio1 <- rast('bioclim/bio1.tif')
bio1 <- aggregate(bio1, 5)
bio1df <- raster::as.data.frame(bio1, xy = T) %>% 
  mutate(z = 1:nrow(.)) %>% 
  na.omit()
bio1 <- bio1df %>% 
  dplyr::select(x,y,z) %>% 
  raster::rasterFromXYZ(.)

# read in endemesm
  end <- fread("outputs/Palaearctic_Endemism_AsCSV3_wgs84.csv")
  
  gb_end <- end %>% 
    group_by(x,y,z) %>% 
    summarise(WE = sum(Endemism, na.rm = T))

## read these all in and save as single CSV
rich <- fread("outputs/Palaearctic_PA_AsCSV3_wgs84.csv")

gb_rich <- rich %>% 
  group_by(x,y,z) %>% 
  summarise(richness = sum(PA, na.rm = T))

#ggplot() +
#  geom_tile(gb_rich, mapping = aes(x = x, y = y, fill = richness))
#ggplot() +
#  geom_tile(gb_end, mapping = aes(x = x, y = y, fill = WE))

# combine richness with endemism df

lj <- left_join(gb_rich, gb_end, by = "z")

lj <- mutate(lj, CWE = WE/richness)
head(lj)

lj <- lj %>% 
  dplyr::rename(x = x.x, y = y.x) %>% 
  dplyr::select(x, y, z, richness, CWE)

#ggplot() +
#  geom_tile(lj, mapping = aes(x = x, y = y, fill = richness)) +
#  scale_fill_viridis_c(option = "turbo", trans = "log")

fwrite(x = na.omit(lj), 
          file = "outputs/palaearctic_rich_cwe_try5.csv", row.names = F)
