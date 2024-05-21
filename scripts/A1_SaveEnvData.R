

library(rnaturalearth)
library(rgeos)
library(dismo)
#library(ENMeval)
#library(dplyr)
#library(stringr)
library(sf)
library(terra)
library(readr)
#library(dplyr)
#library(rJava)
#library(ecospat)



## install_github("jamiemkass/ENMeval") 
## Windows desktop
##setwd("Z:/mybook/narayani/monocot_pd/WCVP/")

## from server
setwd("/srv/mybook/narayani/monocot_pd/WCVP/")

source("./script/02_defineAccessibleArea.R")
source("./script/03_clipModelLayers.R")
#source("./script/04_selectModelVariables.R")
#source("./script/05_save_sdmOutputsTSS.R")



folderTag <- "/srv"
## folderTag <- "Z:"

FromCnt <- 4245
ToCnt <- length(allFiles)
saveFlag <- TRUE

## RunAllFiles (folderTag, FromCnt, ToCnt, saveFlag)

## SaveEnvFiles (folderTag, FromCnt, ToCnt, saveFlag)

allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates", 
                       pattern = ".csv", full.name = TRUE)


SaveEnvFiles <- function(folderTag, fromCount, toCount, saveFlag)
{
  study_proj <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs" #mollweide projection
  geog_proj <- "+proj=longlat +ellps=WGS84 +no_defs" ## geographic projection
  #folderTag <- "Z:"
  
  allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates", 
                         pattern = ".csv", full.name = TRUE)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  ### 3E. Choose projection and project data if necessary
  
  world <- st_transform(world, crs = study_proj)
  
  ## On Moose
  varlist <- list.files(paste(folderTag, "/mybook/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
  ## On Bear
  ##varlist <- list.files(paste(folderTag, "/elements/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
  
  mod_vars <- rast(varlist)
  
  
  #mod_vars <- terra::project(mod_vars, study_proj)
  mod_vars <- terra::project(mod_vars, geog_proj)
  mod_vars2 <- mod_vars
  
  for (i in fromCount:toCount)
    #for (curFile  in allFiles)
    
  {
    print(paste0("Currnet count is ", i))
    curFile <- allFiles[i] 
    curData <- read_csv(curFile)
    ## Take only cleaned records
    curData <- curData[which(curData$.summary == TRUE), c("genus", "species", "decimalLatitude", "decimalLongitude")]
    
    spName <- gsub(".csv", "", basename(curFile))
    print(paste("Current Species ", spName, sep = ":"))
    #aa_shp <- define_accessibleArea(species_df = curData, minBuff = 75000,
    #                                buff_prop = 0.80, projCRS = study_proj)
    
    aa_shp <- define_accessibleArea(species_df = curData, minBuff = 75000,
                                    buff_prop = 0.80, projCRS = geog_proj)
    #plot(aa_shp)
    
    shpName <- paste0("./GlobalDownloadedData/accessibleArea/", spName, ".shp")
    st_write(st_as_sf(aa_shp), shpName, delete_layer = TRUE)
    
    ## Clip variables 
    # mod_vars2 <- mod_vars
    mod_vars <- clip_variableLayers(rstack = mod_vars, accessibleArea = aa_shp)
    
    if (saveFlag == TRUE)
    {
      ## Here save them in a folder
      envFolder <- paste0("./GlobalDownloadedData/envData/", 
                          gsub(".csv", "", basename(curFile)), "/")
      
      n1 <- names(mod_vars)
      x2 <- t(sapply(strsplit(n1, "_"), tail, 2))
      flNames <- paste0(envFolder, x2[,1], x2[,2], ".asc")
      
      if (!dir.exists(envFolder)){
        dir.create(envFolder)
      }
      
      ## Save the data in this folder
      for (i in 1:length(flNames))
      {
        
        writeRaster(mod_vars[[i]], flNames[i], overwrite=TRUE)
        print(i)
        
      }
      
    } ## if saveFlag is true
    mod_vars <- mod_vars2
    
  }
  
}

# SaveEnvFiles (folderTag, FromCnt, ToCnt, saveFlag)



