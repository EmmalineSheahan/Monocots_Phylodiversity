
## https://www.r-bloggers.com/2018/02/installing-rjava-on-ubuntu/
## R version 4.2.1
## on the shell prompt. 
## sudo R CMD javareconf -e


# install.packages("rnaturalearth")
# install.packages("rgeos")
# install.packages("dismo")
# install.packages("ENMeval")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("sf")
# install.packages("terra")
# install.packages("readr")
# install.packages("rJava")
# install.packages("rnaturalearthdata")
# install.packages("rangeBuilder")
# install.packages("usdm")

## changing the java heap space
options(java.parameters = "-Xmx8g")


library(rnaturalearth)
library(rgeos)
library(dismo)
library(ENMeval)
library(dplyr)
library(stringr)
library(sf)
library(terra)
library(readr)
library(dplyr)
library(rJava)
library(ecospat)



## install_github("jamiemkass/ENMeval") 
## Windows desktop
##setwd("Z:/mybook/narayani/monocot_pd/WCVP/")

## from Moose server
 setwd("/srv/mybook/narayani/monocot_pd/WCVP/")

## from Bear server
## setwd("/srv/elements/narayani/monocot_pd/WCVP/")


source("./script/02_defineAccessibleArea.R")
source("./script/03_clipModelLayers.R")
source("./script/04_selectModelVariables.R")
source("./script/05_save_sdmOutputsTSS.R")



folderTag <- "/srv"

FromCnt <- 2307
ToCnt <- 2400
saveFlag <- FALSE


## This is just for checking when program stops. 
allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates/", 
                       pattern = ".csv", full.name = TRUE)

## RunAllFiles (folderTag, FromCnt, ToCnt, saveFlag)


RunAllFiles <- function(folderTag, fromCount, toCount, saveFlag)
{
  study_proj <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs" #mollweide projection
  geog_proj <- "+proj=longlat +ellps=WGS84 +no_defs" ## geographic projection
  #folderTag <- "Z:"
  
  #allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates_prototype/", 
  #                       pattern = ".csv", full.name = TRUE)

  allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates/", 
                         pattern = ".csv", full.name = TRUE)

  ## choose only occurrence files which have clipped env data 
  #allFiles <- paste0("./GlobalDownloadedData/cleanedCoordinates/",
  #            list.dirs("./GlobalDownloadedData/envData/", full.names = FALSE, recursive = FALSE),
  #            ".csv")
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  ### 3E. Choose projection and project data if necessary
  
  world <- st_transform(world, crs = study_proj)
  # 
  # ## On Moose
  # varlist <- list.files(paste(folderTag, "/mybook/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
  # ## On Bear
  # ## varlist <- list.files(paste(folderTag, "/elements/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
  # mod_vars <- rast(varlist)
  # 
  # 
  # #mod_vars <- terra::project(mod_vars, study_proj)
  # mod_vars <- terra::project(mod_vars, geog_proj)
  # mod_vars2 <- mod_vars
  
  for (i in fromCount:toCount)
  #for (curFile  in allFiles)
      
  {
    curFile <- allFiles[i] 
    curData <- read_csv(curFile)
    ## Take only cleaned records
    curData <- curData[which(curData$.summary == TRUE), c("genus", "species", "decimalLatitude", "decimalLongitude")]
    
    spName <- gsub(".csv", "", basename(curFile))
    print(paste0("Current count : ", i))
    envFolder <- paste0("./GlobalDownloadedData/envData/", 
                        gsub(".csv", "", basename(curFile)), "/")
    if (!dir.exists(envFolder))
    {
      
      ## If envdir does not exists then only read these files and then clip 
      ## On Moose
      varlist <- list.files(paste(folderTag, "/mybook/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
      ## On Bear
      ## varlist <- list.files(paste(folderTag, "/elements/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
      mod_vars <- rast(varlist)
      
      
      #mod_vars <- terra::project(mod_vars, study_proj)
      mod_vars <- terra::project(mod_vars, geog_proj)
      mod_vars2 <- mod_vars
      
      ## Till here if global vars is to be read. 
      
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
    } else  ### envfolder is available so just read the envfiles. 
    {
      print("EnvFolder exists ")
      mod_vars = lapply(list.files(envFolder, pattern = ".asc$", full.names = TRUE) , raster)
      
    }   ## if envFolder is not available. 
    
    ##Here run the model
    spp_df <- curData
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    
    ## Generate background points
    #print("Generating background points..")
    #print(mod_vars[[1]])
    bgr1 <- rasterToPoints(mod_vars[[1]])
    names(bgr1)[1:2] <- c("decimalLongitude", "decimalLatitude")
    
    bgpoints <- 10000  
    if (nrow(bgr1) < 10000)
    {
      bgpoints <- ceiling(nrow(bgr1) * 0.8)
      
    }
    print(paste0("background points number ", bgpoints))
    
    bgpt <- bgr1[sample(1:nrow(bgr1), bgpoints), c(1:2)]
    bgpt <- data.frame(bgpt)
    names(bgpt) <-  c("decimalLongitude", "decimalLatitude")
    #print(head(bgpt))
    print("     Running Maxent test model")
    #max_model <- maxent(x = stack(mod_vars), p = coordinates(spp_df), progress = "text") 
    #max_model <- maxent(stack(mod_vars), p = coordinates(spp_df), progress = "text") 
    ## Added this a = bgpt, for the error in randomPoints in maxent function. 
    max_model <- maxent(stack(mod_vars), a = bgpt, p = coordinates(spp_df), progress = "text") 
        
    ## Using the test model, iteratively test the colinearity of variables, removing highly colinear ones one at a time 
    print("Selecting top SDM variables")
    predictors <- select_sdmVariables(pred_vars = stack(mod_vars), maxent_mod = max_model, maxVIF = 5)
    
    print("Evaluating tuning variables in model")
    eval1 <- ENMeval::ENMevaluate(occ = coordinates(spp_df), env = predictors, bg = bgpt,
                                  method = "block", RMvalues = c(0.5, 1, 2, 3, 4),
                                  fc= c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                                  parallel = TRUE, numCores = 10, algorithm = 'maxent.jar')
    
    ### 4A. Prepare the output path and coordinates
    
    
    resultDir = paste0("./GlobalDownloadedData/resultDir/", spName)
  
    if (!dir.exists(resultDir)){
      dir.create(resultDir)
    }
    # Return coordinates to a data frame
    spp_df2 <- as.data.frame(spp_df) 
    
    print("Saving best model")
    save_SDM_results(ENMeval_output = eval1, AUCmin = 0.7, resultDir = resultDir,
                     spp = spName, occ_df = spp_df2)
    
    
    ### 4C. Visualize the model
    ## Load in the rasters
    
    ## This is giving error Do this later. 
    ## r <- raster(paste0(resultDir, "/", spName,"_SDM.tif"))
    ## r_pa <- raster(paste0(resultDir, "/", spName, "_SDM_PA.tif"))
  
    ## fig <- create_sdmFigure(spp = spName, r = r, r_pa = r_pa, 
    ##                        occ_df = spp_df2, 
    ##                        world = world)  
      
    ##mod_vars <- mod_vars2
    gc()
  }
} ## End of function 


## RunAllFiles (folderTag, FromCnt, ToCnt, saveFlag)



##in 1
# 3401 to 3600 
## Current 3511
## Envforlder is missing from 3469 to 3505

##in 2
##3201 to 3400
## Current 3286
## in3
## 3601 - 3800
## Current 3760

RunAllFiles_CLIP_BEFORE_RUNNING <- function(folderTag, fromCount, toCount, saveFlag)
{
  study_proj <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs" #mollweide projection
  geog_proj <- "+proj=longlat +ellps=WGS84 +no_defs" ## geographic projection
  #folderTag <- "Z:"
  
  #allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates_prototype/", 
  #                       pattern = ".csv", full.name = TRUE)
  
  allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates/", 
                         pattern = ".csv", full.name = TRUE)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  ### 3E. Choose projection and project data if necessary
  
  world <- st_transform(world, crs = study_proj)
  
  
  varlist <- list.files(paste(folderTag, "/mybook/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)
  mod_vars <- rast(varlist)
  
  
  #mod_vars <- terra::project(mod_vars, study_proj)
  mod_vars <- terra::project(mod_vars, geog_proj)
  mod_vars2 <- mod_vars
  
  for (i in fromCount:toCount)
    #for (curFile  in allFiles)
    
  {
    curFile <- allFiles[i] 
    curData <- read_csv(curFile)
    ## Take only cleaned records
    curData <- curData[which(curData$.summary == TRUE), c("genus", "species", "decimalLatitude", "decimalLongitude")]
    
    spName <- gsub(".csv", "", basename(curFile))
    
    envFolder <- paste0("./GlobalDownloadedData/envData/", 
                        gsub(".csv", "", basename(curFile)), "/")
    if (!dir.exists(envFolder))
    {
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
    } ## if envFolder is not available. 
    
    ##Here run the model
    spp_df <- curData
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    print("     Running Maxent test model")
    #max_model <- maxent(x = stack(mod_vars), p = coordinates(spp_df), progress = "text") 
    max_model <- maxent(stack(mod_vars), p = coordinates(spp_df), progress = "text") 
    
    ## Using the test model, iteratively test the colinearity of variables, removing highly colinear ones one at a time 
    print("Selecting top SDM variables")
    predictors <- select_sdmVariables(pred_vars = stack(mod_vars), maxent_mod = max_model, maxVIF = 5)
    
    
    
    print("Evaluating tuning variables in model")
    eval1 <- ENMeval::ENMevaluate(occ = coordinates(spp_df), env = predictors,
                                  method = "block", RMvalues = c(0.5, 1, 2, 3, 4),
                                  fc= c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                                  parallel = TRUE, numCores = 2, algorithm = 'maxent.jar')
    
    ### 4A. Prepare the output path and coordinates
    
    
    resultDir = paste0("./GlobalDownloadedData/resultDir/", spName)
    
    if (!dir.exists(resultDir)){
      dir.create(resultDir)
    }
    # Return coordinates to a data frame
    spp_df2 <- as.data.frame(spp_df) 
    
    print("Saving best model")
    save_SDM_results(ENMeval_output = eval1, AUCmin = 0.7, resultDir = resultDir,
                     spp = spName, occ_df = spp_df2)
    
    
    ### 4C. Visualize the model
    ## Load in the rasters
    
    ## This is giving error Do this later. 
    ## r <- raster(paste0(resultDir, "/", spName,"_SDM.tif"))
    ## r_pa <- raster(paste0(resultDir, "/", spName, "_SDM_PA.tif"))
    
    ## fig <- create_sdmFigure(spp = spName, r = r, r_pa = r_pa, 
    ##                        occ_df = spp_df2, 
    ##                        world = world)  
    
    mod_vars <- mod_vars2
    gc()
  }
} ## End of function 





