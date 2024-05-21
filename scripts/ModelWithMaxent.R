

library(raster)
library(dismo)
library(readr)

## open the occurrence file and get only xy in a dataframe. 

#folderTag = "Z:"

allFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates", 
                       pattern = ".csv", full.name = TRUE)

i = 1

curFile <- allFiles[i]
ocData <- read_csv(curFile)
ocData <- cbind(ocData[,"decimalLongitude"], ocData[,"decimalLatitude"])

varlist <- list.files(paste0("./GlobalDownloadedData/envData/", gsub(".csv", "", basename(curFile))), pattern = ".asc$", full.names = TRUE)
envLayers <- lapply(varlist, raster)
envStack <- stack(envLayers)
## On Bear
##varlist <- list.files(paste(folderTag, "/elements/narayani/WorldClim/Global_2.5Min/", sep = ""), pattern = ".tif$", full.names = TRUE)

## mod_vars <- rast(varlist)


xx <- maxent(x = envLayers, p=ocData, args = c(
      'maximumbackground=10000',
      'defaultprevalence=1.00',
      'betamultiplier=0.5',
      'pictures=true',
      'randomtestpoints=30',
      'linear=true',
      'quadratic=true',
      'product=true',
      'threshold=true',
      'hinge=true',
      'threads=2',
      'responsecurves=false',
      'jackknife=false',
      'askoverwrite=false'
))



xx <- maxent(x = envLayers, p=ocData, args = c('maximumbackground=10000', 'betamultiplier=0.5', 'randomtestpoints=30',
  'linear=true',
  'quadratic=true',
  'product=true',
  'threshold=true',
  'hinge=true',
  'responsecurves=false',
  'jackknife=false'
))





