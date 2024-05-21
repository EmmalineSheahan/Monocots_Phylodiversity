## Get all the names with missing species at cleaning step. 

library(readr)
## from desktop
folderTag <- "Z:"

## from moose
## folderTag <- "/srv"

setwd(paste0(folderTag, "/mybook/narayani/monocot_pd/WCVP/"))


############ THIS IS FIRST MISSING SPECIES LIST ##############
############ ONE is coordinate clear does not work for occurrences <= 20
############ SECOND is coordinate clear fails even if occurrences > 20

## Datalist
rawSpList <- list.files("./GlobalDownloadedData/spfiles_WCVP_bels/", 
                        pattern = ".csv$", full.names = TRUE)

## CleanedCoordiantes list
cleanSpList <- list.files("./GlobalDownloadedData/cleanedCoordinates/", 
                          pattern = ".csv$", full.names = TRUE)

## Not in cleaned folder, but from downloaded data

rawInClean <- rawSpList[which(basename(rawSpList) %in% basename(cleanSpList))]
rawNotInClean <- rawSpList[-which(basename(rawSpList) %in% basename(cleanSpList))]

## Now read the files from rawNotInClean list and find out how many 
## occurrences in the file. 

OpTbl <- matrix("", ncol = 2, nrow = length(rawNotInClean))

for (i in 1:length(rawNotInClean))
#for (i in 1:5)
{
  
  #print(paste0("Current iteration: ", i, " Total Rows: " , nrow(dt1)))
  dt1 <- read_csv(rawNotInClean[i])
  OpTbl[i,1] = basename(rawNotInClean[i])
  OpTbl[i,2] = nrow(dt1)
  print(paste0("Current iteration: ", i, " Total Rows: " , nrow(dt1)))
  # print(nrow(dt1))
  
}

write.csv(OpTbl, "./GlobalDownloadedData/missingSp/rawNotInClean.csv", row.names = FALSE)
## Get which ones are less than 20

i = which(as.numeric(OpTbl[,2]) <= 20 & as.numeric(OpTbl[,2]) > 0)
## Now save this speceis files to a different folder for ForBuffer. 
i1 = which(as.numeric(OpTbl[,2]) > 20)

lessThan20 <- OpTbl[i,]
moreThan20 <- OpTbl[i1,]

### Save this less than 20 points


for (j in 1:nrow(lessThan20))
{
  if (as.numeric(lessThan20[j,2]) > 0)
  {
    print ("Copying")
    inpFlName <- paste0("./GlobalDownloadedData/spfiles_WCVP_bels/", lessThan20[j,1])
    opFlName <- paste0("./GlobalDownloadedData/missingSp/forBuffer/", lessThan20[j,1])
    file.copy (inpFlName, opFlName, overwrite = TRUE)
  }

  
}


### now take this to find out why cleaned coordinate not worked
for (j in 1:nrow(moreThan20))
{

    print (paste0("Copying : ", j))
    inpFlName <- paste0("./GlobalDownloadedData/spfiles_WCVP_bels/", moreThan20[j,1])
    opFlName <- paste0("./GlobalDownloadedData/missingSp/forCleanAgain/", moreThan20[j,1])
    file.copy (inpFlName, opFlName, overwrite = TRUE)

  
}



############ THIS IS SECOND MISSING SPECIES LIST ##############
############ CoordinateClean Works but alpha hull fails and thus raster files are not clipped. 
############ 

## Datalist
cleanSpList <- list.files("./GlobalDownloadedData/cleanedCoordinates/", 
                        pattern = ".csv$", full.names = TRUE)

## CleanedCoordiantes list
envSpList <- list.dirs("./GlobalDownloadedData/envData/", 
                      full.names = TRUE, recursive = FALSE)

## In cleaned folder, but not in env folder
cleanSpNameList <- gsub(".csv", "", basename(cleanSpList))

  
cleanInEnv <- cleanSpNameList[which(basename(cleanSpNameList) %in% basename(envSpList))]
cleanNotInEnv <- cleanSpNameList[- which(basename(cleanSpNameList) %in% basename(envSpList))]
## Check whether the shape file for cleanNotInEnv list is available in 
## "./GlobalDownloadedData/accessibleArea/"
shpFiles <- list.files("./GlobalDownloadedData/accessibleArea/", 
                       full.names = TRUE, pattern = ".shp$")

shpSpName <- gsub(".shp", "", basename(shpFiles))

shpFoundNotClipped <- cleanNotInEnv[which(cleanNotInEnv %in% shpSpName)]
shpNotFound <- cleanNotInEnv[-which(cleanNotInEnv %in% shpSpName)]


## Now read the files from rawNotInClean list and find out how many 
## occurrences in the file. 

OpTbl <- matrix("", ncol = 2, nrow = length(rawNotInClean))

for (i in 1:length(rawNotInClean))
  #for (i in 1:5)
{
  
  #print(paste0("Current iteration: ", i, " Total Rows: " , nrow(dt1)))
  dt1 <- read_csv(rawNotInClean[i])
  OpTbl[i,1] = basename(rawNotInClean[i])
  OpTbl[i,2] = nrow(dt1)
  print(paste0("Current iteration: ", i, " Total Rows: " , nrow(dt1)))
  # print(nrow(dt1))
  
}

write.csv(OpTbl, "./GlobalDownloadedData/missingSp/rawNotInClean.csv", row.names = FALSE)



