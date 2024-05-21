### Refer to these 
### https://www.rdocumentation.org/packages/rgbif/versions/3.7.1/topics/occ_search
### https://www.rdocumentation.org/packages/rgbif/versions/3.7.1/topics/occ_count

## check the  occurrences of randomly selected monocot species on gbif 


library(readr)
library(rgbif)
## If the script is running from desktop then, 
FolderTag <- "Z:"

## If the script is running from moose then, 
#FolderTag = "/srv"


## From desktop
#RefData <- read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/Monocots_AcceptedName_linkages.csv", sep = ""), sep = ",")
RefData = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
#RefData$refcanonical = paste(RefData[, "genus"], RefData[, "specificEpithet"], sep = " ")

## Get the list of taxonID which is the unique list of wfo id's and this will have
## the synonyms too. 

## Once we have these names search these names on gbif and find out the total records. 
unqwfo <- unique(RefData$SpeciesId)

random_wfo <- sample(1:length(unqwfo), 50, replace = FALSE)
OpTbl = matrix(0, nrow=0, ncol = 4)
for (i in 1:length(random_wfo))
#for (i in 1:5)
{
  Curwfo <- as.character(random_wfo[i])
  CurSpName <- as.character(RefData[which(RefData$TaxonStatus == "Accepted" & RefData$SpeciesId == Curwfo),
                       c("SpeciesName")])
  curdt <- RefData[which(RefData$SpeciesId == Curwfo), ]
  #curdt$canonical <- paste(curdt$genus, " ", curdt$specificEpithet, sep = "")
  TotCount <- 0 
  for (j in 1:nrow(curdt))
  {
    

    ## By Vijay 
    ## mytaxkey <- name_backbone(as.character(curdt$SpeciesName[j]))$usageKey  
    ## occount <- occ_count(taxonKey=mytaxkey, georeferenced = TRUE)
    
    ## done by me. 
    mytaxkey <- name_backbone(as.character(curdt$SpeciesName[j]))$usageKey
    occount <- occ_count(taxonKey=mytaxkey, georeferenced = TRUE)
    
    ##mytaxkey <- name_suggest(q=as.character(curdt$SpeciesName[j]), rank='species')$usageKey 

    #occount_1 <- occ_count(taxonKey=mytaxkey, georeferenced = TRUE)
    #occount_2 <- occ_search(taxonKey=mytaxkey, limit=0)
    
    TotCount <- TotCount + occount  
    print(paste("Count for " , as.character(curdt$SpeciesName[j]), & "  ", occount, sep = ""))
    print(TotCount)
    
  }
  
  ## open the file of the same species and see the count. 
  ## Species fiels are stored here Z:\mybook\narayani\monocot_pd\georef\datafromjohn\spfiles
  SpFileName = paste(FolderTag, "/mybook/narayani/monocot_pd/georef/datafromjohn/spfiles_WCVP/", 
                     gsub(" ", "_", CurSpName), ".csv", sep = "") 
  if (file.exists(SpFileName))
  {
    spdata <- read.csv(SpFileName)
    DownCount = nrow(spdata)
  } else 
  {
    DownCount = 0
  }
  
  NewRow = c(Curwfo, CurSpName, DownCount, TotCount)
  OpTbl = rbind(OpTbl, NewRow)
}


write.csv(OpTbl, "./results/RandomCheckCount.csv", row.names = TRUE)




### Checking difference between names of WFO and WCVP

RefData_WCVP = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
RefData_WFO = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/georef/datafromjohn/results/MonoNames_WFO.csv", sep = ""), sep = ",") 

WCVP_names = RefData_WCVP$SpeciesName
WFO_names = RefData_WFO$Monocots_Names
commoninboth = WCVP_names[which(WCVP_names %in% WFO_names)]
WCVP_NotIn_WFO = WCVP_names[-which(WCVP_names %in% WFO_names)]



### Now here checking the gbif files for "Acis fabrei" This species has 178 occurrences
### Date 25th May 2022
FolderTag = "Z:"

RefData = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
AccNames = RefData[which(RefData$TaxonStatus == "Accepted"),"SpeciesName"]
AvaNames = gsub("_", " ", gsub(".csv", "", list.files("./DownloadedData/spfiles_WCVP/", pattern = ".csv$")))
Found = which(AccNames %in% AvaNames)
NotFoundNames = unique(as.character(AccNames[-Found]))

NotFoundId = RefData[which(RefData$TaxonStatus == "Accepted"), "SpeciesId"][-Found]

FoundNames = unique(as.character(AccNames[Found]))

## For these NotfoundNames open, all gbif files and get the records and store it in output table

OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/DownloadedData/gbif/", sep = "")

## Get all the gbif files in a list and run a loop in it
gbifFiles = list.files(OccFolder, pattern = ".csv$", full.names = TRUE)

## Here check if there are any records for the accepted name in the gbif file. 
Optbl = NULL 
Optbl1 = NULL
for (dtFile in gbifFiles)
{
  print(basename(dtFile))
  dt1 = read_csv(dtFile)
  dt1$canonical = paste(dt1$genus, dt1$specificEpithet, sep = " ")
  nfNamesIndex = which(as.character(dt1$canonical) %in% NotFoundNames)
  fNamesIndex = which(as.character(dt1$canonical) %in% FoundNames)
  if (length(nfNamesIndex) > 0 )
  {
    dt1$Found = 0
    dt1$Found[nfNamesIndex] = 1
    tbl1 <- table(dt1$canonical[which(dt1$Found == 1)])
    SpNames <- names(tbl1)
    SpCount <- as.numeric(tbl1)
    tbl2 <- cbind(SpNames, SpCount , basename(dtFile), NotFoundId[i])
    Optbl <- rbind(Optbl, tbl2)
    
  }
  
}

## Now check the occurrences here for the accepted names. 

for (i in 1:length(NotFoundNames))
{
  print(NotFoundNames[i])
  mytaxkey <- name_backbone(as.character(NotFoundNames[i]))$usageKey
  occount <- occ_count(taxonKey=mytaxkey, georeferenced = TRUE)
  newRow <- c(NotFoundNames[i], occount, "website", NotFoundId[i])
  Optbl1 <- rbind(Optbl1, newRow)
  print(occount)

}
write.table(Optbl1, "./results/DataNotFound.csv", row.names = FALSE)




### Here exploring how much we have gained after downloading the data from gbif in June 2022 
### Date 10th June 2022
FolderTag = "Z:"

RefData = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
AccNames = RefData[which(RefData$TaxonStatus == "Accepted"),"SpeciesName"]
AvaNames = gsub("_", " ", gsub(".csv", "", list.files("./DownloadedData/spfiles_WCVP/", pattern = ".csv$")))
Found = which(AccNames %in% AvaNames)
NotFoundNames = unique(as.character(AccNames[-Found]))

NotFoundId = RefData[which(RefData$TaxonStatus == "Accepted"), "SpeciesId"][-Found]

FoundNames = unique(as.character(AccNames[Found]))


