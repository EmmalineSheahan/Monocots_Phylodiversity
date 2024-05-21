## This function will download the missing data from gbif. John had given data through gazzetter
## but some of the data was missing. (60% of the accepted names data was missing.) When checked 
## on gbif website there was data for those species. So now downloading the data for the same. 
## In fact, this should have been from the start, we would have saved so much time. 



library(readr)
## If the script is running from desktop then, 
FolderTag = "Z:"

## If the script is running from moose then, 
#FolderTag = "/srv"


DownloadMissingData <- function()
{

  FolderTag = "Z:"
  
  RefData = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
  AccNames = RefData[which(RefData$TaxonStatus == "Accepted"),"SpeciesName"]
  AvaNames = gsub("_", " ", gsub(".csv", "", list.files("./DownloadedData/spfiles_WCVP/", pattern = ".csv$")))
  Found = which(AccNames %in% AvaNames)
  NotFoundNames = unique(as.character(AccNames[-Found]))
  
  
  NotFoundId = as.matrix(RefData[which(RefData$TaxonStatus == "Accepted"), "SpeciesId"][-Found])
  
  
  ## FoundNames = unique(as.character(AccNames[Found]))
  
  dt1 = merge(RefData, NotFoundId, by.x = "SpeciesId", by.y = "V1")

  
  gbif_fields = c("gbifID", "basisOfRecord", "catalogNumber", "collectionCode", "collectionID", "continent", "countryCode", "stateProvince",   "county", "municipality", "locality", "verbatimLocality" , "locationRemarks", "year", "month", "day", "decimalLatitude", "decimalLongitude",
                  "scientificName", "class", "order", "family", "genus", "specificEpithet", "infraspecificEpithet", "taxonomicStatus")  

  OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/DownloadedData/", sep = "")
  for (curid in ids[1:5])
  {
    
    curdt = dt1[which(dt1$SpeciesId == curid),]
    totalSpData = NULL
    AcceptedName = as.character(curdt[which(curdt$TaxonStatus =="Accepted"),"SpeciesName"])
    OpFileName = paste(OccFolder, "spfiles_WCVP/", gsub(" ", "_", AcceptedName), ".csv", sep= "")
    for (i in 1:nrow(curdt))
    {

      spname = as.character(curdt[i,"SpeciesName"])
      mytaxkey <- name_backbone(as.character(spname))$usageKey
      speciesData = occ_search(taxonKey=mytaxkey, fields='all')$data
      ## speciesData = speciesData[, gbif_fields]
      totalSpData = rbind(totalSpData, speciesData)
     
    }
  }    

}
    
    
    
    
    
      
    
#### This is the second function

    
    ## Now run the loop on this and get id and then match to taxonid on gbif
    
    ids = unique(dt1[, "SpeciesId"])
    user = "narayanibarve"
    pwd = "Narayani_128"
    email = "narayani.ku@gmail.com"
    ## taxkey = NULL
    
    
    
      
    gbif_download_key = occ_download(
      pred("taxonKey", mytaxkey), # insert taxon key for the taxon interested in
      format = "SIMPLE_CSV",
      user=user,pwd=pwd,email=email)
  
    
    occ_download_wait(gbif_download_key)  
    
    data_download <- occ_download_get(gbif_download_key, overwrite= TRUE) %>%
      occ_download_import()    
    
    
    gbif_download_key = occ_download(
      pred("taxonKey", famKey), # insert taxon key for the taxon interested in
      format = "SIMPLE_CSV",
      user=user,pwd=pwd,email=email
    )

    gbif_download_key = occ_download(
      pred("taxonKey", famKey), # insert taxon key for the taxon interested in
      format = "SIMPLE_CSV",
      user=user,pwd=pwd,email=email
    )
    
    
    # Specify your GBIF data downlod user details above
    
    #This download will take some time to finish but you can check its status with this command.
    occ_download_wait(gbif_download_key)  
    
      
    
    
 

  
  
  
  
  famKey <- name_backbone("Pieridae")$usageKey
  
  # List GADM_GID for each state or country we need data for
  GADM_ids <- c("USA.10_1","USA.11_1") # Florida and Georgia
  GADM_ids <- c("BRB","BHS", "BLZ", "BRA","COL", "CRI", "CUB", "DMA", "DOM",
                "GUF", "GRD", "GLP", "GTM", "GUY", "HTI", "HND", "JAM", "MTQ",
                "MEX", "SUR", "NLD", "NIC", "PAN", "PRI", "LCA", "TTO", "USA",
                "VCT", "VEN")
  # Perform a download for your desired taxon and retrieve a download key.
  # You can set download filter parameters using pred_in and pred functions
  gbif_download_key = occ_download(
    pred("taxonKey", famKey), # insert taxon key for the taxon interested in
    #pred_in("basisOfRecord",c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION')),
    pred_in("gadm",GADM_ids),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user=user,pwd=pwd,email=email
  )
  # Specify your GBIF data downlod user details above
  
  #This download will take some time to finish but you can check its status with this command.
  occ_download_wait(gbif_download_key)
  
  
  # Once the download has finished, read your data into R.
  data_download <- occ_download_get(gbif_download_key, overwrite= TRUE) %>%
    occ_download_import()
  
  # Use the data as you wish
  View(data_download)
  res <- occ_download_meta(gbif_download_key)
  gbif_citation(res)
  
  
  
  
  
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
  
  
  
  
  
  
  ## This data is where all the linkages of monocots names are stored. 
  #RefData = read.delim("/srv/gspeedq32/narayani/monocot_pd/RefPlantNames/classification.txt")
  ## From moose
  #RefData = read.delim("/srv/mybook/narayani/monocot_pd/Monocots_AcceptedName_linkages.csv", sep = ",")
  
  ## From desktop
  RefData = read.delim(paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/MonoNames_WCVP.csv", sep = ""), sep = ",")
  
  
  ## Acc_Name and syn_binom are both canonical names.  so no need of again this . 
  #RefData$refcanonical = paste(RefData[, "genus"], RefData[, "specificEpithet"], sep = " ")
  
  ## This folder stores the data for idigbio and gbif as well in seperate folder. This was old data
  # OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/georef/datafromjohn/", sep = "")
  
  ## This folder stores the data for idigbio and gbif as well in seperate folder. 
  ## This was  data March 2022
  
  OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/DownloadedData/", sep = "")
  
  DataFolders = c("gbif", "idigbio", "sernec")
  ## For trial
  # DataFolder = DataFolders[1]
  
  
  for (DataFolder in DataFolders)
  {
    FileFolder = paste(OccFolder, DataFolder, sep = "")
    FilesList = list.files(FileFolder, pattern = ".csv$", full.names = TRUE )
    ## for trial
    #filename = FilesList [1]
    
    print(paste("Current DataFolder is ", DataFolder, sep = ""))
    for (filename in FilesList)
    {
      curdata = read_csv(filename)
      #curdata = readRDS(paste(FileFolder, filename, sep = ""))
      ## Attach the scientific name so that easier to match with reference data
      
      if (DataFolder == "idigbio")
      {
        names(curdata)[1] = "gbifid"
      }
      
      ## remove 3 fields and add one more field, change the id to gbifid
      if (DataFolder == "sernec")
      {
        names(curdata)[1] = "gbifid"
        tempdt1 = curdata[, c(1:26,30:38)]
        bels_coordinates_score = 0             
        tempdt2 = curdata[,c(39:42)]
        curdata = cbind(tempdt1, bels_coordinates_score, tempdt2)
        rm(tempdt1, tempdt2)
      }
      
      
      print(paste("Current file name is ", basename(filename)))
      curdata$canonical = paste(as.character(curdata$genus), as.character(curdata$specificEpithet), sep = " ")
      
      ## Match the reference botanical name and botanical name in the current data file. 
      ## based on this matching, get the taxon_id
      curdata$SpeciesId = RefData$SpeciesId[match(curdata$canonical, RefData$SpeciesName)]
      nodata = which(is.na(curdata$SpeciesId) == TRUE)
      if (length(nodata) > 0 )
      {curdata1 = curdata[-nodata,]}
      
      
      ## For each wfoid run the loop and collect the data. 
      unqtaxonid = as.character(unique(curdata1$SpeciesId))
      ## For trial 
      #unqtaxonid1 = unqtaxonid
      #unqtaxonid = unqtaxonid[1:15]
      
      #taxonid = unqtaxonid[1]
      
      print(paste("Total taxon id for file ", filename, " are ", 
                  length(unqtaxonid), sep = ""))
      iteration = 1
      for (taxonid in unqtaxonid)
      {
        print(paste("Current iteration ", iteration), sep = "")
        dataid = which(curdata1$SpeciesId == taxonid)
        spdata = curdata1[dataid, ]
        print(nrow(spdata))
        spname = as.character(RefData[which(RefData$SpeciesId == taxonid & 
                                              as.character(RefData$TaxonStatus) == "Accepted"), "SpeciesName"])
        
        OpFileName = paste(OccFolder, "spfiles_WCVP/", gsub(" ", "_", spname), ".csv", sep= "")
        
        ## Check whether the file is already saved or not. 
        if (file.exists(OpFileName))
        {
          
          write.table(spdata, OpFileName, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)    
          # print("This file exists. ")
          
        } else 
        { 
          
          write.table(spdata, OpFileName, sep = ",", col.names = TRUE, row.names = FALSE) 
        }
        ## Remove the dataid from the curdata1 file. 
        ##if (length(dataid) > 0 )
        ##{
        ##   curdata1 <- curdata1[-dataid, ]
        ##}
        
        iteration = iteration + 1
        
        
      }
      
      
    } # For filename in FileList
    
    
  } ## For DataFolders
  
  
  
  
} ## End of the function


## Input to this function is the Monocot Accepted and synoyms linkage file based on WCVP name
## The output will be Name, TaxonRank and WCVP_id (This wcvp id is given by me which is 
## sequential number)
## OpFile will save the data. This data will be use above so that we can directly match
## the names and assign WCVP id to pull the data from individual occurrence files. 

## If on Desktop
FolderTag = "Z:/"

## If on moose
## FolderTag <- "/srv/"

MonSynFile <- paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MasterData/Mono_Acc_Syn_WCVP.csv", sep = "")
OpFile <- paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/MonoNames_WCVP.csv", sep = "")
NameTable <- function(MonSynFile, OpFile)
{
  
  MonSynData <- read.delim(MonSynFile, sep = ",")  
  UnqAccNames <- unique(Mono_Acc_Syn_WCVP$Acc_Name)
  OpTbl <- matrix(0, nrow = 0, ncol = 3)
  AccId <- 1
  
  for (name1 in UnqAccNames)
  {
    CurData <- MonSynData[which(MonSynData$Acc_Name == name1) ,] 
    AccRow <- c(name1, "Accepted", AccId)
    #OpTbl <- rbind(OpTbl, AccRow)
    if (nrow(CurData) > 0)
    {
      SynData = unique(as.character(CurData$syn_binom))
      SynData = cbind(SynData, "Synonym", AccId)
      
    }
    OpTbl <- rbind(OpTbl, AccRow, SynData)
    
    AccId <- AccId + 1
    
    print(paste("Current id is ", AccId, sep = ""))
  }
  
  OpTbl = data.frame(OpTbl)
  names(OpTbl) = c("SpeciesName", "TaxonStatus", "SpeciesId")
  write.csv(OpTbl, OpFile, row.names = FALSE)
  return(OpTbl)
  
}
