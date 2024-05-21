### This function is to generate species wise list from multiple files of idigbio plant files 
### based on taxonomically sorted names from wfo. 


## Read each occurrence file. Match the names from that file with that of the taxanomic file. 
## add the wfo id as a last field where names matched. and then save the file in a folder
## after all the files are done, then merge them together and seperate the species files if needed
## if can maniipu;late it in a single file, then also no problem. 

library(readr)
## If the script is running from desktop then, 
FolderTag = "Z:"

## If the script is running from moose then, 
#FolderTag = "/srv"


MatchDataNames <- function()
{


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
	## OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/DownloadedData/", sep = "")
	## DataFolders = c("gbif", "idigbio", "sernec")
	
	## This data was June 2022 for gbif
	OccFolder = paste(FolderTag, "/mybook/narayani/monocot_pd/WCVP/GlobalDownloadedData/", sep = "")
	DataFolders = c("gbif/csv")

	## This data was June 2022 for idigbio
	DataFolders = c("idigbio/csv")
	
	
		## For trial
	# DataFolder = DataFolders[1]
	fields_gbif = c("gbifID", "occurrenceID", "phylum", "class", "order", "family", "genus", "species",
	       "infraspecificEpithet", "scientificName", "countryCode", "locality", "stateProvince", 
	       "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "eventDate", 
	       "day", "month", "year", "basisOfRecord")
	
	fields_idig = c("coreid",  "dwc:occurrenceID","dwc:phylum", "dwc:class",  "dwc:order","dwc:family" , "dwc:genus",
	"dwc:specificEpithet","dwc:infraspecificEpithet", "dwc:scientificName", "dwc:countryCode", "dwc:locality",
	"dwc:stateProvince","dwc:decimalLatitude", "dwc:decimalLongitude", "dwc:coordinateUncertaintyInMeters",
	"dwc:eventDate", "dwc:day", "dwc:month","dwc:year", "dwc:basisOfRecord")
	
	for (DataFolder in DataFolders)
	{
		FileFolder = paste(OccFolder, DataFolder, sep = "")
		FilesList = list.files(FileFolder, pattern = ".csv$", full.names = TRUE )
		## for trial
		#print(paste("Current DataFolder is ", DataFolder, sep = ""))
		
		print(paste("Current DataFolder is ", DataFolder, sep = ""))
		for (filename in FilesList)
		{
			# curdata = read_csv(filename)

			#curdata = readRDS(paste(FileFolder, filename, sep = ""))
			## Attach the scientific name so that easier to match with reference data
			
			if (DataFolder == "gbif/csv")
			{
			  curdata <- read_delim(filename, "\t", escape_double = FALSE, trim_ws = TRUE)
			  curdata = curdata[, fields_gbif]
			}
			
			if (DataFolder == "idigbio/csv")
			{
			  #curdata <- read_delim(filename, ",", escape_double = FALSE, trim_ws = TRUE)
			  #curdata <- read_csv(filename, escape_double = FALSE, trim_ws = TRUE)
			  curdata <- read_delim(filename, delim = ",", quote = "\"", escape_double = TRUE, trim_ws = TRUE)
			  
			  curdata <- curdata[, fields_idig]
			  
			  names(curdata)[1] = "gbifid"
			  idNAs = which(is.na(curdata[,"dwc:genus"]))
			  if (length(idNAs) >0)
			  {
			    curdata <- curdata[-idNAs,]
			  }
			  #curdata <- curdata[, fields_idig]
			  species = paste(curdata[,"dwc:genus"][[1]], curdata[,"dwc:specificEpithet"][[1]], sep =" ")
			  curdata1 = curdata[, 1:7]
			  curdata2 = curdata[, 9:21]
			  curdata = cbind(curdata1, species, curdata2)

			  names(curdata) <- fields_gbif
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
			## curdata$canonical = paste(as.character(curdata$genus), as.character(curdata$specificEpithet), sep = " ")
			
			curdata$canonical = curdata$species
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
			#unqtaxonid = unqtaxonid[1:5]
			
			#taxonid = unqtaxonid[1]
			
			print(paste("Total taxon id for file ", filename, " are ", 
			            length(unqtaxonid), sep = ""))
			iteration = 1
			unqtaxonid1 = unqtaxonid
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
			  
			  
			} # for taxonid
			
			
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




### Getting gbif fields for the downloadeddata in June 2022

[1] "gbifID"                           "datasetKey"
[3] "occurrenceID"                     "kingdom"
[5] "phylum"                           "class"
[7] "order"                            "family"
[9] "genus"                            "species"
[11] "infraspecificEpithet"             "taxonRank"
[13] "scientificName"                   "verbatimScientificName"
[15] "verbatimScientificNameAuthorship" "countryCode"
[17] "locality"                         "stateProvince"
[19] "occurrenceStatus"                 "individualCount"
[21] "publishingOrgKey"                 "decimalLatitude"
[23] "decimalLongitude"                 "coordinateUncertaintyInMeters"
[25] "coordinatePrecision"              "elevation"
[27] "elevationAccuracy"                "depth"
[29] "depthAccuracy"                    "eventDate"
[31] "day"                              "month"
[33] "year"                             "taxonKey"
[35] "speciesKey"                       "basisOfRecord"
[37] "institutionCode"                  "collectionCode"
[39] "catalogNumber"                    "recordNumber"
[41] "identifiedBy"                     "dateIdentified"
[43] "license"                          "rightsHolder"
[45] "recordedBy"                       "typeStatus"
[47] "establishmentMeans"               "lastInterpreted"
[49] "mediaType"                        "issue"
>

  
[1] "gbifID"               "basisOfRecord"        "catalogNumber"
[4] "collectionCode"       "collectionID"         "continent"
[7] "countryCode"          "stateProvince"        "county"
[10] "year"                 "month"                "day"
[13] "decimalLatitude"      "decimalLongitude"     "scientificName"
[16] "class"                "order"                "family"
[19] "genus"                "specificEpithet"      "infraspecificEpithet"
[22] "taxonomicStatus"
>
  
