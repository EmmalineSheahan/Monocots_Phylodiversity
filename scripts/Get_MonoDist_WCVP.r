## In this function get only the names of monocots and save it in different file. 



folderTag = "Z:/"
folderTag = "/srv/"

## setwd("Z:/mybook/narayani/monocot_pd/WCVP/GlobalDownloadedData/")
setwd(paste(folderTag, "mybook/narayani/monocot_pd/WCVP/", sep =""))




## Matching countries. 

wcvp_distribution <- read_delim("./MasterData/wcvpnames/wcvp_distribution.txt",
                                "|", escape_double = FALSE, trim_ws = TRUE)


wcvp_names <- read_delim("./MasterData/wcvpnames/wcvp_names.txt",
                         "|", escape_double = FALSE, trim_ws = TRUE)


l1 <- list.files("./GlobalDownloadedData/cleanedCoordinates/", pattern = ".csv$", full.names =  TRUE)

## From raw files. Then apply cleancoordinates from coordinatecleaner

l1 <- list.files("./GlobalDownloadedData/spfiles_WCVP_bels/", pattern = ".csv$", full.names =  TRUE)

monoNameTbl <- NULL
monoDistTbl <- NULL

for (i in 1:length(l1))
#for (i in 1:10)  
{
  print(paste("Current iteration", i, sep = ":"))
  ## Read the file name. 
  curName <- gsub(".csv", "", basename(l1[i]))
  print(paste("This is current name ", curName, sep = ":"))
  curGenus <- strsplit(curName, "_")[[1]][1]
  curSpecies <- strsplit(curName, "_")[[1]][2]
  wcvp_plant_id <- as.character(wcvp_names[which(wcvp_names$genus == curGenus & wcvp_names$species == curSpecies
                                                 & wcvp_names$taxon_status == "Accepted"), "plant_name_id"])

  wcvp_taxon_status <- as.character(wcvp_names[which(wcvp_names$genus == curGenus & wcvp_names$species == curSpecies
                                                     & wcvp_names$taxon_status == "Accepted"), "taxon_status"])
  if (wcvp_taxon_status == "Accepted")
  {
    print("coming here")
    nameTbl <- wcvp_names[which(wcvp_names$accepted_plant_name_id == wcvp_plant_id), ]
    
    distTbl <- merge(wcvp_distribution, nameTbl, by = "plant_name_id")
    
    monoNameTbl <- rbind(monoNameTbl, nameTbl)
    monoDistTbl <- rbind(monoDistTbl, distTbl)
  } else
  {
    print(curName)
    print("This is not Accepted name")
    
  }
}



# monoDistNative <- monoDist[which(monoDist$introduced == 0), ]
monoDistNative <- monoDistTbl[which(monoDistTbl$introduced == 0), ]


write.csv(monoNameTbl, "./MasterData/monoNames_WCVP.csv", row.names = FALSE)
write.csv(monoDistTbl, "./MasterData/monoDist_WCVP.csv", row.names = FALSE)
write.csv(monoDistNative, "./MasterData/monoDistNative_WCVP.csv", row.names = FALSE)
 
monoNameTbl_d <- monoNameTbl[, monoNameFields]
monoDistTbl_d <- monoDistTbl[, monoDistFields]
monoDistNative_d <- monoDistNative[, monoDistFields]

write_delim(monoNameTbl_d, "./MasterData/monoNames_WCVP_d.csv",
            delim = "|", escape = "none", quote = "none")
write_delim(monoDistTbl_d, "./MasterData/monoDist_WCVP_d.csv",
          delim = "|", escape = "none", quote = "none")
write_delim(monoDistNative_d, "./MasterData/monoDistNative_WCVP_d.csv",
          delim = "|", escape = "none", quote = "none")

monoNameFields <- c("plant_name_id", "taxon_status", "family","genus",
                    "species", "taxon_name", "accepted_plant_name_id")

monoDistFields <- c("plant_name_id", "continent_code_l1","continent",
                    "region_code_l2", "region", "area_code_l3", "area",
                    "introduced", "extinct", "location_doubtful", "taxon_status",
                    "family", "genus", "species", "geographic_area", "taxon_name")

# write_csv(
#   x,
#   file,
#   na = "NA",
#   append = FALSE,
#   col_names = !append,
#   quote = c("needed", "all", "none"),
#   escape = c("double", "backslash", "none"),
#   eol = "\n",
#   num_threads = readr_threads(),
#   progress = show_progress(),
#   path = deprecated(),
#   quote_escape = deprecated()
# )
# 



## Trying to intersect region with the points to get only native
## occurrences. 
library(sf)


level3 <- read_sf("./MasterData/shpfiles/level3.shp")


curName <- basename(l1[1])

dt1 <- read.csv("./GlobalDownloadedData/cleanedCoordinates/Achnatherum_bromoides.csv")

dt1_4326 <- st_as_sf(dt1, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326, remove = F)

tt1 <- monoDistTbl[which(monoDistTbl$accepted_plant_name_id == unique(monoNameTbl$accepted_plant_name_id)[1]),]

level3_dt1 <- st_join(dt1_4326, level3, join = st_within)
dt1_within <- merge(level3_dt1, tt1, by.x ="LEVEL3_COD", by.y = "area_code_l3")

