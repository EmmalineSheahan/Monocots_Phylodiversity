library(readr)

library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(sp)
library(maptools)
library(sf)
library(rnaturalearth)

library(stringr)
library(dbplyr)
library(RSQLite)


folderTag = "Z:/"
# folderTag = "/srv/"

## setwd("Z:/mybook/narayani/monocot_pd/WCVP/GlobalDownloadedData/")
#setwd(paste(folderTag, "mybook/narayani/monocot_pd/WCVP/GlobalDownloadedData/", sep =""))
setwd(paste(folderTag, "mybook/narayani/monocot_pd/WCVP/", sep =""))


## Just get the same names as in prototype file
protoFiles <- list.files("./GlobalDownloadedData/cleanedCoordinates_prototype/", pattern = ".csv$", full.names =  TRUE)
i = which(basename(allFiles) %in% basename(protoFiles))
allFiles <- allFiles[i]



allFiles <- list.files("./GlobalDownloadedData/spfiles_WCVP_bels/", pattern = ".csv$", full.names = TRUE)
allFiles1 <- allFiles

allFiles <- allFiles[sample(seq(1:length(allFiles1)), 100, replace = FALSE )]
allFiles <- c(allFiles,
              "./GlobalDownloadedData/spfiles_WCVP_bels/Poa_bulbosa.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Agave_sisalana.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Typha_angustifolia.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Aslima_gramineum.csv")


allFiles <- c("./GlobalDownloadedData/spfiles_WCVP_bels/Poa_bulbosa.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Cyperus_bulbosus.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Luzula_arcuata.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Miscanthus_sacchariflorus.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Melica_picta.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Platanthera_stricta.csv",
              "./GlobalDownloadedData/spfiles_WCVP_bels/Setaria_italica.csv")

## these two coordinate cleaner does not work. 
## Setaria_italica.csv
## Luzula_arcuata.csv


## Include these. 
##Poa bulbosa, Agave sisalana, Typha angustifolia, Aslima gramineum

## Spatial interection reference.
## https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-cropping-join.html

## This is not with sf
## https://medium.com/@snehalgawas/introduction-to-spatial-data-analysis-in-r-using-rgeos-ea69059c3b90



map_coord_clean <- function(curFile) {

  # These 3 files are needed for all species. So open it once before the loop. 
  #monoNames <- read_delim("./MasterData/monoNames_WCVP.csv", delim = ",",
  #                        escape_backslash = TRUE,
  #                        escape_double = FALSE )
  #monoDist <- read_delim("./MasterData/monoDistNative_WCVP.csv",
  #                       escape_backslash = TRUE, delim = ",",
  #                       escape_double = FALSE )

  monoNames <- read_delim("./MasterData/monoNames_WCVP_d.csv", delim = "|",
                          escape_double = FALSE, trim_ws = TRUE )
  monoDist <- read_delim("./MasterData/monoDistNative_WCVP_d.csv", delim = "|",
                         trim_ws = TRUE)
  
  
  
  #monoDist <- read.csv("./MasterData/monoDist_WCVP.csv")
  
  
  #level3_1 <- read_sf("./MasterData/shpfiles/level3.shp")
  #level3_2 <- readOGR("./MasterData/shpfiles/level3.shp")
  
  ## Change in R version this is not working. Going back to readshapepoly
  #level3 <- st_read("./MasterData/shpfiles/level3.shp") 
  crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  level3 <- readShapePoly("./MasterData/shpfiles/level3.shp", proj4string=crswgs84,verbose=TRUE)
  
  refRas <- raster("./MasterData/refRaster.tif")
  refRasPt <- rasterToPoints(refRas)
  
  
  iteration <- 1
  #for (curFile in allFiles)
  for (curFile in iteration:length(allFiles))
  {
    curFile <- allFiles[iteration]
    print(paste("Current iteration ", iteration, sep=":"))
    print(curFile)
    cs1 <- read_csv(curFile)

    if (nrow(cs1) > 0 )
    {
      ## Remove the NA value records from decimalLatitude and decimalLongitude
      tt1 <- cbind(as.numeric(cs1$decimalLatitude), as.numeric(cs1$decimalLongitude))
      
      NAindex <- which(is.na(tt1[,1]) == TRUE | is.na(tt1[,2]) == TRUE)
      if (length(NAindex) > 0 )
      {
        cs1 <- cs1[-NAindex, ]
      }
  
      ## Remove points outside range and take only distinct lat, long. 
      cs1$decimalLatitude <- as.numeric(cs1$decimalLatitude)
      cs1$decimalLongitude <- as.numeric(cs1$decimalLongitude)
      
      cs1 <- cs1 %>% filter(decimalLongitude >= -180 & decimalLongitude <= 180) %>% 
        filter(decimalLatitude >= -90 & decimalLatitude <= 90) %>% 
        distinct(decimalLongitude, decimalLatitude, .keep_all = T) 
      
      
      ### Here add the portion of selecting native occurrences
      ### Open the shape file 
      
      curName <- gsub(".csv", "", basename(curFile))
      curGenus <- strsplit(curName, "_")[[1]][1]
      curSpecies <- strsplit(curName, "_")[[1]][2]
      ## Get the accepted name id from mononames
      
      curPlantId <- as.character(monoNames[which(monoNames$genus == curGenus & monoNames$species == curSpecies) 
                             ,"accepted_plant_name_id"][1,1])
      
      
      
      ## Get the distribution details for the curPlantId 
      ## distTbl <- monoDist[which(monoDist$accepted_plant_name_id == curPlantId),]    
      distTbl <- monoDist[which(monoDist$plant_name_id == curPlantId),]    
      ## Change in version this line below not working goin back to over in sp
      ## cs_4326 <- st_as_sf(cs1, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326, remove = F)
      ## Now do the intersection of 
      ## level3_cs <- st_join(cs_4326, level3, join = st_within)
  
      csX <- cs1[, c("decimalLongitude", "decimalLatitude")]
      coordinates(csX) = ~decimalLongitude + decimalLatitude
      #crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      cs_4326 = SpatialPoints(csX,proj4string = crswgs84)
      inside <- sp::over(cs_4326, level3)
      level3_cs <- cbind(cs1, inside)
      
          
      
          
      #cs_4326 = st_transform(cs_4326,crs=4326)
      
      #sp_points = st_as_sf(pnts,coords = c('longitude',"latitude"))#make points spatial
      #st_crs(sp_points)= 4326 # Give the points a coordinate reference system (CRS)
      #sp_points=st_transform(sp_points,crs = st_crs(countries)) # Match the point and polygon CRS
      
      
      
      
      removeNA <- which(is.na(level3_cs$LEVEL3_COD) == TRUE)
      if (length(removeNA) > 0 )
      {
        level3_cs <- level3_cs[-removeNA,]
      }
      cs_within <- merge(level3_cs, distTbl, by.x ="LEVEL3_COD", by.y = "area_code_l3")
      
      fields_cs1 <- names(cs1)
      fields_cs_within <- c("gbifID", "occurrenceID", "phylum", "class", "order", "family.x",
                      "genus.x", "species.x", "infraspecificEpithet", "scientificName",
                      "countryCode", "locality", "stateProvince", "decimalLatitude",
                      "decimalLongitude", "coordinateUncertaintyInMeters", "eventDate", 
                      "day", "month", "year", "basisOfRecord", "canonical", "SpeciesId", "bels_decimallongitude",
                      "bels_decimallatitude", "LEVEL3_COD" ) 
  
      ## This one is to give the name to the fields.. 
      fields_cs <- c(fields_cs1, "LEVEL3_COD")
      
      cs2 <- cs_within[, fields_cs_within]
      names(cs2) <- fields_cs
      cs3 <- as.data.frame(cs2)
      cs3 <- cs3[, 1:26]
  
      ### Here thin the data first This is taking a lot of time to thin. Will use
      ### a different method now. 
      #cs_ThinData <- thin( loc.data = cs3, lat.col = "decimalLatitude", long.col = "decimalLongitude", spec.col = "species", thin.par = 10, 
      #                   reps = 1,  write.files = FALSE, max.files = 5, locs.thinned.list.return = TRUE,
      #                   verbose = TRUE )
      cs <- cs3
      if (nrow(cs) > 1000)
      {
        print("********** Thinning occ > 1000 **********")
        extPoints <- extract(refRas, cs3[, c("decimalLongitude","decimalLatitude")])
        
        s1 <- seq(1,nrow(cs3))
        s1Tbl <- data.frame(cbind(s1,extPoints))
        
    
        df2 <- lapply(split(s1Tbl, s1Tbl$extPoints),
                      function(subdf) subdf[sample(1:nrow(subdf), 1),]
        )    
        df3 <- do.call('rbind', df2)
        cs <- cs3[df3$s1,]
        #cs <- merge(cs3,cs_ThinData, by.x=c("decimalLongitude","decimalLatitude"), 
        #            by.y=c("Longitude","Latitude"), all.x=FALSE, all.y=TRUE)
        
      }
  
      ##cs4 <- merge(refRasPt, extPoints, by.x = "refRaster", by.y = "extPoints")
      
      ### Will get cs_ThinData from above. 
      
      
      ## Check if the number of occurrences are more than 10 or not. 
      ## CoordinateCleaner gives error if it is less. 
      if (nrow(cs) > 10)
      {
          
        print("********** Occurrences more than 10 **********")
  
        ## Changed the outliers_method to "distance", as "qunatile" was giving 
        ## numerous erros in this function. 
        #cs2 <- clean_coordinates(cs, lon = "decimalLongitude", lat = "decimalLatitude",
        #                         species = "species",
        #                         outliers_method = "distance",
        #                         tests = c("seas","zeros", "outliers"),
        #                         verbose = T)
  
        cs2 <- clean_coordinates(cs, lon = "decimalLongitude", lat = "decimalLatitude",
                                 species = "SpeciesId",
                                 outliers_method = "distance",
                                 tests = c("seas","zeros", "outliers"),
                                 verbose = T)
        
              
    
        world <- ne_countries(scale = "medium", returnclass = "sf")
        us <- ne_countries(continent = "North America", returnclass = "sf")
        spname = gsub(".csv", "", basename(curFile))
        
        a <- ggplot() +
          geom_sf(world, mapping = aes()) +
          geom_point(cs2, mapping = aes(x = decimalLongitude, y = decimalLatitude),
                     color = "purple") + 
          coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
                   ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
          ggtitle(paste(spname, " Uncleaned", sep = "")) + 
          theme_bw()
        # "navy" , "olivedrab3"
        # "blue", "palegreen2"
        cols <- c("TRUE" = "olivedrab3", "FALSE" = "blue2")
        b <- ggplot() +
          geom_sf(world, mapping = aes()) +
          geom_point(cs2, mapping = aes(x = decimalLongitude, y = decimalLatitude,
                                        color = .summary))  +
          # scale_color_manual(values = c("blue2", "olivedrab3")) +
          scale_color_manual(values = cols) +
          coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
                   ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
          labs(color = "Not flagged") +
          ggtitle(paste(spname, " Cleaned", sep = "")) +
          theme_bw()
        
        cp <- cowplot::plot_grid(a, b, nrow = 2, ncol = 1)
        
        ## Save the data and save the plot
        write.csv(x = cs2, 
                  file = paste("./GlobalDownloadedData/cleanedCoordinates/", basename(curFile), sep = ""),
                  row.names = FALSE)
        
        
        ggsave(plot = cp, 
               filename = paste("./GlobalDownloadedData/cleanedCoordinates/", gsub(".csv", ".png", basename(curFile)), sep = ""),
               width = 8, height = 8)
        
        } else 
        {
          if (nrow(cs2) > 0 )
          {
            print("@@@@ Occurrences less than 10 @@@@")
            .val <- TRUE
            .zer  <- TRUE 
            .sea  <- TRUE 
            .otl  <- TRUE
            .summary  <- TRUE
            cs2 = cbind(cs, .val, .zer, .sea, .otl, .summary)
          }
      }
      
      
    
    }# if condition
    iteration <- iteration + 1
    print("here after if ")
    
   } ## for
} ## function. 
print(iteration)
iteration = iteration + 1
print(iteration)


allFiles <- list.files("./spfiles_WCVP_bels/", pattern = ".csv$", full.names = TRUE)
allFiles1 <- allFiles
allFiles <- allFiles1[1001:3000]


lapply(allFiles, map_coord_clean)


which(basename(allFiles) =="Apera_interrupta.csv")

## which species don't have maps?
run <- list.files("outputs/cleanedCoordinatesMaps/") %>% 
  word(1,1, sep = fixed(".")) %>% 
  str_replace("_", " ")

file2 <- allFiles[1:10]

for(i in seq_along(file2)){
  
  tryCatch(map_coord_clean(binomial = file2[i]), error = function(e) next))
  
}




## This raster is 5min raster global
r1 <- raster("./MasterData/Ref_10Km.tif")

getRaster <- function(r1)
{
  r1pt = rasterToPoints(r1)

  xval = sort(unique(r1pt[,1]))
  yval = sort(unique(r1pt[,2]))

  xid = seq(1,length(xval))
  length(xid)
  yid = seq(1,length(yval))
  length(yid)
  xtbl = cbind(xval, xid)
  ytbl = cbind(yval, yid)
  
  r1pt_1 = r1pt
  head(xtbl)
  head(r1pt)
  t1 = merge(r1pt, xtbl, by.x = "x", by.y = "xval")
  head(t1)
  t2 = merge(t1, ytbl, by.x = "y", by.y = "yval")
  head(t2)
  
  id1 = seq(1, nrow(t2))
  t2 = cbind(id1, t2)
  r3 = rasterize(t2[,c("x","y")], r1, t2[,1])
  dt2 = cbind(as.numeric(dt1[, "decimalLongitude"]), as.numeric(dt1[, "decimalLatitude"]))

}


