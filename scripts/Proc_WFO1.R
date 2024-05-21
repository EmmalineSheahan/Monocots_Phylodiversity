library(readr)
library(taxotools)
classification <- read_delim("classification.txt",
                             "\t", escape_double = FALSE, trim_ws = TRUE)

mast1 <- classification[which((classification$taxonRank=="Species" |
                                 classification$taxonRank=="SPECIES") &
                                classification$taxonomicStatus=="Accepted"),]
#mast1 <- mast1[,c("family", "genus", "specificEpithet", "infraspecificEpithet", 
#                  "scientificNameAuthorship")]
mast1$id <- 1:nrow(mast1)
mast1$accid <- 0

# Synonymize Variety and subspecies

mast2 <- classification[which(( classification$taxonRank=="SUBSPECIES" |
                                  classification$taxonRank=="Variety" |
                                  classification$taxonRank=="VARIETY") &
                                classification$taxonomicStatus=="Accepted"),]

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}


id <- max(mast1$id) + 1
addlst1 <- NULL
notproc <- NULL
for(i in 1:dim(mast2)[1]){
  addrec <- mast2[i,]
  scname <- paste(mast2$genus[i],mast2$specificEpithet[i])
  accid <- mast1$id[which(mast1$scientificName==scname)]
  if(!is.integer0(accid)){
    #print(accid)
    addrec$id <- id
    addrec$accid <- accid
    id <- id + 1
    addlst1 <-  rbind(addlst1,addrec)
  } else {
    notproc <-  rbind(notproc,addrec)
  }
}

mast3 <- rbind(mast1,addlst1)

#syn1 <- classification[which(classification$taxonRank=="SPECIES" &
#                               classification$taxonomicStatus=="Synonym"),]
syn1 <- classification[which(classification$taxonomicStatus=="Synonym" ),]

id <- max(mast3$id) + 1
addsyn1 <- NULL
notproc2 <- NULL
for(i in 1:dim(syn1)[1]){
  addrec <- syn1[i,]
  accid <- mast3$id[which(mast3$taxonID==syn1$acceptedNameUsageID[i])]
  if(!(identical(accid, numeric(0)))){  
    print(paste(i,accid))
    addrec$id <- id
    addrec$accid <- accid
    id <- id + 1
    addsyn1 <-  rbind(addsyn1,addrec)
  } else {
    notproc2 <-  rbind(notproc2,addrec) 
  }
}

mast4 <- rbind(mast3,addsyn1)

write.csv(mast4,"wfo_sl.csv",row.names = F)


#-----------
# mast5 <- mast4
#for(i in 372159:1020107) {mast5$id <- i}
#-----------

wfo1 <- mast5[,c(20,21,7,8,9,10,4,6,13)]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

wfo1$Year <- substrRight(wfo1$namePublishedIn,5)
wfo1$Year <- as.numeric(wfo1$Year)
wfo1$Year[which(wfo1$Year<1753 | wfo1$Year>2019)] <- NA

wfo1 <- wfo1[,-9]

names(wfo1) <- c("id", "accid", "family", "genus","species",
                 "subspecies", "taxonRank", "author", "year")
wfo2 <- cast_canonical(wfo1,"genus","species","subspecies")
write.csv(wfo2,"wfo_p1.csv",row.names = F)

# Remove duplicate synonyms
wfo3 <- wfo2[!duplicated(wfo2$canonical),]
wfo3_d <- wfo2[duplicated(wfo2$canonical),]
wfo3_dK <- wfo3_d[which(wfo3_d$accid==0),]
wfo3 <- rbind(wfo3,wfo3_dK)
wfo3 <- wfo3[order(wfo3$id),]

write.csv(wfo3,"wfo_p2.csv",row.names = F)

library(moltools)
wfo4 <- fill_fam_ord(wfo3)
wfo4$source <- "wfo"
write.csv(wfo4,"Plants_20200303.csv",row.names = F)
wfo_final_part1 <- wfo4[which(wfo4$accid==0),]
wfo_final_part2 <- wfo4[which(wfo4$accid!=0),]
write.csv(wfo_final_part1,"Plants_20200317_1.csv",row.names = F)
write.csv(wfo_final_part2,"Plants_20200317_2.csv",row.names = F)

