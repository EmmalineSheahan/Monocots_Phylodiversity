# wcvp to taxolist


wcvp$accid <- 0
#for(i in 1:nrow(wcvp)){
for(i in 104571:nrow(wcvp)){
#  for(i in 1:100){
    print(i)
  if(is.na(wcvp$accepted_plant_name_id[i])){
    wcvp$accid[i] <- 0
  } else {
    if(wcvp$accepted_plant_name_id[i]==wcvp$plant_name_id[i]){
      print(wcvp$plant_name_id[i])
      wcvp$accid[i] <- 0
    } else {
      if (length(which(wcvp$plant_name_id==wcvp$accepted_plant_name_id[i])) > 0)
      {
      wcvp$accid[i] <- wcvp$id[which(wcvp$plant_name_id==wcvp$accepted_plant_name_id[i])]
      } else { print("#####################")
        wcvp$accid[i] <- 0}
        
    }
  }
}

saveRDS(wcvp, "wcvp.rds")
## Taking only accepted and synonym 
wcvp1 = wcvp[which(wcvp$taxon_status == "Accepted" | wcvp$taxon_status == "Synonym"),]


## These names are needed for next processing
## names(wfo1) <- c("id", "accid", "family", "genus","species",
##                 "subspecies", "taxonRank", "author", "year")

wcvp2 <- wcvp1[,c("id", "accid", "family", "genus","species", "infraspecies", "taxon_rank")]
names(wcvp2) <- c("id", "accid", "family", "genus","species", "subspecies", "taxonrank")
wcvp2 <- cast_canonical(wcvp2, "canonical", "genus", "species", "subspecies", verbose=TRUE)
write.csv(wcvp2,"wcvp2_p1.csv",row.names = F)


# Remove duplicate synonyms
wcvp3 <- wcvp2[!duplicated(wcvp2$canonical), ]
wcvp3_d <- wcvp2[duplicated(wcvp2$canonical), ]
wcvp3_dK <- wcvp3_d[which(wcvp3_d$accid==0), ]
wcvp3 <- rbind(wcvp3, wcvp3_dK)
wcvp3 <- wcvp3[order(wcvp3$id), ]
wcvp3$source <- "wcvp"

write.csv(wcvp3,"wcvp_p2.csv",row.names = F)
saveRDS(wcvp3,"wcvpMast.rds")



