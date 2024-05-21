# library(devtools)
# install_github("vijaybarve/taxotools")

## Data is stored in office desktop computer. The folder where data is stored is 
## D:\Narayani\Projects\Monocot_phylogeny\ThroughGazzetteer



library(taxotools)


Monocots_Species_Region = read.csv("./MasterData/Monocots_Species_Regions_cleaned.csv")

## Plantmast is stored here in this folder in rds format. Given by Vijay "D:\Narayani\Projects\Monocot_phylogeny\ThroughGazzetteer\Taxonomy_wfo"

Plantmast <- readRDS("./MasterData/wcvpMast.rds")

mono_cano = cast_canonical(Monocots_Species_Region, genus = "Genus", species = "Species")
mono_cano$id = seq(1, nrow(mono_cano))
mono_cano$accid = 0
all_names_mono = get_synonyms(master = Plantmast, checklist = mono_cano, 
                              commasep = FALSE, verbose=TRUE)
all_names_mono_org = all_names_mono

### This get_synonym function drops the names where there are not synonyms found
### so I will have to externally add the names in the file here
allnames = c(unique(all_names_mono$Acc_Name), all_names_mono$Syn_name)
monoOrg = mono_cano$canonical
`%notin%` <- Negate(`%in%`)
notfound = monoOrg[which(monoOrg %notin% allnames)]
### Till here no names added where there is no synonym

names(all_names_mono) = c("Acc_Name", "Syn_name")
## Removing the "Bromus mollis subglaber pseudo-racemosus" row number 8805
#all_names_mono = all_names_mono[-8805,]
all_names_mono_split = melt_canonical(all_names_mono, canonical = "Syn_name", 
                                      genus = "syn_genus", species = "syn_species", 
                                      subspecies= "syn_infraspecific", 
                                      verbose = TRUE)
all_names_mono_split$syn_binom = paste(all_names_mono_split$syn_genus, 
                                       all_names_mono_split$syn_species, sep = " ")

#write.csv(all_names_mono_split, "./MasterData/Mono_Acc_Syn_WCVP.csv", row.names = FALSE)

## removing duplicates
all_names_unique = all_names_mono_split[!duplicated(all_names_mono_split[c(1,6)]), ]
##all_names_taxo = syn2taxo(all_names_unique, canonical = "Acc_Name", synonym = "syn_binom")

## Now here add the notfound data and then save the file. 
## Brind the notfound into the same data structure as of all_names_unique

dt1 = cbind(notfound, NA, NA, NA,NA,NA)
dt1 = data.frame(dt1)
names(dt1) = names(all_names_unique)

all_Mono_syn_link = rbind(all_names_unique, dt1)

#write.csv(all_names_unique, "./MasterData/Mono_Acc_Syn_WCVP.csv", row.names = FALSE)
write.csv(all_Mono_syn_link, "./MasterData/Mono_Acc_Syn_WCVP.csv", row.names = FALSE)
# 
# ## I want to generate a file with 3 columns Speciesnames taxstatus, species id
# all_names_unique = read.csv("./MasterData/Mono_Acc_Syn_WCVP.csv", stringsAsFactors = FALSE)
# all_names_taxo = syn2taxo(all_names_unique, canonical = "Acc_Name", synonym = "syn_binom")
# 
# all_names_taxo$newid <- all_names_taxo$accid
# all_names_taxo$newid[which(all_names_taxo$newid == 0) ] <- all_names_taxo$id[which(all_names_taxo$newid == 0) ]
# 
# all_names_taxo$TaxonStatus <- "Synonym"
# all_names_taxo$TaxonStatus[which(all_names_taxo$accid == 0) ] <- "Accepted"
# write.csv(all_names_taxo, "./MasterData/all_names_taxo_WCVP.csv", row.names = FALSE)
# 
# MonoNames = all_names_taxo[, c("canonical", "TaxonStatus", "newid")]
# names(MonoNames) = c("SpeciesName", "TaxonStatus", "SpeciesId")
# write.csv(MonoNames, "./MasterData/MonoNames_WCVP.csv", row.names = FALSE)

## Names to be given to John
JohnNames = c(unique(all_Mono_syn_link$Acc_Name), all_Mono_syn_link$syn_binom[!is.na(all_Mono_syn_link$syn_binom)])
write.csv(JohnNames, "./MasterData/OnlyMonoNames_WCVP.csv", row.names = FALSE)
