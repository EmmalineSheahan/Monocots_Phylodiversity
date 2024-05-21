
fields_idig1 = c("coreid",  "dwc:occurrenceID","dwc:phylum", "dwc:class",  "dwc:order","dwc:family" , "dwc:genus",
                "dwc:specificEpithet","dwc:infraspecificEpithet", "dwc:scientificName", "dwc:countryCode", "dwc:locality",
                "dwc:stateProvince","dwc:decimalLatitude", "dwc:decimalLongitude", "dwc:coordinateUncertaintyInMeters",
                "dwc:eventDate", "dwc:day", "dwc:month","dwc:year", "dwc:basisOfRecord")


fields_idig2 = c("coreid",  "dwc.occurrenceID","dwc.phylum", "dwc.class",  "dwc.order","dwc.family" , "dwc.genus",
                 "dwc.specificEpithet","dwc.infraspecificEpithet", "dwc:scientificName", "dwc:countryCode", "dwc.locality",
                 "dwc.stateProvince","dwc.decimalLatitude", "dwc:decimalLongitude", "dwc:coordinateUncertaintyInMeters",
                 "dwc.eventDate", "dwc.day", "dwc.month","dwc:year", "dwc.basisOfRecord")


test1 <- read_csv("GlobalDownloadedData/test1.csv", quote = "\"")

test3 <- read_delim("GlobalDownloadedData/test1.csv", delim = ",", quote = "\"", escape_double = TRUE)


## This is occurrence file. 
cdata1 = read.csv("./idigbio/csv/idigbio2.csv")
idNAs1 = which(is.na(cdata1[,"dwc:genus"]))
if (length(idNAs1)>0 )
{cdata1 = cdata1[-idNAs1,]}

dt1 = cdata1[1:10, fields_idig1]
g1  = as.character(dt1[,"dwc.genus"])
s1  = as.character(dt1[,"dwc.specificEpithet"])

## This is occurrence.raw file. 
#test1 <- read_delim("./idigbio/test.csv", ",", escape_double = FALSE, trim_ws = TRUE)
test1 <- read_delim("./idigbio/csv/idigbio2.csv", ",", escape_double = FALSE, trim_ws = TRUE)
if (length(idNAs2)>0 )
{idNAs2 = which(is.na(test1[,"dwc.genus"]))}

test1 = test1[-idNAs2,]
dt2 = test1[1:10, fields_idig1]
g2  = as.character(dt2[,"dwc.genus"])
s2  = as.character(dt1[,"dwc.specificEpithet"])
