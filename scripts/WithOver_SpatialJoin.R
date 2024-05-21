crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

level3 <- readShapePoly("./MasterData/shpfiles/level3.shp", proj4string=crswgs84,verbose=TRUE)
cs1 <- read_csv(curFile)

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




cs2 <- cs1[, c("decimalLongitude", "decimalLatitude")]
coordinates(cs2) = ~decimalLongitude + decimalLatitude

occ_dt = SpatialPoints(cs2,proj4string = crswgs84)
inside <- sp::over(occ_dt, level3)
inside_2 <- sp::over(occ_dt, level3_2)



### Trying thinning. 


df <- data.frame(matrix(rnorm(80), nrow=40))
df$color <-  rep(c("blue", "red", "yellow", "pink"), each=10)

#df[sample(nrow(df), 3), ] #samples 3 random rows from df, without replacement.



df2 <- lapply(split(df, df$color),
              function(subdf) subdf[sample(1:nrow(subdf), 1),]
)

df4 <- do.call('rbind', df2)


n <- 1
resample <- TRUE
index <- 1:nrow(s1Tbl)
fun <- function(x) sample(x, n, replace = resample)
a <- aggregate(index, by = list(group = s1Tbl$extPoints), FUN = fun )

s1Tbl[c(a$s1),]




