###### Function to clip model variable layers to accessible area

clip_variableLayers <- function(rstack, accessibleArea){
  #aa_proj <- sp::spTransform(accessibleArea, CRSobj = terra::crs(rstack,proj = T))
  #r_crop <- terra::crop(rstack, vect(aa_proj), mask=TRUE)
  r_crop <- terra::crop(rstack, vect(accessibleArea), mask=TRUE)
  
  return(r_crop)
  
}


#r_crop1 <- terra::crop(rstack, vect(accessibleArea), mask=TRUE)
