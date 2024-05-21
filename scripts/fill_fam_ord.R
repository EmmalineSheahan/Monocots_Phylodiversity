fill_fam_ord <- function(master,family_fld=NA,order_fld=NA){
  if(!is.na(family_fld)){
    master <- rename_column(master,family_fld,"family")
  }
  if(!is.na(order_fld)){
    master <- rename_column(master,order_fld,"order")
  }
  mast <- master[which(master$accid==0),]
  print(paste("Processing Accepted names",dim(mast)[1]))
  for(i in 1:dim(mast)[1]){
    print(paste(i," - ", dim(mast)[1]))
    id <- mast$id[i]
    master$family[which(master$accid==id)] <- mast$family[i]
    master$order[which(master$accid==id)] <- mast$order[i]
  }
  if(!is.na(family_fld)){
    master <- rename_column(master,"family",family_fld)
  }
  if(!is.na(order_fld)){
    master <- rename_column(master,"order",order_fld)
  }
  return(master)
}
