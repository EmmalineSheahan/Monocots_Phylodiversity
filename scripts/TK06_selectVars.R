################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 6. TK06_selectVars
#####' 
#####' This script takes diversity metrics (response) and environmental and 
#####' climate zone variables (explanatory) from master data sets created in 
#####' 05_getMasters for regression modeling for each region. It first selects 
#####' variables for each metric (and region) through backwards selection and
#####' minimizing variance inflation. It then creates models for the 
#####' environmental variables only, the core / buffer zone delineation only,
#####' and both for each metric and region, applying appropriate models. Enter 
#####' names of input for all regions.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2024
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### Set up

#### Load in libraries and functions
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(dplyr)
library(car)
library(caret)
library(ggplot2)
library(data.table)
library(betareg)
library(MASS)

#### File set up
### Set up the variables that will be used
## Enter desired diversity metrics here
response <-c("pd_obs", "pd_obs_z", "rpd_obs_z", "gpPD", "SR", "gpSR")
## Enter environmental / Med. climate variables here
vars <- c("bio_1", "bio_4", "bio_18", "bio_19", "clay", "phh2o", "elev", "med_clim")

### Get the Biodiverse / canaper output files
## Enter file appendage names for each region here
names <- c("Aus_ver1", "Cal_ver1", "Chi_ver1", "Med_ver1", "SAf_ver1")
## Get files
files <- paste0("Master_datasets/Master_", names, ".csv")
names <- gsub("_.*", "", names)
keep <- c("x", "y", response, vars)

#### Set up the masters
### Load them in
masters <- lapply(files, fread)
masters <- lapply(masters, as.data.frame)
names(masters)<-names
### Select the proper variables and scale
masters <- lapply(masters, dplyr::select, all_of(keep))
## Function for scaling data
listScale <- function(x, scale_vars){
  x[,scale_vars] <- scale(x[,scale_vars])
  x
}
## Apply scale function to all regions
masters<-lapply(masters, listScale, scale_vars = vars[-8]) #don't scale med_clim
### Make corrections to variables
masters <- lapply(masters, function(x) {
  ## Set the factor variable as a factor in all
  x[,"med_clim"] <- factor(x[,"med_clim"], levels = c(0, 1), 
                   labels = c("Buffer","Core"))
  x
})

##### Variable selection

#### Set formulas and table to store results of variable selection
form <- paste(vars, collapse = " + ")
df_forms <- data.frame(region = rep(names, length(response)),
                       resp = rep(response, each = length(names)), 
                       step_vars_rem = "", vif_vars_rem = "", form = "")

#### Loop through each region and remove variables
for (i in 1:length(masters)){
  temp <- masters[[i]]
  for (resp in response){
    # Update table
    rn <- which(df_forms$region == names[i] & df_forms$resp == resp)
    df_forms[rn,2] <- resp
    ### Get full model
    mod <- lm(as.formula(paste0(resp, " ~ ", form)), temp)
    ### Backwards selection to remove non-significant variables
    back <- step(mod)
    vars_rem <- vars[!(vars %in% names(back$model)[-1])]
    # Update table
    df_forms[rn,3] <- paste(vars_rem, collapse = ",")
    ### Now remove any variables with high VIFs
    vars_temp <- subset(vars, !(vars %in% vars_rem))
    mod <- lm(as.formula(paste0(resp, " ~ ", paste(vars_temp, collapse = " + "))), temp)
    vifs <- vif(mod)
    vars_vif <- vector()
    while (any(round(vifs,1) >= 5)){
      # Make sure to keep BIO18 or BIO19 and remove the next highest
      vifs <- vifs[-which(names(vifs) %in% c("bio_18", "bio_19"))] # SAf pd_obs is the only one where this applies (phh2o replaces bio_19)
      vars_vif <- c(vars_vif, names(vifs[which(vifs == max(vifs))]))
      vars_temp <- subset(vars_temp, !(vars_temp %in% vars_vif))
      mod <- lm(as.formula(paste0(resp, " ~ ", paste(vars_temp, collapse = " + "))), temp)
      vifs <- vif(mod)
    }
    # Update table
    df_forms[rn,4] <- paste(vars_vif, collapse = ",")
    df_forms[rn,5] <- paste0(resp, " ~ ", paste(vars_temp, collapse = " + "))
  }
}

##### Create models

#### Set up objects to store data in
## Info table
df_r2 <- data.frame(region = NA, resp = NA, r2_all = NA, r2_med = NA, r2_env = NA)
## Model lists
mods <- as.list(1:nrow(df_forms)) # Full model (selected formula)
mods2 <- as.list(1:nrow(df_forms)) # Med clim only (whether or not it was selected)
mods3 <- as.list(1:nrow(df_forms)) # Env vars only (med_clim is removed)

#### Loop through each model formula (from above selection) and create the model
for (i in 1:nrow(df_forms)){
  ### Set up / get info
  name = df_forms[i,1]
  resp = df_forms[i,2]
  form <- df_forms[i,5]
  form2 <- sub(" \\+ med_clim", "", form)
  temp <- masters[[which(names == name)]]
  ### Make models (full selected, med_clim, and env only) using appropriate model
  if (resp == "gpPD" | resp == "gpSR"){
    # Correct for (0,1)
    temp[,resp][temp[,resp]==0] = 0.00001
    temp[,resp][temp[,resp]==1] = 0.99999
    ## Full model
    mod <- betareg(as.formula(form), temp)
    r2 <- round(summary(mod)$pseudo.r.squared,3)
    ## Med clim only
    mod2 <- betareg(as.formula(paste0(resp, " ~ med_clim")), temp)
    r22 <- round(summary(mod2)$pseudo.r.squared,3)
    ## Env vars only
    mod3 <- betareg(as.formula(form2), temp)
    r23 <- round(summary(mod3)$pseudo.r.squared,3)
  } else if (resp == "SR"){
    ## Full model
    mod <- glm.nb(as.formula(form), temp)
    r2 <- round(with(summary(mod), 1 - deviance/null.deviance),3) #McFadden's psuedoR2
    ## Med clim only
    mod2 <- glm.nb(as.formula(paste0(resp, " ~ med_clim")), temp)
    r22 <- round(with(summary(mod2), 1 - deviance/null.deviance),3)
    ## Env vars only
    mod3 <- glm.nb(as.formula(form2), temp)
    r23 <- round(with(summary(mod3), 1 - deviance/null.deviance),3)
  } else{
    ## Full model
    mod <- lm(as.formula(form), temp)
    r2 <- round(summary(mod)$r.squared,3)
    ## Med clim only
    mod2 <- lm(as.formula(paste0(resp, " ~ med_clim")), temp)
    r22 <- round(summary(mod2)$r.squared,3)
    ## Env vars only
    mod3 <- lm(as.formula(form2), temp)
    r23 <- round(summary(mod3)$r.squared,3)
  }
  ### Update model list
  mods[[i]] <- mod
  names(mods)[i] <- paste(name, resp, sep = "_")
  mods2[[i]] <- mod2
  names(mods2)[i] <- paste(name, resp, sep = "_")
  mods3[[i]] <- mod3
  names(mods3)[i] <- paste(name, resp, sep = "_")
  ### Update table
  df_r2[i,1] <- name
  df_r2[i,2] <- resp
  df_r2[i,3] <- r2
  df_r2[i,4] <- r22
  df_r2[i,5] <- r23
  print(i)
}

##### Output
## Create output directory if necessary
if (!dir.exists("regressions")) dir.create("regressions")
## Save
saveRDS(mods, "regressions/all_mods.RDS")
saveRDS(mods2, "regressions/core_mods.RDS")
saveRDS(mods3, "regressions/env_mods.RDS")
write.csv(df_forms, "regressions/model_select.csv", row.names = F)
write.csv(df_r2, "regressions/model_R2.csv", row.names = F)
