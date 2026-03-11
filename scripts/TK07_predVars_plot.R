################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 7. TK07_predVars_plot
#####' 
#####' This script takes the models and associated output from 06_selectVars to 
#####' produce model coefficient plots in Figs 2, 4, S1, and S11 as well as 
#####' supplementary tables with R2 (Table S2) and coefficient values (S3-8).
#####' 
#####' Written by Taliesin (Tal) Kinser in 2025
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

##### Set up

#### Load libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(ggplot2)
library(tidyverse)
library(patchwork)
library(data.table)

#### Read in the data
### Read in mods
mods <- readRDS("regressions/all_mods.RDS")
mods2 <- readRDS("regressions/core_mods.RDS")
mods3 <- readRDS("regressions/env_mods.RDS")
### Read in r2 results
results <- read.csv("regressions/model_R2.csv")
### Read in masters
names <- c("Aus_ver1", "Cal_ver1", "Chi_ver1", "Med_ver1", "SAf_ver1")
files <- paste0("Master_datasets/Master_", names, ".csv")
names <- gsub("_.*", "", names)
masters <- lapply(files, fread)
masters <- lapply(masters, as.data.frame)
names(masters)<-names
### Make med_clim a factor
masters <- lapply(masters, function(x) {
  i = which(colnames(x)=="med_clim")
  x[,i] <- factor(x[,i], levels = c(0, 1), 
                  labels = c("Buffer","Core"))
  x
})
### Update naming scheme
results$region <- factor(x=results$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                            labels = c("WNA","Med","Chi","SAf","Aus"))
names(masters) <- gsub("Cal","WNA",names(masters))
names(mods) <- gsub("Cal","WNA",names(mods))
names(mods2) <- names(mods2)
names(mods3) <- names(mods3)

#### Separate out metrics
### Full model
pd <- mods[1:5]
pdz <- mods[6:10]
rpd <- mods[11:15]
gp <- mods[16:20]
sr <- mods[21:25]
gps <- mods[26:30]
### Med clim only
pd2 <- mods2[1:5]
pdz2 <- mods2[6:10]
rpd2 <- mods2[11:15]
gp2 <- mods2[16:20]
sr2 <- mods2[21:25]
gps2 <- mods2[26:30]
### Env only
pd3 <- mods3[1:5]
pdz3 <- mods3[6:10]
rpd3 <- mods3[11:15]
gp3 <- mods3[16:20]
sr3 <- mods3[21:25]
gps3 <- mods3[26:30]

##### Plot functions

#### Function for assemble all the coefficients for the plots 
get_coefs <- function(mod_ls, mod_ls2 = NULL, mod_ls3 = NULL, levs = NULL){
  ### Extract coefficients from the full model
  for (i in 1:length(mod_ls)){
    mod <- mod_ls[[i]]
    reg <- names(mod_ls)[i]
    if (class(mod)[1]=="lm" | class(mod)[1]=="negbin"){
      coefs <- coef(mod)[-1]
    } else{
      coefs <- coef(mod)[-c(1,length(coef(mod)))]
    }
    temp <- data.frame(Region = reg, Var = names(coefs), 
                       Slope = signif(as.numeric(coefs),3), Slope2 = NA)
    ## Add in coefficient from med_clim only model if requested
    if (!is.null(mod_ls2)){
      coefs2 <- signif(as.numeric(coef(mod_ls2[[i]])[2]),3)
      if ("med_climCore" %in% temp$Var){
        temp[which(temp$Var == "med_climCore"),4] <- coefs2
      } else{
        temp <- rbind(temp, list(reg,"med_climCore",NA, coefs2))
      }
    }
    ## Add in coefficients from env only model if requested
    if (!is.null(mod_ls3)){
      mod2 <- mod_ls3[[i]]
      if (class(mod2)[1]=="lm"){
        coefs2 <- coef(mod2)[-1]
      } else{
        coefs2 <- coef(mod2)[-c(1,length(coef(mod2)))]
      }
      temp$Slope2[which(temp$Var %in% names(coefs2))] <- signif(as.numeric(coefs2),3)
    }
    ## Order by slope size
    temp <- temp[order(temp$Slope),]
    if (i == 1){
      sar_plot_df = temp
    } else{
      sar_plot_df <- rbind(sar_plot_df, temp)
    }
  }
  
  ### Set up region as factor variable (if requested)
  sar_plot_df$Region <- gsub(paste0("_.*"),"",sar_plot_df$Region)
  if (!is.null(levs)){
    sar_plot_df$Region <- factor(sar_plot_df$Region, levels = levs)
  }
  
  ### Order the variables as factors
  sar_plot_df$Var <- factor(sar_plot_df$Var, 
                            levels = c("bio_1","bio_4","bio_18","bio_19",
                                       "elev","clay","phh2o","med_climCore"))
  sar_plot_df
}

#### Function for the coefficient plots
coef_plot_fxn <- function(mod_ls, mod_ls2 = NULL, mod_ls3 = NULL, 
                          ylims = NA, size = 2, levs = NULL){
  ### Extract the coefficients
  sar_plot_df <- get_coefs(mod_ls, mod_ls2, mod_ls3, levs)
  
  ### Set up plotting variables
  ## Set each variable as climatic, edaphic, or the core/buffer var
  sar_plot_df$Var_type <- factor(sar_plot_df$Var,
                            labels = c("clim", "clim", "clim", "clim",
                                       "ed", "ed", "ed", "core"))
  ## Create a color variable
  sar_plot_df$Var_col <- as.character(factor(sar_plot_df$Var,
                                 labels = c("black","black","red","red",rep("black",4))))
  ##' Create a variable to push climatic variables to the left and edaphic to 
  ##' right (to limit point overlap)
  sar_plot_df$hj <- as.character(factor(sar_plot_df$Var,
                                        labels = c(rep(-0.1,4),rep(0.1,3),0)))
  
  ### Create the plot
  p <- ggplot(sar_plot_df, aes(x = Region, y = Slope, fill = Var, shape = Var, color = Var,
                             group = Var_type)) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = "gray70")
  if (any(!is.na(sar_plot_df$Slope2))){
    p <- p + geom_point(aes(x = Region, y = Slope2, fill = Var, shape = Var, color = Var,
                        group = Var_type), alpha = 0.5, size = size, col=sar_plot_df$Var_col, 
                    position = position_nudge(x=as.numeric(sar_plot_df$hj)))
  }
  p <- p +
    geom_point(size = size, col=sar_plot_df$Var_col, 
               position = position_nudge(x=as.numeric(sar_plot_df$hj))) +
    scale_shape_manual(values=c(21:24,21:23,21),
                       labels=c("bio_1" = "BIO1", "bio_4"="BIO4", "bio_19"="BIO19",
                                "bio_18"="BIO18", "elev"="Elev.", "phh2o"="pH",
                                "clay"="% Clay", "med_climCore"="Core")) +
    scale_fill_manual(values=c(rep("azure",4),rep("yellow",3),"red"), 
                      labels=c("bio_1" = "BIO1", "bio_4"="BIO4", "bio_19"="BIO19", 
                               "bio_18"="BIO18", "elev"="Elev.", "phh2o"="pH",
                               "clay"="% Clay","med_climCore"="Core")) +
    guides(fill = guide_legend(override.aes = list(shape = c(21:24,21:23,21), 
                                                   color = c("black", "black", "red", "red", rep("black",4))))) +
    labs(x = "Region", y = "Estimated Coefficients", shape = "Predictors", 
         fill = "Predictors", color = "Predictors") 
    
  if (length(ylims) == 2){
    p <- p + ylim(ylims)
  }
  p
}

#### Function for box plots
box_plots <- function(masters, metric, levs = NULL, ylims = NA, size = 2){
  ### Combine masters into one dataset
  full <- bind_rows(masters, .id = "Region")
  full <- subset(full, !is.na(med_clim))
  # Set up region as factor variable (if requested)
  if (!is.null(levs)){
    full$Region <- factor(full$Region, levels = levs)
  }
  ### Plot
  p <- ggplot(full) +
    geom_boxplot(mapping = aes(x = Region, y = get(metric), col = med_clim), fill=NA, 
                 outlier.alpha =0.2, outlier.size = 0.5) +
    scale_color_manual(values = c("blue","red")) +
    labs(x = "Region", y = "Values", col = "Climate zone")
  if (length(ylims) == 2){
    p <- p + ylim(ylims)
  }
  p
}

#### Function for R2 plot
R2_plot_fxn <- function(res, metric, size = 2){
  ### Get info from the R2 results table
  res <- subset(res, resp == metric)
  res <- res[,c("region","r2_all","r2_med","r2_env")]
  
  ### Prepare table for plotting
  res <- res %>% 
    tidyr::pivot_longer(cols = -1, names_to = "mod_type",values_to = "r2")
  res$mod_type <- gsub("r2_","",res$mod_type)
  res$mod_type <- factor(res$mod_type, levels = c("med","env","all"))
  
  ### Make plot
  p <- ggplot(res, aes(x = region, y = r2, shape = mod_type)) + geom_point(size=size) + 
    scale_shape_manual(values = c(1,3,19), 
                       labels = c("med"="Core effect",
                                  "env"="Env. effect",
                                  "all"="Both effect")) +
    labs(x = "Region", y = "R-squared") + ylim(0,1)
  p
}

#### Function for combined plot (use all plotting functions above)
mod_res_plot_fxn <- function(mod_ls, mod_ls2 = NULL, mod_ls3 = NULL, masters,
                             results, metric, size = 2, 
                             levs = NULL, coef_ylims = NA, box_ylims = NA){
  ### Make each plot
  coef_plot <- coef_plot_fxn(mod_ls, mod_ls2, mod_ls3, coef_ylims, size, levs) + 
    theme(axis.text.x = element_blank(),legend.title = element_text(size = 14),
          legend.margin=margin(0, 0, 0, -35))
  box_plot <- box_plots(masters, metric, levs = levs, box_ylims, size)+ 
    theme(axis.text.x = element_blank(),legend.title = element_text(size = 14), 
          legend.margin=margin(0, 0, 0, -5))
  R2_plot <- R2_plot_fxn(results, metric, size)+
    theme(legend.title = element_blank(),legend.margin=margin(0, 0, 0, -5))
  ### Combine into one
  coef_plot + box_plot + R2_plot + 
    plot_layout(axis_titles ="collect_x", ncol=1, nrow = 3, heights = c(3,2,2)) &
    theme(panel.background = element_rect(fill = "white"), 
          #panel.grid.major.y = element_line(color = "gray50"),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA), 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 12), plot.margin = unit(c(.1,.1,.1,.1),'lines'),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
}

##### Make the plots for each metric
### Set theme
Theme = 
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        legend.text = element_text(size = 12), 
        plot.margin = unit(c(.1,.1,.1,.1),'lines'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.title = element_text(size = 14),
        legend.margin=margin(0, 0, 0, -8))
### Plots (..2 is for supplemental Fig S1; sr and gps are only in supp Fig S11)
## PD
pd_plot <- mod_res_plot_fxn(mod_ls= pd, masters = masters, results = results,
                            metric = "pd_obs", size=3, levs = levels(results$region),
                            coef_ylims = c(-0.12,0.12))
pd2_plot <- coef_plot_fxn(mod_ls= pd, mod_ls2 = pd2, mod_ls3 = pd3, 
                          size=3, levs = levels(results$region)) + Theme
## sesPD
pdz_plot <- mod_res_plot_fxn(mod_ls= pdz, masters = masters, results = results,
                             metric = "pd_obs_z", size=3, levs = levels(results$region),
                             coef_ylims = c(-1,1))
pdz2_plot <- coef_plot_fxn(mod_ls= pdz, mod_ls2 = pdz2, mod_ls3 = pdz3, 
                          size=3, levs = levels(results$region)) + Theme
## sesRPD
rpd_plot <- mod_res_plot_fxn(mod_ls= rpd, masters = masters, results = results,
                             metric = "rpd_obs_z", size=3, levs = levels(results$region),
                             coef_ylims = c(-1.1,1.1))
rpd2_plot <- coef_plot_fxn(mod_ls= rpd, mod_ls2 = rpd2, mod_ls3 = rpd3, ylims = c(-1.2,1.2),
                          size=3, levs = levels(results$region)) + Theme
## gpPD
gp_plot <- mod_res_plot_fxn(mod_ls= gp, masters = masters, results = results,
                            metric = "gpPD", size=3, levs = levels(results$region),
                            coef_ylims = c(-0.7,0.3))
gp2_plot <- coef_plot_fxn(mod_ls= gp, mod_ls2 = gp2, mod_ls3 = gp3, ylims = c(-0.8,0.8),
                          size=3, levs = levels(results$region)) + Theme
## SR
sr_plot <- mod_res_plot_fxn(mod_ls= sr, mod_ls2 = sr2, mod_ls3 = sr3, 
                            masters = masters, results = results,
                            metric = "SR", size=3, levs = levels(results$region))
## gpSR
gps_plot <- mod_res_plot_fxn(mod_ls= gps, mod_ls2 = gps2, mod_ls3 = gps3, 
                             masters = masters, results = results, metric = "gpSR", 
                             size=3, levs = levels(results$region))

### Save
# Create output directory if necessary
if (!dir.exists("plots/pred_plots")) dir.create("plots/pred_plots")
dims <- list(width = 5.8, height = 8)
ggsave("plots/pred_plots/pd_pred_plot.png", pd_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/pdz_pred_plot.png", pdz_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/rpd_pred_plot.png", rpd_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/gpd_pred_plot.png", gp_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/sr_pred_plot.png", sr_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/gps_pred_plot.png", gps_plot, height = dims$height, width = dims$width, units = "in")
# Plots with coefficients from environment-only and core-only models
dims <- list(width = 5.4, height = 3.6)
ggsave("plots/pred_plots/pd2_pred_plot.png", pd2_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/pdz2_pred_plot.png", pdz2_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/rpd2_pred_plot.png", rpd2_plot, height = dims$height, width = dims$width, units = "in")
ggsave("plots/pred_plots/gpd2_pred_plot.png", gp2_plot, height = dims$height, width = dims$width, units = "in")

##### Supplemental tables

# Create output directory if necessary
if (!dir.exists("suppTabs")) dir.create("suppTabs")
#### Finalize the table of R2 values (Table S2)
### Order rows by region name (see top of script) and response
results$resp <- factor(x=results$resp, 
                       levels = c("pd_obs","pd_obs_z","rpd_obs_z","gpPD","SR","gpSR"))
results <- results[order(results$resp,results$region),]
## Update names for table
names(results)[3:4] <- c("r2_both", "r2_core")
### Save
write.csv(results,"suppTabs/R2Tab.csv", row.names = F)

####' Create tables the coefficients from each of the three models for each
####' response and region
### Make tables (S3-8)
pd_tab <- get_coefs(pd, pd2, pd3, levels(results$region)) %>% arrange(Region, Var)
pdz_tab <- get_coefs(pdz, pdz2, pdz3, levels(results$region)) %>% arrange(Region, Var)
rpd_tab <- get_coefs(rpd, rpd2, rpd3, levels(results$region)) %>% arrange(Region, Var)
gp_tab <- get_coefs(gp, gp2, gp3, levels(results$region)) %>% arrange(Region, Var)
sr_tab <- get_coefs(sr, sr2, sr3, levels(results$region)) %>% arrange(Region, Var)
gps_tab <- get_coefs(gps, gps2, gps3, levels(results$region)) %>% arrange(Region, Var)
### Save
write.csv(pd_tab, "suppTabs/coefTab_PD.csv", row.names = F)
write.csv(pdz_tab, "suppTabs/coefTab_sesPD.csv", row.names = F)
write.csv(rpd_tab, "suppTabs/coefTab_sesRPD.csv", row.names = F)
write.csv(gp_tab, "suppTabs/coefTab_gpPD.csv", row.names = F)
write.csv(sr_tab, "suppTabs/coefTab_SR.csv", row.names = F)
write.csv(gps_tab, "suppTabs/coefTab_gpSR.csv", row.names = F)
