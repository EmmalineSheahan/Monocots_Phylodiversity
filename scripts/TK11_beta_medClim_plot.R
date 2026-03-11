################################################################################
#####' Data analysis scripts for https://doi.org/10.1111/geb.70209:
#####' 11. TK11_beta_medClim_plot
#####' 
#####' This script uses the within / among distances from 09_beta_medClim and 
#####' the PERMANOVA results from 10_beta_perman to create the box plots on 
#####' phylogenetic turnover and nestedness between the core and buffer zones
#####' of each region for Figure 4 and the taxic equivalent for Fig S12. Run
#####' separately for phylobeta and taxicbeta. Any "ver" or "agg2" appendages
#####' will be removed automatically.
#####' 
#####' Written by Taliesin (Tal) Kinser in 2025
#####' University of Florida / Florida Museum of Natural History
###### tkinser@ufl.edu
################################################################################

#### Set up
## Set to "phylobeta" or "taxicbeta"
fh = "phylobeta"
## Libraries
#setwd("enter/working_dir/here/") # all code assumes a common working directory
library(tidyverse)
library(patchwork)
## File directory
indir <- "output/med_clim"

#### Get together permanova results
### Read in all the result files
files <- list.files(indir, pattern = "permanova", full.names = T)
perm <- lapply(files[grep(fh,files)], readRDS)
names(perm) <- c("Aus","Cal","Chi","Med","SAf")
### Make into a large data frame
perm <- lapply(perm, as.data.frame)
perm <- bind_rows(perm, .id = "region")
### Prep data frame
## Keep only necessary rows and remove names
perm <- perm[grep("med_clim",rownames(perm)),]
rownames(perm) <- NULL
## Clean column names and keep only necessary ones
colnames(perm) <- gsub("Pr\\.\\.F\\.","PrF",colnames(perm))
colnames(perm) <- gsub("\\.","_",colnames(perm))
perm <- perm[,grep("R2|F|region",colnames(perm))]
## Set up plotting columns
perm <- perm %>% 
  tidyr::pivot_longer(cols = -1, names_to = c("pop","metric","stat_type"), 
                      values_to = c("stat"), names_sep = "_")
perm <- perm %>% pivot_wider(names_from = stat_type, values_from = stat)
## Prepare data for plotting
perm$R2 <- round(perm$R2, 2)
perm$R2[perm$R2<0] = 0
perm$PrF[perm$PrF>=0.05]=""
perm$PrF[perm$PrF!=""]="**"
## Rename/order regions
perm$region <- factor(x=perm$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                      labels = c("WNA","Med","Chi","SAf","Aus"))
### Separate out into the four groups
perm_all_sim <- perm %>% filter(pop == "all" & metric == "sim") %>% select("region","R2","PrF")
perm_all_sne <- perm %>% filter(pop == "all" & metric == "sne") %>% select("region","R2","PrF")
perm_geo_sim <- perm %>% filter(pop == "geo" & metric == "sim") %>% select("region","R2","PrF")
perm_geo_sne <- perm %>% filter(pop == "geo" & metric == "sne") %>% select("region","R2","PrF")

#### Get the nestedness (SNE) and turnover (Simpson) data
files <- list.files(indir, pattern = paste0(fh, "Core"), full.names = T)
### SNE all taxa
sne_all <- lapply(files[grep("sne_all",files)], read.csv)
names(sne_all) <- c("Aus","Cal","Chi","Med","SAf")
## Set up for plotting
sne_all <- bind_rows(sne_all, .id = "region")
sne_all <- sne_all %>% 
  tidyr::pivot_longer(cols = 3:4, names_to = c("pop","group"), 
                      values_to = "beta", names_sep = "_") %>% select("region","group","beta")
sne_all$group <- factor(sne_all$group, levels = c("w","a")) 
sne_all$region <- factor(x=sne_all$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                         labels = c("WNA","Med","Chi","SAf","Aus"))
### SNE geophytes
sne_geo <- lapply(files[grep("sne_geo",files)], read.csv)
names(sne_geo) <- c("Aus","Cal","Chi","Med","SAf")
sne_geo <- bind_rows(sne_geo, .id = "region")
sne_geo <- sne_geo %>% 
  tidyr::pivot_longer(cols = 3:4, names_to = c("pop","group"), 
                      values_to = "beta", names_sep = "_") %>% select("region","group","beta")
sne_geo$group <- factor(sne_geo$group, levels = c("w","a")) 
sne_geo$region <- factor(x=sne_geo$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                         labels = c("WNA","Med","Chi","SAf","Aus"))
### Simpson all taxa
sim_all <- lapply(files[grep("sim_all",files)], read.csv)
names(sim_all) <- c("Aus","Cal","Chi","Med","SAf")
sim_all <- bind_rows(sim_all, .id = "region")
sim_all <- sim_all %>% 
  tidyr::pivot_longer(cols = 3:4, names_to = c("pop","group"), 
                      values_to = "beta", names_sep = "_") %>% select("region","group","beta")
sim_all$group <- factor(sim_all$group, levels = c("w","a")) 
sim_all$region <- factor(x=sim_all$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                         labels = c("WNA","Med","Chi","SAf","Aus"))
### Simpson geophytes
sim_geo <- lapply(files[grep("sim_geo",files)], read.csv)
names(sim_geo) <- c("Aus","Cal","Chi","Med","SAf")
sim_geo <- bind_rows(sim_geo, .id = "region")
sim_geo <- sim_geo %>% 
  tidyr::pivot_longer(cols = 3:4, names_to = c("pop","group"), 
                      values_to = "beta", names_sep = "_") %>% select("region","group","beta")
sim_geo$group <- factor(sim_geo$group, levels = c("w","a")) 
sim_geo$region <- factor(x=sim_geo$region, levels = c("Cal","Med","Chi","SAf","Aus"),
                         labels = c("WNA","Med","Chi","SAf","Aus"))

#### Make plots
if (fh == "phylobeta") app = "phylo" else app = ""
### Simpson all taxa
p1 <- ggplot(sim_all) +
  geom_boxplot(mapping = aes(x = region, y = beta, col = group), fill=NA, 
               outlier.alpha =0.2, outlier.size = 0.5) +
  scale_color_manual(values = c("blue","red"), 
                     labels = c("w"="Core v Core","a"="Core v Buff.")) +
  geom_text(data=perm_all_sim, mapping = aes(x=region,y=1,label = paste0(R2,PrF))) +
  labs(x = "Region", y = paste0(app, "\u03b2 (All taxa)"), col = "Comparison", 
       title = "Turnover") + theme(axis.text.x = element_blank())
### SNE all taxa
p2 <- ggplot(sne_all) +
  geom_boxplot(mapping = aes(x = region, y = beta, col = group), fill=NA, 
               outlier.alpha =0.2, outlier.size = 0.5) +
  scale_color_manual(values = c("blue","red"), 
                     labels = c("w"="Core v Core","a"="Core v Buff.")) +
  geom_text(data=perm_all_sne, mapping = aes(x=region,y=1,label = paste0(R2,PrF))) +
  labs(x = "Region", y = paste0(app, "\u03b2 (All taxa)"), col = "Comparison", 
       title = "Nestedness") + theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank())
### Simpson geophytes 
p3 <- ggplot(sim_geo) +
  geom_boxplot(mapping = aes(x = region, y = beta, col = group), fill=NA, 
               outlier.alpha =0.2, outlier.size = 0.5) +
  scale_color_manual(values = c("blue","red"), 
                     labels = c("w"="Core v Core","a"="Core v Buff.")) +
  geom_text(data=perm_geo_sim, mapping = aes(x=region,y=1,label = paste0(R2,PrF))) +
  labs(x = "Region", y = paste0(app, "\u03b2 (Geophytes)"), col = "Comparison")
### SNE geophytes
p4 <- ggplot(sne_geo) +
  geom_boxplot(mapping = aes(x = region, y = beta, col = group), fill=NA, 
               outlier.alpha =0.2, outlier.size = 0.5) +
  scale_color_manual(values = c("blue","red"), 
                     labels = c("w"="Core v Core","a"="Core v Buff.")) +
  geom_text(data=perm_geo_sne, mapping = aes(x=region,y=1,label = paste0(R2,PrF))) +
  labs(x = "Region", y = paste0(app, "\u03b2 (Geophytes)"), col = "Comparison") +
  theme(axis.text.y = element_blank())
### Make full plot and save
p <- p1 + p2 + p3 + p4 + 
  plot_layout(axis_titles = "collect", ncol=2, nrow = 2, guide = "collect") &
  theme(panel.background = element_rect(fill = "white"), 
        #panel.grid.major.y = element_line(color = "gray50"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), plot.margin = unit(c(.1,.1,.1,.1),'lines'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) & ylim(c(0,1))
ggsave(paste0("plots/", fh, "_analysis_fig.png"), p, height = 6, width = 9, 
       units = "in", dpi = "retina")                                                                           
