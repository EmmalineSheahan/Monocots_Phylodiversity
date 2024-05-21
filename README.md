# Monocots_Phylodiversity
A repository for code written to conduct Phylogenetic diversity, Phylogenetic endemism, and CANAPE analysis on Monocots in five Mediterranean biomes

1.	Rob_code.R - Creates buffers for species with too few points to adequately model. The generated shapefiles were plotted and filtered manually in order to ensure all ranges are appropriate. Ranges which could not be cleaned and redone are stored in the failed_buffer_plants.csv file.
2.  sdm_crs_transformation_and_directory_partition.R - eparates the results of the SDMs by region, converts the rasters to the Mollweide projection, aligns their extents to the global extent, removes species manually checked to be problematic.
3.	Richness_maps.R - Creates raster stacks of each species per region, produces a richness raster and plot for each region.
4.	Pdfs_for_manual_checking.R - Creates a pdf of the plots of each species raster for each region
5.	Calculating_tax_endemism.R - Calculates and plots the taxonomic endemism for each region
6.	Geophytes.R - Calculates and plots the proportion of geophytes in each region
7.	Pam_tree_for_biodiverse.R - Creates presence-absence matrices for each region, as well as prunes the overall tree to create region specific trees. 
8.	Endemism_pams.R - Creates presence-absence matrices for each region where the extent is the extent of the largest ranged species known to exist in a given region. This way, the full ranges of the species can be considered when calculating endemism. 
9.	Plot_diodiverse_results.R - Uses the results imported from PD and PE analyses done in biodiverse to create plots of PD, RPD, PD per Taxon, PE, RPE, and the significance values for those variables
10.	Canape_plot.R - Uses the results imported from PE analyses done in biodiverse to plot the Categorical Analysis of Paleo and Neo Endemism
