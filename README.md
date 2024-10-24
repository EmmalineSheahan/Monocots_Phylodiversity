# Monocots_Phylodiversity
A repository for code written to conduct Phylogenetic diversity, Phylogenetic endemism, and CANAPE analysis on Monocots in five Mediterranean biomes

Code written by Emmaline Sheahan:
1.	Rob_code.R - Creates buffers for species with too few points to adequately model. The generated shapefiles were plotted and filtered manually in order to ensure all ranges are appropriate. Ranges which could not be cleaned and redone are stored in the failed_buffer_plants.csv file.
2.  sdm_crs_transformation_and_directory_partition.R - separates the results of the SDMs by region, converts the rasters to the Mollweide projection, aligns their extents to the global extent, removes species manually checked to be problematic.
3.	Richness_maps.R - Creates raster stacks of each species per region, produces a richness raster and plot for each region.
4.	Pdfs_for_manual_checking.R - Creates a pdf of the plots of each species raster for each region
5.	Calculating_tax_endemism.R - Calculates and plots the taxonomic endemism for each region
6.	Geophytes.R - Calculates and plots the proportion of geophytes in each region
7.	Pam_tree_for_biodiverse.R - Creates presence-absence matrices for each region, as well as prunes the overall tree to create region specific trees. 
8.	Endemism_pams.R - Creates presence-absence matrices for each region where the extent is the extent of the largest ranged species known to exist in a given region. This way, the full ranges of the species can be considered when calculating endemism. 
9.	Plot_diodiverse_results.R - Uses the results imported from PD and PE analyses done in biodiverse to create plots of PD, RPD, PD per Taxon, PE, RPE, and the significance values for those variables
10.	Canape_plot.R - Uses the results imported from PE analyses done in biodiverse to plot the Categorical Analysis of Paleo and Neo Endemism

Code written by Narayani Barve:
1.  01_cleanCoordinates.R - script detailing how downloaded coordinates are processed and cleaned for SDM
2.  02_defineAccessibleArea.R - script for a function which defines the accessible area for SDM
3.  03_clipModelLayers.R - script containing a function to clip environmental layers to a species' accessible area
4.  04_selectModelVariables.R - script detailing how environmental variables are selcted for each species' SDM
5.  05_save_sdmOutputsTSS.R - script for a function which selects and saves SDM model outputs of interest
6.  06_create_sdmFigure.R - script which outputs a figure of the SDM produced for a given species
7.  A1_SaveEnvData.R - the script where environmental data is clipped to the accessible area of each species and saved
8.  GenSpList_WCVP.R - this script ensures that taxonomic names match between lists for species being modelled
9.  GenSpTable_WCVP.R - this script ensures matching between gbif and idigbio records
10.  Get_MonoDist_WCVP.r - this script ensures that only monocots are used for modeling given information from the wcvp
11.  ModelWithMaxent.R - this script generates Maxent models of each species
12.  Proc_WCVP.R - this script involves taxonomic name cleaning and organization using information from the wcvp
13.  Proc_WFO1.R - this script involves taxonomic name cleaning and organization using information from WFO
14.  RandomCheck_WCVP.R - checks monocot species occurrence record counts on gbif
15.  Rem_GenSpList_WCVP.R - downloads data from gbif for species which were previously missing
16.  SpNamesWCVP.R - finds monocot taxa in wcvp
17.  WCVP_list.R - gets accepted and synonym names for taxa from wcvp
18.  WithOver_SpatialJoin.R - tests results of certain cleaning decisions
19.  WorkFlow.R - where the full SDM pipeline is run
