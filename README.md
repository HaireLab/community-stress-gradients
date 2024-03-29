# community-stress-gradients
Finding out where and when Community Matters!

The readme file is a place to keep the project description and a list of the goals of the analysis. It includes a directory of code and data.

Overall research question:
The purpose of this study is to fully examine our dataset from 13 burned areas located across the western US to address emerging questions regarding ecosystem trajectories and transformation following severe fire in dry pine mixed conifer forests. The particular focus here is on how plant community affects post-fire seedling establishment, and determine whether community effects vary across local and regional climate gradients using the Stress Gradient Hypothesis as an anchor.

The stress gradient hypothesis posits that facilitation relationships will be more common in stressful environments, and that these convert to competitive relationships in more productive/benign environments. Stress gradients may be linear or curvilinear. Amelioration of a given stressor is probably maximized at moderate stress rather than at extreme levels. Particular topical in the context of climate change, thinking about water stress and temperature stresses.

An archive of the data can be found here:
Miller, Carol; Krawchuk, Meg A.; Coop, Jonathan D.; Downing, William M.; Walker, Ryan B.; Haire, Sandra L.; Chong, Geneva; Whitman, Ellen; Parisien, Marc-André. 2021. Field and spatial data for: Understanding the role of fire refugia in promoting ecosystem resilience of dry forests in the western United States. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2021-0003

### Directory of data and code

#### Data summaries and exploratory analyses

data.summaries.Rmd (and data.summaries.html): First look at the field data (in progress)

#### PCA climate space
climate.space.bioclim.R: use bioclimatic data to run pca for the extent of the study region. Now using 1991-2020 normals.
csplots.pcs.R: use the output from the above to plot the location of the study burns in the climate space (x=PC1, y=PC2)
    example output: pcscatter3.png
    
#### Climate Water Deficit 
terraclimate.data.R: download netCDF data and crop to regions with sample points from 13 study sites and extract point values across post-fire years

def.data.assembly.R: calculate statistics of the overall distribution of Def values at the sample points across post-fire years

plot.def.stats.R: plot statistics of the overall distribution of def values at each sample point across post-fire years

#### Tree age graphs

trees.and.spei.plots_2002.R: For sites burned in 2002, assemble the data and plot SPEI, year of origin (derived from internode counts), and heat load index 

#### Terrain metrics
terrain.functions.R: calculate metrics with raster and RSAGA 

