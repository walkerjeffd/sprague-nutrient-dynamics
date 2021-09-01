# Sprague River Basin - Water Quality Loads and Trends
# Jeffrey D Walker, PhD
# 2014-2015

source('functions.R')
# load data
source('load_detection_limits.R')
source('load_outliers.R')
source('load_network.R')

source('load_kt_sprague.R')

# TODO: update files below here for new package versions

source('load_kt_sprague_synop.R') # address this part of the project in the next meeting

source('load_gis.R')
source('load_nlcd.R')
source('load_geomorph.R')
source('load_pou.R')

source('load_ghcnd.R') # difference in site numbers and data available
source('load_snotel.R')
source('load_owrd.R')
source('load_usgs.R')
source('load_prism.R')

# compute flows and loads
source('compute_flows.R') # updated, figures marked with -update to compare with original output
source('compute_flow_precip.R') # updated, check with Jeff and Jake on WYEAR grouping in figures
source('compute_loads.R')

# plot datasets
source('plot_dataset_outliers.R') # plots look good w up to 2020 data
source('plot_dataset_splots.R')
source('plot_dataset_timeseries.R')
source('plot_synoptic.R')
source('plot_maps.R')

# source('plot_flow_ciy_method.R')

# plot met data
#### UPDATE THESE TO REFLECT NEW DATA IMPORT
source('plot_nlcd.R')
source('plot_prism.R')
source('plot_snotel.R')
source('plot_climate.R')

# plot loads
source('plot_loads_summary.R') # works when I open and run, but not from here ***
source('plot_loads_maps.R') # there is a difference in format between the site names in two of the dataframes, one a geometry file and one not. at this time, i cannot get them into the same format, needs additional troubleshooting
source('plot_loads_tiles.R') # works

source('plot_loads_network.R')
source('plot_loads_landuse.R')

source('plot_nutrient_dynamics.R')
source('plot_gannett.R') # need one more file
source('plot_anthro.R')

# source('plot_loads_data.R')
# source('plot_loads_residuals.R')
# source('plot_loads_problems.R')

# compute and plot trends
source('compute_trends.R') # wq package not working, need to find alternate for seasonal kendall test
