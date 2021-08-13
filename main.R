# Sprague River Basin - Water Quality Loads and Trends
# Jeffrey D Walker, PhD
# 2014-2015

source('functions.R')
# load data
source('load_detection_limits.R') # works
source('load_outliers.R') # works
source('load_network.R') # works

source('load_kt_sprague.R') # works

# TODO: update files below here for new package versions

source('load_kt_sprague_synop.R') # works

source('load_gis.R') # works
source('load_nlcd.R') # works, needs a few lines fixed (questions)
source('load_geomorph.R') # works
source('load_pou.R') # works

source('load_ghcnd.R') # works
source('load_snotel.R') # works
source('load_owrd.R') # works
source('load_usgs.R') # works
source('load_prism.R') # works

source('import_ghcnd.R') # Good, except there is a difference in site numbers and data available
source('import_snotel.R') # looks good
source('import_owrd.R') # looks good
#source('import_usgs.R')

# compute flows and loads
source('compute_flows.R') # works
source('compute_flow_precip.R') # works
source('compute_loads.R') # works

# plot datasets
source('plot_dataset_outliers.R') # works
source('plot_dataset_splots.R') # works
source('plot_dataset_timeseries.R') # works
source('plot_synoptic.R') # works
source('plot_maps.R') # WORKS

# source('plot_flow_ciy_method.R')

# plot met data
source('plot_nlcd.R') # works
source('plot_prism.R') # works
source('plot_snotel.R') # WORKS
source('plot_climate.R') # works

# plot loads
source('plot_loads_summary.R') # works when I open and run, but not from here ***
source('plot_loads_maps.R') # there is a difference in format between the site names in two of the dataframes, one a geometry file and one not. at this time, i cannot get them into the same format
source('plot_loads_tiles.R') # works

source('plot_loads_network.R') # works
source('plot_loads_landuse.R') # works

source('plot_nutrient_dynamics.R') # works
source('plot_gannett.R') # need one more file
source('plot_anthro.R') # works

# source('plot_loads_data.R')
# source('plot_loads_residuals.R')
# source('plot_loads_problems.R')

# compute and plot trends
source('compute_trends.R') # wq package not working, need to find alternate for seasonal kendall test
