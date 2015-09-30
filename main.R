# Sprague River Basin - Water Quality Loads and Trends
# Jeffrey D Walker, PhD
# 2014-2015

# load data
source('load_detection_limits.R')
source('load_outliers.R')
source('load_network.R')

source('load_kt_sprague.R')
source('load_kt_sprague_synop.R')

source('load_gis.R')
source('load_nlcd.R')
source('load_geomorph.R')
source('load_pou.R')

source('load_ghcnd.R')
source('load_snotel.R')
source('load_owrd.R')
source('load_usgs.R')
source('load_prism.R')

# compute flows and loads
source('compute_flows.R')
source('compute_flow_precip.R')
source('compute_loads.R')

# plot datasets
source('plot_dataset_outliers.R')
source('plot_dataset_splots.R')
source('plot_dataset_timeseries.R')
# source('plot_synoptic.R')
source('plot_maps.R')

# source('plot_flow_ciy_method.R')

# plot met data
source('plot_nlcd.R')
source('plot_prism.R')
source('plot_snotel.R')

# plot loads
source('plot_loads_summary.R')
source('plot_loads_maps.R')
source('plot_loads_tiles.R')

source('plot_loads_network.R')
source('plot_loads_landuse.R')

# source('plot_loads_data.R')
# source('plot_loads_residuals.R')
# source('plot_loads_problems.R')

# compute and plot trends
source('compute_trends.R')
