# How these files were made
# BBL 2021-12-21

# install gdal
brew install gdal

# download clay 15-30 cm and SOC 0-30 cm and make geotiff
gdal_translate -of GTiff -co "TILED=YES" -co "COMPRESS=DEFLATE" -co "PREDICTOR=2" -co "BIGTIFF=YES" "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_15-30cm_mean.vrt" "clay_15-30cm_mean.tif"
gdal_translate -of GTiff -co "TILED=YES" -co "COMPRESS=DEFLATE" -co "PREDICTOR=2" -co "BIGTIFF=YES" "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data//ocs/ocs_0-30cm_mean.vrt" "ocs_0-30cm_mean.tif"


# Note Kanishka's moirai code is here:
# https://github.com/JGCRI/moirai/blob/carbon_detail_updates/ancillary/bash_scripts/get_soil_grids_mean.sh
