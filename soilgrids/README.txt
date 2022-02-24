# How these files were made
# BBL 2021-12-21

The RMarkdown analysis depends on two GeoTIFF files that we made
# from the SoilGrids dataset. These are files are very large and
# not included in the repository, but here's how to make them:

# install gdal
> brew install gdal

> gdalinfo --version
GDAL 3.3.3, released 2021/10/25

# download clay 15-30 cm and make geotiff
> gdal_translate -of GTiff -co "TILED=YES" -co "COMPRESS=DEFLATE" -co "PREDICTOR=2" -co "BIGTIFF=YES" "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/clay/clay_15-30cm_mean.vrt" "clay_15-30cm_mean.tif"

# same thing for SOC 0-30 cm
> gdal_translate -of GTiff -co "TILED=YES" -co "COMPRESS=DEFLATE" -co "PREDICTOR=2" -co "BIGTIFF=YES" "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data//ocs/ocs_0-30cm_mean.vrt" "ocs_0-30cm_mean.tif"

# Based on Kanishka Narayan's gdal_translate code for morai
