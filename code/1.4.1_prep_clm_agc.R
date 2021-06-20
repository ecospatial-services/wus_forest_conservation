# This script prepares CLM simulation output for analysis. Specifically, subsetting, georeferencing, and reprojecting to western USA.
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-02-20
rm(list=ls())
setwd('A:/research/')
require(raster)
require(maptools)
require(rgdal)
require(R.utils)
aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80")
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

yrs.all <- 2015:2099
yois <- yrs.all[c(1,16,36)] # (2015, 2030, 2015)

# LOAD DATA SETS --------------------------------------------------------------------------------
wus.wgs84.shp <- readOGR('data/boundaries/WUS_wgs84.shp')
wus.aea.shp <- readOGR('data/boundaries/WUS_aea.shp')
wus.1km.r <- raster('data/boundaries/wus_states_aea_1km.tif')

# CLM domain extent rasters
lat.rst <- raster('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='LATIXY')
lon.rst <- raster('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='LONGXY')

agc.ipsl.bau.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='AGC')
agc.ipsl.bau.stk <- agc.ipsl.bau.stk[[c(1,16,36)]]
names(agc.ipsl.bau.stk) <- yois

agc.ipsl.noharv.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_noharvest_merge.nc', varname='AGC')
agc.ipsl.noharv.stk <- agc.ipsl.noharv.stk[[c(1,16,36)]]
names(agc.ipsl.noharv.stk) <- yois

agc.miroc.bau.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/MIROC_2015_2099_BAU_merge.nc', varname='AGC')
agc.miroc.bau.stk <- agc.miroc.bau.stk[[c(1,16,36)]]
names(agc.miroc.bau.stk) <- yois

agc.miroc.noharv.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/MIROC_2015_2099_noharvest_merge.nc', varname='AGC')
agc.miroc.noharv.stk <- agc.miroc.noharv.stk[[c(1,16,36)]]
names(agc.miroc.noharv.stk) <- yois


# GEOREFERENCE RASNTERS ------------------------------------------------------------------------------------------------
# specifcy domain extent
lat.min <- cellStats(lat.rst, min)
lat.max <- cellStats(lat.rst, max)
lon.min <- cellStats(lon.rst, min)
lon.max <- cellStats(lon.rst, max)

# georef
agc.ipsl.bau.2015 <- raster(as.matrix(agc.ipsl.bau.stk[[1]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.bau.2030 <- raster(as.matrix(agc.ipsl.bau.stk[[2]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.bau.2050 <- raster(as.matrix(agc.ipsl.bau.stk[[3]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.bau.stk <- stack(agc.ipsl.bau.2015, agc.ipsl.bau.2030, agc.ipsl.bau.2050)

agc.ipsl.noharv.2015 <- raster(as.matrix(agc.ipsl.noharv.stk[[1]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.noharv.2030 <- raster(as.matrix(agc.ipsl.noharv.stk[[2]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.noharv.2050 <- raster(as.matrix(agc.ipsl.noharv.stk[[3]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.ipsl.noharv.stk <- stack(agc.ipsl.noharv.2015, agc.ipsl.noharv.2030, agc.ipsl.noharv.2050)

agc.miroc.bau.2015 <- raster(as.matrix(agc.miroc.bau.stk[[1]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.bau.2030 <- raster(as.matrix(agc.miroc.bau.stk[[2]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.bau.2050 <- raster(as.matrix(agc.miroc.bau.stk[[3]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.bau.stk <- stack(agc.miroc.bau.2015, agc.miroc.bau.2030, agc.miroc.bau.2050)

agc.miroc.noharv.2015 <- raster(as.matrix(agc.miroc.noharv.stk[[1]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.noharv.2030 <- raster(as.matrix(agc.miroc.noharv.stk[[2]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.noharv.2050 <- raster(as.matrix(agc.miroc.noharv.stk[[3]]), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
agc.miroc.noharv.stk <- stack(agc.miroc.noharv.2015, agc.miroc.noharv.2030, agc.miroc.noharv.2050)


# MASK TO WESTERN USA -----------------------------------------------------------------------------------------
agc.ipsl.bau.stk <- mask(crop(agc.ipsl.bau.stk, wus.wgs84.shp), wus.wgs84.shp)
agc.ipsl.noharv.stk <- mask(crop(agc.ipsl.noharv.stk, wus.wgs84.shp), wus.wgs84.shp)
agc.miroc.bau.stk <- mask(crop(agc.miroc.bau.stk, wus.wgs84.shp), wus.wgs84.shp)
agc.miroc.noharv.stk <- mask(crop(agc.miroc.noharv.stk, wus.wgs84.shp), wus.wgs84.shp)


# REPORJECT TO LAEA AT 1 KM ------------------------------------------------------------------------------------
agc.ipsl.bau.aea.stk <- projectRaster(agc.ipsl.bau.stk, crs = aea, method = 'bilinear', res = 1000)
agc.ipsl.noharv.aea.stk <- projectRaster(agc.ipsl.noharv.stk, crs = aea, method = 'bilinear', res = 1000)
agc.miroc.bau.aea.stk <- projectRaster(agc.miroc.bau.stk, crs = aea, method = 'bilinear', res = 1000)
agc.miroc.noharv.aea.stk <- projectRaster(agc.miroc.noharv.stk, crs = aea, method = 'bilinear', res = 1000)


# NUDGE INTO ALIGNMENT WITH COMMON 1 KM GRID ---------------------------------------------------------------
agc.ipsl.bau.aea.stk <- raster::resample(agc.ipsl.bau.aea.stk, wus.1km.r, method = 'ngb')
agc.ipsl.noharv.aea.stk <- raster::resample(agc.ipsl.noharv.aea.stk, wus.1km.r, method = 'ngb')
agc.miroc.bau.aea.stk <- raster::resample(agc.miroc.bau.aea.stk, wus.1km.r, method = 'ngb')
agc.miroc.noharv.aea.stk <- raster::resample(agc.miroc.noharv.aea.stk, wus.1km.r, method = 'ngb')

# CONVERT CLM AGC from g C / m2 to Mg C / 1 km2 (pixel) ------------------------------------------------------------------------
# Mg C / pxl = (g C/ m2) * (10^3^2 m / km2) * (Mg / 10^6 g)

# WRITE OUT RASTERS ---------------------------------------------------------------------------------------
mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea')
writeRaster(agc.ipsl.bau.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_AGC_IPSL_BAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite = T)
writeRaster(agc.ipsl.noharv.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_AGC_IPSL_NOHARV_2015_2030_2050_MgPxl_1km_aea.tif', overwrite = T)
writeRaster(agc.miroc.bau.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_AGC_MIROC_BAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite = T)
writeRaster(agc.miroc.noharv.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_AGC_MIROC_NOHARV_2015_2030_2050_MgPxl_1km_aea.tif', overwrite = T)


# COMPUTE DIFFERNCES BETWEEN SCENARIOS (NO HARV VS BAU) AND TIME PERIOD (2030/2050 - 2015)------------------------------------------------------
mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/')
mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/')

# differences between scenarios
agc.ipsl.scen.dif.stk <- agc.ipsl.noharv.aea.stk - agc.ipsl.bau.aea.stk
agc.miroc.scen.dif.stk <- agc.miroc.noharv.aea.stk - agc.miroc.bau.aea.stk 
agc.ens.scen.dif.stk <- (agc.ipsl.scen.dif.stk + agc.miroc.scen.dif.stk) / 2

writeRaster(agc.ipsl.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_AGC_IPSL_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.miroc.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_AGC_MIROC_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.ens.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_AGC_ENSEMBLE_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)

# differences between time periods
agc.ipsl.bau.yr.dif.stk <- agc.ipsl.bau.aea.stk[[c(2,3)]] - agc.ipsl.bau.aea.stk[[1]]
agc.ipsl.noharv.yr.dif.stk <- agc.ipsl.noharv.aea.stk[[c(2,3)]] - agc.ipsl.noharv.aea.stk[[1]]
agc.miroc.bau.yr.dif.stk <- agc.miroc.bau.aea.stk[[c(2,3)]] - agc.miroc.bau.aea.stk[[1]]
agc.miroc.noharv.yr.dif.stk <- agc.miroc.noharv.aea.stk[[c(2,3)]] - agc.miroc.noharv.aea.stk[[1]]
agc.ens.bau.yr.dif <- (agc.ipsl.bau.yr.dif.stk + agc.miroc.bau.yr.dif.stk) / 2
agc.ens.noharv.yr.dif <- (agc.ipsl.noharv.yr.dif.stk + agc.miroc.noharv.yr.dif.stk) / 2


writeRaster(agc.ipsl.bau.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_IPSL_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.ipsl.noharv.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_IPSL_NOHARV_2030and2050_minus_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.miroc.bau.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_MIROC_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.miroc.noharv.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_MIROC_NOHARV_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.ens.bau.yr.dif, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_ENS_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
writeRaster(agc.ens.noharv.yr.dif, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_AGC_ENS_NOHARV_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)


# END SCRIPT -------------------------------------------------------------------------------------------------
