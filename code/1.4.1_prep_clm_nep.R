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
yoi <- c(2020,2030,2050)
yoi.indx <- which(yrs.all %in% yoi)

# LOAD DATA SETS --------------------------------------------------------------------------------
wus.wgs84.shp <- readOGR('data/boundaries/WUS_wgs84.shp')
wus.aea.shp <- readOGR('data/boundaries/WUS_aea.shp')
wus.1km.r <-  raster('data/boundaries/wus_aea_1km.img')

# CLM domain extent rasters
lat.rst <- raster('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='LATIXY')
lon.rst <- raster('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='LONGXY')

nep.ipsl.bau.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_BAU_merge.nc', varname='NEP')
names(nep.ipsl.bau.stk) <- yrs.all

nep.ipsl.noharv.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/IPSL_2015_2099_noharvest_merge.nc', varname='NEP')
names(nep.ipsl.noharv.stk) <- yrs.all

nep.miroc.bau.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/MIROC_2015_2099_BAU_merge.nc', varname='NEP')
names(nep.miroc.bau.stk) <- yrs.all

nep.miroc.noharv.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/MIROC_2015_2099_noharvest_merge.nc', varname='NEP')
names(nep.miroc.noharv.stk) <- yrs.all


# COMPUTE CUMULATIVE NEP FROM 2020 TO BOTH 2030 AND 2050 --------------------------------------------------------------
# Convert from g C / m2 / second to g C /m2 / year (i.e., multiply by 3.15*10^7 seconds / year) and then sum across years
cnep.ipsl.bau.2030 <- sum(nep.ipsl.bau.stk[[yoi.indx[1]:yoi.indx[2]]]*(3.15*10^7))
cnep.ipsl.bau.2050 <- sum(nep.ipsl.bau.stk[[yoi.indx[1]:yoi.indx[3]]]*(3.15*10^7))

cnep.ipsl.noharv.2030 <- sum(nep.ipsl.noharv.stk[[yoi.indx[1]:yoi.indx[2]]]*(3.15*10^7))
cnep.ipsl.noharv.2050 <- sum(nep.ipsl.noharv.stk[[yoi.indx[1]:yoi.indx[3]]]*(3.15*10^7))

cnep.miroc.bau.2030 <- sum(nep.miroc.bau.stk[[yoi.indx[1]:yoi.indx[2]]]*(3.15*10^7))
cnep.miroc.bau.2050 <- sum(nep.miroc.bau.stk[[yoi.indx[1]:yoi.indx[3]]]*(3.15*10^7))

cnep.miroc.noharv.2030 <- sum(nep.miroc.noharv.stk[[yoi.indx[1]:yoi.indx[2]]]*(3.15*10^7))
cnep.miroc.noharv.2050 <- sum(nep.miroc.noharv.stk[[yoi.indx[1]:yoi.indx[3]]]*(3.15*10^7))


# GEOREFERENCE RASTERS ------------------------------------------------------------------------------------------------
# specifcy domain extent
lat.min <- cellStats(lat.rst, min)
lat.max <- cellStats(lat.rst, max)
lon.min <- cellStats(lon.rst, min)
lon.max <- cellStats(lon.rst, max)

cnep.ipsl.bau.2030 <- raster(as.matrix(cnep.ipsl.bau.2030), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.ipsl.bau.2050 <- raster(as.matrix(cnep.ipsl.bau.2050), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.ipsl.bau.stk <- stack(cnep.ipsl.bau.2030, cnep.ipsl.bau.2050)

cnep.ipsl.noharv.2030 <- raster(as.matrix(cnep.ipsl.noharv.2030), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.ipsl.noharv.2050 <- raster(as.matrix(cnep.ipsl.noharv.2050), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.ipsl.noharv.stk <- stack(cnep.ipsl.noharv.2030, cnep.ipsl.noharv.2050)

cnep.miroc.bau.2030 <- raster(as.matrix(cnep.miroc.bau.2030), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.miroc.bau.2050 <- raster(as.matrix(cnep.miroc.bau.2050), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.miroc.bau.stk <- stack(cnep.miroc.bau.2030, cnep.miroc.bau.2050)

cnep.miroc.noharv.2030 <- raster(as.matrix(cnep.miroc.noharv.2030), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.miroc.noharv.2050 <- raster(as.matrix(cnep.miroc.noharv.2050), xmn=lon.min-360, xmx=lon.max-360, ymn=lat.min, ymx=lat.max, crs=wgs84)
cnep.miroc.noharv.stk <- stack(cnep.miroc.noharv.2030, cnep.miroc.noharv.2050)

# DISAGGREGATE FROM ~4 km to ~1 km RESOLUTION (but here still in degrees) -----------------------------------------
cnep.ipsl.bau.stk <- raster::disaggregate(cnep.ipsl.bau.stk, fact = 4)
cnep.ipsl.noharv.stk <- raster::disaggregate(cnep.ipsl.noharv.stk, fact = 4)
cnep.miroc.bau.stk <- raster::disaggregate(cnep.miroc.bau.stk, fact = 4)
cnep.miroc.noharv.stk <- raster::disaggregate(cnep.miroc.noharv.stk, fact = 4)

# REPORJECT TO LAEA AT 1 KM ------------------------------------------------------------------------------------
cnep.ipsl.bau.aea.stk <- projectRaster(cnep.ipsl.bau.stk, wus.1km.r, crs = aea, method = 'bilinear', res = 1000)
cnep.ipsl.noharv.aea.stk <- projectRaster(cnep.ipsl.noharv.stk, wus.1km.r, crs = aea, method = 'bilinear', res = 1000)
cnep.miroc.bau.aea.stk <- projectRaster(cnep.miroc.bau.stk, wus.1km.r, crs = aea, method = 'bilinear', res = 1000)
cnep.miroc.noharv.aea.stk <- projectRaster(cnep.miroc.noharv.stk, wus.1km.r, crs = aea, method = 'bilinear', res = 1000)

# MASK TO WESTERN USA -----------------------------------------------------------------------------------------
cnep.ipsl.bau.aea.stk <- mask(cnep.ipsl.bau.aea.stk, wus.aea.shp)
cnep.ipsl.noharv.aea.stk <- mask(cnep.ipsl.noharv.aea.stk, wus.aea.shp)
cnep.miroc.bau.aea.stk <- mask(cnep.miroc.bau.aea.stk, wus.aea.shp)
cnep.miroc.noharv.aea.stk <- mask(cnep.miroc.noharv.aea.stk, wus.aea.shp)

# NOTE THAT CUMULATIVE NEP IS IN UNITS OF g C / m2 WHICH IS THE SAME AS Mg C / 1 km2 (pixel) ------------------------------------------------------------------------
# UNIT CONVERSION: Mg C / pxl = (g C/ m2) * (10^3^2 m / km2) * (Mg / 10^6 g)

# WRITE OUT RASTERS ---------------------------------------------------------------------------------------
mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea')
writeRaster(cnep.ipsl.bau.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_IPSL_BAU_2020_to_2030_2050_MgCPxl_1km_aea.tif', overwrite = T)
writeRaster(cnep.ipsl.noharv.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_IPSL_NOHARV_2020_to_2030_2050_MgCPxl_1km_aea.tif', overwrite = T)
writeRaster(cnep.miroc.bau.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_MIROC_BAU_2020_to_2030_2050_MgCPxl_1km_aea.tif', overwrite = T)
writeRaster(cnep.miroc.noharv.aea.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_MIROC_NOHARV_2020_to_2030_2050_MgCPxl_1km_aea.tif', overwrite = T)

# # COMPUTE DIFFERNCES BETWEEN SCENARIOS (NO HARV VS BAU) AND TIME PERIOD (2030/2050 - 2015)------------------------------------------------------
# mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/')
# mkdirs('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/')
# 
# # differences between scenarios
# cnep.ipsl.scen.dif.stk <- cnep.ipsl.noharv.aea.stk - cnep.ipsl.bau.aea.stk
# cnep.miroc.scen.dif.stk <- cnep.miroc.noharv.aea.stk - cnep.miroc.bau.aea.stk 
# cnep.ens.scen.dif.stk <- (cnep.ipsl.scen.dif.stk + cnep.miroc.scen.dif.stk) / 2
# 
# writeRaster(cnep.ipsl.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_cnep_IPSL_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.miroc.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_cnep_MIROC_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.ens.scen.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/scenario_difs/CLM_cnep_ENSEMBLE_HARVminusBAU_2015_2030_2050_MgPxl_1km_aea.tif', overwrite=T)
# 
# # differences between time periods
# cnep.ipsl.bau.yr.dif.stk <- cnep.ipsl.bau.aea.stk[[c(2,3)]] - cnep.ipsl.bau.aea.stk[[1]]
# cnep.ipsl.noharv.yr.dif.stk <- cnep.ipsl.noharv.aea.stk[[c(2,3)]] - cnep.ipsl.noharv.aea.stk[[1]]
# cnep.miroc.bau.yr.dif.stk <- cnep.miroc.bau.aea.stk[[c(2,3)]] - cnep.miroc.bau.aea.stk[[1]]
# cnep.miroc.noharv.yr.dif.stk <- cnep.miroc.noharv.aea.stk[[c(2,3)]] - cnep.miroc.noharv.aea.stk[[1]]
# cnep.ens.bau.yr.dif <- (cnep.ipsl.bau.yr.dif.stk + cnep.miroc.bau.yr.dif.stk) / 2
# cnep.ens.noharv.yr.dif <- (cnep.ipsl.noharv.yr.dif.stk + cnep.miroc.noharv.yr.dif.stk) / 2
# 
# 
# writeRaster(cnep.ipsl.bau.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_IPSL_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.ipsl.noharv.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_IPSL_NOHARV_2030and2050_minus_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.miroc.bau.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_MIROC_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.miroc.noharv.yr.dif.stk, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_MIROC_NOHARV_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.ens.bau.yr.dif, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_ENS_BAU_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
# writeRaster(cnep.ens.noharv.yr.dif, 'ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/period_difs/CLM_cnep_ENS_NOHARV_2030and2050_minus_2015_MgPxl_1km_aea.tif', overwrite=T)
# 
# 
# # END SCRIPT -------------------------------------------------------------------------------------------------
