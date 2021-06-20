# This R script rasterizes, clips, and reprojects geospatial data sets
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-02-21
rm(list=ls())
setwd('A:/research/')
require(raster)
require(maptools)
require(rgdal)
require(sf)
require(fasterize)
require(dplyr)

calc_mode <- function(v,...) {
  uniqv <- unique(v)
  as.numeric(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# LOAD DATA SETS ============================================================================================
# wus.wgs84.shp <- readOGR('data/boundaries/WUS_wgs84.shp')
wus.shp <- readOGR('data/boundaries/WUS_aea.shp')
wus.1km.r <- raster('data/boundaries/wus_aea_1km.img')

padus.v2.1.shp <- st_read('ecospatial_services/wus_forest_conservation/gis_data/usgs_padus_v2.1_wus_aea.shp')
padus.v2.cbi.shp <- st_read('data/admin/PADUSCBIEdition_v2/PADUSCBIEdition_v2_wus_aea.shp')

gepa.r <- raster('data/conservation_priorities/GreaterEcosystemsOfProtectedAreas_USA/data/commondata/raster_data/nCWD_sum_conus300extract_Reclass_usgsPRJ_CONUSextract.tif')
agc.r <- raster('data/biomass/wilson_usfs_carbon/Data/carbon_ag_mg_ha.img')
totc.r <- raster('data/biomass/wilson_usfs_carbon/Data/carbon_tot_mg_ha.img')
fortype.r <- raster('data/landcover/conus_forestgroup/conus_forestgroup.img')

vuln.drought.r <- raster('ecospatial_services/wus_fmec/gis/vulnerability/WUSA_BAU_drought_vulnerability_2020_2049_aea_4km.tif')
vuln.fire.r <- raster('ecospatial_services/wus_fmec/gis/vulnerability/WUSA_BAU_fire_vulnerability_2020_2049_aea_4km.tif')

water.imp.shp <- st_read('ecospatial_services/wus_forest_conservation/gis_data/usfs_forests_on_the_edge_surface_drinking_water_importance.shp')

# GREATER ECOSYSTEMS ============================================================================================
wus.gepa.r <- crop(gepa.r, wus.shp)
wus.gepa.r <- mask(wus.gepa.r, wus.shp)
wus.gepa.r <- raster::resample(wus.gepa.r, wus.1km.r)
wus.gepa.r[wus.gepa.r == 128] <- NA
wus.gepa.r
writeRaster(wus.gepa.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_greater_ecosystems_1km_aea.tif', overwrite=T)

# FOREST EXTENT ============================================================================================
wus.vuln.drought.4km.r <- crop(fortype.r, wus.shp)
wus.for.extent.250m.r <- wus.fortype.250m.r
wus.for.extent.250m.r <- wus.for.extent.250m.r > 0
wus.for.extent.1km.r <- raster::aggregate(wus.for.extent.250m.r, fact=4, fun=calc_mode)
wus.for.extent.1km.r <- raster::resample(wus.for.extent.1km.r, wus.1km.r, method = 'ngb')
wus.for.extent.1km.r <- raster::mask(wus.for.extent.1km.r, wus.1km.r)
writeRaster(wus.for.extent.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_extent_1km_aea.tif', overwrite=T)

wus.for.extent.1km.mask.r <- wus.for.extent.1km.r
wus.for.extent.1km.mask.r[wus.for.extent.1km.mask.r == 0] <- NA

# FOREST CARBON STOCKS ============================================================================================
# forest aboveground carbon 
wus.agc.r <- crop(agc.r, wus.shp)
wus.agc.r <- mask(wus.agc.r, wus.shp)
writeRaster(wus.agc.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_aboveground_carbon_MgHa_250m_aea.tif', overwrite=T)

wus.agc.mg.pxl.250m.r <- wus.agc.r * 6.25 # there are 6.25 ha per pixel (250 x 250 m) 
wus.agc.mg.pxl.1km.r <- raster::aggregate(wus.agc.mg.pxl.250m.r, fact=4, fun=sum)
wus.agc.mg.pxl.1km.r <- raster::resample(wus.agc.mg.pxl.1km.r, wus.1km.r, method = 'bilinear')
wus.agc.mg.pxl.1km.r[is.na(wus.for.extent.1km.mask.r)] <- 0
wus.agc.mg.pxl.1km.r <- mask(wus.agc.mg.pxl.1km.r, wus.1km.r)
writeRaster(wus.agc.mg.pxl.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_aboveground_carbon_MgPxl_1km_aea.tif', overwrite=T)

# forest total carbon
wus.totc.r <- crop(totc.r, wus.shp)
wus.totc.r <- mask(wus.totc.r, wus.shp)
wus.totc.mg.pxl.250m.r <- wus.totc.r * 6.25 # there are 6.25 ha per pixel (250 x 250 m)
wus.totc.mg.pxl.1km.r <- raster::aggregate(wus.totc.mg.pxl.250m.r, fact=4, fun=sum)
wus.totc.mg.pxl.1km.r <- raster::resample(wus.totc.mg.pxl.1km.r, wus.1km.r, method = 'bilinear')
wus.totc.mg.pxl.1km.r[is.na(wus.for.extent.1km.mask.r)] <- 0
wus.totc.mg.pxl.1km.r <- mask(wus.totc.mg.pxl.1km.r, wus.1km.r)
writeRaster(wus.totc.mg.pxl.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_total_carbon_MgPxl_1km_aea.tif', overwrite=T)

wus.totc.mg.ha.1km.r <- wus.totc.mg.pxl.1km.r/100
writeRaster(wus.totc.mg.ha.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_total_carbon_MgHa_1km_aea.tif', overwrite=T)


# PAD US ============================================================================================
unique(padus.v2.1.shp$Own_Type)
unique(padus.v2.cbi.shp$own_type)

unique(padus.v2.1.shp$Mang_Type)

owner.key <- data.table(owner.abb = c('FED','STAT','DESG','JNT','DIST','LOC','NGO','PVT','TRIB','UNK'),
                        owner.cbi = c('Federal Land','State Land','','Joint Ownership','','Local Land','Private Conservation Land','Private Land','Native American Land','Unknown'),
                        owner.code = 1:10)

mang.key <- data.table(mang.abb = c('FED','STAT','JNT','DIST','LOC','NGO','PVT','TRIB','UNK'),
                       mang.code = 1:9)

# PAD-US 2 CBI Owner Type
padus.v2.cbi.shp$owner.code <- owner.key$owner.code[match(padus.v2.cbi.shp$own_type, owner.key$owner.cbi)]
padus.v2.cbi.owner.code.1km.r <- fasterize(padus.v2.cbi.shp, wus.1km.r, field = 'owner.code', fun = 'min')
plot(padus.v2.cbi.owner.code.1km.r, col = topo.colors(9))

# PAD-US 2.1 Owner Type
padus.v2.1.shp$owner.code <- owner.key$owner.code[match(padus.v2.1.shp$Own_Type, owner.key$owner.abb)]
padus.v2.1.owner.code.1km.r <- fasterize(padus.v2.1.shp, wus.1km.r, field = 'owner.code', fun = 'min')
plot(padus.v2.1.owner.code.1km.r, col = topo.colors(9))

# PAD-US 2.1 Management Type
padus.v2.1.shp$mang.code <- mang.key$mang.code[match(padus.v2.1.shp$Mang_Type, mang.key$mang.abb)]
padus.v2.1.mang.code.1km.r <- fasterize(padus.v2.1.shp, wus.1km.r, field = 'mang.code', fun = 'min')
plot(padus.v2.1.mang.code.1km.r, col = rainbow(9))

# Fill grid cells in PAD-US 2.1 that have missing ownership (these are mostly all private lands)
padus.v2.1.owner.code.1km.r[is.na(padus.v2.1.owner.code.1km.r)==T] <- padus.v2.cbi.owner.code.1km.r[is.na(padus.v2.1.owner.code.1km.r)==T]
plot(padus.v2.1.owner.code.1km.r, col = rainbow(10))

# Fill grid cells in PAD-US 2.1 that have "Unknown" ownership using Management Type
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 1] <- 1
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 2] <- 2
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 3] <- 4
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 4] <- 5
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 5] <- 6
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 6] <- 7
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 7] <- 8
padus.v2.1.owner.code.1km.r[padus.v2.1.owner.code.1km.r == 10 & padus.v2.1.mang.code.1km.r == 8] <- 9

# write out
writeRaster(padus.v2.1.owner.code.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_owner_type_1km_aea.tif', overwrite=T)


# -------------

# PAD-US 2 CBI status code
padus.v2.cbi.shp$gap_sts <- as.numeric(padus.v2.cbi.shp$gap_sts)
padus.v2.cbi.shp$gap_sts[is.na(padus.v2.cbi.shp$gap_sts)] <- 5
padus.v2.cbi.gap.code.1km.r <- fasterize(padus.v2.cbi.shp, wus.1km.r, field = 'gap_sts', fun = 'min')
plot(padus.v2.cbi.gap.code.1km.r, col = rainbow(5))

# PAD-US 2.1 GAP status code
padus.v2.1.shp$GAP_Sts <- as.numeric(padus.v2.1.shp$GAP_Sts)
padus.v2.1.gap.code.1km.r <- fasterize(padus.v2.1.shp, wus.1km.r, field = 'GAP_Sts', fun = 'min')
plot(padus.v2.1.gap.code.1km.r, col = rainbow(4))

padus.v2.1.gap.code.1km.r[is.na(padus.v2.1.gap.code.1km.r)==T] <- padus.v2.cbi.gap.code.1km.r[is.na(padus.v2.1.gap.code.1km.r)==T]
plot(padus.v2.1.gap.code.1km.r, col = rainbow(5))

writeRaster(padus.v2.1.gap.code.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_gap_status_code_1km_aea.tif', overwrite=T)

# INVENTORIED ROADLESS AREA---------------
# PAD-US 2.1 GAP status code
padus.v2.1.ira.shp <- filter(padus.v2.1.shp, Des_Tp == 'IRA')
padus.v2.1.ira.shp$Des_Tp_num <- 1
padus.v2.1.ira.1km.r <- fasterize(padus.v2.1.ira.shp, wus.1km.r, field = 'Des_Tp_num', fun = 'min')
padus.v2.1.ira.1km.r[is.na(padus.v2.1.ira.1km.r)] <- 0
padus.v2.1.ira.1km.r <- mask(padus.v2.1.ira.1km.r, wus.1km.r)
writeRaster(padus.v2.1.ira.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_inventoried_roadless_areas_1km_aea.tif', overwrite=T)


# FOREST VULNERABILITY TO DROUGHT AND FIRE ============================================================================================
# vuln.drought.1km.r <- raster::disaggregate(vuln.drought.r, fact = 4)
# vuln.drought.1km.r <- raster::resample(vuln.drought.1km.r, wus.1km.r, method = 'ngb')
vuln.drought.1km.r <- raster::resample(vuln.drought.r, wus.1km.r, method = 'ngb')
val.cells <- raster::Which(vuln.drought.1km.r > 2, cells = T)
adj.cells <- adjacent(vuln.drought.1km.r, cells=val.cells, directions=8, pairs=F)
writeRaster(vuln.drought.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_vulnerability_drought_clm_bau_2020_2049_1km_aea.tif', overwrite=T)

vuln.fire.1km.r <- raster::disaggregate(vuln.fire.r, fact = 4)
vuln.fire.1km.r <- raster::resample(vuln.fire.r, wus.1km.r, method = 'ngb')
writeRaster(vuln.fire.1km.r, 'ecospatial_services/wus_forest_conservation/gis_data/wus_forest_vulnerability_fire_clm_bau_2020_2049_1km_aea.tif', overwrite=T)


# WATERSHED IMPORTANCE =================================================================================================================
water.imp.aea.shp <- st_transform(water.imp.shp, crs(wus.1km.r))
water.imp.aea.r <- fasterize(water.imp.aea.shp, wus.1km.r, field = 'IMP1')
writeRaster(water.imp.aea.r, 'ecospatial_services/wus_forest_conservation/gis_data/usfs_surface_drinking_water_importance_1km_aea.tif', overwrite = T)

# HOLDING TO PROBABLY DELETE ==================================================================================================================
