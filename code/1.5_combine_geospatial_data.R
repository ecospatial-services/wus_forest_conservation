# This script combines 1 km resolution raster geospatial data sets for the western United States into one large data table for subsequent analysis.   
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-03-21
rm(list=ls())
setwd('A:/research/')
require(raster)
require(data.table)
require(ggplot2)
require(dplyr)

# LOAD DATA SETS --------------------------------------------------------------------------------------------
wus.r <- raster('data/boundaries/wus_aea_1km.img')
wus.r[wus.r == 1] <- NA

# state boundaries
states.r <- raster('data/boundaries/wus_states_aea_1km.tif')
state.key <- data.table(state.code = 1:11, state.name = c('WA','OR','CA','ID','NV','UT','AZ','MT','WY','CO','NM'))
state.order <- c('AZ','CA','CO','ID','MT','NV','NM','OR','UT','WA','WY')

# EPA ecoregions
ecoreg.r <- raster('data/landcover/us_eco_l3_201708/wus_eco_l3_aea_1km.tif')
ecoreg.key <- fread('data/landcover/us_eco_l3_201708/wus_eco_l3_key.csv')
names(ecoreg.key) <- c('ecoreg.code','ecoreg.name','area_m2')

# USGS GAP status, ownership, inventoried roadless 
gap.code.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_gap_status_code_1km_aea.tif')

owner.code.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_owner_type_1km_aea.tif')
owner.key <- data.table(owner.code = 1:10, owner.abb = c('FED','STAT','DESG','JNT','DIST','LOC','NGO','PVT','TRIB','UNK'))

gap.ira.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_padus_v2.1_inventoried_roadless_areas_1km_aea.tif')

# USFS FIA forest extent
for.extent.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_forest_extent_1km_aea.tif')

# USFS FIA tree AGC
agc.wil.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_forest_aboveground_carbon_MgPxl_1km_aea.tif')
totc.wil.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_forest_total_carbon_MgPxl_1km_aea.tif')

# Forest vulnerability to drought or fire
drought.vuln.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_forest_vulnerability_drought_clm_bau_2020_2049_1km_aea.tif')
fire.vuln.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/wus_forest_vulnerability_fire_clm_bau_2020_2049_1km_aea.tif')

# USFS FIA tree species richness 
tree.rich.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_richness_1km/fia_tree_species_richness_1km_aea.tif')

# USGS GAP species richness
amphib.rich.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/gap_species_habitat/species_richness_1km/wus_gap_amphibian_species_richness_1km_aea.tif')
bird.rich.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/gap_species_habitat/species_richness_1km/wus_gap_bird_species_richness_1km_aea.tif')
mammal.rich.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/gap_species_habitat/species_richness_1km/wus_gap_mammal_species_richness_1km_aea.tif')
reptile.rich.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/gap_species_habitat/species_richness_1km/wus_gap_reptile_species_richness_1km_aea.tif')

# Drinking water importance
water.imp.r <- raster('ecospatial_services/wus_forest_conservation/gis_data/usfs_surface_drinking_water_importance_1km_aea.tif')

# EXTRACT DATA FROM RASTERS ----------------------------------------------------------- 
pxl.dt <- data.table(pxl.id = 1:ncell(states.r))
pxl.dt$state.code <- values(states.r)
pxl.dt$ecoreg.code <- values(ecoreg.r)
pxl.dt$gap.code <- values(gap.code.r)
pxl.dt$owner.code <- values(owner.code.r)
pxl.dt$gap.ira <- values(gap.ira.r)
pxl.dt$forest.extent <- values(for.extent.r)
pxl.dt$agc.MgPxl <- values(agc.wil.r)
pxl.dt$totc.MgPxl <- values(totc.wil.r)
pxl.dt$drought.vuln <- values(drought.vuln.r)
pxl.dt$fire.vuln <- values(fire.vuln.r)

# species richness
pxl.dt$tree.rich <- values(tree.rich.r)
pxl.dt$amphib.rich <- values(amphib.rich.r)
pxl.dt$bird.rich <- values(bird.rich.r)
pxl.dt$mammal.rich <- values(mammal.rich.r)
pxl.dt$reptile.rich <- values(reptile.rich.r)

# watershed importance 
pxl.dt$water.imp <- values(water.imp.r)

# add keys to decipother pixel specific data
pxl.dt <- pxl.dt[state.key, on = 'state.code']
pxl.dt <- pxl.dt[ecoreg.key, on = 'ecoreg.code']
pxl.dt <- pxl.dt[owner.key, on = 'owner.code']

# 
pxl.dt <- pxl.dt[is.na(forest.extent) == F]

# write out
fwrite(pxl.dt, 'ecospatial_services/wus_forest_conservation/output/wus_geospatial_data_by_pxl.csv')

# END SCRIPT -----------------------------------------------------------------------