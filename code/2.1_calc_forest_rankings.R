# This R script
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-03-21
rm(list=ls())
require(raster)
require(data.table)
require(ggplot2)
require(dplyr)
require(rgdal)
require(R.utils)
setwd('A:/research/ecospatial_services/wus_forest_conservation/')
mkdirs('gis_data/forest_conservation_priority_ranks') # create output directory for rasters 

# LOAD DATA SETS --------------------------------------------------------------------------------------------
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')
clm.pxl.dt <- fread('output/wus_clm_cnep_by_pxl_model_scenario_period.csv')

wus.shp <- readOGR('A:/research/data/boundaries/WUS_aea.shp')
wus.r <- raster('A:/research/data/boundaries/wus_aea_1km.img')
wus.r[wus.r == 1] <- NA

# subset to forest areas
forest.pxl.dt <- pxl.dt[forest.extent == 1]
clm.pxl.dt <- clm.pxl.dt[pxl.id %in% forest.pxl.dt$pxl.id]

# Add state names, ecoregion names, and forest vulnerability to the CLM data table
clm.pxl.dt$state.name <- pxl.dt$state.name[match(clm.pxl.dt$pxl.id, pxl.dt$pxl.id)]
clm.pxl.dt$ecoreg.name <- pxl.dt$ecoreg.name[match(clm.pxl.dt$pxl.id, pxl.dt$pxl.id)]
clm.pxl.dt$fire.vuln <- pxl.dt$fire.vuln[match(clm.pxl.dt$pxl.id, pxl.dt$pxl.id)]
clm.pxl.dt$drought.vuln <- pxl.dt$drought.vuln[match(clm.pxl.dt$pxl.id, pxl.dt$pxl.id)]

# WRITE OUT SEVERAL OTHER RASTERS FOR FORESTLANDS =====================================================

# current protected forests
current.gap1.r <- wus.r
forest.cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code == 1]
current.gap1.r[forest.cpa.pxl.dt$pxl.id] <- 1
writeRaster(current.gap1.r, 'gis_data/wus_padus_v2.1_gap_status_1_forest_1km_aea.tif', overwrite=T)

current.gap12.r <- wus.r
forest.cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2]
current.gap12.r[forest.cpa.pxl.dt$pxl.id] <- 1
writeRaster(current.gap12.r, 'gis_data/wus_padus_v2.1_gap_status_1and2_forest_1km_aea.tif', overwrite=T)

# forest ownership
forest.owner.r <- wus.r
forest.owner.pxl.dt <- pxl.dt[forest.extent == 1]
forest.owner.r[forest.owner.pxl.dt$pxl.id] <- forest.owner.pxl.dt$owner.code
writeRaster(forest.owner.r, 'gis_data/wus_padus_v2.1_owner_type_forest_1km_aea.tif', overwrite=T)

# high forest vulnerability to drought
forest.high.drought.vuln.r <- wus.r
forest.high.drought.vuln.dt <- pxl.dt[forest.extent == 1 & drought.vuln == 6]
forest.high.drought.vuln.r[forest.high.drought.vuln.dt$pxl.id] <- 1
plot(forest.high.drought.vuln.r)
writeRaster(forest.high.drought.vuln.r, 'gis_data/wus_forest_vulnerability_drought_high_clm_bau_2020_2049_1km_aea.tif', overwrite=T)

# high forest vulnerability to fire
forest.high.fire.vuln.r <- wus.r
forest.high.fire.vuln.dt <- pxl.dt[forest.extent == 1 & fire.vuln == 6]
forest.high.fire.vuln.r[forest.high.fire.vuln.dt$pxl.id] <- 1
plot(forest.high.fire.vuln.r)
writeRaster(forest.high.fire.vuln.r, 'gis_data/wus_forest_vulnerability_fire_high_clm_bau_2020_2049_1km_aea.tif', overwrite=T)

# high forest vulnerability to drought or fire
forest.high.vuln.r <- sum(forest.high.drought.vuln.r, forest.high.fire.vuln.r, na.rm=T)
forest.high.vuln.r[forest.high.vuln.r == 2 ] <- 1
forest.high.vuln.r <- mask(forest.high.vuln.r, wus.shp)
plot(forest.high.vuln.r)
writeRaster(forest.high.vuln.r, 'gis_data/wus_forest_vulnerability_fire_or_drought_high_clm_bau_2020_2049_1km_aea.tif', overwrite=T)

# COMPUTE FOREST CARBON AND BIODIVERSITY PRIORITY RANKS WITH AND WITHOUT VULNERABILITY MASK ===========================
output.lst <- list()
mask.vuln <- c('no','yes')
for (i in mask.vuln){
  
  # MASK OR DON'T MASK HIGH VULNERABILITY AREAS ---------------------------------------------------
  forest.pxl.dt[, vuln.mask := i] # record whether vulnerability is being masked
  
  if (i == 'yes'){
    forest.pxl.dt[fire.vuln == 6 | drought.vuln == 6, ':='(totc.MgPxl = 0, tree.rich = 0, amphib.rich = 0, bird.rich = 0, mammal.rich = 0, reptile.rich = 0)]
  }
  
  # COMPUTE FOREST CARBON RANKS -------------------------------------------------------------------
  
  # compute rank of cumulative change in forest NEP
  clm.ens.noharv.2050.dt <- clm.pxl.dt[model == "ENSEMBLE" & scenario == "NOHARV" & year == 'x2050']
  clm.ens.noharv.2050.dt[, cnep.2050.rank := percent_rank(cnep.MgPxl), by = c('state.name','ecoreg.name')]
  
  # f masking vulnerability, then set cnep rank to zero in high vulnerability areas. 
  if (i == 'yes'){clm.ens.noharv.2050.dt[fire.vuln == 6 | drought.vuln == 6, cnep.2050.rank := 0]}
  
  # push cnep rank in main data.table
  forest.pxl.dt$cnep.2050.MgPxl <- clm.ens.noharv.2050.dt$cnep.MgPxl[match(forest.pxl.dt$pxl.id, clm.ens.noharv.2050.dt$pxl.id)]
  
  forest.pxl.dt$cnep.2050.rank <- clm.ens.noharv.2050.dt$cnep.2050.rank[match(forest.pxl.dt$pxl.id, clm.ens.noharv.2050.dt$pxl.id)]
  forest.pxl.dt[is.na(cnep.2050.rank), cnep.2050.rank := 0]
  
  # compute rank for current total forest carbon 
  forest.pxl.dt[, totc.rank := percent_rank(totc.MgPxl), by = c('state.name','ecoreg.name')]
  
  # compute overall forest AGC rank
  forest.pxl.dt[, carbon.rank := percent_rank((cnep.2050.rank + totc.rank)), by = c('state.name','ecoreg.name')]
  
  
  # COMPUTE BIODIVERSITY RANKS ------------------------------------------------------
  
  # compute biodiversity richness ranks by state
  forest.pxl.dt[, ':='(tree.rich.rank = percent_rank(tree.rich),
                amphib.rich.rank = percent_rank(amphib.rich),
                bird.rich.rank = percent_rank(bird.rich),
                mammal.rich.rank = percent_rank(mammal.rich),
                reptile.rich.rank = percent_rank(reptile.rich)), by = c('state.name','ecoreg.name')]
  
  # compute animal diversity rank by summing ranks across vertebrate taxa and the re-ranking
  forest.pxl.dt[, animal.rich.rank := percent_rank((amphib.rich.rank + bird.rich.rank + mammal.rich.rank + reptile.rich.rank)), by = state.name]
  
  # compute oveall biodiversity rank by summing ranks across trees and vertebrates, then re-ranking 
  forest.pxl.dt[, biodiv.rank := percent_rank((tree.rich.rank + animal.rich.rank)), by = state.name]
  
  
  # COMPUTE COMPOSITE CARBON + BIODIVERSITY RANK  ---------------------------------
  forest.pxl.dt[, forest.rank := percent_rank((carbon.rank + biodiv.rank)), by = c('state.name','ecoreg.name')]
  
  # STORE DATA TABLE WITH RANKS IN OUTPUT LIST -------------------------------------
  output.lst[[i]] <- data.frame(forest.pxl.dt)
  
  # SPATIALIZE FOREST CARBON AND BIODIVERSITY RANKS ---------------------------------
  vars.to.map <- c("cnep.2050.rank", "totc.rank", "carbon.rank", "tree.rich.rank", "amphib.rich.rank", "bird.rich.rank", "mammal.rich.rank",
                   "reptile.rich.rank", "animal.rich.rank", "biodiv.rank", "forest.rank")
  
  for (j in vars.to.map){
    r <- wus.r
    r[forest.pxl.dt$pxl.id] <- forest.pxl.dt[[j]]
    plot(r, main = j)
    outname <- paste0('gis_data/forest_conservation_priority_ranks/wus_forest_', gsub("\\.","_", j), '_stateBYecoreg_', i, '_mask_vuln_1km_aea.tif')
    writeRaster(r, outname, overwrite=T)
  }
}

# WRITE OUT FILE WITH RANKINGS --------------------------------------------------
forest.pxl.ranks.dt <- rbindlist(output.lst)
fwrite(forest.pxl.ranks.dt, 'output/wus_geospatial_data_by_pxl_with_rankings.csv')

# END SCRIPT -------------------------------------------------------------------------------