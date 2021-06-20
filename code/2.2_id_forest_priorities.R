# This R script
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-03-21
rm(list=ls())
require(raster)
require(data.table)
require(ggplot2)
require(viridis)
require(dplyr)
require(rgdal)
require(R.utils)
setwd('A:/research/ecospatial_services/wus_forest_conservation/')

# LOAD DATA SETS --------------------------------------------------------------------------------------------
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')
forest.pxl.dt <- fread('output/wus_geospatial_data_by_pxl_with_rankings.csv')
clm.pxl.dt <- fread('output/wus_clm_cnep_by_pxl_model_scenario_period.csv')

wus.shp <- readOGR('A:/research/data/boundaries/WUS_aea.shp')
wus.r <- raster('A:/research/data/boundaries/wus_aea_1km.img')
wus.r[wus.r == 1] <- NA

# SPECIFY WHAT IS HIGH VULNERABILITY 
pxl.dt <- pxl.dt[, vuln.high := as.character()]
pxl.dt <- pxl.dt[drought.vuln == 6, vuln.high := 'yes']
pxl.dt <- pxl.dt[fire.vuln == 6, vuln.high := 'yes']
pxl.dt <- pxl.dt[is.na(vuln.high), vuln.high := 'no']

gap.status <- c(1,2) # used as 1 or 1&2
cons.targets <- c(30,50)
mask.high.vuln <- c('no','yes')

# i = gap.status[1]
# j = cons.targets[1]
# k = mask.high.vuln[1]

outlist <- list()
cnt = 1
for (i in gap.status){
  
  # COMPUTE CURRENT EXTENT OF FORESTLAND AND PROTECTED FOREST LAND BY STATE ==================================================
  setorder(pxl.dt, state.name)
  state.cpa.smry.dt <- pxl.dt[, .(total.Mha = round(.N*10^-4, 2)), by = state.name]
  state.cpa.smry.dt$total.cpa.Mha <- pxl.dt[gap.code <= i, .(total.cpa.Mha = round(.N*10^-4, 2)), by = state.name][,2]
  state.cpa.smry.dt[, total.cpa.pcnt := round(total.cpa.Mha/total.Mha*100,1)]
  state.cpa.smry.dt$forest.Mha <- pxl.dt[forest.extent == 1, .(total.Mha = round(.N*10^-4, 2)), by = state.name][,2]
  state.cpa.smry.dt[, forest.pcnt := round(forest.Mha/total.Mha*100,1)]
  state.cpa.smry.dt$forest.cpa.Mha <- pxl.dt[forest.extent == 1 & gap.code <= i, .(forest.cpa.Mha = round(.N*10^-4, 2)), by = state.name][,2]
  state.cpa.smry.dt[, forest.cpa.pcnt := round(forest.cpa.Mha/forest.Mha*100,1)]
  
  region.cpa.smry.dt <- pxl.dt[, .(state.name = 'region', total.Mha = round(.N*10^-4, 2))]
  region.cpa.smry.dt$total.cpa.Mha <- pxl.dt[gap.code <= i, .(total.cpa.Mha = round(.N*10^-4, 2))][,1]
  region.cpa.smry.dt[, total.cpa.pcnt := round(total.cpa.Mha/total.Mha*100,1)]
  region.cpa.smry.dt$forest.Mha <- pxl.dt[forest.extent == 1, .(total.Mha = round(.N*10^-4, 2))][,1]
  region.cpa.smry.dt[, forest.pcnt := round(forest.Mha/total.Mha*100,1)]
  region.cpa.smry.dt$forest.cpa.Mha <- pxl.dt[forest.extent == 1 & gap.code <= i, .(forest.cpa.Mha = round(.N*10^-4, 2))][,1]
  region.cpa.smry.dt[, forest.cpa.pcnt := round(forest.cpa.Mha/forest.Mha*100,1)]
  
  state.cpa.smry.dt <- rbind(state.cpa.smry.dt, region.cpa.smry.dt)
  state.cpa.smry.dt

  # IDENTIFY AMOUNT OF LAND NEEDED TO HIT CONSERVATION target ======================================================================================
  for (j in cons.targets){
    # Addional land needed to hit conservation targets
    state.cpa.smry.dt[, ':='(total.cpa.deficit.Mha = round((total.Mha * (j-total.cpa.pcnt)/100),1),
                                total.cpa.deficit.pcnt = round((j-total.cpa.pcnt),1),
                                forest.cpa.deficit.Mha = round((forest.Mha * (j-forest.cpa.pcnt)/100),1),
                                forest.cpa.deficit.pcnt = round((j-forest.cpa.pcnt),1))]
    
    # write out summary table
    if (i == 1){
      outname <- paste0('output/state_land_forest_protection_GAP_1_target_',j,'_pcnt_summary.csv')
      } else {outname <- paste0('output/state_land_forest_protection_GAP_1and2_target_',j,'_pcnt_summary.csv')}
    
    state.cpa.smry.dt
    fwrite(state.cpa.smry.dt, outname)
    
    # IDENTIFY LOCATION OF LAND NEEDED TO HIT CONSERVATION targetS WITH AND WITHOUT CONSIDERING FOREST VULNERABILITY ==========================
    for (k in mask.high.vuln){
      
      # select forest rankings computed with or without the vulnerability mask  
      forest.pxl.vuln.dt <- forest.pxl.dt[vuln.mask == k]
      
      # get current protected area deficit into form needed for analysis
      state.cpa.deficit <- state.cpa.smry.dt[, c("state.name","forest.cpa.deficit.Mha")]
      state.cpa.deficit[, ':='(forest.cpa.deficit.km2 = forest.cpa.deficit.Mha*10^4)]
      state.cpa.deficit <- state.cpa.deficit[, c("forest.cpa.deficit.Mha") := NULL]
      state.cpa.deficit <- state.cpa.deficit[state.name != 'region']
      
      # add CPA deficit to geospatial data
      forest.pxl.vuln.dt <- forest.pxl.vuln.dt[state.cpa.deficit, on = 'state.name']
      
      # select forest pixels that are not CPA
      forest.cnpa.pxl.dt <- forest.pxl.vuln.dt[gap.code > i]
      
      # each pixel is 1km2  
      forest.cnpa.pxl.dt[, area.km2 := 1]
      
      # compute cumulative areas summing across forest rank by state
      setorder(forest.cnpa.pxl.dt, state.name, -forest.rank)
      forest.cnpa.pxl.dt[, forest.rank.cum.area.km2 := cumsum(area.km2), by = state.name]
      
      # compute cumulative areas summing across carbon rank by state
      setorder(forest.cnpa.pxl.dt, state.name, -carbon.rank)
      forest.cnpa.pxl.dt[, carbon.rank.cum.area.km2 := cumsum(area.km2), by = state.name]
      
      # compute cumulative areas summing across biodiversity rank by state
      setorder(forest.cnpa.pxl.dt, state.name, -biodiv.rank)
      forest.cnpa.pxl.dt[, biodiv.rank.cum.area.km2 := cumsum(area.km2), by = state.name]
      
      # identify pixels needed to reach conservation targets
      forest.cnpa.pxl.dt[forest.rank.cum.area.km2 <= forest.cpa.deficit.km2, forest.add.target := 1]
      forest.cnpa.pxl.dt[carbon.rank.cum.area.km2 <= forest.cpa.deficit.km2, carbon.add.target := 1]
      forest.cnpa.pxl.dt[biodiv.rank.cum.area.km2 <= forest.cpa.deficit.km2, biodiv.add.target := 1]
      
      forest.pa.add.pxl.dt <- forest.cnpa.pxl.dt[, c('pxl.id','state.name','owner.abb','gap.ira','forest.add.target','carbon.add.target','biodiv.add.target')]
      forest.pa.add.pxl.dt <- melt(forest.pa.add.pxl.dt, id.vars = c('pxl.id','state.name','owner.abb','gap.ira'), variable.name = 'priority', value.name = 'area.km2')
      forest.pa.add.pxl.dt[, priority := as.character(priority)]
      forest.pa.add.pxl.dt[, target := j] 
      forest.pa.add.pxl.dt[, priority := substr(priority, 1, 6)]
      forest.pa.add.pxl.dt[, gap.status := i]
      forest.pa.add.pxl.dt[, vuln.mask := k]
      forest.pa.add.pxl.dt <- forest.pa.add.pxl.dt[is.na(area.km2) == F]
      
      # save to output list
      setcolorder(forest.pa.add.pxl.dt, c('gap.status','target','priority','vuln.mask','state.name', 'owner.abb', 'area.km2','pxl.id'))
      outlist[[cnt]] <- forest.pa.add.pxl.dt
      
      # spatialize 
      mkdirs('gis_data/forest_to_meet_conservation_targets')
      forest.add.target.r <- wus.r
      forest.add.target.r[forest.cnpa.pxl.dt$pxl.id] <- forest.cnpa.pxl.dt$forest.add.target
      plot(forest.add.target.r)
      outname <- paste0('gis_data/forest_to_meet_conservation_targets/wus_forest_gap_',i,'_to_meet_target_',j,'_pcnt_forest_ranking_', k, '_vuln_mask_1km_aea.tif')
      writeRaster(forest.add.target.r, outname, overwrite=T)
      
      carbon.add.target.r <- wus.r
      carbon.add.target.r[forest.cnpa.pxl.dt$pxl.id] <- forest.cnpa.pxl.dt$carbon.add.target
      plot(carbon.add.target.r)
      outname <- paste0('gis_data/forest_to_meet_conservation_targets/wus_forest_gap_',i,'_to_meet_target_',j,'_pcnt_carbon_ranking_', k, '_vuln_mask_1km_aea.tif')
      writeRaster(carbon.add.target.r, outname, overwrite=T)
      
      biodiv.add.target.r <- wus.r
      biodiv.add.target.r[forest.cnpa.pxl.dt$pxl.id] <- forest.cnpa.pxl.dt$biodiv.add.target
      plot(biodiv.add.target.r)
      outname <- paste0('gis_data/forest_to_meet_conservation_targets/wus_forest_gap_',i,'_to_meet_target_',j,'_pcnt_biodiv_ranking_', k, '_vuln_mask_1km_aea.tif')
      writeRaster(biodiv.add.target.r, outname, overwrite=T)
      
      cnt = cnt + 1
    }
  }  
}

forest.pa.add.all.pxl.dt <- rbindlist(outlist)
fwrite(forest.pa.add.all.pxl.dt, 'output/wus_forest_preservation_priority_additions_to_reach_targets_by_pxl.csv')


# SUMMARIZE FOREST VULNERABILITY BY STATE ==========================================================================================================
state.vuln.smry.dt <- pxl.dt[, .(total.Mha = round(.N*10^-4, 2)), by = state.name]
state.vuln.smry.dt$forest.Mha <- pxl.dt[forest.extent == 1, .(total.Mha = round(.N*10^-4, 2)), by = state.name][,2]
state.vuln.smry.dt[, forest.pcnt := round(forest.Mha/total.Mha*100,1)]
state.vuln.smry.dt$fire.vuln.high.Mha <- pxl.dt[forest.extent == 1 & fire.vuln == 6, .(fire.vuln.high.Mha = round(.N*10^-4, 2)), by = state.name][,2]
state.vuln.smry.dt[, fire.vuln.high.pcnt := round(fire.vuln.high.Mha / forest.Mha * 100, 1)]
state.vuln.smry.dt$drought.vuln.high.Mha <- pxl.dt[forest.extent == 1 & drought.vuln == 6, .(fire.vuln.high.Mha = round(.N*10^-4, 2)), by = state.name][,2]
state.vuln.smry.dt[, drought.vuln.high.pcnt := round(drought.vuln.high.Mha / forest.Mha * 100, 1)]
state.vuln.smry.dt$vuln.high.Mha <- pxl.dt[forest.extent == 1 & vuln.high == 'yes', .(vuln.high.Mha = round(.N*10^-4, 2)), by = state.name][,2]
state.vuln.smry.dt[, vuln.high.pcnt := round(vuln.high.Mha / forest.Mha * 100, 1)]

region.vuln.smry.dt <- pxl.dt[, .(state.name = 'region', total.Mha = round(.N*10^-4, 2))]
region.vuln.smry.dt$forest.Mha <- pxl.dt[forest.extent == 1, .(total.Mha = round(.N*10^-4, 2))][,1]
region.vuln.smry.dt[, forest.pcnt := round(forest.Mha/total.Mha*100, 1)]
region.vuln.smry.dt$fire.vuln.high.Mha <- pxl.dt[forest.extent == 1 & fire.vuln == 6, .(fire.vuln.high.Mha = round(.N*10^-4, 2))][,1]
region.vuln.smry.dt[, fire.vuln.high.pcnt := round(fire.vuln.high.Mha / forest.Mha * 100, 1)]
region.vuln.smry.dt$drought.vuln.high.Mha <- pxl.dt[forest.extent == 1 & drought.vuln == 6, .(fire.vuln.high.Mha = round(.N*10^-4, 2))][,1]
region.vuln.smry.dt[, drought.vuln.high.pcnt := round(drought.vuln.high.Mha / forest.Mha * 100, 1)]
region.vuln.smry.dt$vuln.high.Mha <- pxl.dt[forest.extent == 1 & vuln.high == 'yes', .(vuln.high.Mha = round(.N*10^-4, 2))][,1]
region.vuln.smry.dt[, vuln.high.pcnt := round(vuln.high.Mha / forest.Mha * 100, 1)]

state.vuln.smry.dt <- rbind(state.vuln.smry.dt, region.vuln.smry.dt)
state.vuln.smry.dt

fwrite(state.vuln.smry.dt, 'output/state_forest_vulnerability_summary.csv')

print('All finished!!!')


# SUMMARIZE CURRENT FOREST PROTECTION FOR EACH ECOREGION ==============================================================================================
setorder(pxl.dt, ecoreg.name)
ecoreg.cpa.smry.dt <- pxl.dt[, .(total.Mha = round(.N*10^-4, 2)), by = ecoreg.name]
ecoreg.cpa.smry.dt$total.cpa.Mha <- pxl.dt[gap.code <= 2, .(total.cpa.Mha = round(.N*10^-4, 2)), by = ecoreg.name][,2]
ecoreg.cpa.smry.dt[, total.cpa.pcnt := round(total.cpa.Mha/total.Mha*100,1)]
ecoreg.cpa.smry.dt$forest.Mha <- pxl.dt[forest.extent == 1, .(total.Mha = round(.N*10^-4, 2)), by = ecoreg.name][,2]
ecoreg.cpa.smry.dt[, forest.pcnt := round(forest.Mha/total.Mha*100,1)]
ecoreg.forest.cpa.smry.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2, .(forest.cpa.Mha = round(.N*10^-4, 2)), by = ecoreg.name]
ecoreg.cpa.smry.dt$forest.cpa.Mha <- ecoreg.forest.cpa.smry.dt$forest.cpa.Mha[match(ecoreg.cpa.smry.dt$ecoreg.name, ecoreg.forest.cpa.smry.dt$ecoreg.name)]
ecoreg.cpa.smry.dt[is.na(forest.cpa.Mha), forest.cpa.Mha := 0]
ecoreg.cpa.smry.dt[, forest.cpa.pcnt := round(forest.cpa.Mha/forest.Mha*100,1)]
ecoreg.cpa.smry.dt[is.na(forest.cpa.pcnt), forest.cpa.pcnt := 0]
ecoreg.cpa.smry.dt

fwrite(ecoreg.cpa.smry.dt, 'output/ecoreg_land_forest_protection_GAP_1and2.csv')

# summarize 
# ecoreg.cpa.smry.dt <- fread('output/ecoreg_land_forest_protection_GAP_1and2.csv')
forest.ecoreg.cap.smry.dt <- ecoreg.cpa.smry.dt[forest.pcnt >= 1]
n.forest.ecoregs <- nrow(forest.ecoreg.cap.smry.dt) # atleast 1% forest cover
n.forest.ecoreg.cpa.gte30pcnt <- nrow(forest.ecoreg.cap.smry.dt[forest.cpa.pcnt >= 30])
n.forest.ecoreg.cpa.gte50pcnt <- nrow(forest.ecoreg.cap.smry.dt[forest.cpa.pcnt >= 50])

n.forest.ecoregs
n.forest.ecoreg.cpa.gte30pcnt
n.forest.ecoreg.cpa.gte50pcnt
n.forest.ecoreg.cpa.gte30pcnt/n.forest.ecoregs*100
n.forest.ecoreg.cpa.gte50pcnt/n.forest.ecoregs*100

# COMPUTE CORRELATION BETWEEN FOREST CARBON AND BIODIVERSITY RANKS FOR EACH ECOREGION ------
# by ecoregion ---------
priority.cors <- forest.pxl.ranks.dt[, .(r = cor.test(carbon.rank, biodiv.rank, method = 'spearman')$estimate, 
                                         p = round(cor.test(carbon.rank, biodiv.rank, method = 'spearman')$p.value,3)),
                                         by = c('ecoreg.name','vuln.mask')]
priority.cors <- priority.cors[ecoreg.name %in% forest.ecoreg.cap.smry.dt$ecoreg.name] # subset to forested ecoregions
setorder(priority.cors, vuln.mask, r)
priority.cors

round(fivenum(priority.cors[vuln.mask == 'no']$r, na.rm = T),2)
round(fivenum(priority.cors[vuln.mask == 'yes']$r, na.rm = T),2)

# by state -----------
priority.cors <- forest.pxl.ranks.dt[, .(r = cor.test(carbon.rank, biodiv.rank, method = 'spearman')$estimate, 
                                         p = round(cor.test(carbon.rank, biodiv.rank, method = 'spearman')$p.value,3)),
                                     by = c('state.name','vuln.mask')]
setorder(priority.cors, vuln.mask, r)
priority.cors

round(fivenum(priority.cors[vuln.mask == 'no']$r, na.rm = T),2)
round(fivenum(priority.cors[vuln.mask == 'yes']$r, na.rm = T),2)

# END SCRIPT ==================================================================================================