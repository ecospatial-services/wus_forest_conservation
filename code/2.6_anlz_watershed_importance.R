# This script summarizes waterhed importance for drinking water across preservation targets and scenarios
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-06-1
rm(list=ls())
setwd('A:/research/ecospatial_services/wus_forest_conservation/')
require(raster)
require(data.table)
require(ggplot2)
require(ggpubr)
require(viridis)
require(dplyr)

# LOAD FILES =============================================================================================
cons.add.pxl.dt <- fread('output/wus_forest_preservation_priority_additions_to_reach_targets_by_pxl.csv')
pxl.dt <- fread('output/wus_geospatial_data_by_pxl.csv')

# percent rank of each watershed for drinking water importance
pxl.dt$water.imp.rank <- percent_rank(pxl.dt$water.imp)

# drop some unneaded columns
cons.add.pxl.dt <- cons.add.pxl.dt[, c('state.name', 'owner.abb', 'area.km2','gap.ira') :=  NULL]
cons.add.pxl.dt[, status := 'proposed']
cons.add.pxl.dt <- cons.add.pxl.dt[gap.status == 2] # focus on lands protected under GAP 1 or 2 
cons.add.pxl.dt[, gap.status := NULL]

clm.pxl.dt <- clm.pxl.dt[year != 'x2030']
# clm.pxl.dt <- clm.pxl.dt[scenario != 'BAU']

# identify currently protected forest
cpa.pxl.dt <- pxl.dt[forest.extent == 1 & gap.code <= 2]

# variables needed when expanding the CPA data table to represent all targets/priorities/vulns/models 
priorities <- unique(cons.add.pxl.dt$priority)
targets <- unique(cons.add.pxl.dt$target)
vuln.mask <- c('no','yes')
pxls <- cpa.pxl.dt$pxl.id


# SURFACE DRINKING WATER IMPORTANCE =============================================================================================

# expand current protected area data table
cpa.pxl.expanded.dt <- data.table(expand.grid(pxl.id =  pxls, target = 0, priority = priorities, vuln.mask = vuln.mask))
cpa.pxl.expanded.dt[, status := 'current']

# expand current protected area data table again so these can be summed with preservation targets
cpa.all.priorities.dt <- data.table(expand.grid(pxl.id =  pxls, target = targets, priority = priorities, vuln.mask = vuln.mask))
cpa.all.priorities.dt[, status := 'proposed']

# join together current and proposed land areas 
cons.pxl.dt <- rbind(cpa.pxl.expanded.dt, cons.add.pxl.dt, cpa.all.priorities.dt)

# append total ecosystem carbon to data table
cons.pxl.dt$water.imp.rank <- pxl.dt$water.imp.rank[match(cons.pxl.dt$pxl.id, pxl.dt$pxl.id)]

# compute 
water.imp.rank.thresh <- 0.75
high.water.imp.smry <- pxl.dt[water.imp.rank >= water.imp.rank.thresh, .(area.km2 = .N)]

forest.high.water.imp.smry <- pxl.dt[forest.extent == 1 & water.imp.rank >= water.imp.rank.thresh, .(area.km2 = .N)]
forest.cpa.high.water.imp.smry <- pxl.dt[forest.extent == 1 & gap.code <= 2 & water.imp.rank >= water.imp.rank.thresh, .(area.km2 = .N)]

forest.high.water.imp.smry/high.water.imp.smry # fraction of high water importance that occurs in forest
forest.cpa.high.water.imp.smry/high.water.imp.smry # fraction of all high water importance lands that occur in protected forest

forest.cpa.high.water.imp.smry/forest.high.water.imp.smry # fraction of high water importance forest that is protected 

water.imp.smry.dt <- cons.pxl.dt[water.imp.rank >= water.imp.rank.thresh, .(area.km2 = .N), by = c('target','priority','vuln.mask')]
water.imp.smry.dt[, land.pcnt := round(area.km2 / high.water.imp.smry$area.km2*100)]
water.imp.smry.dt[, forest.pcnt := round(area.km2 / forest.high.water.imp.smry$area.km2*100)]
water.imp.smry.dt

fwrite(water.imp.smry.dt, 'output/wus_surface_water_importance_top75pcnt.csv')


# FIGURE =================================================================================
# ...