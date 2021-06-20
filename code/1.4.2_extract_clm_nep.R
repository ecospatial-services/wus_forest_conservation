# This script extracts data for select time perieds from the CLM simulations 
# Author: Logan Berner, EcoSpatial Services L.L.C.
# Date: 2021-02-20
rm(list=ls())
setwd('A:/research/')
require(raster)
require(data.table)
require(ggplot2)
require(dplyr)

yoi <- c(2030,2050)

# LOAD DATA SETS --------------------------------------------------------------------------------------------
wus.r <- raster('data/boundaries/wus_aea_1km.img')

# CLM simulations of tree nep
ipsl.bau.aea.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_IPSL_BAU_2020_to_2030_2050_MgCPxl_1km_aea.tif')
ipsl.noharv.aea.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_IPSL_NOHARV_2020_to_2030_2050_MgCPxl_1km_aea.tif')
miroc.bau.aea.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_MIROC_BAU_2020_to_2030_2050_MgCPxl_1km_aea.tif')
miroc.noharv.aea.stk <- stack('ecospatial_services/wus_forest_conservation/gis_data/CLM_2015_2099_output/geotifs_aea/CLM_CUMULATIVE_NEP_MIROC_NOHARV_2020_to_2030_2050_MgCPxl_1km_aea.tif')


# EXTRACT DATA FROM RASTERS -----------------------------------------------------------  --------------------------------------

# clm ipsl bau 
clm.ipsl.bau.dt <- data.table(pxl.id = 1:ncell(wus.r), values(ipsl.bau.aea.stk))
names(clm.ipsl.bau.dt)[2:3] <- paste0('x',yoi)
clm.ipsl.bau.dt$model <- 'IPSL'
clm.ipsl.bau.dt$scenario <- 'BAU'
clm.ipsl.bau.dt <- melt(clm.ipsl.bau.dt, id=c(1,4,5), variable.name = 'year', value.name = 'cnep.MgPxl')

# clm miroc bau 
clm.miroc.bau.dt <- data.table(pxl.id = 1:ncell(wus.r), values(miroc.bau.aea.stk))
names(clm.miroc.bau.dt)[2:3] <- paste0('x',yoi)
clm.miroc.bau.dt$model <- 'MIROC'
clm.miroc.bau.dt$scenario <- 'BAU'
clm.miroc.bau.dt <- melt(clm.miroc.bau.dt, id=c(1,4,5), variable.name = 'year', value.name = 'cnep.MgPxl')

# clm ipsl noharv 
clm.ipsl.noharv.dt <- data.table(pxl.id = 1:ncell(wus.r), values(ipsl.noharv.aea.stk))
names(clm.ipsl.noharv.dt)[2:3] <- paste0('x',yoi)
clm.ipsl.noharv.dt$model <- 'IPSL'
clm.ipsl.noharv.dt$scenario <- 'NOHARV'
clm.ipsl.noharv.dt <- melt(clm.ipsl.noharv.dt, id=c(1,4,5), variable.name = 'year', value.name = 'cnep.MgPxl')

# clm miroc noharv 
clm.miroc.noharv.dt <- data.table(pxl.id = 1:ncell(wus.r), values(miroc.noharv.aea.stk))
names(clm.miroc.noharv.dt)[2:3] <- paste0('x',yoi)
clm.miroc.noharv.dt$model <- 'MIROC'
clm.miroc.noharv.dt$scenario <- 'NOHARV'
clm.miroc.noharv.dt <- melt(clm.miroc.noharv.dt, id=c(1,4,5), variable.name = 'year', value.name = 'cnep.MgPxl')


# COMBINE DATA ACROSS SCENARIOS AND CLIMATE MODELS, COMPUTE ENSEMBLE, WRITE OUT ------------------------
clm.pxl.dt <- rbind(clm.ipsl.bau.dt, clm.ipsl.noharv.dt, clm.miroc.bau.dt, clm.miroc.noharv.dt)

# compute ensemble (IPSL, MIROC) average for each scenario and time period
clm.ens.pxl.dt <- clm.pxl.dt[, .(cnep.MgPxl = mean(cnep.MgPxl), model = "ENSEMBLE"), by = c('pxl.id','scenario','year')]
clm.pxl.dt <- rbind(clm.pxl.dt, clm.ens.pxl.dt)

# drop pixels without data
clm.pxl.dt <- na.omit(clm.pxl.dt)

# write output table
fwrite(clm.pxl.dt, 'ecospatial_services/wus_forest_conservation/output/wus_clm_cnep_by_pxl_model_scenario_period.csv')

# END SCRIPT -------------------------------------------------------------------------------------------

# wus.r[wus.r == 1] <- NA
# pxl.dt <- data.table(pxl.id = 1:ncell(wus.r))
# pxl.dt$state.code <- values(wus.r)
# pxl.dt$ecoreg.code <- values(ecoreg.r)

# # state boundaries
# wus.r <- raster('data/boundaries/wus_states_aea_1km.tif')
# state.key <- data.table(state.code = 1:11, state.name = c('WA','OR','CA','ID','NV','UT','AZ','MT','WY','CO','NM'))
# state.order <- c('AZ','CA','CO','ID','MT','NV','NM','OR','UT','WA','WY')
# 
# # EPA ecoregions
# ecoreg.r <- raster('data/landcover/us_eco_l3_201708/wus_eco_l3_aea_1km.tif')
# ecoreg.key <- fread('data/landcover/us_eco_l3_201708/wus_eco_l3_key.csv')
# names(ecoreg.key) <- c('ecoreg.code','ecoreg.name','area_m2')
# 

# # add other pixel specific data
# clm.pxl.dt <- clm.pxl.dt[pxl.dt, on = 'pxl.id']
# clm.pxl.dt <- clm.pxl.dt[state.key, on = 'state.code']
# clm.pxl.dt <- clm.pxl.dt[ecoreg.key, on = 'ecoreg.code']
# clm.pxl.dt <- na.omit(clm.pxl.dt)



# # compute differences between NO HARVEST and BAU scenarios
# clm.scen.dif.dt <- dcast(clm.pxl.dt, formula = pxl.id + model + year ~ scenario, value.var = 'nep.MgPxl')
# clm.scen.dif.dt <- clm.scen.dif.dt[, .(scenario = 'NOHARVminusBAU', nep.MgPxl = NOHARV - BAU), by = c('pxl.id','model','year')]
# clm.pxl.dt <- rbind(clm.pxl.dt, clm.scen.dif.dt)

