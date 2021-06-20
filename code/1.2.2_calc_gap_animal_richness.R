# This R script generates species richness maps for four animal taxa using habitat range maps from GAP.
# Author: Logan Berner, EcoSpatial Services LLC
# Date: 2012-03-21
rm(list=ls())
require(raster)
require(maptools)
require(R.utils)
require(data.table)
require(rgdal)
require(foreach)
require(doParallel)

setwd('A:/research/ecospatial_services/wus_forest_conservation/')

# LOAD DATA SETS ---------------------------------------------------------------------------------------------------------
wus.1km.aea.r <- raster('A:/research/data/boundaries/wus_aea_1km.img') 
wus.1km.aea.r[wus.1km.aea.r > 0] <- 1

wus.aea.shp <- readOGR('A:/research/data/boundaries/WUS_aea.shp')

# species table
sp.dt <- fread('gis_data/gap_species_habitat/gap_species_table.csv')
sp.dt <- sp.dt[, 1:4]
setnames(sp.dt, c('index','common.name','scientific.name'),c('species.code','name.common','name.scientific'))

# rasters
sp.files <- list.files('gis_data/gap_species_habitat/species_habitat_1km/', full.names = T)
sp.indices <- as.numeric(gsub('.tif','', list.files('gis_data/gap_species_habitat/species_habitat_1km/')))


# COMPUTE SPECIES RICHNESS FOR EACH TAXA USING ALL SPECIES =======================================================================
mkdirs('gis_data/gap_species_habitat/species_richness_1km')
taxa <- unique(sp.dt$taxa)

# clust <- makeCluster(4)
# registerDoParallel(clust)
# foreach(i = taxa) %dopar% {
#   require(raster)

for (i in taxa){
  # tmp directory
  tmp.dir <- paste('C:/tmp/',i, sep='')
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  taxa.dt <- sp.dt[taxa == i]
  taxa.stk <- raster::stack(sp.files[sp.indices %in% taxa.dt$index])
  taxa.sp.sum <- sum(taxa.stk, na.rm = T)
  taxa.sp.sum <- raster::mask(taxa.sp.sum, wus.aea.shp)
  outname.aea <- paste('gis_data/gap_species_habitat/species_richness_1km/wus_gap_',tolower(i),'_species_richness_1km_aea.tif', sep='')
  writeRaster(taxa.sp.sum, outname.aea, overwrite=T)
  
  # clean up  
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)

  # status  
  print(i)
}


# EXTRACT ALL HABITAT GRID CELLS FOR EACH SPECIES INTO GIANT DATATABLE ==========================================================
mkdirs('gis_data/gap_species_habitat/species_habitat_extracts')
sp.files <- list.files('gis_data/gap_species_habitat/species_habitat_1km/', full.names = T)
for (i in 1:length(sp.files)){
  sp.r <- raster(sp.files[i])
  sp.pxl.dt <- data.table(pxl.id = 1:ncell(sp.r[[1]]), habitat = values(sp.r))
  sp.pxl.dt$species.code <- as.numeric(sp.indices[i])
  sp.pxl.dt <- sp.pxl.dt[habitat == 1] # take grid cells with habitat
  sp.pxl.dt <- sp.pxl.dt[sp.dt, on = 'species.code']
  sp.pxl.dt$kingdom <- 'animal'
  sp.pxl.dt <- sp.pxl.dt[is.na(pxl.id) == F]
  outname <- paste0('gis_data/gap_species_habitat/species_habitat_extracts/', sp.indices[i],'.csv')
  fwrite(sp.pxl.dt, outname)
  print(i)
}

# 
# sp.stk <- raster::stack(sp.files)
# sp.pxl.dt <- as.data.table(values(sp.stk))
# sp.pxl.dt$pxl.id <- 1:ncell(sp.stk[[1]])
# sp.pxl.long.dt <- melt(sp.pxl.dt, id.vars = 'pxl.id', variable.name = 'species.code', value.name = 'habitat')
# sp.pxl.long.dt <- sp.pxl.long.dt[habitat == 1] # take grid cells with habitat
# sp.pxl.long.dt[, species.code := as.numeric(gsub('X','',species.code))]
# sp.pxl.long.dt <- sp.pxl.long.dt[sp.dt, on = 'species.code']
# sp.pxl.long.dt$kingdom <- 'animal'
# sp.pxl.long.dt <- sp.pxl.long.dt[is.na(pxl.id) == F]
# sp.pxl.long.dt
# fwrite(sp.pxl.long.dt, 'output/wus_animal_sp_habitat_by_pxl.csv')
# END SCRIPT -----------------------------------------------------------------------------------------------