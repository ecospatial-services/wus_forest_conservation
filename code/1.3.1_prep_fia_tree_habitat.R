# This R script takes FIA species basal area maps (250 m) for the USA and (1) scales them to 1 km resolution with gdal;
# (2) clips/masks them to WUS, and (3) produces a composite species richness map.
rm(list=ls())
require(raster)
require(maptools)
require(R.utils)
require(rgdal)
require(data.table)
require(foreach)
require(doParallel)

# boundary
wus.aea.shp <- readOGR('A:/research/data/boundaries/WUS_aea.shp')
wus.extent <- extent(wus.aea.shp)

# FMEC template
wus.1km.aea.r <- raster('A:/research/data/boundaries/wus_aea_1km.img') 
wus.1km.aea.r[wus.1km.aea.r > 0] <- 1

sp.files <- list.files('A:/research/data/tree_ranges/wilson_usa_tree_sp_distribution_basal_area/rasters/', full.names = T)
sp.names <- gsub('.img','', list.files('A:/research/data/tree_ranges/wilson_usa_tree_sp_distribution_basal_area/rasters/', full.names = F))

sp.key.dt <- fread('A:/research/data/tree_ranges/wilson_usa_tree_sp_distribution_basal_area/species_table.csv')

# create output directory 
mkdirs('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/')
mkdirs('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_habitat_1km/')

# ===================================================================================================================  
# look through species files
clust <- makeCluster(6)
registerDoParallel(clust)
foreach(i=1:length(sp.files)) %dopar% {
  require(raster)
  tmp.dir <- paste0('C:/tmp/', sp.names[i])
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  # load specias habitat map
  sp.r <- raster(sp.files[i])
  
  # crop to western USA
  sp.r.wus.250m <- raster::crop(sp.r, wus.aea.shp)
  
  # check if species occurs in westurn USA. If not, skip to next species
  if(cellStats(sp.r.wus.250m, stat = 'max') == 0){
    gc()
    removeTmpFiles()
    unlink(tmp.dir, recursive = T)
    next()
  }
  
  # aggregate, mask, align, write out 
  sp.r.wus.1km <- raster::aggregate(sp.r.wus.250m, fact = 4, fun = 'max')
  sp.r.wus.1km[sp.r.wus.1km > 0] <- 1
  sp.r.wus.1km <- raster::mask(sp.r.wus.1km, wus.aea.shp)
  sp.r.wus.1km <- raster::resample(sp.r.wus.1km, wus.1km.aea.r, method = 'ngb')
  outname <- paste('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_habitat_1km/',sp.names[i],'.tif', sep='')
  writeRaster(sp.r.wus.1km, outname, overwrite=T, datatype='INT1U')
  
  # clean up
  gc()
  removeTmpFiles()
  unlink(tmp.dir, recursive = T)
}
  
stopCluster(clust)


# COMPUTE TREE SPECIES RICHNESS ---------------------------------------------------------------
mkdirs('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_richness_1km')

sp.files <- list.files('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_habitat_1km/', full.names = T)

tmp.dir <- paste('C:/tmp/tree_richness')
tempfile(tmpdir=tmp.dir)
rasterOptions(tmpdir=tmp.dir)
  
taxa.stk <- raster::stack(sp.files)
taxa.sp.sum <- sum(taxa.stk, na.rm = T)
taxa.sp.sum <- raster::mask(taxa.sp.sum, wus.aea.shp)

outname <- 'A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_richness_1km/fia_tree_species_richness_1km_aea.tif'
writeRaster(taxa.sp.sum, filename = outname, overwrite=T, datatype='INT1U')


# EXTRACT ALL HABITAT GRID CELLS FOR EACH SPECIES INTO GIANT DATATABLE ==========================================================
sp.files <- list.files('A:/research/ecospatial_services/wus_forest_conservation/gis_data/fia_tree_species_habitat/species_habitat_1km/', full.names = T)
tree.stk <- raster::stack(sp.files)
tree.pxl.dt <- as.data.table(values(tree.stk))
tree.pxl.dt$pxl.id <- 1:ncell(tree.stk[[1]])
tree.pxl.long.dt <- melt(tree.pxl.dt, id.vars = 'pxl.id', variable.name = 'species.code', value.name = 'habitat')
tree.pxl.long.dt <- tree.pxl.long.dt[habitat == 1] # take grid cells with habitat
tree.pxl.long.dt[, species.code := as.numeric(gsub('s','',species.code))]
tree.pxl.long.dt <- tree.pxl.long.dt[sp.key.dt, on = 'species.code']
tree.pxl.long.dt$taxa <- 'Tree'
tree.pxl.long.dt$kingdom <- 'Plant'
tree.pxl.long.dt <- tree.pxl.long.dt[is.na(pxl.id) == F]
fwrite(tree.pxl.long.dt, 'A:/research/ecospatial_services/wus_forest_conservation/output/wus_tree_sp_habitat_by_pxl.csv')

# END SCRIPT -----------------------------------------------------------------------------