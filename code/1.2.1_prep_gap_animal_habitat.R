# This R script takes GAP species habitat maps (30 m) for the USA and (1) cales them to 1 km resolution with gdal;
# (2) clips/masks them to WUS, and (3) produces taxa composite species richness maps.
rm(list=ls())
require(raster)
require(maptools)
require(R.utils)
require(rgdal)
require(data.table)
require(gdalUtils)

# geospatial info
aea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80")
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# boundary
wus.aea <- readOGR('A:/research/data/boundaries/WUS_aea.shp')
wus.extent <- extent(wus.aea)

# FMEC template
wus.tmplt.1km.aea <- raster('A:/research/data/boundaries/wus_aea_1km.img') 
wus.tmplt.1km.aea[wus.tmplt.1km.aea > 0] <- 1

zip.files <- list.files('I:/large_data_sets/gap_species_habitat_maps_zip/')
file.indices <- sort(as.numeric(gsub('.zip','', zip.files)))

for (i in 1:length(file.indices)){
  require(raster)
  tmp.dir <- paste('C:/tmp/R_',i, sep='')
  tempfile(tmpdir=tmp.dir)
  rasterOptions(tmpdir=tmp.dir)
  
  # unzip 
  zip.file.name <- paste0('I:/large_data_sets/gap_species_habitat_maps_zip/', i, '.zip')
  unzip.file.name <- paste0(tmp.dir, '/', i, '.tif')
  unzip(zip.file.name, exdir = tmp.dir)
  
  zip.subfile.name <- grep('zip', list.files(tmp.dir, full.names = T), value = T)
  unzip(zip.subfile.name, exdir = tmp.dir)
  
  # load specias habitat map and check spatial extent
  tif.file.name <- grep(glob2rx('*.tif'), list.files(tmp.dir, full.names = T), value = T)
  sp.r <- raster(tif.file.name)
  sp.extent <- extent(sp.r)
  
  # check if species occurs in westurn USA. If so, continue; if not, skip to next species
  if (is.null(intersect(sp.extent, wus.extent)) == T){
    gc()
    removeTmpFiles()
    unlink(tmp.dir, recursive = T)
    next()
  } else {
      tif.1km.file.name <- paste(tmp.dir, '1km.tif', sep='/')  
      gdalwarp(srcfile = tif.file.name, dstfile = tif.1km.file.name, tr = c(1000,1000), r = 'max')
      sp.r.wus.1km <- raster(tif.1km.file.name)
      sp.r.wus.1km <- raster::crop(sp.r.wus.1km, wus.aea)
      sp.r.wus.1km[sp.r.wus.1km > 0] <- 1
      sp.r.wus.1km <- raster::resample(sp.r.wus.1km, wus.tmplt.1km.aea, method = 'ngb')
      outname <- paste('C:/tmp/species_habitat_1km/',i,'.tif', sep='')
      writeRaster(sp.r.wus.1km, outname, overwrite=T, datatype='INT1U')
      gc()
      removeTmpFiles()
      unlink(tmp.dir, recursive = T)
  }
  print(paste0('done: ', i))
}

# END SCRIPT -----------------------------------------------------------------------------