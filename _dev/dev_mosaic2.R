# library(raster)
# r <- raster()
# r1 <- crop(r, extent(-10, 10, -10, 10))
# r2 <- crop(r, extent(0, 20, 0, 20))
# r3 <- crop(r, extent(10, 30, 10, 30))
#
# r1[] <- 1:ncell(r1)
# r2[] <- 1:ncell(r2)
# r3[] <- 1:ncell(r3)
#
# rasters1 <- list(r1, r2, r3)
#
# mos <- mosaic(rasters1,fun=mean)
#
# do.call(function(...)mosaic(...,fun=mean,na.rm=T), rasters1)
