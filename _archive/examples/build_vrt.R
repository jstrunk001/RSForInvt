library(lasR)
library(raster)
options(scipen = 10e7)

r1 = read_dtm("D:\\data\\usgs_dtms\\dtm_tiles\\183_302.dtm")
r2 = raster("D:\\data\\usgs_dtms\\dtm_tiles\\183_302.vrt")
r3 = raster("D:\\data\\usgs_dtms\\dtm_tiles\\183_302c.vrt")
plot(r1)
plot(r2)
plot(r3)

writeRaster(r1, "D:\\data\\usgs_dtms\\dtm_tiles\\183_302.tif")

r4 = (r1 + r3)

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
paste(c(
  603823.783642799
,cos(pi/2) * 28.8453
,-sin(pi/2) * 28.8453
,996664.234221291
,sin(pi/2) * 28.8453
,cos(pi/2) * 28.8453
),collapse=",")

#reflection?
paste(
  c(
  603823.783642799
  ,1
  ,0
  ,996664.234221291
  ,1
  ,0
  )
  ,collapse=","
  )

c()

paste()

<GeoTransform>603823.783642799,28.8452974093174,0,996664.234221291,0,28.8452974093174</GeoTransform>
