dir_sqlite = "D:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\gridmetrics_sqlite\\"

require(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2017_NAIPDAP_Metrics.db"),sychronous = "off")


x=dbGetQuery(db,"select center_x,center_y,ht_p95 from gm")
x1=x[!is.na(x$center_x) & !is.na(x$center_y)  ,  ]
#x1[x1==-9999] = 0
x1[x1==-9999] = NA

library(raster)
r1=rasterFromXYZ(x1[,c("center_x","center_y","ht_p95")])
writeRaster(r1,"D:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\rasters\\ht_p95_test_a.img", datatype='FLT4S',overwrite=TRUE)
gc()

