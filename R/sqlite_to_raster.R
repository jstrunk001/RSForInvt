#'@name sqlite_to_raster
#'@title
#'  helpers to convert sqlite table to rasters
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 3/16/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param db a database object, tested for sqlite database
#'@param tb_gm which table has gridmetrics data
#'@param colsxy  names of xy columns
#'@param cols2Raster which columns to grab
#'@param format  raster formate  e.g. .tif , .img etc
#'@param dirOut  where to export rasters
#'@param raster_prefix give a name to rasters for id purposes
#'@param crs  proj4 string or other crs notation used by raster package (see writeRaster)
#'@param nProc  number of cores to use in converting to raster
#'@param doDebug run in debug mode and only read 50k rows?
#'
#'@param thresh character string like "2_50" that matches height representation in column names in tb_gm
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import raster
#'
#
#'@seealso \code{\link{rasterFromXYZ}}\cr \code{\link{writeRaster}}\cr

#Desired upgrades to this function, something like:
# "select ?? from gm where NOT NULL center_x and NOT NULL center_y")
#
#
#'@export
#'@rdname sqlite_to_raster
sqlite_to_raster = function(
  db
  ,tb_gm="gm"
  ,colsxy = c("center_x","center_y")
  ,cols2Raster = colsSomeX() # or colsAllX()
  ,format = ".img"
  ,dirOut = "E:\\projects\\2017_NAIP\\rasters\\"
  ,raster_prefix = ""
  ,crs = NA
  ,nProc = 4
  ,doDebug=F
  ,doBuild=F
  ,setNeg9999toNA = T

){


  warning("update me to query only coordinates that are not null")

  if(doBuild & F){

    dimsxy=c(1000,1000)
    r0 = raster::raster( nrows=dimsxy[1], ncols=dimsxy[2], xmn=0, xmx=dimsxy[1], ymn=0, ymx=dimsxy[2])
    ids = 1:length(r0)
    r0[] = ids
    timesA = timesB = list()
    xyz_test = as.data.frame(r0,xy=T)
    #for approach A we only need to make the "holder" raster 1x, and then repeatedly load id values
    r0_test = raster::rasterFromXYZ(xyz_test[,c(1:2)],digits=2)

    r0_test[] = -9999
    for(i in 1:50){
      timesA = c( timesA, system.time( { r0_test[] = ids})["elapsed"] )
      timesB = c( timesB, system.time({r0_test = raster::rasterFromXYZ(xyz_test[,c(1:3)] ) }  )["elapsed"])
    }
    summary( unlist(timesA ))
    summary( unlist(timesB ))
    browser()
  }
  debugRows = 5000000
  require(raster)
  if(doDebug) xy = dbGetQuery(db,paste("select",paste(colsxy,collapse=","),"from",tb_gm,"limit",debugRows))
  else xy = dbGetQuery(db,paste("select",paste(colsxy,collapse=","),"from",tb_gm))

  #xy = dbGetQuery(db, "select center_x,center_y from vw_gm_id_10674")

  if(!dir.exists(dirOut)) dir.create(dirOut)

  raster::beginCluster(nProc)
  r0 = raster::rasterFromXYZ(xy , digits=2)
  xy1 = xy
  coordinates(xy) = xy[,colsxy]
  ids = raster::cellFromXY(r0, xy1)
  r0[] = NA

  for(i in 1:length(cols2Raster)){

    print(paste("start:",cols2Raster[i],"at",Sys.time()))
    if(doDebug) dati = dbGetQuery(db,paste("select",cols2Raster[i],"from",tb_gm,"limit",debugRows))
    else dati = dbGetQuery(db,paste("select",cols2Raster[i],"from",tb_gm))

    if(setNeg9999toNA){ dati[dati[,1] == -9999 ,1] = NA}

    r0[ids] = dati[,1]
    outi = file.path(dirOut,paste(raster_prefix,cols2Raster[i],format,sep=""))
    raster::writeRaster(r0,outi,overwrite=TRUE,crs = crs)

    print(paste("complete:",cols2Raster[i],"at",Sys.time()))

  }

  raster::endCluster()

}

#'@rdname sqlite_to_raster
#'@export
colsAll = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('row','col','center_x','center_y','total_return_count_above_XXTHRESHXX','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','return_2_count_above_XXTHRESHXX','return_3_count_above_XXTHRESHXX','return_4_count_above_XXTHRESHXX','return_5_count_above_XXTHRESHXX','return_6_count_above_XXTHRESHXX','return_7_count_above_XXTHRESHXX','return_8_count_above_XXTHRESHXX','return_9_count_above_XXTHRESHXX','other_return_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_6_00_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean','identifier')))

#'@rdname sqlite_to_raster
#'@export
colsID = function()return(c('row','col','center_x','center_y','identifier'))

#'@export
#'@rdname sqlite_to_raster
colsAllX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('total_return_count_above_XXTHRESHXX','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','return_2_count_above_XXTHRESHXX','return_3_count_above_XXTHRESHXX','return_4_count_above_XXTHRESHXX','return_5_count_above_XXTHRESHXX','return_6_count_above_XXTHRESHXX','return_7_count_above_XXTHRESHXX','return_8_count_above_XXTHRESHXX','return_9_count_above_XXTHRESHXX','other_return_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_6_00_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')))

#'@rdname sqlite_to_raster
#'@export
colsSomeX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_aad','ht_l1','ht_l2','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_6_00_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')))


#'@rdname sqlite_to_raster
#'@export
colsFewX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('ht_minimum','ht_maximum','ht_mean','ht_stddev','ht_p05','ht_p20','ht_p60','ht_p90','percentage_first_returns_above_XXTHRESHXX')))

