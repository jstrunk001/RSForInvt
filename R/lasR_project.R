#'@title
#'  Build Lidar / DEM tiling project
#'
#'@description
#'  Scans lidar and dems and finds out where they intersect
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 3/28/2017 Created \cr
#'1.1 \tab 3/17/2020 force users to use either geopackage or shapefile \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>

#'@param dir_las where are las files
#'@param dir_dtm where are dtm files
#'@param dir_project where to place project
#'@param project project name
#'@param project_dtm dtm project name
#'@param project_las las project name
#'@param dtm_year year of dtm files
#'@param las_year year of las files
#'@param scan_dtms ?scan dtm files
#'@param scan_las ?scan las files
#'@param tile_size processing tile size
#'@param pixel_size raster pixel size
#'@param xmn,xmx,ymn,ymx bound box for processing grid
#'@param crs projection string
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'  lasR_project(
#'   dir_las="C:\\Temp\\las_test\\"
#'   ,dir_dtm="C:\\Temp\\dtm_test\\"
#'   ,dir_project="C:\\Temp\\naip_2015_t1650_p66\\"
#'   ,project="test_project"
#'   ,project_dtm="some_project"
#'   ,project_las="some_project"
#'   ,dtm_year="2099"
#'   ,dlas_year="2099"
#'   ,scan_dtms=T
#'   ,scan_las=T
#'   ,create_project=T
#'   ,tile_size=1650
#'   ,pixel_size=66
#'   ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
#'   ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
#' )
#'
#'
#'@import DBI RSQLite data.table rgdal rgeos sp raster
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{scan_las}}\cr
#'

lasR_project=function(

  dir_las=NA
  ,dir_dtm=NA
  ,dir_project="c:/lidar_projects/"
  ,project="test_project"
  ,project_dtm="test_project"
  ,project_las="test_project"
  ,dtm_year="2099"
  ,las_year="2099"
  ,do_scan_dtms=T
  ,do_scan_las=T
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
  ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  ,mask=NA
  ,return=F

){
  Sys.time()
  require("DBI")
  require("RSQLite")
  require("data.table")
  library("rgdal")
  require("rgeos")
  require("sp")
  require("raster")
  require(plyr)

  warning("UPDATE ME!!! Allow me to 'update' intersections without complete reset")

  #create project folder
  project_path=file.path(dir_project,project)
  if(!dir.exists(project_path)) dir.create(project_path,recursive=T)

  #create sqlite database / tables

  #inventory las and dtms
  if(do_scan_las) scan_las(project=project_las, project_year=las_year,dir_las=dir_las,create_polys=T)
  print("scan_las");print(Sys.time())
  if(do_scan_dtms) scan_dtm(project=project_dtm, project_year=dtm_year,dir_dtm=dir_dtm)
  print("scan_dtm");print(Sys.time())

  #file names
  path_dtm_proj=paste(dir_dtm,"/manage_dtm",sep="")
  path_las_proj=paste(dir_las,"/manage_las",sep="")

  #read in las and dtm polygons
  dtm_polys=readOGR(path_dtm_proj,"dtm_polys")
  las_polys=readOGR(path_las_proj,"las_polys")

  #buffer polygons
  dtm_polys1=buffer(dtm_polys,pixel_size*2+1,dissolve=F);gc()
  las_polys1=buffer(las_polys,pixel_size*2+1,dissolve=F);gc()
  print("buffer complete");print(Sys.time())

  #create processing tiles
  if( is.na(xmn) | is.na(xmx) | is.na(ymn) | is.na(ymx)  ){
    ext = extent(las_polys1)
    xmn = ext@xmin
    xmx = ext@xmax
    ymn = ext@ymin
    ymx = ext@ymax
  }
  proc_rast=raster(xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,resolution=tile_size,crs=crs);gc()
  proc_rast[]=cellsFromExtent(proc_rast,extent(proc_rast));gc()
  xy=raster::as.data.frame(proc_rast,xy=T)
  print("tile scheme complete");print(Sys.time())

  #create sub-processing tiles (100x density) for intersection with polygons
  proc_rast1=raster(xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,resolution=tile_size/10,crs=crs);gc()
  xy1=raster::as.data.frame(proc_rast1,xy=T);gc()
  xy1[,"layer"]=NULL;gc()
  xy1[,"tile_id"]=cellFromXY(proc_rast, xy1[,c(1:2)]);gc()
  proc_rast1[]=xy1[,"tile_id"];gc()
  print("sub-tiles to fix edge problem");print(Sys.time())

  #mask if desired
  if(!is.na(mask[1])){
    mask1=buffer(mask,tile_size)
    proc_rast=crop(proc_rast,mask1)
    proc_rast1=crop(proc_rast1,mask1)
  }
  print("mask");print(Sys.time())

  #extract dtm tiles with polygons
  ex_dtm=extract(proc_rast1,dtm_polys1);gc()
  names(ex_dtm)=dtm_polys1$file_path
  ex_dtm1=lapply(ex_dtm[sapply(ex_dtm,length)>0],unique);gc()
  print("extract dtm polygons");print(Sys.time())

  #extract las tiles with polygons
  ex_las=extract(proc_rast1,las_polys1);gc()
  if("file_path" %in% names(las_polys1)) names(ex_las)=las_polys1$file_path
  else if("fil_pth" %in% names(las_polys1)) names(ex_las)=las_polys1$fil_pth
  ex_las1=lapply(ex_las[sapply(ex_las,length)>0],unique);gc()
  print("extract las polygons");print(Sys.time())

  #create dataframe from dtm and las intersections on tiles
  tiles_las_df=data.frame(rbindlist(mapply(function(tile_id,file){data.frame(tile_id,las_file=file,stringsAsFactors=F)},ex_las1,names(ex_las1),SIMPLIFY=F)))
  print("create dataframe from dtm and las intersections on tiles A");print(Sys.time())
  tiles_dtm_df=data.frame(rbindlist(mapply(function(tile_id,file){data.frame(tile_id,dtm_file=file,stringsAsFactors=F)},ex_dtm1,names(ex_dtm1),SIMPLIFY=F)))
  print("create dataframe from dtm and las intersections on tiles B");print(Sys.time())
  tiles_dtm_agg=aggregate(dtm_file~tile_id,data=tiles_dtm_df,FUN=function(x)paste(unique(x),collapse=","))
  tiles_las_agg=aggregate(las_file~tile_id,data=tiles_las_df,FUN=function(x)paste(unique(x),collapse=","))
  print("create dataframe from dtm and las intersections on tiles C");print(Sys.time())

  tiles_las_dtm=merge(tiles_las_agg,tiles_dtm_agg,by="tile_id")
  print("Merge");print(Sys.time())

  #add tile bounds
  tiles_coords = merge(x=tiles_las_dtm,y=xy,by.x="tile_id",by.y="layer")
  crd = tiles_coords[,c("x","y")]
  ts2 = tile_size/2
  bbx = data.frame(
    mnx = crd[,"x"] - ts2
    ,mny = crd[,"y"] - ts2
    ,mxx = crd[,"x"] + ts2
    ,mxy = crd[,"y"] + ts2
  )
  tiles_bbx = data.frame(tiles_coords,bbx)

  #create polys from bboxs and write to file
  tile_polys0 = bbox2polys(tiles_bbx[,c("tile_id","mnx","mxx","mny","mxy")])
  row.names(tiles_bbx)=tiles_bbx[,c("tile_id")]
  tile_polys1=SpatialPolygonsDataFrame(tile_polys0,tiles_bbx)

  #write project to file
  #write polygons
  n_err=0
  #write_test=try(writeOGR(tile_polys1, project_path, sprintf("lasR_project%03d", n_err+1), driver="ESRI Shapefile"),silent=T)
  write_test=try(writeOGR(tile_polys1, project_path, sprintf("lasR_project%03d.gpkg", n_err+1), driver="GPKG"),silent=T)

  if(class(write_test)=="try-error"){

    #n_err=length(list.files(project_path,"lasR_project.*shp"))
    #write_test=try(writeOGR(tile_polys1, project_path, sprintf("lasR_project%03d", n_err+1), driver="ESRI Shapefile"),silent = T)

    n_err=length(list.files(project_path,"lasR_project.*gpkg"))
    write_test=try(writeOGR(tile_polys1, project_path, sprintf("lasR_project%03d.gpkg", n_err+1), driver="GPKG"),silent=T)
  }
  if(class(write_test)=="try-error"){
    warning("Error trying to write lasR_project geometry",write_test)
    browser()
  }
  #write project to file
  #write csv
  out_csv=file.path(project_path,sprintf("lasR_project%03d.csv", n_err+1))
  write_test=try(write.csv(tiles_bbx,out_csv))
  if(class(write_test)=="try-error") warning("Error trying to write lasR_project geometry",write_test)

  if(return) return(tile_polys1)

}



