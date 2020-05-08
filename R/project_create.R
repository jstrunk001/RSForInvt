#'@title
#'  Build point cloud processing tiling project (.laz, .las, .dtm) for use with FUSION
#'
#'@description
#'  Scans lidar and dems and finds out where they intersect a project tiling scheme
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/07/2020 New package derived from old lasR package \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>

#'@param dir_las where are las files
#'@param dir_dtm where are FUSION dtm files - eventuall enable any dtm type (.img, .tif etc)
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
#' proj_tn = RSForInvt::project_create(
#'   #'proj = project_create(
#'   dir_las="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\lidar_tiles\\"
#'   ,dir_dtm="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\DTM_fusion\\"
#'   ,path_gpkg_out="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\R\\DAP_Lidar_analysis\\RSForInvt\\project\\TNLidar_RSForInvtProject.gpkg"
#'   ,layer_project = "RSForInvt_prj"
#'   ,layer_config = "RSForInvt_config"
#'   ,overwrite_project = T
#'   ,project_dtm="lidar_dtm"
#'   ,project_las="lidar_las"
#'   ,dtm_year="2018"
#'   ,las_year="2018"
#'   ,do_scan_dtms=F #'we already scanned the dtm folder - ok, to rescan, but slower
#'   ,do_scan_las=F #'we already scanned the las folder - ok, to rescan, but slower
#'   ,tile_size=1650
#'   ,pixel_size=66
#'   #',xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
#'   ,proj4 = "+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
#'   ,mask=NA
#'   ,return=T
#' )
#'
#'
#'@import DBI RSQLite data.table rgdal rgeos sp raster
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{scan_las}}\cr
#'
#'
#'desired updates:
#'  add ability to scan fusion dtms and generic raster dtms
#'

project_create=function(

  dir_las=NA
  ,dir_dtm=NA
  ,recurse_dtm = F
  ,recurse_las = F
  ,path_gpkg_out="c:/lidar_projects/someProject_RSForInvt.gpkg"
  ,layer_project = "RSForInvt_prj"
  ,layer_config = "RSForInvt_config"
  ,overwrite_project = T
  ,project_dtm="someProject_dtm"
  ,project_las="someProject_las"
  ,dtm_year="2099"
  ,las_year="2099"
  ,do_scan_dtms=T
  ,do_scan_las=T
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=c(NA,561066)
  ,xmx=c(NA,2805066)
  ,ymn=c(NA,33066)
  ,ymx=c(NA,1551066)
  ,proj4 = c(NA,"+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
  ,mask=NA
  ,return=T

){

  options(stringsAsFactors = F)
  this_time = Sys.time()

  require("DBI")
  require("RSQLite")
  require("data.table")
  library("rgdal")
  require("rgeos")
  require("sp")
  require("raster")
  require("plyr")

  warning("UPDATE ME!!! Allow me to 'update' intersections without complete reset")

  #create project folder
  project_path=dirname(path_gpkg_out)
  if(!dir.exists(project_path)) dir.create(project_path,recursive=T)

  #create sqlite database / tables

  #inventory las and dtms
  if(do_scan_las) scan_las(project=project_las, project_year=las_year,dir_las=dir_las,create_polys=T , recursive = recurse_las , proj4 = proj4)
  print("scan_las");print(Sys.time())
  if(do_scan_dtms) scan_dtm(project=project_dtm, project_year=dtm_year,dir_dtm=dir_dtm, recursive = recurse_dtm , proj4 = proj4)
  print("scan_dtm");print(Sys.time())

  #file names
  path_dtm_proj=paste(dir_dtm,"/manage_dtm/manage_dtm.gpkg",sep="")
  path_las_proj=paste(dir_las,"/manage_las/manage_las.gpkg",sep="")

  #read in las and dtm polygons
  dtm_polys=readOGR(dsn = path_dtm_proj,"dtm_polys")
  las_polys=readOGR(dsn = path_las_proj,"las_polys")

  #get proj4 if not provided and add to dtms if needed
  if(!is.na(proj4)) proj4_in = proj4
  else proj4_in = proj4string(las_polys)

  dtm_proj4 = proj4string(dtm_polys)
  if(is.na(dtm_proj4)) proj4string(dtm_polys) = proj4_in

  #buffer polygons
  dtm_polys1=gBuffer(dtm_polys,byid=T,width=round(pixel_size*2+1),capStyle="square");gc()
  #dtm_polys1=buffer(dtm_polys,round(pixel_size*2+1),dissolve=F);gc()
  #las_polys1=buffer(las_polys,round(pixel_size*2+1),dissolve=F);gc()
  las_polys1=gBuffer(las_polys,byid=T,width=round(pixel_size*2+1),capStyle="square");gc()

  print("buffer complete");print(Sys.time())

  #create processing tiles
  if( ( is.na(xmn[1]) | is.na(xmx[1]) | is.na(ymn[1]) | is.na(ymx[1]) ) ){
    ext = extent(las_polys1)
    xmn = ext@xmin
    xmx = ext@xmax
    ymn = ext@ymin
    ymx = ext@ymax
  }
  proc_rast=raster(xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,resolution=tile_size,crs=crs(proj4_in));gc()
  proc_rast[]=cellsFromExtent(proc_rast,extent(proc_rast));gc()
  xy=raster::as.data.frame(proc_rast,xy=T)
  print("tile scheme complete");print(Sys.time())

  #create sub-processing tiles (100x density) for intersection with polygons
  proc_rast1=raster(xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,resolution=tile_size/10,crs=crs(proj4_in));gc()
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

  #create config file
  df_config = data.frame(
    path_gpkg_out = path_gpkg_out
    ,layer_project = layer_project
    ,layer_config  =  layer_config
    ,layer_las_buf = "las_tiles_bfr"
    ,layer_dtm_buf = "dtm_tiles_bfr"
    ,tile_buffer = ts2

    ,dir_las = dir_las
    ,dir_dtm = dir_dtm
    ,project_dtm  = project_dtm
    ,project_las  = project_las

    ,dtm_year  = dtm_year
    ,las_year  = las_year
    ,n_las = nrow(las_polys)
    ,n_dtm = nrow(dtm_polys)
    ,n_tile = nrow(tile_polys1)
    ,origin_x = origin(proc_rast)[1]
    ,origin_y = origin(proc_rast)[2]

    ,overwrite_project  =  overwrite_project
    ,xmn  = xmn
    ,ymn  = ymn
    ,xmx  = xmx
    ,ymx = ymx
    ,do_scan_dtms = do_scan_dtms
    ,do_scan_las  = do_scan_las
    ,tile_size  = tile_size
    ,pixel_size  = pixel_size
    ,proj4  = proj4
    ,has_mask  = is.na(mask)
    )

  #write project polygons
  proj4string(tile_polys1) = proj4_in
  sf_proj = sf::st_as_sf(tile_polys1)
  try(sf::st_write(obj = sf_proj , dsn = path_gpkg_out , layer = layer_project, driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

  #write dtm polygons
  proj4string(dtm_polys1) = proj4_in
  sf_dtm_bfr = sf::st_as_sf(dtm_polys1)
  try(sf::st_write(obj = sf_dtm_bfr , dsn = path_gpkg_out , layer = "dtm_tiles_bfr", driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

  #write las polygons - fix names to remove "[.]"  and " "
  las_polys2 = las_polys1
  names(las_polys2@data) = gsub("[.]","_",names(las_polys2@data ))
  names(las_polys2@data) = gsub(" ","_",names(las_polys2@data ))
  las_polys2@data$file_path = normalizePath(as.character(las_polys1@data$file_path), winslash = "/")
  proj4string(las_polys2) = proj4_in
  sf_las_bfr = sf::st_as_sf(las_polys2)
  try(sf::st_write(obj = sf_las_bfr , dsn = path_gpkg_out , layer = "las_tiles_bfr", driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

  #write config table
  sqlite_proj = dbConnect(RSQLite::SQLite(), path_gpkg_out)
  smry_write_err = try(dbWriteTable(sqlite_proj ,layer_config , df_config, overwrite = T))
  dbDisconnect(sqlite_proj)

  #save RDS object for redundancy
  l_res = list(config=df_config , project_plys = tile_polys1 ,  dtm_tiles_bfr = dtm_polys1 ,  las_tiles_bfr = las_polys2)
  outRDS = file.path(dirname(path_gpkg_out), gsub("[.]gpkg",".RDS",basename(path_gpkg_out),ignore.case=T))
  saveRDS(l_res,outRDS)

  #return data to users
  if(return) return(l_res)

}



