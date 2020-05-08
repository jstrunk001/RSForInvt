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
#'  lasR_project(
#'   dir_las="C:\\Temp\\las_test\\"
#'   ,dir_dtm="C:\\Temp\\dtm_test\\"
#'   ,dir_project="C:\\Temp\\naip_2015_t1650_p66\\"
#'   ,project="test_project"
#'   ,project_dtm="some_project"
#'   ,project_las="some_project"
#'   ,dtm_year="2099"
#'   ,las_year="2099"
#'   ,scan_dtms=T
#'   ,scan_las=T
#'   ,tile_size=1650
#'   ,pixel_size=66
#'   ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
#'   ,proj4  ="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
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
  ,layer_plys = "RSForInvt_ply"
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
  ,proj4 = "+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  ,mask=NA
  ,return=T

){
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

  #create config file
  layer_config = data.frame(
    path_project = path_project
    ,layer_plys = layer_plys
    ,layer_config  =  layer_config

    ,dir_las = dir_las
    ,dir_dtm = dir_dtm
    ,project_dtm  = project_dtm
    ,project_las  = project_las

    ,dtm_year  = dtm_year
    ,las_year  = las_year
    ,n_las = n_las
    ,n_dtm = n_dtm
    ,n_tile = n_tile
    ,origin_x = origin_x
    ,origin_y = origin_y

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
    # ,  =
    )




  #write project to file
  #write polygons
  sqlite_proj = dbConnect(RSQLite::SQLite(), path_gpkg_out)
  proj_tables = dbListTables(sqlite_proj)
  layer_plys_exist = layer_plys %in% proj_tables
  layer_config_exist = layer_config %in% proj_tables

  #test for existing tables and make new names if overwrite = T
  if(overwrite_project){

    try(dbSendQuery(sqlite_proj, paste("drop table",layer_plys)) )
    try(dbSendQuery(sqlite_proj, paste("drop table",layer_config)) )

  }else{

    if(layer_plys_exist | layer_config_exist){

      for(i in 1:10){

        layer_plys_i = sprintf(paste(layer_plys,"%03d"), i)
        layer_config_i = sprintf(paste(layer_config,"%03d"), i)

        proj_tables = dbListTables(sqlite_proj)
        layer_plys_exist = layer_plys_i %in% proj_tables
        layer_config_exist = layer_config_i %in% proj_tables

        #make sure we can do i in both
        if((!layer_plys_exist) & (!layer_config_exist)){
          warning(i," versions of project already exist")
          i =30
          layer_plys = layer_plys_i
          layer_config= layer_config_i
        }
      }
      if(i == 10) stop("10 versions of project already exist, please clean up project geopackage and try again")
    }
  }

  #write config table
  smry_write_err = try(dbWriteTable(sqlite_proj ,layer_config , proj_config))
  dbDisconnect(sqlite_proj)

  #write polygons
  sf_obj = sf::st_as_sf(tile_polys1)
  try(sf::st_write(obj = sf_obj , dsn = path_gpkg_out , layer = layer_plys, driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

  #return data to users
  if(return) return(list(path_gpkg = path_gpkg_out , plys = tile_polys1 , config=proj_config , layer_plys = layer_pls, layer_config = layer_config))

}



