#'@title
#'  scan a directory of dtm files
#'
#'@description
#'  scan a directory of dtm files
#'
#'@details
#'  scan a directory of dtm files
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param proj4_name human interpretable name of projection
#'@param proj4 proj4 string for projection
#'@param dir_dtm where to find dtm files
#'@param recursive T/F recurse into dir_dtm subdirectories ?
#'@param pattern pattern to use in searching for dtm files
#'@param notes and descriptionn that may be helpful in using a project
#'@param create_polys output shapefiles of polygon bboxes
#'@param return T/F return objects
#'
#'@return
#'  NULL
#'
#'  or
#'
#'  list(
#'   project_id - data.frame with one one row
#'   ,dtm_ids - data.frame with as many rows as dtms
#'   ,plys - SpatialPolygonsDataFrame with all of dtm extents
#'  )
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import maptools sp uuid
#'
#'@export
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_dtm}}\cr \code{\link{read_dtm_header}}\cr

scan_dtm=function(
  project="wa_dtm"
  ,project_year="2099"
  ,proj4_name=c(NA,"NAD_1983_HARN_StatePlane_Washington_South_FIPS_4601_Feet")
  ,proj4=c(NA,"1395 +proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
  ,dir_dtm=""
  ,recursive = F
  ,pattern="[.]dtm$"
  ,notes=""
  ,create_polys=T
  ,return = F

){

  require("uuid")
  #require("RSQLite")

  proc_date=Sys.time()

  files_dtm=list.files(dir_dtm,full.names=T,recursive=recursive,include.dirs = FALSE,pattern=pattern)
  if(length(files_dtm)==0) stop("'scan_dtm' argument dir_dtm is not a directory or is empty")

  #prepare / read project_id file
  project_id_folder=paste(dir_dtm,"/manage_dtm/",sep="")
  project_id_csv=paste(project_id_folder,"project_id.csv",sep="")
  dtm_id_csv=paste(project_id_folder,"dtm_id.csv",sep="")
  dtm_gpkg=paste(project_id_folder,"manage_dtm.gpkg",sep="")

  #create out directory if missing
  if(!dir.exists(project_id_folder)) try(dir.create(project_id_folder),silent=T)

  #create or connect to geopackage
  con_gpkg = dbConnect(RSQLite::SQLite(), dtm_gpkg)
  tables_gpkg = dbListTables(con_gpkg)

  #Test for files
  #exist_project_id_folder=dir.exists(project_id_folder)
  exist_project_id_csv=file.exists(project_id_csv)
  exist_dtm_id_csv=file.exists(dtm_id_csv)

  exist_project_id_gpkg = "project_id" %in% tables_gpkg
  exist_dtm_id_gpkg = "dtm_id" %in% tables_gpkg
  exist_dtm_ply_gpkg = "dtm_polys" %in% tables_gpkg

  #if(exist_project_id_csv){
  if(exist_project_id_gpkg){

    #project_id_df=read.csv(project_id_csv)
    project_id_df = dbReadTable( con_gpkg , "project_id" , stringsAsFactors = F )

  }else{

    #make fresh project id table
    project_id_df=data.frame(
      project_id=UUIDgenerate(use.time = NA)
      ,project=project
      ,project_year=project_year
      ,load_date=proc_date
      ,file_path=dir_dtm
      ,notes=notes
      ,proj4_name=proj4_name
      ,proj4=proj4
    )

    #write to file
    write.csv(project_id_df, project_id_csv,row.names=F)
    dbWriteTable(con_gpkg, "project_id" ,project_id_df )

  }

  #if(exist_dtm_id_csv){
  if(exist_dtm_id_gpkg){

    #dtm_id_df = read.csv(dtm_id_csv,stringsAsFactors = F)
    dtm_id_df = dbReadTable(con_gpkg , "dtm_id" , stringsAsFactors = F)

  }else{

    dtm_id_df = data.frame()

  }
  proj_id=project_id_df[1,"project_id"]

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory dtm files. /n Currently trasitioning to gpkg"
  disclaimer_txt=paste(project_id_folder,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #check if dtm files exist
  names_dtm=basename(files_dtm)
  names_dtm_exist = names_dtm %in% dtm_id_df$file_name
  dtm_update = sum(!names_dtm_exist) > 0

  #update dtms
  if(dtm_update){

    #identify missing records
    files_dtm=files_dtm[!names_dtm_exist]

    #get lidar headers
    headers=read_dtm_header(files_dtm)

    #prep data for database
    names(headers)=gsub("max","max_",gsub("min","min_",tolower(names(headers))))
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"dtm_id"]=sapply(1:nrow(headers),function(x)UUIDgenerate())
    headers[,"file_name"]=basename(files_dtm)
    headers[,"file_path"]=files_dtm
    headers[,"load_date"]=proc_date
    headers[,"notes"]=notes
    headers[,"min_x"]=headers[,"ll_x"]
    headers[,"min_y"]=headers[,"ll_y"]
    headers[,"max_x"]=headers[,"min_x"]+headers[,"n_cols"]*headers[,"col_spacing"]
    headers[,"max_y"]=headers[,"min_y"]+headers[,"n_rows"]*headers[,"row_spacing"]

    if(nrow(dtm_id_df) > 0) dtm_id_df=rbind(headers,dtm_id_df[,names(headers)])
    else dtm_id_df = headers

    write.csv( dtm_id_df , dtm_id_csv )
    dbWriteTable( con_gpkg , "dtm_id" , dtm_id_df )

  }

  dbDisconnect(con_gpkg)

  if(create_polys){

    #dtm_id_df=read.csv(dtm_id_csv)

    polys_rds=paste(project_id_folder,"dtm_polys.rds",sep="")
    polys_shp=paste(project_id_folder,"dtm_polys.shp",sep="")

    dtm_polys=bbox2polys(dtm_id_df[,c("dtm_id","min_x","max_x","min_y","max_y")])
    row.names(dtm_id_df)=dtm_id_df[,"dtm_id"]
    dtm_polys=sp::SpatialPolygonsDataFrame(dtm_polys,dtm_id_df)

    #save outputs
    try(saveRDS(dtm_polys,polys_rds))

    sf_obj = sf::st_as_sf(dtm_polys)
    try(sf::st_write(obj = sf_obj , dsn = dtm_gpkg , layer = "dtm_polys", driver="GPKG" , layer_options = c("OVERWRITE=yes") ))

  }

  if(return) return(list(project_id = project_id_df, dtm_ids = dtm_id_df  , plys = dtm_polys))


}
