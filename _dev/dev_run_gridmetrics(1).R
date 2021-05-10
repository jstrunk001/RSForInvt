#'@title
#'  run gridmetrics across a project
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
#'1.0 \tab 2018-01-28 Header added \cr
#'1.1 \tab 2020-03-17 force csv or geopackage \cr
#'1.2 \tab 2021-04-30 remove fast cache and stub for processing with lidR \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param project_gpkg polygon file of intersections created by project_make() function
#'@param n_core number of corest to run process on
#'@param gridmetrics_path where is gridmetrics.exe (FUSION)
#'@param heightbreak Height break for cover calculation
#'@param cellsize output raster resolution
#'@param minht set minht for gridmetrics.ex
#'@param first T/F use only first returns
#'@param intensity T/F include intensity metrics
#'@param outlier c(-5,500) range of inclusion
#'@param fusion_switches other fusion switches as a string e.g. "/noground"
#'@param xmn,xmx,ymn,ymx set extent for analysis
#'@param fun a custom function if gridmetrics is not used
#'@param temp temp folder to hold batch files for use with gridmetrics.exe
#'@param n_cache experimental - number of las files to copy to cache
#'@param dir_dtm in case path to dtms has changed from project
#'@param dir_las in case path to las has changed from project
#'@param skip_existing skip tiles if they already have output csvs
#'@param table output folder name
#'@param existing_coms path to existing batch comamnds, incase processing was interrupted the first time
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'  gmi=run_gridmetrics(
#'  project_gpkg="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\RSForInvt_project003.gpkg"
#' ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\"
#' ,dir_dtm="c:\\usgs_dtms\\dtms\\"
#' ,dir_las="D:\\naip_2015_laz\\"
#' ,n_core=10
#' ,existing_coms="C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
#' ,n_cache=400
#' )
#'

#'
#'@import parallel rgdal raster
#'
#'@export
#
#'@seealso \code{\link{project_make}}\cr \code{\link{lidR::gridmetrics}}\cr

run_gridmetrics=function(

   proj_polys = NA
  ,proj_gpkg_path = NA
  ,layer_proj_polys = "RSForInvt_prj"
  ,layer_proj_config = "RSForInvt_config"
  ,dir_out = "c:/temp/test_project/gridmetrics"
  ,n_core = 4
  ,gridmetrics_path = "c:\\fusion\\gridmetrics.exe"
  ,heightbreak = 3
  ,minht = NA
  ,cellsize = 66

  ,first = T
  ,intensity = F
  ,outlier = c(-5,400)
  ,fusion_switches = "/nointensity /first"
  ,xmn = 561066 , xmx= 2805066 , ymn=33066 , ymx=1551066
  ,fun = compute_metrics2 #list(min=min,max=max,mean=mean,sd=sd)#,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))
  ,temp = "c:\\temp\\run_gridmetrics\\"

  ,new_dtm_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)
  ,new_las_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)

  ,skip_existing = T

  ,existing_coms = c(NA,"C:\\Temp\\run_gridmetrics\\2017Aug17_100740\\all_commands.txt")   #skip setting up new dtm and las files

  ,do_run = T

  ,debug = F

  ,... #additonal arguments to fns

  ){

  if(is.na(minht)) minht = heightbreak

  options(scipen = 999)

  requireNamespace("parallel")
  requireNamespace("raster")
  requireNamespace("rgdal")

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  gm_out=backslash(paste(dir_out,"/gridmetrics_csv/",sep=""))
  if(!dir.exists(gm_out)) try(dir.create(gm_out,recursive=T))

  #create csv folder dump
  if(is.na(existing_coms[1])) temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  if(!is.na(existing_coms[1])) temp = paste(dirname(existing_coms[1]),"/",sep="")
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

  coms_out=file.path(temp,"all_commands.txt")

 #load project
  if(!is.na(proj_polys[1])){
    proj = proj_polys
  }
  if(is.na(proj_polys[1]) & !is.na(proj_gpkg_path ) ){
    proj_polys = rgdal::readOGR(dsn=proj_gpkg_path , layer = layer_proj_polys )
    #proj_polys = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_polys )
    #df_config = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_config )
  }
  if(is.na(proj_polys[1]) & is.na(proj_gpkg_path ) ) stop("must provide either proj_polys or proj_gpkg_path")

  print("load project");print(Sys.time())
  #fix drive paths in lasR_project

  #rename paths in case files have moved
  if(!is.na(new_dtm_path)[1]){
    new_dtm_path_in = normalizePath(new_dtm_path)
    names(new_dtm_path_in) = names(new_dtm_path)
    dtm_old = normalizePath(proj_polys@data[,"dtm_file"])
    dtm_new = gsub(new_dtm_path_in["from"], new_dtm_path_in["to"], dtm_old , fixed = T)
    proj_polys@data[,"dtm_file"] = dtm_new
  }
  if(!is.na(new_las_path)[1]){
    new_las_path_in = normalizePath(new_las_path)
    names(new_las_path_in) = names(new_las_path)
    las_old = normalizePath(proj_polys@data[,"las_file"])
    las_new = gsub(new_las_path_in["from"], new_las_path_in["to"], las_old , fixed = T)
    proj_polys@data[,"las_file"] = las_new
  }

  #skip existing files
  if(skip_existing){

    files_done=list.files(gm_out,pattern="[.]csv")
    ids_done=gsub("_.*","",files_done)
    files_exist=as.character(proj_polys@data[,"tile_id"]) %in% ids_done
    proj_polys=subset(proj_polys,subset=!files_exist)

  }
  print("skip files");print(Sys.time())

  #prepare output directory
  proj_polys@data[,"outf"]=paste(gm_out,proj_polys@data[,"tile_id"],".csv",sep="")

  print(paste(nrow(proj_polys@data),"tiles to process"))

  #prepare batch commands


    proj_polys@data[,"dtm_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_dtm.txt",sep=""))
    proj_polys@data[,"las_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_las.txt",sep=""))
    proj_polys@data[,"switches"]=paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(proj_polys@data[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                              ,sep="")

    if(!is.null(fusion_switches))
      coms_df=data.frame(gm=paste(gridmetrics_path[1],fusion_switches)
                         ,sw=proj_polys@data[,c("switches")]
                         ,ids=paste("/id:",proj_polys@data[,"tile_id"],sep="")
                         ,dtms=forwardslash(proj_polys@data[,c("dtm_txt")])
                         ,hb=heightbreak
                         ,cs=cellsize
                         ,outf=proj_polys@data[,"outf"]
                         ,las=proj_polys@data[,"las_txt"]
                         )

    if(is.null(fusion_switches))
      coms_df=data.frame(gridmetrics_path[1]
                         ,ids=paste("/id:",proj_polys@data[,"tile_id"],sep="")
                         ,proj_polys@data[,c("switches","dtm_txt")]
                         ,heightbreak
                         ,cellsize
                         ,proj_polys@data[,"outf"]
                         ,proj_polys@data[,"las_txt"]
                         )

    coms=apply(coms_df,1,paste,collapse=" ")
    print("set up commands");print(Sys.time())

    if(is.na(existing_coms[1]) ){

      writeLines(coms,coms_out)

      for(i in 1:nrow(proj_polys@data)){
        writeLines(gsub(",","\n",proj_polys@data[i,"las_file"]),proj_polys@data[i,"las_txt"])
        writeLines(gsub(",","\n",proj_polys@data[i,"dtm_file"]),proj_polys@data[i,"dtm_txt"])
      }
      print("create and write list of dtms and las files");print(Sys.time())
    }

    #get identical behavior if debugging
    #the commands are now randomly selected to help with file interference

    #add gridded alternative
    #send chunks of tiles to the same thread

    if(F){

      browser()

      #build raster with large tiles


      #create points from lower left corner of processing tiles



      #intersect points with large tiles



      #split commands by large processing tiles



    }


    set.seed(50)
    if(do_run){

      if(n_core>1 ){
        print("begin parallel processing");print(Sys.time())

        clus=makeCluster(n_core)
        clusterEvalQ(clus,{library(RSForInvt);gc()})

        res=parLapplyLB(clus,sample(coms),shell);gc()

        gc();stopCluster(clus);gc()

      }else{
       lapply(coms,shell) ;gc()
      }
      print("run fusion (done)");print(Sys.time())
  }

}



