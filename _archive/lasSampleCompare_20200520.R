#  if projections for polyA and polyB differ, then either make sure projections are defined in polyA,polyB or set proj4 strings below

## ?? update to scan las folder ???

rTenthAcreFt = sqrt(43560/10/pi)
rAcreFt = sqrt(43560/pi)
r5AcresFt = sqrt(5*43560/pi)

rTenthAcreM = sqrt(43560/10/pi) * 0.3048
rAcreM = sqrt(43560/pi) * 0.3048
r5AcresM = sqrt(5*43560/pi) * 0.3048

#from https://www.rdocumentation.org/packages/gbutils/versions/0.2-1/source
isNA <- function(x){
  is.atomic(x) && length(x) == 1 && is.na(x)
}

clean_path=function(
  path
  ,backslash=T
  ,force_endslash=F
){

  path_in=unlist(path)

  #fix paths
  path_ok=gsub("XXXLEADINGXXX","\\\\\\\\",gsub("\\\\\\\\","\\\\",gsub("^\\\\\\\\","XXXLEADINGXXX",gsub("/","\\\\",gsub("\\\\\\\\$","",gsub("//$","",path_in))))))

  #if slashcap
  if(force_endslash) path_ok=paste(gsub("\\\\$","",path_ok),"\\",sep="")

  #use forward slash
  if(!backslash) path_ok= gsub("\\\\","/",path_ok)

  #return data
  return (path_ok)

}
backslash=function(path) clean_path(path,backslash=T)

if(class(try(lidR::catalog_laxindex(),silent=T)) == "try-error") catalog_laxindex = function(ctg){
  stopifnot(is(ctg, "LAScatalog"))

  opt_chunk_size(ctg)   <- 0
  opt_chunk_buffer(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE

  create_lax_file = function(cluster)
  {
    rlas::writelax(cluster@files)
    return(0)
  }

  options <- list(need_buffer = FALSE, drop_null = FALSE)

  catalog_apply(ctg, create_lax_file,.options = options())
  return(invisible())
}

clipFusion=function(
  idxyd=NA #id,x,y,diameter
  ,dir_las = NA
  ,dir_dtm = NA
  ,dir_clipdata="c:\\fusion\\clipdata.exe"
  ,dir_out = NA
  ,out_f = c(".las",".laz")
  ,clipdata_switches="/height /shape:1"
  ,n_core=6
  ,temp = "c:\\temp\\clipdata"
  ,run=T

){

  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")
  require(parallel)
  if(is.na(dir_las)) stop("dir_las not provided")
  if(is.na(dir_out)){
    warning("dir_out not provided, using temp:",temp)
    dir_out=temp
  }
  if(is.na(dir_dtm)){
    warning("dir_dtm not provided, points will not be elevation adjusted")
  }
  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  dir.create(temp,recursive=T)

  #prepare coordinates
  cds_df= data.frame(
    xmin = idxyd[,2] - idxyd[,4]/2
    ,ymin = idxyd[,3] - idxyd[,4]/2
    ,xmax = idxyd[,2] + idxyd[,4]/2
    ,ymax = idxyd[,3] + idxyd[,4]/2
  )
  #output files
  lasz_out = file.path(dir_out,paste(idxyd[,1],out_f[1],sep=""))

  #prepare dtm list
  if(!is.na(dir_dtm)){
    dtm_list=file.path(temp,"dtm_list.txt")
    dtm_files = list.files(dir_dtm, full.names=T,pattern="[.]dtm$")
    writeLines(dtm_files,dtm_list)
    dtm_switch = paste("/dtm:",dtm_list,sep="",collapse="")
  }
  #prepare las list
  lasz_list=file.path(temp,"lasz_list.txt")
  lasz_files = unlist(c(list.files(dir_las, full.names=T,pattern="[.]las$")
                        ,list.files(dir_las, full.names=T,pattern="[.]laz$")))
  writeLines(lasz_files,lasz_list)

  #prepare commands
  cmd_df = data.frame(dir_cd=dir_clipdata)
  if(!is.na(clipdata_switches[1]))if(nchar(clipdata_switches[1]) > 0) cmd_df = data.frame(cmd_df,sw=clipdata_switches[1])
  if(!is.na(dir_dtm)) cmd_df = data.frame(cmd_df,dtm_sw=dtm_switch)
  cmds_df = data.frame(cmd_df,lasz_list,lasz_out,cds_df)
  cmds = apply(cmds_df,1,paste,collapse=" ")

  #write commands to batch file
  cmds_out = file.path(temp,"fusion_commands.bat")
  writeLines(cmds, cmds_out)

  #run commands
  if(run){
    require(parallel)
    clus=makeCluster(n_core)
    res=parLapply(clus,cmds,shell);gc()
    gc();stopCluster(clus);gc()
    return(list(res=res,cmds=cmds))

  }else{

    return(list(res=NA,cmds=cmds))

  }

}

lasSampleCompare=function(

  pathClipData = "c:/fusion/clipdata.exe"
  ,pathCloudmetrics = "c:/fusion/cloudmetrics.exe"
  ,pathOutA = ""
  ,pathOutB = ""
  ,pathLasA = ""
  ,pathLasB = NA
  ,pathDTMA = NA
  ,pathDTMB = NA
  ,patternA = ".*[.]la[s|z]{1}$" #matches ".las" or ".laz"
  ,patternB = ".*[.]la[s|z]{1}$"
  ,extentPolyA = NA # polygon with extent of project A
  ,extentPolyB = NA # (optional) polygon with extent of project B which will be interesected with A
  ,samplePoly = NA # (optional) provide shapefile of target sample locations - assumed to be in projection of A or extent
  ,proj4A = NA # (optional) see ?? for proj4 strings: https://www.spatialreference.org/
  ,proj4B = NA # (optional) see ?? for proj4 strings: https://www.spatialreference.org/
  ,extentSample = c(llx = NA, lly = NA, ulx = NA, uly = NA) # (optional) alternative to extentPolyA and extentPolyB
  ,nSample = 500
  ,radii = list( feet = c(FtTenthAc = 37.2, FtAcre = 117.8, Ft5Acres = 263.3 ) , meters = c(MtenthAc = 11.3, MAcre = 35.9, M5Acres = 80.3   ) )[[1]]
  ,sampleShape = c("circle","square")[1]
  ,sampleType = c("regular","random","hexagonal")
  ,doSteps = c("sample","clip","metrics")
  ,switchesClipdata = "" #optional switches to FUSION's polyclip.exe
  ,switchesCloudmetrics = ""  #optional switches to FUSION's cloudmetrics.exe
  ,procMethod = c("lidR","FUSION")
  ,nCore = 2
  ,temp = "c:\\temp\\clipdata"

){

  require(sp)
  require(rgdal)
  require(raster)
  require(rgeos)

  radii_in  = try(sort(radii, decreasing = T))

  #figure out what is present
  hasOutA = !is.na(pathOutA)
  hasOutB = !is.na(pathOutB)
  hasPathA = !is.na(pathLasA)
  hasPathB = !is.na(pathLasB)
  hasPolyA = !isNA(extentPolyA)
  hasPolyB = !isNA(extentPolyB)
  hasProj4A = !is.na(proj4A )
  hasProj4B = !is.na(proj4B )
  hasPathDTMA = !is.na(pathDTMA)
  hasPathDTMB = !is.na(pathDTMB)
  hasSample = !is.na(samplePoly)
  hasExt = !isNA(extentSample[1])
  hasClipSw = nchar(switchesClipdata) > 0
  hasCMSw = nchar(switchesCloudmetrics) > 0
  hasShape = sampleShape[1] %in% c("circle","square","round")
  hasType = sampleType %in% c("regular","random","hexagonal")

  #auto assign variables
  date_in = format(Sys.time(), "%Y%b%d%H%M%S")

  #prepare spatial data for extents
  loadPoly=function(x,proj4){
    require(rgdal)
    if(!inherits(x,"Spatial")) x_in = readOGR(x)
    else x_in = x
    if(!is.na(proj4)) proj4string(x_in) = proj4
    return(x_in)
  }
  loadExtent=function(x){
    require(raster)
    x_in = as(extent(x),"SpatialPolygons")
    return(x_in)
  }
  #load spatial data

  if(hasPolyA) extentPolyA_in = loadPoly(extentPolyA , proj4A)
  if(hasPolyB) extentPolyB_in = loadPoly(extentPolyB , proj4B)
  if(hasExt) extentPoly_in = loadExtent(extentSample)

  #chart path forward
  polyAOnly = (hasPathA & !hasPathB & !hasExt)
  polyAB = (hasPathA & hasPathB )
  polyAExt = (hasPathA & hasExt)
  extOnly = hasExt & !hasPolyA & !hasPolyB

  #check projections
  if(hasPolyA) hasCRSPolyA = !is.na(proj4string(extentPolyA_in))
  if(hasPolyB) hasCRSPolyB = !is.na(proj4string(extentPolyB_in))
  if(polyAB){

    #assign proj4
    if(!hasCRSPolyA & hasProj4A) proj4string(extentPolyA_in) = proj4A
    if(!hasCRSPolyB & hasProj4B) proj4string(extentPolyB_in) = proj4B
    bad_proj = !compareCRS(extentPolyA_in, extentPolyB_in)

    #check for proj4 again
    hasCRSPolyA = !is.na(proj4string(extentPolyA_in))
    hasCRSPolyB = !is.na(proj4string(extentPolyB_in))

    #transform if needed
    if(!bad_proj) extentPolyB_proj4A = extentPolyB_in
    if(bad_proj & hasCRSPolyA & hasCRSPolyB) extentPolyB_proj4A = spTransform(extentPolyB_in, proj4string(extentPolyA_in))
  }

  #catch errors
  errExtent = !hasExt & !hasPolyA & !hasPolyB & !hasSample
  errShape = !hasShape & !hasSample
  errType = !hasType & !hasSample
  errExtPolyB = hasExt & hasPolyB & !hasPolyA
  errProj4 = bad_proj & (!hasCRSPolyA | !hasCRSPolyB)
  errPath = !hasPolyA | (polyAB & !hasPathB) | (polyAExt & !hasPathB)
  warnProj4 = (polyAOnly & !hasCRSPolyA) | (polyAB & !hasCRSPolyA)

  #throw errors based on arguments
  if(errExtent) stop("must at minimum provide argument 'PolyA' or 'extentSample' ")
  if(errShape) stop("shape must be 'circle' or 'square' or you must provide a 'samplePoly'")
  if(errType) stop("'sampleType' must be 'regular','random','hexagonal' or you must provide a 'samplePoly'")
  if(errExtPolyB) stop("oops: argument 'extentSample' can be used with 'extentPolyA', but 'extentSample' cannot be used with argument 'extentPolyB' ")
  if(errProj4) stop("Couldn't confirm that 'extPolyA' and 'extPolyB' had the same projections - define both polygon projections (e.g. arcmap) or provide proj4 strings")
  if(errPath) stop("Either 'pathLasA' or 'pathLasB' is missing: 'pathLasB' is optional but must be provided if two extents are provided")

  #throw warnings
  if(!hasOutA){
    pathOutA = file.path(pathLasA,"clipLas",date_in)
    dir.create(pathOutA , recursive = T)
    warning("'pathOutA' argument not provided, using '",pathOutA,"'")
  }
  if(!hasOutB & hasPathB){
    pathOutB = file.path(pathLasB,"clipLas",date_in)
    dir.create(pathOutB , recursive = T)
    warning("'pathOuBt' argument not provided, using '",pathOutB,"'")
  }
  if(warnProj4) warning("'extentPolyA' does not have a projection and 'proj4A' string not provided")

  #create output folders
  if(hasOutA ){
    if(!is.null(names(radii_in))) pathsOutA_in = paste(pathOutA,paste("clipElev",names(radii_in),sep="_"),sep="")
    if(is.null(names(radii_in))) pathsOutA_in = paste(pathOutA,paste("clipElev_Rad",radii_in,"Elev",sep=""),sep="")
    sapply(pathsOutA_in , function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)
  }
  if(hasOutB ){
    if(!is.null(names(radii_in))) pathsOutB_in = paste(pathOutB,paste("clipElev",names(radii_in),sep="_"),sep="")
    if(is.null(names(radii_in))) pathsOutB_in = paste(pathOutB,paste("clipElev_Rad",radii_in,sep=""),sep="")
    sapply(pathsOutB_in ,function(x,...) if(!dir.exists(x)) dir.create(x,...) , recursive = T)
  }

  #intersect extents
  if(polyAExt) extInA = gIntersection(extentPolyA_in, extentPoly_in)
  if(polyAB) extInA = gIntersection(extentPolyA_in, extentPolyB_in)
  if(polyAOnly) extInA = extentPolyA_in
  if(extOnly) extInA = extentPoly_in

  #sample
  sInA = spsample( extInA , n = nSample , type = sampleType[1] )
  sInDFA = SpatialPointsDataFrame(sInA, data.frame(id=1:nrow(sInA@coords)))

  #buffer sample
  fn_buff = function(r, x, shape){
    if(shape == "circle" | shape == "round") res = rgeos::gBuffer( x , width= r ,capStyle="round" , byid=T)
    if(shape == "square") res = rgeos::gBuffer(x,width=r,capStyle="square" , byid=T)
    SpatialPolygonsDataFrame(res, data.frame(id=1:length(res@polygons)))
  }
  sInBuffA = lapply(radii_in, fn_buff, sInDFA , sampleShape)

  #reproject spatial data for project B or extent - if necessary
  if(polyAB){
    if(bad_proj & hasCRSPolyB){
      sInB = spTransform(sInA , proj4string(extentPolyB_in))
      extInB = spTransform(extInA , proj4string(extentPolyB_in))
      sInDFB = spTransform(sInBuffA , proj4string(extentPolyB_in))
      sInBuffB = sapply(sInBuffA , spTransform, proj4string(extentPolyB_in))

    }else{
      sInB = sInA
      extInB = extInA
      sInDFB = sInDFA
      sInBuffB = sInBuffA

    }
  }
  if(polyAExt){
    if(bad_proj & hasProj4B){
      sInB = spTransform(sInA , proj4B)
      extInB = spTransform(extInA , proj4B)
      sInDFB = spTransform(sInBuffA , proj4B)
      sInBuffB = sapply(sInBuffA , spTransform, proj4B)

    }else{
      sInB = sInA
      extInB = extInA
      sInDFB = sInDFA
      sInBuffB = sInBuffA

    }
  }

  #write shape files
  if("sInDFA" %in% ls() ){
    #prep
    pathSampleADir = file.path(pathOutA,"shapefiles")
    if(!dir.exists(pathSampleADir)) errPathOutA = try(dir.create(pathSampleADir, recursive = T))
    #write
    writeTestSInDFA = try(writeOGR(sInDFA ,pathSampleADir, paste(date_in,"_SamplePoints",sep=""), driver="ESRI Shapefile"),silent=T)
    writeTestExtA = try(writeOGR(extInA ,pathSampleADir, paste(date_in,"_SampleExtentA",sep=""), driver="ESRI Shapefile"),silent=T)
    for(i in 1:length(sInBuffA)){
      write_test=try(writeOGR(sInBuffA[[i]] ,pathSampleADir, paste(date_in,"_SamplePointPolys",names(sInBuffA)[[i]],sep=""), driver="ESRI Shapefile"),silent=T)
    }

  }

  if("sInDFB" %in% ls() ){
    #prep
    pathSampleBDir = file.path(pathOutB,"shapefiles")
    if(!dir.exists(pathSampleBDir)) errPathOutB = try(dir.create(pathSampleBDir, recursive = T))
    #write
    writeTestSInDFB = try(writeOGR(sInDFB ,pathSampleBDir, paste(date_in,"_SamplePoints",sep=""), driver="ESRI Shapefile"),silent=T)
    writeTestExtB = try(writeOGR(extInB ,pathSampleBDir, paste(date_in,"_SampleExtentB",sep=""), driver="ESRI Shapefile"),silent=T)
    for(i in 1:length(sInBuffB)){
      write_test=try(writeOGR(sInBuffB[[i]] ,pathSampleBDir, paste(date_in,"_SamplePointPolys",names(sInBuffB)[[i]],sep=""), driver="ESRI Shapefile"),silent=T)
    }

  }

  #clip plots
  if(procMethod[1] == "lidR"){

    #First clip plots

    warning("It is recommended to use 'lasindex -i *.las' from within 'pathLasA' las directory before using this function")
    #build lasR catalogs
    require(lidR)
    if(hasPathA){
      closeAllConnections()
      #clip largest extent
      ctgA <- lidR::catalog(pathLasA)
      lidR::opt_cores(ctgA) <- nCore
      lidR::opt_output_files(ctgA) <- paste0(pathsOutA_in[1], "/clip_{ID}")

      if(sampleShape == "circle") ctgA_clip1 = lidR::lasclipCircle(ctgA,sInA@coords[1:20,1],sInA@coords[1:20,2],radii_in[1])
      if(sampleShape == "square") ctgA_clip1 = lidR::lasclipRectangle(ctgA
                                                                      , sInA@coords[1:20,1] - radii_in[1]
                                                                      , sInA@coords[1:20,2] - radii_in[1]
                                                                      , sInA@coords[1:20,1] + radii_in[1]
                                                                      , sInA@coords[1:20,2] + radii_in[1]
                                                                      )

      closeAllConnections()

      #Begin subclips
      if(!"ctgA_clip1" %in% ls()) ctgA_clip1 = list(lidR::catalog(pathsOutA_in[1]))
      lCtgs = list(ctgA_clip1)

      if(length(radii_in[1]) > 0){
        lidR::opt_cores(ctgA_clip1) <- nCore

        for(i in 2:length(radii_in) ){
          plot(ctgA_clip1)
          lidR::opt_output_files(ctgA_clip1) <- paste0(pathsOutA_in[i], "/clip_{ID}")

          if(sampleShape == "circle") lCtgs[[i]] = lidR::lasclipCircle(ctgA_clip1,sInA@coords[1:20,1],sInA@coords[1:20,2],radii_in[i])
          if(sampleShape == "square") lCtgs[[i]] = lidR::lasclipRectangle(ctgA_clip1
                                                                          , sInA@coords[1:20,1] - radii_in[1]
                                                                          , sInA@coords[1:20,2] - radii_in[1]
                                                                          , sInA@coords[1:20,1] + radii_in[1]
                                                                          , sInA@coords[1:20,2] + radii_in[1]
                                                                          )
          closeAllConnections()
        }
      }
    }
    if(hasPathB){
      closeAllConnections()
      #clip largest extent
      ctgB <- lidR::catalog(pathLasB)
      lidR::opt_cores(ctgB) <- nCore
      lidR::opt_output_files(ctgB) <- paste0(pathsOutB_in[1], "/clip_{ID}")

      if(sampleShape == "circle") ctgB_clip1 = lidR::lasclipCircle(ctgB,sInB@coords[1:20,1],sInB@coords[1:20,2],radii_in[1])
      if(sampleShape == "square") ctgB_clip1 = lidR::lasclipRectangle(ctgB
                                                                      , sInB@coords[1:20,1] - radii_in[1]
                                                                      , sInB@coords[1:20,2] - radii_in[1]
                                                                      , sInB@coords[1:20,1] + radii_in[1]
                                                                      , sInB@coords[1:20,2] + radii_in[1]
                                                                      )

      closeAllConnections()

      #Begin subclips
      if(!"ctgB_clip1" %in% ls()) ctgB_clip1 = list(lidR::catalog(pathsOutB_in[1]))
      lCtgs = list(ctgB_clip1)

      if(length(radii_in[1]) > 0){
        lidR::opt_cores(ctgB_clip1) <- nCore

        for(j in 2:length(radii_in) ){
          plot(ctgB_clip1)
          lidR::opt_output_files(ctgB_clip1) <- paste0(pathsOutB_in[j], "/clip_{ID}")

          if(sampleShape == "circle") lCtgs[[j]] = lidR::lasclipCircle(ctgB_clip1,sInB@coords[1:20,1],sInB@coords[1:20,2],radii_in[j])
          if(sampleShape == "square") lCtgs[[j]] = lidR::lasclipRectangle(ctgB_clip1
                                                                          , sInB@coords[1:20,1] - radii_in[1]
                                                                          , sInB@coords[1:20,2] - radii_in[1]
                                                                          , sInB@coords[1:20,1] + radii_in[1]
                                                                          , sInB@coords[1:20,2] + radii_in[1]
                                                                          )
          closeAllConnections()

        }
      }
    }

    #subtract ground elevations


    #compute plot metrics


    #combine plot metrics


  }

  if(procMethod == "FUSION"){

    if(sampleShape == "circle"){
      if(hasPathDTMA) swClipdata = "/height /shape:1"
      if(!hasPathDTMA) swClipdata = "/shape:1"
    }
    if(sampleShape == "square"){
      if(hasPathDTMA) swClipdata = "/height /shape:0"
      if(!hasPathDTMA) swClipdata = "/shape:0"
    }

    if(hasPathA){
      gc()
    #clip first radius
      clipFusion(
        idxyd=data.frame(id=paste("clip",1:nrow(sInA@coords),sep="_"),coordinates((sInA)),2*radii_in[1])
        ,dir_las = pathLasA
        ,dir_dtm = pathDTMA
        ,dir_clipdata=pathClipData
        ,dir_out = pathsOutA_in[1]
        ,out_f = ".laz"
        ,clipdata_switches=swClipdata
        ,n_core = nCore
        ,temp = temp
        ,run=T
      )
      closeAllConnections()
      gc()

      #filter off height switch for sub-clips -> already height if desired
      swClipdata = gsub("^[ ]","",gsub("/height","",swClipdata))

      if(length(radii_in[1]) > 0){
        for(j in 2:length(radii_in) ){
          clipFusion(
            idxyd=data.frame(id=paste("clip",1:nrow(sInA@coords),sep="_"),coordinates((sInA)),2*radii_in[j])
            ,dir_las = pathsOutA_in[1] #subsample from original clips
            ,dir_dtm = NA
            ,dir_clipdata=pathClipData
            ,dir_out = pathsOutA_in[j]
            ,out_f = ".laz"
            ,clipdata_switches=swClipdata
            ,n_core = nCore
            ,temp = temp
            ,run=T
          )
          closeAllConnections()
          gc()
        }
      }

    }
    if(hasPathB){
      gc()
      #clip first radius
      clipFusion(
        idxyd=data.frame(id=1:nrow(sInB@coords),coordinates((sInB)),2*radii_in[1])
        ,dir_las = pathLasB
        ,dir_dtm = pathDTMB
        ,dir_clipdata=pathClipData
        ,dir_out = pathsOutB_in[1]
        ,out_f = ".laz"
        ,clipdata_switches=swClipdata
        ,n_core = nCore
        ,temp = temp
        ,run=T
      )
      closeAllConnections()
      gc()

      #filter off height switch for sub-clips -> already height if desired
      swClipdata = gsub("^[ ]","",gsub("/height","",swClipdata))

      if(length(radii_in[1]) > 0){
        for(j in 2:length(radii_in) ){
          clipFusion(
            idxyd=data.frame(id=paste("clip",1:nrow(sInB@coords),sep="_"),coordinates((sInB)),2*radii_in[j])
            ,dir_las = pathsOutB_in[1] #subsample from original clips
            ,dir_dtm = NA
            ,dir_clipdata=pathClipData
            ,dir_out = pathsOutB_in[j]
            ,out_f = ".laz"
            ,clipdata_switches=swClipdata
            ,n_core = nCore
            ,temp = temp
            ,run=T
          )
          closeAllConnections()
          gc()
        }
      }

    }

  }


}


if( F){
  library(rgdal)
  library(rgeos)
  #poly1=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las","las_polys")
  poly1=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/las_polys.rds")
  poly1a=gBuffer(poly1,width = 5)
  saveRDS(poly1a,"D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
  #poly2=rgdal::readOGR("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las","las_polys")
  poly2=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/las_polys.rds")
  poly2a=gBuffer(poly2,width = 5)
  saveRDS(poly2a,"D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

}
if(F){
#if( !"poly1" %in% ls() ){

  library(rgdal)
  library(rgeos)
  poly1a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/manage_las/buffer5_las_polys.rds" )
  poly2a=readRDS("D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/manage_las/buffer5_las_polys.rds" )

}

if(F){
  lasSampleCompare(
    pathOutA = "d:/temp/hood_canal_test/clip3in/"
    ,pathOutB = "d:/temp/hood_canal_test/clip6in/"
    ,pathLasA = "D:/data/wadnr_hood_canal/las/hood_canal_3in_DSM_2015/"
    ,pathLasB = "D:/data/wadnr_hood_canal/las/hood_canal_6in_DSM_2015/"
    ,extentPolyA = poly1a
    ,extentPolyB = poly2a
    ,nCore = 6
    ,nSample = 150
    ,procMethod = "FUSION"
  )
}

