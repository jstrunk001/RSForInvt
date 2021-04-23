#'@title
#'  clip plots or polygons form lidar
#'
#'@description
#'  clip plots or polygons form lidar - requires that one first make a project using project which
#'  includes both lidar and dtms
#'
#'@details
#'  There are three ways to provide the input shapes to this function - either as a
#'  list of plot ids, coordinates and plot diameters (4 column data.frame), as a list of vertices
#'  organized by plot_id (3 column data.frame), or as a polygon shapefile (path to shapefile) inwich case
#'  the plot id field must be specified "id_field_plots".
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param idxyd dataframe with point id, x and y coordinates, and circle diameter
#'@param idxy (optional) dataframe with polygon vertices (x,y) organized by point id
#'@param plot_polys (optional) shapefile path, or sp object with plot boundaries
#'@param id_field_plots (optional) in the event that a polygon is provided
#'@param project_polys spatial polygons dataframe (intersection between dtm and las tiles)
#'@param project path to spatial polygons dataframe (intersection between dtm and las tiles)
#'@param plot_tile_intersect path to spatial polygons dataframe representing intersection between plots and project
#'@param dir_out where to send clipped plots
#'@param height T/F subtract the ground
#'@param do_plot T/F make a plot of the intersections
#'@param return T/F return intersectiosn
#'@param n_core number of cores to use in clipping
#'@param dir_dtm in case path to dtms has changed from project
#'@param dir_las in case path to las has changed from project
#'@param skip_existing skip csv files that have been processed already - in case processing was interupted


#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import rgdal plyr rgeos raster lidR data.table parallel
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))


clip_plots=function(

  idxyd=NA #id,x,y,diameter
  ,idxy=NA #id,xy coordinates of vertices for each polygon
  ,plot_polys=NA #shapefile path, or sp object for plots
  ,id_field_plots="plot"
  ,las_polys=NA
  ,dtm_polys=NA
  # ,project=NA
  # ,project_polys=NA
  # ,plot_tile_intersect=NA
  ,dir_out=NA
  ,height=F
  ,do_plot=F
  ,return=F
  ,n_core=6
  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)
  ,skip_existing=T
  ,fix_dsm_bug = F # laz files for hood canal DSMs are screwed up

){
  if(interactive()){
    require(rgdal)
    require(plyr)
    require(rgdal)
    require(rgeos)
    require(raster)
    require(lidR)
    require(data.table)
    require(parallel)
  }

  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  dir_skip=file.path(dir_out,"skip")
  if(!file.exists(dir_skip)) try(dir.create(dir_skip, recursive=T),silent=T)

  if(!inherits(las_polys,"SpatialPolygonsDataFrame")){
    las_polys = readOGR(las_polys,stringsAsFactors=F)
  }
  if(!inherits(dtm_polys,"SpatialPolygonsDataFrame")){
    dtm_polys = readOGR(dtm_polys,stringsAsFactors=F)
  }

    print("load dtms and las");print(Sys.time())

    #create sp objects for plots
    plot_polys_in=NULL
    if(!is.na(plot_polys[1])){
      if(inherits(plot_polys,"SpatialPolygonsDataFrame")) plot_polys_in=plot_polys
      if(inherits(plot_polys,"character")) plot_polys_in=readOGR(plot_polys,stringsAsFactors=F)#readOGR(dirname(plot_polys),gsub("[.]shp","",basename(plot_polys)))
      if(is.null(plot_polys)) stop("Class of plot_polys must be character or SpatialPolygonsDataFrame")
    }
    if(!is.na(unlist(idxy)[1]) & !inherits(plot_polys_in,"SpatialPolygonsDataFrame")){
      plot_polys_in=points2polys(idxy)
    }
    if(!is.na(unlist(idxyd)[1]) & !inherits(plot_polys_in,"SpatialPolygonsDataFrame")){

      seq1=c(seq(-pi,pi,.1),pi+.1)
      circle1=data.frame(sin(seq1),cos(seq1))
      spl_idxyd=split(idxyd,1:nrow(idxyd))
      idxy=rbind.fill(lapply(spl_idxyd,function(x,circle1)data.frame(id=x[,1],x=circle1[,1]*x[,4]+x[,2],y=circle1[,2]*x[,4]+x[,3]),circle1))
      row.names(idxyd)=idxyd[,1]

      plot_polys_in0=points2polys(idxy)
      plot_polys_in=SpatialPolygonsDataFrame(plot_polys_in0,data=idxyd)

    }

    if(!id_field_plots %in% names(plot_polys_in@data)){stop("argument id_field_plots does not match a column in input plot data")}
    
    #fix row names
    row.names(plot_polys_in)=as.character(plot_polys_in@data[,id_field_plots])

    print("Get / create Plot Polys");print(Sys.time())

    #clean up self intersections
    las_polys_b=gBuffer(las_polys, byid=TRUE, width=0)
    dtm_polys_b=gBuffer(dtm_polys, byid=TRUE, width=0)
    plot_polys_b=gBuffer(plot_polys_in, byid=TRUE, width=0)

    #clip data to match extents
    ext_las=as(extent(as.vector(t(bbox(las_polys_b)))), "SpatialPolygons")
    ext_dtm=as(extent(as.vector(t(bbox(dtm_polys_b)))), "SpatialPolygons")
    ext_plot=as(extent(as.vector(t(bbox(plot_polys_b)))), "SpatialPolygons")
    
    las_polys_ext=gIntersection(las_polys_b,ext_plot, byid=T,drop_lower_td=T)
    dtm_polys_ext=gIntersection(dtm_polys_b,ext_plot, byid=T,drop_lower_td=T)
    plot_polys_ext=gIntersection(dtm_polys_b,gIntersection(las_polys_b,ext_plot, byid=T,drop_lower_td=T), byid=T,drop_lower_td=T)

    #patch data back onto clipped polygons
    row.names(proj_polys_ext)=gsub(" 1","",row.names(proj_polys_ext))
    row.names(plot_polys_ext)=gsub(" 1","",row.names(plot_polys_ext))
    keep_proj=row.names(proj_polys_b@data) %in% row.names(proj_polys_ext)
    keep_plot=row.names(plot_polys_b@data) %in% row.names(plot_polys_ext)
    proj_polys_b1=proj_polys_b[keep_proj,]
    plot_polys_b1=plot_polys_b[keep_plot,]

    plot_polys_spdf=SpatialPolygonsDataFrame(plot_polys_ext,plot_polys_b1@data)
    proj_polys_spdf=SpatialPolygonsDataFrame(proj_polys_ext,proj_polys_b1@data)

    print("clip, recombine plots with data");print(Sys.time())

    #intersect plots with tiles
    test = gIntersection( plot_polys_spdf , proj_polys_spdf, byid=T )

    proj_plot_x = gIntersects(plot_polys_spdf , proj_polys_spdf , byid=T,returnDense=F)
    proj_plot_x1=proj_plot_x[sapply(proj_plot_x,function(x)length(x)>0)]

    print("intersect plots and tiles");print(Sys.time())


    #parse intersections and compile data
    fn_parse=function(x,y,plots,tiles){

      data.frame(plots[as.character(x),,drop=F],tiles[y,],row.names=NULL)

    }

    plots_tiles=rbind.fill(mapply(fn_parse
                                  ,x=names(proj_plot_x1)
                                  ,y=proj_plot_x1
                                  ,SIMPLIFY = F
                                  , MoreArgs =
                                    list(
                                      plots=plot_polys_spdf@data
                                      ,tiles=proj_polys_spdf@data
                                      )
                                  )
                           )


    #merge duplicate records
    dup_id=.dup2(as.character(plots_tiles[,id_field_plots]))
    dups=plots_tiles[dup_id,]
    no_dups_df=plots_tiles[!dup_id,]
    spl_dups=split(dups,as.character(dups[,id_field_plots]))
    dups_df=.fn_merge(spl_dups)
    plots_tiles_unq=rbind(no_dups_df,dups_df)

    plots_tiles_unq[,id_field_plots]=as.character(plots_tiles_unq[,id_field_plots])
    row.names(plots_tiles_unq)=plots_tiles_unq[,id_field_plots]

    print("merge duplicates");print(Sys.time())

    #add records to geometry
    good_polys=names(plot_polys_ext) %in% as.character(plots_tiles_unq[,id_field_plots])


    plot_polys_merge=SpatialPolygonsDataFrame(plot_polys_ext[good_polys,],plots_tiles_unq)

  }else{
    plot_polys_merge=readOGR(plot_tile_intersect,stringsAsFactors=F)

  }

  print("fix paths");print(Sys.time())
  #fix drive paths in project
  if(!is.na(dir_dtm)) plot_polys_merge@data[,"dtm_file"]=unlist(lapply(plot_polys_merge@data[,"dtm_file"],function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_dtm))
  if(!is.na(dir_las)) plot_polys_merge@data[,"las_file"]=unlist(lapply(plot_polys_merge@data[,"las_file"],function(...,dir_las)paste(file.path(dir_las,basename(strsplit(...,",")[[1]])),collapse=","),dir_las=dir_las))



  if(do_plot ){
    if(! is.na(plot_tile_intersect)) plot(proj_polys_spdf)
    plot(plot_polys_merge,border="red",add=T,lwd=10)
  }

  print("skip existing");print(Sys.time())
  #skip existing files
  if(skip_existing){
    files_out_dir=unlist(c(list.files(dir_out,pattern="[.]las"),list.files(dir_skip,pattern="[.]las")))
    if(length(files_out_dir) > 0) {
      out_nms=paste("plot_",plot_polys_merge@data[,id_field_plots],".las",sep="")
      plot_polys_merge=plot_polys_merge[!out_nms %in% files_out_dir,]
    }
  }

  print("write shapefile");print(Sys.time())
  #write shapefile of intersections
  dir_overlap=file.path(dir_out,"plot_tile_overlap")
  if(!dir.exists(dir_overlap)) dir.create(dir_overlap)
  if(!file.exists(paste(dir_overlap,"plot_merge_tiles.shp",sep="\\"))) writeOGR(plot_polys_merge,dir_overlap,"plot_merge_tiles", driver="ESRI Shapefile")

  print("split polygons");print(Sys.time())
  #clip points
  spl_plots=sp::split(plot_polys_merge,1:nrow(plot_polys_merge))

  browser()
  
  print("initiate clipping");print(Sys.time())
  if(n_core>1){
    clus=makeCluster(n_core)
    clusterEvalQ(clus,library(RSForInvt))
    res=parLapply(clus,spl_plots,.try_clip_plots,dir_out = dir_out,height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)
    stopCluster(clus)
  }
  if(n_core<2){

    lapply(spl_plots,.try_clip_plots,dir_out = dir_out , height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)

  }

  #.try_clip_plots(x=spl_plots[[2]],dir_out = dir_out , height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)
  print("clip plots");print(Sys.time())


  print("write outputs");print(Sys.time())

  if(return) return(plot_polys_merge)

}
.dup2=function(x,type=c("all","first","last"),value=F,invert=F){

  if(type[1]=="all") index = duplicated(x) | duplicated(x, fromLast=TRUE)
  if(type[1]=="first") index = duplicated(x)
  if(type[1]=="last") index = duplicated(x, fromLast=TRUE)

  if(invert) index = !index

  if(value) return(x[index])
  if(!value) return(index)

}

.fn_merge=function(x){
  if(interactive()){ require(data.table)}
  if(class(x)=="list") x_in=data.frame(rbindlist(lapply(x,.fn_merge)))
  else{
    x_in=x[1,]
    x_in[,"tile_id"]=paste(as.character(x[,"tile_id"]),collapse=",")
    x_in[,"las_file"]=paste(unique(strsplit(paste(x[,"las_file"],collapse=","),",")[[1]]),collapse=",")
    x_in[,"dtm_file"]=paste(unique(strsplit(paste(x[,"dtm_file"],collapse=","),",")[[1]]),collapse=",")
    if("x.1" %in% names(x_in)) x_in[,"x.1"]=paste(x[,"x.1"],collapse=",")
    if("y.1" %in% names(x_in)) x_in[,"y.1"]=paste(x[,"y.1"],collapse=",")
    x_in[,"mnx"]=min(x[,"mnx"])
    x_in[,"mxx"]=max(x[,"mxx"])
    x_in[,"mny"]=min(x[,"mny"])
    x_in[,"mxy"]=max(x[,"mxy"])
  }
  return(x_in)
}

.try_clip_plots=function(...){

  #require(lasR)

  .clip_plots=function(x,id,dir_out,return=F,height=T, fix_dsm_bug = F){

    if(interactive()){
      require(lidR)
      #require(lasR)
      require(plyr)
    }

    las_files_in = grep("[.]la.$",as.character(unlist(strsplit(x@data[,"las_file"],",")[1])),value=T)
    dtm_files_in = grep("[.].{,4}$",unlist(strsplit(x@data[,"dtm_file"],",")[1]),value=T)
    las_in=readLAS(files = las_files_in)
    if(fix_dsm_bug) las_in@header@PHB['Header Size'] = 235
    dtm_in=read_dtm(dtm_files_in)
    dtm_poly=try(crop(dtm_in,gBuffer(x,width=20)))

    if(class(dtm_poly)=="try-error"){warning("plot and dem do not intersect, plot: ",x@data[,1]);return()}

    if(length(x@polygons[[1]]@Polygons)>1){

      #clip to plot bbox
      x_ext=as(extent(x),"SpatialPolygons")
      las_clip_in=lasclip(las_in, x_ext@polygons[[1]]@Polygons[[1]], inside = TRUE)

      #clip individual sub polygons
      las_poly_list=lapply(x@polygons[[1]]@Polygons,function(x,las){lasclip(las, x@polygons[[1]]@Polygons[[1]], inside = TRUE)}, las_clip_in)

      #merge las from subplots back together
      points_merge=do.call(rbind,lapply(las_poly_list,function(x)x@data))
      las_poly=LAS(points_merge,las_poly_list[[1]]@header)

    }else{

      #simple clip for contiguous polygon
      #poly_coords=x@polygons[[1]]@Polygons[[1]]@coords
      #las_poly=lasclip(las_in, "polygon", poly_coords , inside = TRUE)
      las_poly=lasclip(las_in, x@polygons[[1]]@Polygons[[1]] , inside = TRUE)

    }

    #write empty file if there are no points in las_poly
    is_skip = class(las_poly)!="LAS"
    if(!is_skip) is_skip = length(las_poly$X) == 0
    if(is_skip){
      skip_file=file.path(dir_out,"skip",paste(id,"_",x@data[1,id],".las",sep=""))
      file.create(skip_file)
      return()
    }
    if(height) las_hts = lasnormalize(las_poly, dtm_poly)
    if(!height) las_hts = las_poly

    #write to file
    if(!dir.exists(dir_out)) dir.create(dir_out)

    if(class(las_hts)=="LAS"){
      out_file_i=file.path(dir_out,paste(id,"_",x@data[1,id],".laz",sep=""))
      err=try(writeLAS(las_hts,out_file_i))
    }else{
      skip_file=file.path(dir_out,"skip",paste(id,"_",x@data[1,id],".las",sep=""))
      file.create(skip_file)
    }

  }

  try(.clip_plots(...))

}

