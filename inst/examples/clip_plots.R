   if(F){

  library(lasR)
  library(rgdal)
  library(rgeos)
  library(raster)
  library(maptools)
  library(helpers)

  #get projection info
  source("C:\\box\\BoxSync\\sync\\R\\my_packages\\helpers\\R\\get_proj4.r")
  get_proj4(details=c("HARN","wash","ftUS","nad83"))
  prj_to="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  get_proj4(details=c("HARN","nad83"))
  prj_from="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

  plots0=readOGR("D:\\projects\\2017_WA_DSM_Pilot\\GIS\\merged_Wasp.shp",stringsAsFactors=F)
  plots1=subset(plots0,subset=plots0@data$statecd==53)
  plots2=gBuffer(plots1,width=24,byid=T)
  plots3=aggregate(plots2,by=c("plot"))

  proj=readOGR("D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp",stringsAsFactors=F)


  clip_plots(plot_polys=plots3
             ,lasR_project_polys=proj
             ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips"
             ,n_core=20
  )

  clip_plots(plot_polys=plots3
             ,lasR_project_polys=proj
             ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips_elev"
             ,n_core=20
             ,height=F
  )

   }

   if(F){

     library(lasR)
     library(rgdal)
     library(rgeos)
     library(raster)
     library(maptools)
     library(helpers)

     #get projection info
     source("d:\\box\\sync\\R\\my_packages\\helpers\\R\\get_proj4.r")
     get_proj4(details=c("HARN","wash","ftUS","nad83"))
     prj_to="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
     get_proj4(details=c("HARN","nad83"))
     prj_from="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

     plots0=readOGR("D:\\Box\\RMA~VMaRS~Team\\SRS_2009\\SRS_Plot_locations_FINAL\\LIDAR_Plot_NAD83_UTM_Z17N_2009.shp",stringsAsFactors=F)


     proj=readOGR("D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp",stringsAsFactors=F)


     clip_plots(plot_polys=plots3
                ,lasR_project_polys=proj
                ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips"
                ,n_core=20
     )

     clip_plots(plot_polys=plots3
                ,lasR_project_polys=proj
                ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips_elev"
                ,n_core=20
                ,height=F
     )

   }

   #use FUSION to clip
   if(F){

     library(lasR)
     library(rgdal)
     library(rgeos)
     library(raster)
     library(maptools)
     library(helpers)


     plots0 = readOGR("E:\\SavannahRiverSite\\Field Measurements\\SRS_Plot_locations_FINAL\\LIDAR_Plot_NAD83_UTM_Z17N_2009.shp",stringsAsFactors=F)
     plots1=gBuffer(plots0,width=100,byid=T)

     proj1=readOGR("E:\\SavannahRiverSite\\prj\\2009_SRS\\lasR_project001.shp",stringsAsFactors=F)

     clip_plots(plot_polys = plots1
                ,lasR_project_polys = proj1
                ,dir_out = "E:\\SavannahRiverSite\\plot_clips\\"
                ,n_core = 4
                ,id_field_plot = "Plot"
                ,height=T
                ,dir_las="E:\\SavannahRiverSite\\LAS"
                ,dir_dtm="E:\\SavannahRiverSite\\GRID"
     )




    #  dat1 = plots0@data
    #  dat1[,c("minx","miny","maxx","maxy")] = rbind.fill(apply(dat1[,c("x","y")],1,function(z,r=100) data.frame(minx=z[1]-100,miny=z[2]-100,maxx=z[1]+100,maxy=z[2]+100)))
    #  dat1[,c("outf")] = paste("D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\plot_clips\\plot_",dat1[,c("Plot")],sep="")
    #
    #  dat1[,c("outf")] = paste("D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\plot_clips\\plot_",dat1[,c("Plot")],sep="")
    #  write.csv(dat1[,c("outf","minx","miny","maxx","maxy")],"D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\clipdata_input.txt",row.names = F,col.names = F)
    #
    #  shell("C:\\FUSION\\clipdata.exe /height /dtm:D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\GRID\\*.dtm D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\LAS\\*.las D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\clipdata_input.txt")
    #
    #  shell("d: && cd D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\GRID\\ && C:\\FUSION\\clipdata.exe /height /dtm:*.dtm D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\LAS\\*.las D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\clipdata_input.txt",intern = T,wait=T)
    #
    #
     }
