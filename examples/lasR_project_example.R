if(F){
#
# tile_project(
#   dir_las="C:\\Temp\\las_test\\"
#   ,dir_dtm="C:\\Temp\\dtm_test\\"
#   ,dir_project="C:\\Temp\\naip_2015_t1650_p66\\"
#   ,project="test_project"
#   ,project_dtm="some_project"
#   ,project_las="some_project"
#   ,dtm_year="2099"
#   ,dlas_year="2099"
#   ,scan_dtms=T
#   ,scan_las=T
#   ,create_project=T
#   ,tile_size=1650
#   ,pixel_size=66
#   ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
#   ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
# )
#library(lasR)
#
#
# # run_gridmetrics(
# #   tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
# #   ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
# #   )
#
# if(F){
#
#   gmi=run_gridmetrics(
#     tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
#     ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
#     ,gridmetrics=c("lasR")
#     ,n_core=1
#   )
}
if(F){

  library(lasR)

  scan_las(

    project="2015_naip"
    ,project_year=2015
    ,dir_las ="G:\\projects\\2015_NAIP\\laz\\"
    #,dir_las ="D:\\test"

    )


}


if(F){


  library(lasR)

  #first create FUSION dtms
  if(F){
    dtms_tn = list.files("D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\DTM",full.names=T, pattern = "[.]img",recursive=T)
    dtms_out_tn
    for(i in 1:length(dtms_tn)){
      ri = raster(dtms[i])


    }

  }

  # proj_area=readOGR(dsn="C:\\projects\\2017_WA_DSM_Pilot\\boundary",layer="5_counties_WASP")

  lasR_project(
    dir_las="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\lidar_tiles"
    ,dir_dtm="D:\Box\VMARS\Projects\DAP_evaluation_Meston\Data\Tennessee\DTM"
    ,dir_project="d:\\projects\\2017_WA_DSM_Pilot_usgs\\"
    ,project="2017Aug_NAIP_usgs"
    ,project_dtm="usgs"
    ,project_las="naip"
    ,dtm_year="2017"
    ,las_year="2015"
    #,mask=proj_area
    ,do_scan_dtms=T
    ,do_scan_las=T
    ,tile_size=3300
    ,pixel_size=100
    ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
    ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  )




}


if(F){

  #library(rgdal)
  library(lasR)

 # proj_area=readOGR(dsn="C:\\projects\\2017_WA_DSM_Pilot\\boundary",layer="5_counties_WASP")

  lasR_project(
    dir_las="D:\\projects\\naip_2015_laz"
    ,dir_dtm="D:\\usgs_dtms\\dtms"
    ,dir_project="d:\\projects\\2017_WA_DSM_Pilot_usgs\\"
    ,project="2017Aug_NAIP_usgs"
    ,project_dtm="usgs"
    ,project_las="naip"
    ,dtm_year="2017"
    ,las_year="2015"
    #,mask=proj_area
    ,do_scan_dtms=T
    ,do_scan_las=T
    ,tile_size=3300
    ,pixel_size=100
    ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
    ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  )

  # lasR_subset("C:\\projects\\2017_WA_DSM_Pilot\\archive\\DSM_Pilot_B_lasR\\intersections.csv"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/boundary/5_counties_WASP.shp"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/lasR_project"
  # )
}
if(F){

  #library(rgdal)
  library(lasR)

  # proj_area=readOGR(dsn="C:\\projects\\2017_WA_DSM_Pilot\\boundary",layer="5_counties_WASP")

  lasR_project(
    dir_las="D:\\projects\\naip_2015_laz"
    ,dir_dtm="D:\\projects\\lidar_dtms"
    ,dir_project="d:\\projects\\2017_WA_DSM_Pilot\\"
    ,project="2017Aug_NAIP"
    ,project_dtm="lidar"
    ,project_las="naip"
    ,dtm_year="2017"
    ,las_year="2015"
    #,mask=proj_area
    ,do_scan_dtms=F
    ,do_scan_las=F
    ,tile_size=3300
    ,pixel_size=66
    ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
    ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  )

  # lasR_subset("C:\\projects\\2017_WA_DSM_Pilot\\archive\\DSM_Pilot_B_lasR\\intersections.csv"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/boundary/5_counties_WASP.shp"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/lasR_project"
  # )
}
if(F){

  library(lasR)

  gmi=run_gridmetrics(
    lasR_project_poly="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp"
    ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_05\\"
    ,dir_dtm="c:\\usgs_dtms\\dtms\\"
    ,dir_las="D:\\naip_2015_laz\\"
    ,n_core=12
    #,existing_coms="C:\\Temp\\run_gridmetrics\\2017Dec13_151040\\all_commands.txt"
    ,fast_cache="r:\\temp"
   )

}
if(F){

  #library(lasR)


  gmi=run_gridmetrics(

    lasR_project_poly="c:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\lasR_project001.shp"
    ,dir_out="c:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\"
    ,dir_dtm="c:\\data\\FUSION_DTMS\\"
    ,dir_las="G:\\NAIP_dsm_2015\\las_files\\"
    ,n_core=15
    ,existing_coms="C:\\Temp\\run_gridmetrics\\2017Aug19_141611\\all_commands.txt"
    ,gridmetrics_path="c:\\fusion\\gridmetrics.exe"

  )

}
# if(F){
#   coms=readLines("C:\\Temp\\run_gridmetrics\\2017Aug19_141611\\all_commands.txt")
#   clus=makeCluster(15)
#   clusterEvalQ(clus,{library(lasR);gc()})
#   res=parLapply(clus,coms,shell);gc()
#   gc();stopCluster(clus);gc()
# }

if(F){

  #library(rgdal)
  library(lasR)

  # proj_area=readOGR(dsn="C:\\projects\\2017_WA_DSM_Pilot\\boundary",layer="5_counties_WASP")

  lasR_project(
    dir_las="D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\LAS"
    ,dir_dtm="D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\GRID"
    ,dir_project="D:\\Box\\rs_share\\2009_SRS_lidar\\SavannahRiverSite\\prj"
    ,project="2009_SRS"
    ,project_dtm="lidar"
    ,project_las="lidar"
    ,dtm_year="2009"
    ,las_year="2009"
    #,mask=proj_area
    ,do_scan_dtms=F
    ,do_scan_las=T
    ,tile_size=3300
    ,pixel_size=66
    ,xmn=NA#561066,xmx=2805066,ymn=33066,ymx=1551066
    ,crs=""#"+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  )

  # lasR_subset("C:\\projects\\2017_WA_DSM_Pilot\\archive\\DSM_Pilot_B_lasR\\intersections.csv"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/boundary/5_counties_WASP.shp"
  #             ,"C:/projects/2017_WA_DSM_Pilot/DSM_Pilot_5cnty_lasR/lasR_project"
  # )
}

