#'@title
#'  Import CSV representation of project and create polygon data frame
#'
#'@description
#'  Import CSV representation of project and create polygon data frame
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/07/2020 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <jacob.strunk@@usda.gov>

#'@param path_csv location of csv project
#'@param path_gpkg location of gpkg output
#'@param layer_out_ply layer name to use for polygons in gpkg
#'@param layer_out_summary layer name to use for project summary in gpkg
#'@param return = F
#'@param proj4 projection string
#'@param project project name
#'@param project_dtm dtm project name
#'@param project_las las project name
#'@param dtm_year year of dtm files
#'@param las_year year of las files
#'
#'@return
#'  <Delete and Replace>
#'

#'@examples
#'
#'  project_import_csv(
#'   path_csv =
#'   ,dir_dtm="C:\\Temp\\dtm_test\\"
#'   ,dir_project="C:\\Temp\\naip_2015_t1650_p66\\"
#'   ,project="test_project"
#'   ,project_dtm="some_project"
#'   ,project_las="some_project"
#'   ,dtm_year="2099"
#'   ,las_year="2099"
#'   ,proj4 ="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
#' )
#'
#'
#'@import DBI RSQLite data.table rgdal rgeos sp raster
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{scan_las}}\cr
#'

project_import_csv=function(

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


}
