#'@title
#'  convert xy coordinates to fia plots
#'
#'@description
#'  supply fia plot centers, and have have Spatial Polygons Dataframe returned, where each record is
#'  an FIA plot footprint: 4 fixed area subplots of 24 foot radius, a central subplot and 3 peripheral
#'  subplots 120 feet from plot center at 0,120,240 degrees.
#'
#'  Ignores declination - assumes everything faces projected north
#'
#'@details
#'  <Delete and Replace>
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
#'@param idxy dataframe with id, x, y columns available
#'@param names link dataframe columns to variable names used by funciton
#'@param create_polys TRUE / FALSE
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'    res=xy2FIAplot(data.frame(plot=1:10, x=101:110*10000, y=101:110*10000),create_polys=T)
#'    spplot(res[1,],zcol=1,aspect=1,scales=list(draw=T),key.space=list(x=0.2,y=0.9,corner=c(0,1)))
#'    spplot(res)
#'
#'@import plyr raster sp rgeos
#'
#'@export
#
#'@seealso \code{\link{bbox2polys}}\cr

#Desired upgrades to this function:
# add declination
# add different subplot distances
# add different polygon radii
# enable supplying any subplot as an initial condition

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))

xy2FIApoly=function(idxy,names=c(plot="plot",x="x",y="y"),create_polys=T){

  # sqrt((120*cos(pi/6))^2+(120*sin(pi/6))^2)
  if(interactive()) require(plyr)

  names["subplot"]="subplot"
  idxy[,names["subplot"]]=1

  dat2=idxy
  dat2[,names["subplot"]]=2
  dat2[,names["y"]]=idxy[,names["y"]]+120

  dat3=idxy
  dat3[,names["subplot"]]=3
  dat3[,names["x"]]=dat3[,names["x"]] + 103.923
  dat3[,names["y"]]=dat3[ , names["y"] ] - 60

  dat4=idxy
  dat4[,names["subplot"]]=4
  dat4[,names["x"]]=dat4[,names["x"]]-103.923
  dat4[,names["y"]]=dat4[,names["y"]]-60

  df_all=rbind.fill(idxy,dat2,dat3,dat4)


  if(create_polys){

    if(interactive()) library(raster);library(sp);library(rgeos)
    datin0=df_all
    coordinates(datin0)=datin0[,names[c("x","y")]]
    datin1=gBuffer(datin0,width=24,byid=T)
    datin2=aggregate(datin1,by=names["plot"])
    datin3=merge(datin2,idxy,by=names["plot"])

    return(datin3)
  }

  if(!create_polys) return(df_all)


}


# if(F){
#   res=xy2FIAplot(data.frame(plot=1:10, x=101:110*10000, y=101:110*10000),create_polys=T)
#   spplot(res[1,],zcol=1,aspect=1,scales=list(draw=T),key.space=list(x=0.2,y=0.9,corner=c(0,1)))
#
#   library(leaflet)
#   l1=leaflet(data=res[1,],options=leafletOptions(crs=leafletCRS(proj4def="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")))
#   addPolygons(l1,data=res[1:2,])
# }

