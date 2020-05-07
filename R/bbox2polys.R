#'@title
#'  create polygons from dataframe of bounding coordinates and ids
#'
#'@description
#'  create polygons from dataframe of bounding coordinates and ids
#'
#'@details
#'  create polygons from dataframe of bounding coordinates and ids
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
#'@param idxxyy dataframe with 5 columns - id, minx,maxx, miny, maxy - representing the id, and bounding x,y of polygons
#'
#'@return
#'  SpatialPolygons object
#'
#'@examples
#'  #none yet
#'
#'@importFrom plyr rbind.fill
#'@import sp
#'
#'@export
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_las}}\cr

#'@export
bbox2polys=function(idxxyy){

  idxy_in=plyr::rbind.fill(
    apply(idxxyy,1,function(x) data.frame(id=x[1]
                                          ,x=as.numeric(x[c(2,2,3,3,2)])
                                          ,y=as.numeric(x[c(4,5,5,4,4)])
                                          ,row.names=NULL,stringsAsFactors=F)
          )
  )

  points2polys(idxy_in)

}

