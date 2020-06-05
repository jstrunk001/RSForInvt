# This example demonstrates creation of some sp objects.

You can find a more complete demonstration of working with spatial
object in R here:
<https://mgimond.github.io/Spatial/reading-and-writing-spatial-data-in-r.html>

**Build SpatialPointsDataframe** Dealing with points is relatively easy
compared to polygons or lines

``` r
library(raster)
library(sp)
library(rgdal)
library(sf)

# Approach 1
npts = 50
pts_df = data.frame(id=1:npts,x=rnorm(npts)*1000+10e6, y=rnorm(npts)*1000+10e6 , z=rnorm(npts))
coordinates(pts_df) = ~x+y
# Approach 2
npts = 50
pts_df = data.frame(id=1:npts,x=rnorm(npts)*1000+10e6, y=rnorm(npts)*1000+10e6 , z=rnorm(npts))
coordinates(pts_df) = pts_df[,c("x","y")]

pts_df
```

    ## class       : SpatialPointsDataFrame 
    ## features    : 50 
    ## extent      : 9997817, 10002085, 9996822, 10002057  (xmin, xmax, ymin, ymax)
    ## crs         : NA 
    ## variables   : 4
    ## names       : id,                x,                y,                 z 
    ## min values  :  1, 9997816.67226889, 9996821.57556506, -1.62692054132175 
    ## max values  : 50, 10002084.7571761, 10002057.0077936,  1.72937583825549

``` r
plot(pts_df)
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
spplot(pts_df,zcol="z")
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

\*\* Add projection to pts\_df and write to a shapefile. \*\*

You will need to figure out your projection string if you don’t already
have it e.g. you can search here <https://www.spatialreference.org/>

I have selected this projection for this toy example
<https://www.spatialreference.org/ref/epsg/3692/proj4/>

See here for more details on projections in R:
<https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf>

``` r
some_proj4 = "+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +to_meter=0.3048006096012192 +no_defs"

proj4string(pts_df) = some_proj4

#shapefiles are not great, but they are the most common spatial objects (field name restrictions, file size limitations, proprietary, etc.)
if(file.exists("c:/temp/example_pts.shp")) unlink("c:/temp/example_pts.shp",force=T)
rgdal::writeOGR(pts_df,dsn="c:/temp","example_pts1", driver = "ESRI Shapefile", overwrite_layer = T)

#A better option is the open source geopackage (it's like an ESRI file geodatabase). Unfortunately rgdal has pretty lousy support for geopackages as far as I can tell, e.g it cannot append. 
if(file.exists("c:/temp/demo_geopackage.gpkg")) try(unlink("c:/temp/demo_geopackage.gpkg",force=T))
rgdal::writeOGR(pts_df,dsn="c:/temp/demo_geopackage.gpkg","example_pts1", driver = "GPKG")

#the sf package does a better job, but you have to convert from sp to sf first... It seems like sf objects are a better data model (basically an R data frame), but most spatial packages in R still use sp objects.
pts_df_sf = sf::st_as_sf(pts_df)

#this add a new layer to the geopackage - not possible with rgdal (for geopackages)
if(file.exists("c:/temp/demo_geopackage1.gpkg")) try(unlink("c:/temp/demo_geopackage1.gpkg",force=T))
sf::st_write(pts_df_sf,dsn="c:/temp/demo_geopackage1.gpkg", "example_pts2", driver="GPKG") 
```

    ## Writing layer `example_pts2' to data source `c:/temp/demo_geopackage1.gpkg' using driver `GPKG'
    ## Writing 50 features with 4 fields and geometry type Point.

``` r
#we can even append new rows to an existing object using sf - definitely not possible with rgdal (for geopackages)
pts_df_sf2 = pts_df_sf  
row.names(pts_df_sf2 ) = as.numeric(row.names(pts_df_sf )) + max(as.numeric(row.names(pts_df_sf ))) 
pts_df_sf2$id = pts_df_sf$id + max(pts_df_sf$id)
sf::st_write(pts_df_sf2,dsn="c:/temp/demo_geopackage1.gpkg", "example_pts2", driver="GPKG",append=T) 
```

    ## Updating layer `example_pts2' to data source `c:/temp/demo_geopackage1.gpkg' using driver `GPKG'
    ## Updating existing layer example_pts2
    ## Writing 50 features with 4 fields and geometry type Point.

``` r
#verify that append works
pts_df_sf3 = sf::read_sf(dsn="c:/temp/demo_geopackage1.gpkg", "example_pts2")

#notice that pts_df_sf3 has npts*2 records ...
nrow(pts_df_sf)
```

    ## [1] 50

``` r
nrow(pts_df_sf2)
```

    ## [1] 50

``` r
nrow(pts_df_sf3)
```

    ## [1] 100

``` r
knitr::kable(tail(pts_df_sf3,10))
```

|  id |        x |        y |           z | geom                                  |
| --: | -------: | -------: | ----------: | :------------------------------------ |
|  91 |  9998938 |  9998481 | \-1.2236453 | c(9998938.14482203, 9998481.40337359) |
|  92 |  9998950 | 10000648 |   1.7293758 | c(9998950.33234319, 10000647.9258047) |
|  93 | 10000523 | 10000279 | \-0.6348512 | c(10000523.0655949, 10000278.7992963) |
|  94 | 10000501 | 10000537 |   0.4877401 | c(10000500.6143778, 10000537.2479593) |
|  95 |  9998096 |  9999289 | \-0.4760096 | c(9998095.64539971, 9999289.40211879) |
|  96 |  9999569 | 10002057 | \-0.1011775 | c(9999569.01906272, 10002057.0077936) |
|  97 |  9999608 |  9999820 |   0.0907292 | c(9999608.34978395, 9999820.18597659) |
|  98 |  9998834 |  9999482 |   0.8604712 | c(9998833.8037794, 9999481.8751414)   |
|  99 |  9999673 |  9998526 | \-0.6683756 | c(9999673.09227062, 9998526.03367687) |
| 100 |  9999528 | 10001361 | \-0.4358499 | c(9999527.80034536, 10001361.0445843) |

``` r
#DT::datatable(pts_df_sf3)
```

\*\* buffer points to create polygons\*\* this is an easy way to make a
SpatialPolygonsDataFrame

``` r
bf1 = raster::buffer(pts_df , dissolve=FALSE)
bf1
```

    ## class       : SpatialPolygonsDataFrame 
    ## features    : 50 
    ## extent      : 9997816, 10002086, 9996821, 10002058  (xmin, xmax, ymin, ymax)
    ## crs         : +proj=lcc +lat_0=45.3333333333333 +lon_0=-120.5 +lat_1=47.3333333333333 +lat_2=45.8333333333333 +x_0=500000.0001016 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs 
    ## variables   : 4
    ## names       : id,                x,                y,                 z 
    ## min values  :  1, 9997816.67226889, 9996821.57556506, -1.62692054132175 
    ## max values  : 50, 10002084.7571761, 10002057.0077936,  1.72937583825549

**Create SpatialPolygonsDataFrame**

Making a SpatialPolygonsDataFrame manually is a lot of work

1.  create single polygon objects sp::Polygon(coords,
    hole=as.logical(NA))
2.  make a list of polygon objects and combine polygons
    sp::Polygons(srl, ID)
3.  combine list of Polygons sp::SpatialPolygons(Srl, pO,
    proj4string=CRS(as.character(NA)))
4.  finally, merge spatial data with data frame
    sp::SpatialPolygonsDataFrame(Sr, data, match.ID = TRUE)

now lets make a circle

``` r
  #number of vertices
  nvtx = 100
  #split circle into nvtx slices - add zero to end to close polygon
  rads0 = c(seq(0,2*pi,length.out = nvtx),0)
  
  #compute x,y coordinates
  radius0 = 100
  xcds = radius0*cos(rads0)
  ycds = radius0*sin(rads0)
  plycds = data.frame(x=xcds, y=ycds)
  
  #verify that we built a circle
  plot(plycds,asp=1,type="l")
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
  #build sp objects
  ply1a = sp::Polygon(plycds)
  ply1b = sp::Polygon(plycds+50)
  ply2a = sp::Polygons(list(ply1a),ID=1)
  ply2b = sp::Polygons(list(ply1b),ID=2)
  ply2c = sp::Polygons(list(ply1a,ply1b),ID=1)
  #same data, two ways to store it
  ply3a = SpatialPolygons(list(ply2a,ply2b))
  ply3b = SpatialPolygons(list(ply2c))
  #take a look
  ply3a
```

    ## class       : SpatialPolygons 
    ## features    : 2 
    ## extent      : -99.94965, 150, -99.98741, 149.9874  (xmin, xmax, ymin, ymax)
    ## crs         : NA

``` r
  plot(ply3a)
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
  ply3b
```

    ## class       : SpatialPolygons 
    ## features    : 1 
    ## extent      : -99.94965, 150, -99.98741, 149.9874  (xmin, xmax, ymin, ymax)
    ## crs         : NA

``` r
  plot(ply3b)
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
  #finish the object
  ply4a = SpatialPolygonsDataFrame( ply3a , data.frame(id=1:length(ply3a)))
  ply4b = SpatialPolygonsDataFrame( ply3b , data.frame(id=1:length(ply3b)))
  #look at final products
  ply4a
```

    ## class       : SpatialPolygonsDataFrame 
    ## features    : 2 
    ## extent      : -99.94965, 150, -99.98741, 149.9874  (xmin, xmax, ymin, ymax)
    ## crs         : NA 
    ## variables   : 1
    ## names       : id 
    ## min values  :  1 
    ## max values  :  2

``` r
  ply4b
```

    ## class       : SpatialPolygonsDataFrame 
    ## features    : 1 
    ## extent      : -99.94965, 150, -99.98741, 149.9874  (xmin, xmax, ymin, ymax)
    ## crs         : NA 
    ## variables   : 1
    ## names       : id 
    ## value       :  1

``` r
  par(mfrow=c(1,2))
  plot(ply4a,col=ply4a@data[,"id"])
  plot(ply4b,col=ply4b@data[,"id"])
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
  #pretty interesting, in the second approach, R interprets the overlap as void...
```

**Create SpatialLinesDataFrame**

Lines use a similar process to polygons. I have taken a shortcut here
and based my example on the sp vignette.

``` r
  # based on example from the sp vignette:
  l1 = cbind(c(1,2,3),c(3,2,2))
  rownames(l1) = letters[1:3]
  l1a = cbind(l1[,1]+.05,l1[,2]+.05)
  rownames(l1a) = letters[1:3]
  l2 = cbind(c(1,2,3),c(1,1.5,1))
  rownames(l2) = letters[1:3]
  Sl1 = Line(l1)
  Sl1a = Line(l1a)
  Sl2 = Line(l2)
  S1 = Lines(list(Sl1, Sl1a), ID="a")
  S2 = Lines(list(Sl2), ID="b")
  Sl = SpatialLines(list(S1,S2))
  summary(Sl)
```

    ## Object of class SpatialLines
    ## Coordinates:
    ##   min  max
    ## x   1 3.05
    ## y   1 3.05
    ## Is projected: NA 
    ## proj4string : [NA]

``` r
  plot(Sl, col = c("red", "blue"))
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
  #create sp dataframe
  sldf = SpatialLinesDataFrame(Sl, data.frame(id = names(Sl), someY=rnorm(length(Sl)),row.names=names(Sl)), match.ID = TRUE)
  
  #plot spatial objects
  sp::spplot(sldf,"someY",lwd=3)
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

buffer lines to make polygons

``` r
  lnbuff1 = raster::buffer(sldf,.5,dissolve=F) 
```

    ## Loading required namespace: rgeos

``` r
  lnbuff2 = raster::buffer(sldf,.5,dissolve=T) 
  #or
  lnbuff1 = rgeos::gBuffer(sldf,width=.5,byid = F ) 
  lnbuff2 = rgeos::gBuffer(sldf,width=.5,byid = T) 
  
  #look at results
  lnbuff1
```

    ## class       : SpatialPolygons 
    ## features    : 1 
    ## extent      : 0.5055763, 3.55, 0.5055763, 3.543844  (xmin, xmax, ymin, ymax)
    ## crs         : NA

``` r
  lnbuff2
```

    ## class       : SpatialPolygonsDataFrame 
    ## features    : 2 
    ## extent      : 0.5055763, 3.55, 0.5055763, 3.543844  (xmin, xmax, ymin, ymax)
    ## crs         : NA 
    ## variables   : 2
    ## names       : id,              someY 
    ## min values  :  a, -0.682495193464782 
    ## max values  :  b, -0.540171410426587

``` r
  par(mfrow=c(1,2))
  raster::plot(lnbuff1)
  raster::plot(sldf,col=1:length(sldf),add=T)
  raster::plot(lnbuff2)
  raster::plot(sldf,col=1:length(sldf),add=T)
```

![](build_sp_objects_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
