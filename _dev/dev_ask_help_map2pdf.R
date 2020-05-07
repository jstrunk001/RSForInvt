map2tiff=function(tiff_out
                 ,dir_temp="c:\\temp"
                 ,ext #required, extent of first object plotted
                 #,crs=NA
                 ,res=300
                 ,xy_inches=6
){

  #require("gdalUtils")
  require("raster")

  #query graphics
  graph_in=recordPlot()
  par_in=par()
  dims_in=dev.size(units = c("in", "cm", "px"))
  mai_in=par_in$mai
  pin_in=par_in$pin
  plt_in=par_in$plt
  usr_in = par_in$usr
  din_in = par_in$din
  fin_in = par_in$fin

  #manually calculate mai - default mai does not account for legend when plotting a raster imager
  mai_in = c(plt_in[3] * fin_in[2],plt_in[1] * fin_in[1],fin_in[2] - plt_in[4] * fin_in[2],fin_in[1] - plt_in[2] * fin_in[1])*1.5

  #compute new graph dimensions etc.
  dx=xmax(ext) - xmin(ext)
  dy=ymax(ext) - ymin(ext)
  x_rat=min(1,dx/dy)
  y_rat=min(1,dy/dx)
  x_in = xy_inches[1]*x_rat + sum(mai_in[c(2,4)])
  y_in = xy_inches[1]*y_rat + sum(mai_in[c(1,3)])
  xrat= dx / x_in
  yrat= dy / y_in
  xyrat = min(xrat,yrat)

  #create new extent to include margins
  lx=xmin(ext) - mai_in[2] * xrat
  ly=ymin(ext) - mai_in[1] * yrat
  ux=xmax(ext) + mai_in[4] * xrat
  uy=ymax(ext) + mai_in[3] * yrat

  #export plot to a file
  tif_temp=file.path(dir_temp,gsub("[.]tif","_temp.tif",basename(tiff_out)))
  tiff(tif_temp,height=y_in,width=x_in,units="in",res=res)
  replayPlot(graph_in)
  dev.off()

  #load and georeference map
  r_map = brick(tif_temp)
  extent(r_map) = c(lx,ux,ly,uy)

  #write final geo-referenced raster
  writeRaster(r_map,filename=tiff_out,overwrite=T)
  unlink(tif_temp)

}

if(F){
  library(raster)

  set.seed(55)

  xy=data.frame(x=rnorm(50)*5000+200,y=rnorm(50)*15000+200)
  coordinates(xy)=~x+y
  rtest=raster(ext=extent(xy),res=50)
  rtest[] = rnorm(length(rtest))
  writeRaster(rtest,"c:\\temp\\tiff001.tif",overwrite=T)

  plot(rtest)
  plot(xy,add=T,pch=16)

  map2tiff(tiff_out="c:\\temp\\maptiff001.tif",ext=extent(rtest))

  rdat=raster("c:\\temp\\maptiff001.tif")
  rmap=raster("c:\\temp\\maptiff001.tif")

  plot(rmap)
  plot(rdat,add=T,alpha=.5)
}
