map2pdf=function(pdf_out
                 ,dir_temp="c:\\temp"
                 ,ext #required, extent of first object plotted
                 #,crs=NA
                 ,res=300
                 ,xy_inches=6
                 ){

  #require("gdalUtils")
  require("raster")

  #query graphics
  #par_in=par()

  graph_in=recordPlot()

  # #save plot
  # tif_temp=file.path(dir_temp,gsub("[.]tif","_temp",basename(pdf_out)))
  # windows()
  # replayPlot(graph_in)
  # savePlot(tif_temp,"tif")

  # graph_in=recordPlot()
  #
  # graph_in[[1]][[19]][[2]][[2]]$mai=c(0,0,0,0)
  # graph_in[[1]][[19]][[2]][[2]]$mar=c(0,0,0,0)

  #par(mar=c(0,0,0,0))
  #replayPlot(graph_in)

  par_in=par()
  dims_in=dev.size(units = c("in", "cm", "px"))
  mai_in=par_in$mai
  pin_in=par_in$pin
  plt_in=par_in$plt
  usr_in = par_in$usr
  din_in = par_in$din
  fin_in = par_in$fin

  #manually calculate mai - default mai does not account for legend
  mai_in = c(plt_in[3] * fin_in[2],plt_in[1] * fin_in[1],fin_in[2] - plt_in[4] * fin_in[2],fin_in[1] - plt_in[2] * fin_in[1])
  #mai_in=c(1.30, 1, 1, 1.45)

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
  lx=xmin(ext) - mai_in[2] * xyrat
  ly=ymin(ext) - mai_in[1] * xyrat
  ux=xmax(ext) + mai_in[4] * xyrat
  uy=ymax(ext) + mai_in[3] * xyrat

  #export plot a file
  tif_temp=file.path(dir_temp,gsub("[.]tif","_temp.tif",basename(pdf_out)))


  #replayPlot(graph_in)

  #tiff(tif_temp,height=y_in,width=x_in,units="in",res=res)
  tiff(tif_temp,height=fin_in[2],width=fin_in[1],units="in",res=res)

  replayPlot(graph_in)
  dev.off()

  #load / georeference plot
  r_map = brick(tif_temp)
  extent(r_map) = c(lx,ux,ly,uy)

  if(T){

    plotRGB(r_map)
    #plot(r_map[[1]])
    plot(rtest,add=T,legend=F,alpha=.5)

  }


  if(F){
    extent(r_map) = c(lx,ux,ly,uy)
    extent(r_map) = c(lx-1,ux+1,ly-1,uy+1)

    plotRGB(r_map)
    plot(rtest,add=T,legend=F,alpha=.5)
    extent(rtest)
    extent(r_map)

    plot(as(extent(r_map),"SpatialPolygons"),add=T)
    plot(as(extent(rtest),"SpatialPolygons"),add=T)

    #oringal plot
    windows(height=y_in,width=x_in,restoreConsole = TRUE)
    replayPlot(graph_in)
    plot(as(extent(rtest),"SpatialPolygons"),add=T)
    points(1.25*300,1.7*300,col="red",pch=16,cex=2)

  }

  #plot(r_map)


  #raster(tif_temp,xmin=lx,ymin=ly,xmax=ux,ymax=uy)
#  r_map = raster(tif_temp)

  #write final geo-referenced raster
  #writeRaster(r_map,filename=pdf_out,format="GTiff",datatype="FLT4S",overwrite=T)
  writeRaster(r_map,filename=pdf_out,overwrite=T)
  unlink(tif_temp)

  #gdal_translate(tif_temp,pdf_out,of = "GTiff")


}

if(F){
library(raster)

set.seed(55)
xy=data.frame(x=rnorm(50)*5000+200,y=rnorm(50)*15000+200)
coordinates(xy)=~x+y
rtest=raster(ext=extent(xy),res=50)
rtest[] = rnorm(length(rtest))

#par(mai=c(0,0,0,0),mar=c(0,0,0,0))
plot(rtest)
plot(xy,add=T,pch=16)
#plot(as(extent(xy),"SpatialPolygons"),add=T)


map2pdf(pdf_out="c:\\temp\\test1gpdf01.tif",ext=extent(rtest))
writeRaster(rtest,"c:\\temp\\test1gpdf_original.tif",overwrite=T)

writeRaster(rtest,"test1",format="PDF",overwrite=T)

#plot(raster("c:\\temp\\test1gpdf1.tif"))
}


#
# graph_1=recordPlot()
# par1=par()
# tiff("c:\\temp\\testplot.tif",width = 2*480, height = 2*480)
# graph_1
# dev.off()
#
# r1=raster("c:\\temp\\testplot.tif")
# plot(r1)

