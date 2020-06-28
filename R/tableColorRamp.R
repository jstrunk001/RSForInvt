#'@title
#'  Makes a table with cells colored by relative rank (e.g. blue to red)
#'
#'@description
#'  Makes a table with cells colored by relative rank (e.g. blue to red)
#'
#'@details
#'  Uses the image function to make a table of colors and writes input data values to color cells
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
#'1.0 \tab 6/28/2020 Created and added to package\cr
#'
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param  data data frame of input values to plto
#'@param  colorRamp color ramp function, e.g. colorRamps package or grDevices::colorRampPalette(c("blue", "red"))
#'@param  nColors integer number of colors
#'@param  pdfOut optional output pdf file, NA will result in plotting the R interface
#'@param  pdfHeight width of output pdf
#'@param  pdfWidth height of output pdf
#'@param  plotMar plot margins
#'@param  plotMain main argument to image function
#'@param  legendPrecision decimal precision to use for legend object
#'@param ... additional arguments to image function
#'
#'@return
#'  NULL - makes a plot or writes plot to pdf file
#'
#'@examples
#'
#'    test = data.frame(matrix(rnorm(100)*100,nrow = 10))
#'    tableColorRamp(test)
#'
#'
#'@export
#'@seealso \code{\link{image}}\cr

#Desired upgrades to this function:

tableColorRamp = function(
                          data
                          #, colorRamp = grDevices::colorRampPalette(c("blue","dodgerblue3","yellow","firebrick1",  "firebrick4"))
                          , colorRamp = colorRamps::blue2red
                          , nColors = 20
                          , pdfOut = c("c:\\temp\\test.pdf",NA)[1]
                          , pdfHeight = 8
                          , pdfWidth = 11
                          , plotMar = c(8,8,8,10)
                          , plotMain = "Table Of Values"
                          , legendPrecision = 0
                          , addStat = c(NA,median,mean,sum)[1]
                          , statText = "-MEDIAN-"
                          , statLine = 2
                          ,...
                          ){

  if(!is.na(addStat)){
    dat_in = rbind(apply(data,2,addStat),data)
    row.names(dat_in)[1] = statText
  }else{
    dat_in = data
  }

  if(!is.na(pdfOut)) pdf( pdfOut , height=pdfHeight , width=pdfWidth )
  par( mar = plotMar )

  image( 1:ncol(dat_in) , 1:nrow(dat_in) , t(dat_in) , col = colorRamp(nColors) , axes = FALSE,xlab="",ylab="", main = plotMain, ...)
  axis(1, 1:ncol(dat_in), colnames(dat_in),las=2)
  axis(2, 1:nrow(dat_in), rownames(dat_in),las=2)
  for (x in 1:ncol(dat_in))
    for (y in 1:nrow(dat_in))
      text(x, y, round(dat_in[y,x]))

  dat_in_range = range(dat_in)

  ncols_in = ncol(dat_in)
  nrows_in = nrow(dat_in)

  plotrix::color.legend(ncols_in + 1.5,1,ncols_in + 2,nrows_in
                        , round(seq(dat_in_range[1],dat_in_range[2] , length.out = nColors),legendPrecision)
                        , colorRamp(nColors)
                        , gradient="y"
                        )

  if(!is.na(addStat)) abline(h=1.5,lwd=3)

  if(!is.na(pdfOut)) dev.off()

}


if(F){

  test = data.frame(matrix(rnorm(100)*100,nrow = 10))
  tableColorRamp(test)


}

