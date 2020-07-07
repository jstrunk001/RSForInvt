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
                          , colsColor = NA
                          , nColors = 20
                          , pdfOut = c("c:\\temp\\test.pdf",NA)[1]
                          , pdfHeight = 8
                          , pdfWidth = 11
                          , plotMar = c(8,8,8,10)
                          , plotMain = "Table Of Values"
                          , legendPrecision = 0
                          , legendPos = list(xl = NA,NA,xr = NA ,yt = NA)
                          , addStat = c(NA,median,mean,sum)[1]
                          , statDirection = c("vertical","horizontal")
                          , statText = "-MEDIAN-"
                          , statLine = 2
                          , plotRowNames = T
                          , plotColNames = T
                          , vLines = NA
                          , hLines = NA

                          ,...
                          ){

  if(is.function(addStat)){
    if(statDirection[1] == "vertical"){
      dat_in1 = plyr::rbind.fill(data.frame(t(apply(data[,colsColor],2,addStat))),data)
      row.names(dat_in1) = c( statText, row.names(data))
    }
    if(statDirection[1] == "horizontal"){
      dat_in1 = data.frame(stat=apply(data[,colsColor],1,addStat),data)
      names(dat_in1)[names(dat_in1)=="stat"] = statText
      if(!is.na(colsColor[1])) colsColor = c(colsColor,statText)

    }
  }else{
    dat_in1 = data
  }

  #deal with columns without color
  if(!is.na(colsColor[1])) colsWhite = names(data)[! names(data) %in% colsColor ]
  else{
    colsWhite = NULL
    colsColor = names(dat_in1)
  }

  #prepare color matrix with NA values for non-numeric columns
  if(length(colsWhite) > 0){
    dat_in2 = dat_in1[,c(colsWhite,colsColor),drop=F]
    dat_in3 = dat_in2
    dat_in3[,colsWhite] = NA
    mat_col =   t(dat_in3)
  }else{
    dat_in3 = dat_in1
    mat_col =   t(dat_in3)
  }

  if(!is.na(pdfOut)) pdf( pdfOut , height=pdfHeight , width=pdfWidth )
  par( mar = plotMar )

  #add colors to plot, first one is white to allow for fields without color
  #ncol_plot = ncol(data) + !is.function(addStat)
  #graphics::image( 1:ncol_plot , 1:nrow(dat_in1) , t(dat_in1) , col = "white" , axes = FALSE,xlab="",ylab="", main = plotMain, ...)
  graphics::image(   1:ncol(dat_in3)
                   , 1:nrow(dat_in3)
                   , mat_col
                   , col = colorRamp(nColors)
                   , axes = FALSE
                   ,xlab=""
                   ,ylab=""
                   , main = plotMain
                   , add = F
                   ,...)

  #add axes

  if(plotRowNames) axis(3, 1:ncol(dat_in2), colnames(dat_in2),tick = F , line = -1, gap.axis = -1 , las=1)
  if(plotColNames) axis(2, 1:nrow(dat_in2), rownames(dat_in2),tick = F , line = NA,las=2)
  #if(plotColNames) axis(2, 1:nrow(dat_in1), rownames(dat_in2),tick = F , line = NA,las=2)

  #add values to plot
  for (x in 1:ncol(dat_in2))
    for (y in 1:nrow(dat_in2)){
      if(names(dat_in2)[x] %in% colsColor) if(!is.na(dat_in2[y,x])) text(x, y, round(dat_in2[y,x],legendPrecision))
      if(!names(dat_in2)[x] %in% colsColor) if(!is.na(dat_in2[y,x])) text(x, y, dat_in2[y,x])
    }

  #add text values
  if(length(colsWhite)>1){
    for (x in 1:length(colsWhite))
      for (y in 1:nrow(dat_in1))
        text(x, y, dat_in1[row.names(dat_in1)[y],colsWhite[x]])
  }

  #prepare legend position
  if(is.na(legendPos[1])){
    ncols_in = ncol(dat_in2)
    nrows_in = nrow(dat_in2)
    legendPos = list(xl = ncols_in + 1.3,yb = 1,xr = ncols_in + 1.8 ,yt = nrows_in)
  }

  #add legend to plot
  dat_in_range = range(dat_in3,na.rm = T)
  plotrix::color.legend(legendPos$xl,legendPos$yb,legendPos$xr,legendPos$yt
                        , round(seq(dat_in_range[1],dat_in_range[2] , length.out = nColors),legendPrecision)
                        , colorRamp(nColors)
                        , gradient="y"
                        )
  #add line to plot
  #if(is.function(addStat) & statDirection[1] =="vertical") abline(h=1.5,lwd=3)
  #if(is.function(addStat) & statDirection[1] =="horizontal") abline(v=ncol(dat_in2)-.5,lwd=3)
  if(!is.na(vLines[1])) abline(v=vLines)
  if(!is.na(hLines[1])) abline(h=hLines)
  #close pdf
  if(!is.na(pdfOut)) dev.off()

}


if(F){

  test = data.frame(matrix(rnorm(100)*100,nrow = 10))
  tableColorRamp(test)


}

