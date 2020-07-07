#'@title
#'  create colored flextable from input dataframe and write to ms word
#'
#'@description
#'  create colored flextable from input dataframe and write to ms word
#'
#'@details
#'
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
#'1.0 \tab 7/7/2020 function created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param x input data.frame
#'@param colFields numeric fields to color
#'@param cols sequence of colors as from colorRamps::blue2red(40)
#'@param breaks (optional) sequence of break values to use in slicing x into colors
#'@param breaksType (optional) if breaks = NA, then use either quantile or equal interval breaks
#'@param outDocx (optional) provide location to write flex table as word document
#'@param preview should a temporary word document with flex table be opened
#'@param digits how many digits should be retained (round) for numeric values
#'@param return T/F should a flextable be returned?
#'
#'@return
#'  NULL or a flex table if return == T
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import flextable
#'
#'@export
#
#'@seealso \code{\link{print.flextable}}\cr \code{\link{flextable}}\cr \code{\link{quantile}}\cr \code{\link{save_as_docx}}\cr

#Desired upgrades to this function:
#
#

color.flextable = function(
                   x
                   ,colFields
                   ,cols = colorRamps::blue2red(40)[10:40]
                   ,breaks = NA
                   ,breaksType = c("quantile","equal")
                   ,outDocx=NA
                   ,preview=T
                   ,digits=0
                   ,return=T
){

  ncolors = length(cols)
  x_in = x
  x_in[,colFields] = round(x[,colFields],digits)

  #prepare breaks
  if(!is.na(breaks[1])) breaks_in = breaks
  if(is.na(breaks[1])){

    if(breaksType == "quantile" ) breaks_in = quantile(unlist(x_in[,colFields]), seq(0,1,length.out = length(cols)))
    if(breaksType == "equal" )    breaks_in = seq(min(x_in[,colFields]),max(x_in[,colFields]) , length.out = ncolors )
  }

  #fix duplicates in breaks (if data are clumped e.g.)
  dups = duplicated(breaks_in)
  ndups = sum(dups)
  if(ndups  > 0 ){
    breaks_in = unique(breaks_in)
    omit_cols = round((1:ndups)*(ncolors/(ndups + 1)))
    cols_in = cols[-omit_cols]
    warning("There were ",ndups," duplicate breaks. These colors were removed:",cols[omit_cols])
  }else{
    cols_in = cols
  }

  #cut inputs
  cutIn <- cut(
    as.matrix(x_in[,colFields])
    ,breaks = breaks_in
    ,include.lowest = TRUE
    ,label = FALSE
  )
  #assign colors to recors
  colsIn = cols_in[cutIn]

  #prepare ft table
  ftIn1 = flextable::flextable(x)
  ftIn2 = flextable::colformat_num(ftIn1 , j = colFields, digits = digits)
  ftIn3 = flextable::bg(ftIn2, j = colFields, bg = colsIn)

  #preview / write table
  flextable:::print.flextable(ftIn3 , preview = "docx")
  if(!is.na(outDocx))  flextable::save_as_docx(ftIn3 , values = NULL, path = outDocx)

  if(return) ftIn3

}
