
##Header for DNR remote sensing inventory project
#'@name
#'fit_pdf
#'
#'@title
#'fit and compare pdfs
#'
#'@description
#' fit and compare pdfs, three indices are reported which are similar to R-square, RMSE, and standard error
#' for the comparisons. One of the fits, the baseline, is treated as the population pdf (much like the population
#' mean when computing r-squared).
#'
#' a pdf fitting function is specialized to enable comparison across a desired range
#' a function which computes fitting indices which enable comparison between fitted pdfs
#'
#'@details
#'
#'fit multiple pdfs with fit_pdf, and specify the range of possibly y values across with comparisons should be made
#'then run pdf_compare to evaluate the relative performance of the fit
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 4/1/2014 operation - header needs work \cr
#'}
#'
#'
#'@author
#'Jacob Strunk <Jacob.strunk@@dnr.wa.gov>
#'
#'@param y vector of respone values
#'@param wts optional vector of weights for each y
#'@param bins bins to use in fitting pdf (pmf..)
#'@param lims upper and lower limits to compare across
#'@param MA T/F use moving average smoother?
#'@param MA_n number of bins to include in smoother
#'@param plot T/F plot histograms of pdf?
#'
#'@param fit_pdf1 first pdf to use in comparison - object returned by fit_pdf()
#'@param fit_pdf2 second pdf to use in comparison - object returned by fit_pdf()
#'@param plot T/F plot two histograms?
#'@param add T/F histogram to existing plot? - in the case of multiple comparisons...
#'@param ... additional arguments to plot

#'
#'@return
#'fit_pdf: a pdf specially prepared to be handled by pdf_compare
#'
#'@import spatstat TTR
#'
#'@examples
#'
#'             pdf("c:/temp/test.pdf")
#'             x1=fit_pdf(rnorm(500,0,1),plot=F,bins=seq(-10,200,1))
#'             x2=fit_pdf(rnorm(500,0,1),plot=F,bins=seq(-10,200,1))
#
#'             pdf_compare(x1,x2,plot=T)
#
#'             x1=fit_pdf(rnorm(500,0,1),plot=F,bins=seq(100,200,1))
#'             x2=fit_pdf(rbeta(500,1,1),plot=F,bins=seq(100,200,1))
#
#'             pdf_compare(x1,x2,plot=T)
#'             dev.off()
#'
#'
#'
#'
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr
#'
#'@export
#'@rdname fit_pdf
fit_pdf=function(
                 y
                 ,wts=NA
                 ,bins=NA
                 ,breaks=NA#seq(min(y),max(y),(max(y)-min(y))/50)
                 ,lims=range(c(y,bins))
                 ,MA=TRUE
                 ,MA_n=3
                 ,plot=F
                 ){
                  require(spatstat)
                  require(TTR)
                  require(plotrix)

                  if(!is.na(bins[1])){

                    bins=sort(bins)
                    bw=abs(bins[2]-bins[1])
                    breaks=c(bins-bw/2,max(bins)+bw/2)

                  }

                  if(!is.na(bins[1])){

                    breaks=sort(breaks)
                    bw=abs(breaks[2]-breaks[1])
                    bins=bins[-length(breaks)]+bw/2

                  }

                  if(length(y)==0 ){

                       warning("empty y vector, pdf values set to zero")
                       return( data.frame( bins=bins,y_freq=0,y_dens=0))

                  }

                  if(length(unique(y))==1){

                     if(!is.na(wts[1])) wts=c(wts,wts)/2
                     else  wts= rep(0.5/length(y),2*length(y))
                     y=jitter(c(y,y))

                  }

                  if(!is.na(wts[1])) {
                    h_in=weighted.hist(y,w=wts,breaks=breaks,plot=FALSE)
                  }else{
                    h_in=hist(y,breaks=breaks,plot=FALSE)
                  }

                # browser()

                  if(MA ){

                    #moving average
                    require(TTR)
                    sma_c=c(rep(0,MA_n-1),SMA(h_in[["counts"]],MA_n)[-(1:(MA_n-1))])
                    sma_c[(1:(MA_n-1))]=sma_c[MA_n]
                    sma_d=c(rep(0,MA_n-1),SMA(h_in[["density"]],MA_n)[-(1:(MA_n-1))])
                    sma_d[(1:(MA_n-1))]=sma_d[MA_n]

                    h_in[["counts"]]=sma_c
                    h_in[["density"]]=sma_d

                    if(plot) lines(bins,sma_c,col="red",lwd=2)

                  }

                 return(
                   data.frame(
                               bins=h_in[["mids"]]
                               ,y_freq=h_in[["counts"]]
                               ,y_dens=h_in[["density"]]
                           )
                      )
             }


#'@export
#'@rdname fit_pdf
pdf_compare=function(
                       fit_pdf1
                       ,fit_pdf2
                       ,plot=TRUE
                       ,legend=T
                       ,xlab="bin"
                       ,ylab="y_dens"
                       ,add=F
                       #,rebin=T
                       ,...
                       ){

                  #re-bin to match both datasets?


                  #do bins match
                  if(length(fit_pdf1[,1] )==0 | length(fit_pdf2[,1] )==0){
                       warning("Provided pdf object is of length zero")
                       return(data.frame(test=c("R2","ssd","sd_rel"),index_dens=NA,index_freq=NA))
                  }
                  if(
                     sum(fit_pdf1[,1] %in% fit_pdf2[,1]) !=nrow(fit_pdf1) |
                      sum(fit_pdf2[,1] %in% fit_pdf1[,1]) !=nrow(fit_pdf2)
                      )
                                      stop("Bins don't match between provided densities")


                  #prepare indices
                  indices=data.frame(
                             test=c("R2","ssd","sd_rel")
                             ,
                             index_dens=round(unlist(c(
                                #1-sum((diff(fit_pdf1[,"y_dens"])-diff(fit_pdf2[,"y_dens"]))^2)/sum(c(abs(diff(fit_pdf1[,"y_dens"])+diff(fit_pdf2[,"y_dens"])))^2,na.rm=TRUE)
                                1-sum((fit_pdf1[,"y_dens"]-fit_pdf2[,"y_dens"])^2)/sum(((fit_pdf1[,"y_dens"]-mean(fit_pdf1[,"y_dens"]))^2),na.rm=TRUE)
                                #1-sum((fit_pdf2[,"y_dens"]-fit_pdf1[,"y_dens"])^2)/sum((fit_pdf2[,"y_dens"]+fit_pdf1[,"y_dens"])^2)
                                ,sum(((fit_pdf1[,"y_dens"])-(fit_pdf2[,"y_dens"]))^2)
                                ,sqrt(mean((fit_pdf1[,"y_dens"]-fit_pdf2[,"y_dens"])^2))/mean(fit_pdf1[,"y_dens"])
                              )),5)
                             ,index_freq=round(unlist(c(
                                1-sum((fit_pdf2[,"y_freq"]-fit_pdf1[,"y_freq"])^2)/sum((fit_pdf1[,"y_freq"]+fit_pdf2[,"y_freq"])^2,na.rm=TRUE)
                                ,sum((fit_pdf1[,"y_freq"]-fit_pdf2[,"y_freq"])^2)
                                ,sqrt(mean((fit_pdf1[,"y_freq"]-fit_pdf2[,"y_freq"])^2))/mean(fit_pdf1[,"y_freq"])
                              )),5)
                              ,row.names=NULL
                           )
                  #plot distributions
                  if(plot){

                   if(!add){
                       plot(
                         fit_pdf1[,c("bins","y_dens")]
                         ,type="l"
                         ,ylim=c(range(c(fit_pdf1[,c("y_dens")],fit_pdf2[,c("y_dens")])))
                         ,xlim=c(range(c(fit_pdf1[,c("bins")],fit_pdf2[,c("bins")])))
                         ,xlab=xlab
                         ,ylab=ylab
                         ,lwd=2
                         ,...
                       )
                       if(legend) legend("topright",legend=c("pdf1","pdf2",paste("Index_I=",round(indices[1,2],2))),lty=c(1,1,0),lwd=2,col=c(1,2,0),inset=.01)
                       lines(fit_pdf2[,c("bins","y_dens")],col="red",lwd=2)
                   }else{

                   lines(fit_pdf1[,c("bins","y_dens")],lwd=2,col=3)
                   if(legend) legend("bottomright",legend=c("pdf1","pdf2",paste("Index_I=",round(indices[1,2],2))),lty=c(1,1,0),lwd=2,col=c(3,4,0),inset=.01)
                   lines(fit_pdf2[,c("bins","y_dens")],col=4,lwd=2)

                   }
                  }
                  return(indices)
              }



