#
#'@title 
#' xy plot of variables in two dataframes
#'
#'@description 
#'
#'accepts two dataframes and prepares pairplots, results in fewer combinations than plot(xy_df)
#'
#'@details
#'
#'<Delete and Replace>
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 Jan 29 Roxygen header prepared \cr
#'}
#'
#'
#'@author
#'Jacob Strunk <Jacob.strunk@@dnr.wa.gov> 

#'
#'@param x first data.frame
#'@param y second data.frame
#'@param str_max number of characters per name to display
#'@param oneLine T/F add one to one line?
#'@param mai_edge "mai" parameters for composite figure
#'@param tile optional figure title
#'@param ... additional arguments to plot
#'
#'@return
#'<Delete and Replace>
#'
#'@examples
#'
#' pairs2( x = x_ht_debug ,  y = y1 , title = "Hello" )
#'
#'
#'@seealso \code{\link{plot}}\cr 
#'
#'

#'@export
pairs2=function(
                x
                ,y
                ,str_max = 12
                ,oneLine = TRUE
                ,mai_fig = c(.3,.3,.3,0)
                ,mai_edge = c(0,0,0,0) 
                ,title = NA
                ,lattice = F
                ,...
                ){
        
        if(!lattice){
                
                par( omi = mai_fig )

                ncols = length(names(x))
                nrows = length(names(y))
                col_widths = c(.9,rep(.9,ncols-1))
                row_heights = c(rep(.9,nrows-1),.9)
                        
                ly_in = layout(matrix(1:(ncols*nrows), nrows , ncols , byrow = TRUE),
                       widths = col_widths,  heights = row_heights
                )
                #layout.show(ly_in)

                #function to iterate across y
                plot_ij=function(i,j ){
                        
                        #prepare margins
                        v_mai=c(0,0,0,0)
                        if( i==1 ) v_mai[2] = mai_edge[2]
                        if( i==ncols ) v_mai[4] = mai_edge[4]
                        if( j==1 ) v_mai[3] = mai_edge[3]
                        if( j==nrows ) v_mai[1] = mai_edge[1]
                        par( mai=v_mai )
                        
                        #plot data
                        plot(x[,i],y[,j],xaxt="n",yaxt="n",...,xlab="",ylab="")
                        if(oneLine){
                                par(xpd=F)
                                lines(c(-10e6,10e6),c(-10e6,10e6))
                                par(xpd=T)
                        }                
                        
                        #print y axis when in position 1
                        if( i==1 ) mtext(substr(names(y)[j],1,str_max),2,1)
                        if (j==nrows) mtext(substr(names(x)[i],1,str_max),1,1)
                }
                
                nm_combn = data.frame(expand.grid(col_id = 1:length(names(x)),row_id=1:length(names(y))))
                mapply(plot_ij, nm_combn[,1] ,nm_combn[,2]  )

                if(!is.na(title)){
                        par(xpd=F)
                        mtext(title, outer = TRUE, cex = 1.5 , line = .5)
                }
        }
        if(lattice){
                
                
                     
        }

}


pairs2( x = x_ht ,  y = y1 , title = "Response PCs Versus Height PCs" )
