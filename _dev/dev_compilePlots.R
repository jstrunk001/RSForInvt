#'@title
#'  <Delete and Replace>
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import parallel plyr
#'
#'@export
#
#'@seealso \code{\link{compileTrees}}\cr

#Desired upgrades to this function:
#
#

##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


compilePlots = function(tl,plot_area="Plot_size",IDs=c("Plot"), name_spp = "_genus_pine_", nm_hw = "_hw_" , nclus=4){

  #split tree list in preparation for compilation
  spl_tlComp1 = split(tl,tl_comp1[,IDs])

  #run compilation in parallel
  #library(parallel)
  clus1 = makeCluster(4)
  l_comp = parLapply(clus1, spl_tlComp1 , .compile1plot)
  stopCluster(clus1)

  #merge compile plots
  plots_in = plyr::rbind.fill( l_comp )

  plots_in

}


.compile1plot = function(tli,plot_area="Plot_size",IDs=c("Plot"), name_spp = "_genus_pine_", nm_hw = "_hw_"){

  nms_genus = grep(name_spp,names(tli),value=T)
  nms_hw = grep(nm_hw,names(tli),value=T)
  nms_sum = c(nms_genus, nms_hw)

  #prep df for results

  dfi = data.frame(tli[1,IDs,drop=F])
  tli[,nms_sum][is.na(tli[,nms_sum])] = 0

  #comput ba by genus and hw/sw
  ba_genus = apply(apply(tli[,nms_sum],2,function(x,y){x/y}, tli[,plot_area]),2,sum)
  dfi[1,names(ba_genus)] = ba_genus
  dfi[1,nms_sum][is.nan(unlist(dfi[1,nms_sum]))] = 0

  #get dominant genus / hw vs sw
  if(apply(dfi[1,nms_sum],1,sum)>0){

    dfi[1,"dom_genus"] = gsub(paste(".*",name_spp,sep=""),"",nms_genus[which.max(dfi[1,nms_genus])])
    dfi[1,"dom_hwsw"] = c("hw","sw")[1+ (dfi[1,"ba_ft_hw_0"] > dfi[1,"ba_ft_hw_1"]) ]
  }

  dfi
}
