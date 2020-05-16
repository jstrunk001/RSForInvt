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
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param data (optional) either provide a list of data with appropriate names or provide dir_in
#'@param dir_in (optional) either provide a list of data with appropriate names or provide dir_in
#'@param dir_out where to place compiled plots
#'@param dbcl breaks to use in computing diameter classes
#'@param dbclY variables to cross with dbcl otherwise blank or NA
#'@param sppY variables to cross with spp otherwise blank or NA
#'@param sppDbclY which fields should be crossed with spp and dbcl
#'@param return T/F
#'@param sql_filter sql code to use in subsetting observations
#'@param ba_spp_dbcl (deprecated) -> transition to "spp_dbcl"
#'@param spp_dbcl which fields should be crossed with spp_dbcl
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import plyr parallel
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#updates to do:
#	enable sql queries agains each input table

compilePlots=function(

	data = list(
	              dfTree = data.frame(PLT_CN = 1, year = 2020 , dbh = NA , spcd = NA , ac = 0.1  )
	             , dfPlot = data.frame()
	             )
	,trNms = c( plotIds = c("PLOT", "YEAR") , trIds = c("tr_cn") , dbh = "DIA" , ht = "HT" , spcd = "SPCD" , trWt = "TPA" )
	,plotNms = c(pltIds = c( "STATE" , "COUNTY" , "PROJECT" , "PLOT" , "YEAR" ), pltWt = NA )

	,plot_filter = c(NA, "select * from dfPlot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")
	,tree_filter = c(NA, "select * from dfTree where dbh > 2 ")

	,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))

	,fnCompute = list(
	   plotWtMn(dfTree_in , trNms , dbh_units = c("in","cm") , ht_units = c("ft","m") )
	   ,plotWtSum( dfTree_in , trNms , vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT") )
	   #, other custom tree compilation functions
	 )

	,return = F
	,doDebug = F

	,nclus = 4

){

	require(plyr)
	require(sqldf)

	if(!"dfTree" %in% names(data)) stop("data must include \"dfTree\" table - \"dfTree\" was not found")
	if(!"dfPlot" %in% names(data)) warning("\"dfPlot\" table not found.\nIf only \"dfTree\" table is provided, please make sure their are NA dfTree records for plots with no trees on them.\n"
																			 ,"If plots without trees are omitted, you will have biased (too high) landscape estimates typically.\nA clearcut is an example of a"
																			 ,"'Forest' plot without any trees on it.")

	dir_out = dir_out[1]
	if(!is.na(dir_out)) if(!dir.exists(dir_out)) dir.create( dir_out , recursive = T )

	#check that all datasets have "CN" or "PLT_CN" field
	for(i in 1:length(data)){
		if(sum(!plotIDs %in% names(data[[i]])) > 0) stop(names(data)[[i]], paste(names(data)[i],"does not have some plotIDs:",plotIDs[!plotIDs %in% names(data[[i]])]))
	}

	#subset data before compilation
	if(!is.na(plot_filter[1]) & ("dfPlot" %in% names(data))){
		dfPlot_in = sqldf(plot_filter, envir=as.environment(data))
	}
	if((!"dfPlot_in" %in% ls()) & ("dfPlot" %in% names(data))){
		dfPlot_in = data$dfPlot
	}
	if(!is.na(tree_filter[1]) & ("dfTree" %in% names(data)) ){
		dfTree_in = sqldf(tree_filter, envir=as.environment(data))
	}
	if((!"dfTree_in" %in% ls()) & ("dfTree" %in% names(data))){
		dfTree_in = data$dfTree
	}

	#merge trees and plots
	if(("dfPlot_in" %in% ls()) & ("dfTree_in" %in% ls())){
		dat_in = merge(x=dfPlot, y = dfTree, by = plotIDs, all.x)
	}
	if((!"dfPlot_in" %in% ls()) & ("dfTree_in" %in% ls())){
		dat_in = dfTree
	}

	#get unique id fields
	if(length(plotIDs) > 1) ids_uniq = unique(dat_in[,plotIDs])
	if(length(plotIDs) == 1){
		ids_uniq = data.frame(X=unique(dat_in[,plotIDs]))
		names(ids_uniq) = plotIDs
	}

	#stub
	#to do - update code to not compile plots with bad tree ids ?
	if(F){
		no_tr = is.na(dat_in[trNms["id"],])
		dat_notree = dat_in[no_tr,]
		dat_tree = dat_in[!no_tr,]
	}
	spl_IDs = split(ids_uniq,1:nrow(ids_uniq),drop=T)
	if(doDebug) spl_IDs = spl_IDs[sample( length(spl_IDs),1000)]

	if(nclus == 1){
		res_i = lapply( spl_IDs , .compile_1plot , trs = dat_in, trNms = trNms , plotIDs = plotIDs, fnArg = fnArg, fnCompute = fnCompute)
	}
	if(nclus >1){

		clus_in=parallel::makeCluster(nclus)
		parallel::clusterExport(clus_in,c(".subsetIDs",".compile_1plot"),envir = environment())
		parallel::clusterEvalQ(clus_in,{gc()})

		#process plots
		res_i = parallel::parLapply(clus_in , spl_IDs , .compile_1plot , trs = dat_in,  trNms = trNms , plotIDs = plotIDs, fnCompute = fnCompute)

		closeAllConnections()
	}

	#bind plots
	res_in = plyr::rbind.fill(res_i[sapply(res_i,is.data.frame)])

	if(!is.na(dir_out)){
		out_csv = file.path(dir_out,"plot_compile.csv")
		out_rds = file.path(dir_out,"plot_compile.rds")
		write.csv(res_in,out_csv)
		saveRDS(res_in,out_rds)
	}
	if(return) return(dat_in)
	gc()
}

#compile tree list form one plot
.compile_1plot=function(
  id
  ,trs
  ,trNms
  ,plotNms
  ,fnCompute
)
{

  trs_i = .subsetIDs(trs,id)

  #get grouping variables as seed columns with id fields
  trs_ID = trs_i[1, plotIDs]

  #iterate through compute functions and append dataframes horizontally
  for(i in 1:length(fnCompute)){
    fni = fnCompute[[i]]
    #if(nrow(trs_i) > 1 ) browser()
    resi = try(fni(trs=trs_i, trNms = trNms , fnArg=fnArg[[i]] ))
    if(class(resi) == "try-error") return(NULL)
    if(i==1) res_in = data.frame(trs_ID, resi)
    if(i>1) res_in = data.frame(res_in, resi)
  }
  return(res_in)

}

#subset all dataframes in a list based on cnd
.subset_cn = function(data,cn){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		data_i = data[[i]][!is.na(data[[i]][,"PLT_CN"]) ,	]
		data[[i]] = data_i [data_i[,"PLT_CN"] == cn,	]
	}
	return(data)
}


.subset_IDs = function(data,IDs){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		IDsOK_i = names(IDs)[names(IDs) %in% names(data[[i]])]
		data[[i]] = merge(IDs,data[[i]],by = IDsOK_i)
	}
	return(data)
}


.subsetIDs = function(data,IDs){

	mrgi = merge(y=data,x=IDs, by = names(IDs), all.x = T, all.y=F)
	return(mrgi)
}





#compute attributes from trees by plot
#each function must return a dataframe with 1 row:  zeros or NAs as appropriate if there are no trees
#functions must have elipsis argument ...
#fnArg is an optional set of additional arguments specific to each function
plotWtSum = function(
	trs
	,trNms
	,fnArg
	,...
){
	#catch records without any actual trees - and set to zero
	bad_ids = is.na(trs[,trNms[["id"]]])
	tr_in = trs[!bad_ids,]
	sumNm_in = fnArg[["vSumNm"]][fnArg[["vSumNm"]] %in% names(tr_in)]
	if(length(fnArg[["vSumNm"]]) != length(sumNm_in)) warning("not all columns provided to plotWtSum with argument 'vSumNm' are present in table 'trs' - only columns present were summed")
	if(nrow(tr_in) > 0){
		sum_in = lapply( sumNm_in , function(colNm,wtNm,x,...) data.frame( sum(x[,colNm]*x[,wtNm],na.rm=T)), wtNm = trNms[["trWt"]] , x = tr_in )
		sum_in = data.frame(matrix(unlist(sum_in),nrow = 1))
		names(sum_in) = sumNm_in
	}else{
		sum_in = tr_in[1,sumNm_in]
		sum_in[1,sumNm_in] = 0
	}
	return(sum_in)
	gc()
}

plotWtMn = function(
	trs
	,trNms
	,fnArg
	,...
){

	#remove trees without ID fields
	bad_ids = is.na(trs[,trNms[["id"]]])
	tr_in = trs[!bad_ids,]

	#correct for NA weights
	tr_in[is.na(tr_in[,trNms[["dbh"]]]),] = 0
	tr_in[is.na(tr_in[,trNms[["trWt"]]]),] = 0

	if(nrow(tr_in) > 0){

		wtmn = data.frame(
			ntree = nrow(tr_in)
			,ba_ftac = sum(.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]],na.rm=T)
			,ba_ftac_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)
			,ba_m3ha_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)*(0.3048^2)/0.404686
			,qmd = sqrt(sum(tr_in[,trNms[["trWt"]]]*tr_in[,trNms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,trNms[["trWt"]]],na.rm=T))
			,lorht = sum(tr_in[,trNms[["ht"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,trNms[["dbh"]]]^2,na.rm=T)
		)

	}else{

		wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)

	}

	return( wtmn )

}


plotWtMn_B = function(
	trs
	,trNms
	,fnArg
	,...
){

	if(nrow(trs)>1)browser()
	bad_ids = is.na(trs[,trNms[["id"]]])
	tr_in = trs[!bad_ids,]

	wtmn = data.frame(

		ntree = nrow(tr_in)
		,ba_ftac = sum(.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]],na.rm=T)
		,ba_ftac_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)
		,ba_m3ha_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)*(0.3048^2)/0.404686
		,qmd = sqrt(sum(tr_in[,trNms[["trWt"]]]*tr_in[,trNms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,trNms[["trWt"]]],na.rm=T))
		,lorht = sum(tr_in[,trNms[["ht"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,trNms[["dbh"]]]^2,na.rm=T)

	)

	if(nrow(tr_in) > 0){
	}else{
		wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)
	}

	return( wtmn )

}


if(F){

  tltest = readRDS("D:\\Box\\VMARS\\Projects\\2019 Savannah River\\R\\Jacob Post-Stratification Evaluation\\data/tlManuscript_20200515.RDS")

  compilePlots(

    data = list(
      dfTree = tltest
      #, dfPlot = data.frame()
    )
    ,trNms = c( trIds = c("UNQ_TR") , pltIds = "Plot" , dbh = "DIA" , ht = "HT" , spcd = "SPCD" , trWt = "TPA" )
    ,plotNms = c(pltIds = c( "STATE" , "COUNTY" , "PROJECT" , "PLOT" , "YEAR" ) )

    ,plot_filter = c(NA, "select * from dfPlot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")
    ,tree_filter = c(NA, "select * from dfTree where dbh > 2 ")

    ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))

    ,fnCompute = list(
      plotWtMn(dfTree_in , trNms , dbh_units = c("in","cm") , ht_units = c("ft","m") )
      ,plotWtSum( dfTree_in , trNms , vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT") )
      #, other custom tree compilation functions
    )

    ,return = F
    ,doDebug = F

    ,nclus = 4

  )

}


if(F){

	source("compileTrees")
	if(!"tr1" %in% ls()) tr1 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\tr.rds")
	if(!"tr2" %in% ls()) tr2 = compileTrees(tr1)

}

if(F){


	#load and fix FIA data
	if(!"fiaDat" %in% ls()){

		dat_paths = list.files("D:\\data\\RFIA\\NIMS\\2018-10-24\\",full.names=T,pattern="[.]rds$")
		tree0 = compileTrees(readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\tr.rds"))
		plot0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\pl_snp.rds")
		plot0[,c("PLT_CN")] = plot0[,c("CN")]	#fix weird cn inconsistency
		cond0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\cond.rds")

		fiaDat = list(
			plot = plot0
			,tree = tree0
			,cond = cond0
		)

	}
	#these are columns added to tree0 by compileTrees
	vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT",'ba_ft','ba_ft_dbcl2','ba_ft_dbcl6','ba_ft_dbcl10','ba_ft_dbcl14','ba_ft_dbcl18','ba_ft_dbcl22','ba_ft_dbcl26','ba_ft_dbcl30','ba_ft_dbcl41','ba_ft_dbcl525','ba_ft_dbclNA','ba_ft_SPGRPCD10','ba_ft_SPGRPCD11','ba_ft_SPGRPCD12','ba_ft_SPGRPCD13','ba_ft_SPGRPCD15','ba_ft_SPGRPCD17','ba_ft_SPGRPCD18','ba_ft_SPGRPCD19','ba_ft_SPGRPCD21','ba_ft_SPGRPCD22','ba_ft_SPGRPCD24','ba_ft_SPGRPCD44','ba_ft_SPGRPCD45','ba_ft_SPGRPCD46','ba_ft_SPGRPCD47','ba_ft_SPGRPCD48'
						 ,'SPGRPCD_dbcl_10_2','SPGRPCD_dbcl_10_6','SPGRPCD_dbcl_10_10','SPGRPCD_dbcl_10_14','SPGRPCD_dbcl_10_18','SPGRPCD_dbcl_10_22','SPGRPCD_dbcl_10_26','SPGRPCD_dbcl_10_30','SPGRPCD_dbcl_10_41','SPGRPCD_dbcl_10_525','SPGRPCD_dbcl_10_NA','SPGRPCD_dbcl_11_2','SPGRPCD_dbcl_11_6','SPGRPCD_dbcl_11_10','SPGRPCD_dbcl_11_14','SPGRPCD_dbcl_11_18','SPGRPCD_dbcl_11_22','SPGRPCD_dbcl_11_26','SPGRPCD_dbcl_11_30','SPGRPCD_dbcl_11_41','SPGRPCD_dbcl_11_525','SPGRPCD_dbcl_11_NA'
						 ,'SPGRPCD_dbcl_12_2','SPGRPCD_dbcl_12_6','SPGRPCD_dbcl_12_10','SPGRPCD_dbcl_12_14','SPGRPCD_dbcl_12_18','SPGRPCD_dbcl_12_22','SPGRPCD_dbcl_12_26','SPGRPCD_dbcl_12_30','SPGRPCD_dbcl_12_41','SPGRPCD_dbcl_12_525','SPGRPCD_dbcl_12_NA','SPGRPCD_dbcl_13_2','SPGRPCD_dbcl_13_6','SPGRPCD_dbcl_13_10','SPGRPCD_dbcl_13_14','SPGRPCD_dbcl_13_18','SPGRPCD_dbcl_13_22','SPGRPCD_dbcl_13_26','SPGRPCD_dbcl_13_30','SPGRPCD_dbcl_13_41','SPGRPCD_dbcl_13_525','SPGRPCD_dbcl_13_NA'
						 ,'SPGRPCD_dbcl_15_2','SPGRPCD_dbcl_15_6','SPGRPCD_dbcl_15_10','SPGRPCD_dbcl_15_14','SPGRPCD_dbcl_15_18','SPGRPCD_dbcl_15_22','SPGRPCD_dbcl_15_26','SPGRPCD_dbcl_15_30','SPGRPCD_dbcl_15_41','SPGRPCD_dbcl_15_525','SPGRPCD_dbcl_15_NA','SPGRPCD_dbcl_17_2','SPGRPCD_dbcl_17_6','SPGRPCD_dbcl_17_10','SPGRPCD_dbcl_17_14','SPGRPCD_dbcl_17_18','SPGRPCD_dbcl_17_22','SPGRPCD_dbcl_17_26','SPGRPCD_dbcl_17_30','SPGRPCD_dbcl_17_41','SPGRPCD_dbcl_17_525','SPGRPCD_dbcl_17_NA'
						 ,'SPGRPCD_dbcl_18_2','SPGRPCD_dbcl_18_6','SPGRPCD_dbcl_18_10','SPGRPCD_dbcl_18_14','SPGRPCD_dbcl_18_18','SPGRPCD_dbcl_18_22','SPGRPCD_dbcl_18_26','SPGRPCD_dbcl_18_30','SPGRPCD_dbcl_18_41','SPGRPCD_dbcl_18_NA','SPGRPCD_dbcl_19_2','SPGRPCD_dbcl_19_6','SPGRPCD_dbcl_19_10','SPGRPCD_dbcl_19_14','SPGRPCD_dbcl_19_18','SPGRPCD_dbcl_19_22','SPGRPCD_dbcl_19_26','SPGRPCD_dbcl_19_30','SPGRPCD_dbcl_19_41','SPGRPCD_dbcl_19_525','SPGRPCD_dbcl_19_NA','SPGRPCD_dbcl_21_2'
						 ,'SPGRPCD_dbcl_21_6','SPGRPCD_dbcl_21_10','SPGRPCD_dbcl_21_14','SPGRPCD_dbcl_21_18','SPGRPCD_dbcl_21_22','SPGRPCD_dbcl_21_26','SPGRPCD_dbcl_21_30','SPGRPCD_dbcl_21_NA','SPGRPCD_dbcl_22_2','SPGRPCD_dbcl_22_6','SPGRPCD_dbcl_22_10','SPGRPCD_dbcl_22_14','SPGRPCD_dbcl_22_18','SPGRPCD_dbcl_22_22','SPGRPCD_dbcl_22_26','SPGRPCD_dbcl_22_30','SPGRPCD_dbcl_22_41','SPGRPCD_dbcl_22_525','SPGRPCD_dbcl_22_NA','SPGRPCD_dbcl_24_2','SPGRPCD_dbcl_24_6','SPGRPCD_dbcl_24_10'
						 ,'SPGRPCD_dbcl_24_14','SPGRPCD_dbcl_24_18','SPGRPCD_dbcl_24_22','SPGRPCD_dbcl_24_26','SPGRPCD_dbcl_24_30','SPGRPCD_dbcl_24_41','SPGRPCD_dbcl_24_525','SPGRPCD_dbcl_24_NA','SPGRPCD_dbcl_44_2','SPGRPCD_dbcl_44_6','SPGRPCD_dbcl_44_10','SPGRPCD_dbcl_44_14','SPGRPCD_dbcl_44_18','SPGRPCD_dbcl_44_22','SPGRPCD_dbcl_44_26','SPGRPCD_dbcl_44_30','SPGRPCD_dbcl_44_41','SPGRPCD_dbcl_44_525','SPGRPCD_dbcl_44_NA','SPGRPCD_dbcl_45_2','SPGRPCD_dbcl_45_6','SPGRPCD_dbcl_45_10'
						 ,'SPGRPCD_dbcl_45_14','SPGRPCD_dbcl_45_18','SPGRPCD_dbcl_45_22','SPGRPCD_dbcl_45_26','SPGRPCD_dbcl_45_30','SPGRPCD_dbcl_45_41','SPGRPCD_dbcl_45_NA','SPGRPCD_dbcl_46_2','SPGRPCD_dbcl_46_6','SPGRPCD_dbcl_46_10','SPGRPCD_dbcl_46_14','SPGRPCD_dbcl_46_18','SPGRPCD_dbcl_46_26','SPGRPCD_dbcl_46_41','SPGRPCD_dbcl_46_NA','SPGRPCD_dbcl_47_2','SPGRPCD_dbcl_47_6','SPGRPCD_dbcl_47_10','SPGRPCD_dbcl_47_14','SPGRPCD_dbcl_47_18','SPGRPCD_dbcl_47_22','SPGRPCD_dbcl_47_26'
						 ,'SPGRPCD_dbcl_47_30','SPGRPCD_dbcl_47_41','SPGRPCD_dbcl_47_525','SPGRPCD_dbcl_47_NA','SPGRPCD_dbcl_48_NA')


	plDat = compilePlots(

		data=fiaDat
		,dir_out = c(file.path("d:/data/RFIA/Compile/",format(Sys.Date())),NA)
		,doDebug = F
		,fnArg = list(
			vSumNm = vSumNm
			#vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT")
		)

	)


}

#only runs if outside of a function call
#this allows sourcing to re-load functions that are being debugged while inside of another function
if(identical(environment(),.GlobalEnv) & F){

	#load and fix FIA data
	if(!"tree" %in% ls() | T){

		tree = read.csv("D:\\data\\RFIA\\mergeFIA\\2019-08-26\\mergeFIA.csv")
		#cond0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\cond.rds")

	}
	compilePlots( data = list(tree = tree), return = T, tree_filter = "select * from tree where INVYR > 2013 and STATECD = 53", doDebug = F,nclus=5
								#,plotIDs = c("PLT_CN","PLOT","INVYR","STATECD","COUNTYCD","CTY_CN","PLOT_STATUS_CD","EVAL_GRP")
								,plotIDs = c("PLT_CN","PLOT","INVYR","STATECD","COUNTYCD","CTY_CN","PLOT_STATUS_CD")
	)


}



