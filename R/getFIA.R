#'@title
#'  Get FIA data from citrix (Make Sure You Are Using 32bit R)
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
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param nims_channel <Delete and Replace>
#'@param dbqrys oracle slq querys to grab tables. Must name queries (tr,pl_snp,cond,coords,wts)
#'@param dir_out where to save records for individual tables
#'@param return should the data be returned after the function call
#'@param put_data_parent_env should the data be placed in the parent environment where the call was made from
#'@param stringsAsFactors should strings be converted to factors
#'
#'@return
#'  nothing or a list of data frames
#'
#'@examples
#'
#'  #Make Sure You Are Using 32bit R
#'
#' library(RODBC)
#' chn1 = odbcConnect("FIA01P")
#'
#' getFIA(chn1)
#'
#' res = getFIA(return = T)
#' head(res[["tr"]])
#'
#' getFIA(return = T)
#' head(tr)
#'
#'  #get an idea of what each table looks like
#'  getFIA(chn1
#'				,dbqrys=c(
#'					tr = 'fs_nims_pnwrs.nims_tree_vw where rownum < 50'
#'					,pl_snp = 'fs_nims_pnwrs.nims_plotsnap_vw where rownum < 50'
#'					,cond = 'fs_nims_pnwrs.nims_cond_vw where where rownum < 50'
#'					,coords = 'FS_NIMS_PNWRS.NIMS_GPS_PNWRS where rownum < 50'
#'					,wts = 'fs_nims_pnwrs.nims_pop_expall_vw where where rownum < 50'
#'				))
#'
#'
#'@import some_package,some_package2
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

getFIA = function(
	nims_channel
	,dbqrys=c(
		tr = 'select * from fs_nims_pnwrs.nims_tree_vw where statecd > -1 and invyr > -1 and COUNTYCD < 10E6 and rownum < 50'
		,pl_snp = 'select * from fs_nims_pnwrs.nims_plotsnap_vw where statecd > -1 and invyr > -1 and COUNTYCD < 10E6 and rownum < 50'
		,cond = 'select * from fs_nims_pnwrs.nims_cond_vw where statecd > -1 and invyr > -1 and COUNTYCD < 10E6 and rownum < 50'
		,coords = 'select * from FS_NIMS_PNWRS.NIMS_GPS_PNWRS where statecd > -1 and SAMPLE_YEAR > -1 and COUNTYCD < 10E6 and rownum < 50'
		,wts = 'select * from fs_nims_pnwrs.nims_pop_expall_vw where statecd > -1 and invyr > -1 and COUNTYCD < 10E6 and rownum < 50'
	)
	,dir_out = c(file.path("c:/data/RFIA/NIMS",format(Sys.Date())),NA)
	,return = F
	,put_data_parent_env = F
	,stringsAsFactors = F
){


	#create output directory
	dir_out = dir_out[1]
	if(!is.null(dir_out))if(!is.na(dir_out))if(!dir.exists(dir_out)) dir_err = try(dir.create(dir_out,recursive=T))
	if(!dir.exists(dir_out)) dir_out = choose.dir(caption = "dir_out does not exist or cannot be made")
	if(!dir.exists(dir_out)) stop("dir_out not provided or not accessible")
	if(return) r_list() = list()

	#read data
	for(i in 1:length(dbqrys)){

		qryi = dbqrys[i]
		tbi = sqlQuery(nims_channel , qryi , stringsAsFactors = stringsAsFactors)
		if(put_data_parent_env) assign(dbqrys[i] , tbi , envir = parent.env(environment()))
		if(return) r_list[[dbqrys[i]]] = tbi
		tbi_csv = file.path(dir_out,paste(names(dbqrys)[i],".csv",sep=""))
		tbi_rds = file.path(dir_out,paste(names(dbqrys)[i],".rds",sep=""))
		tbi_qry = file.path(dir_out,paste(names(dbqrys)[i],".sql",sep=""))
		write.csv(tbi,tbi_csv)
		saveRDS(tbi,tbi_rds)
		writeLines(qryi,tbi_qry)
	}

	if(return) return(r_list)

}

if(F){
	library(RODBC)
	chn1 = odbcConnect("FIA01P")

	getFIA(chn1
					 ,dbqrys=c(
					 	tr = 'select * from (select * from fs_nims_pnwrs.nims_tree_vw order by plt_cn) where statecd = 53 and invyr > 2013 and invyr < 2050 and rownum < 5000'
					 	,pl_snp = 'select * from (select * from fs_nims_pnwrs.nims_plotsnap_vw order by cn) where statecd = 53 and invyr > 2013 and invyr < 2050'
					 	,cond = 'select * from (select* from fs_nims_pnwrs.nims_cond_vw order by plt_cn) where statecd = 53 and invyr > 2013 and invyr < 2050'
					 	,coords = 'select * from (select * from ANL_PNW_FIA_PGM.PGM_BEST_COORDS_VW order by plt_cn) where statecd = 53 and SAMPLE_YEAR > 2013 and SAMPLE_YEAR < 2050'
					 	,wts = 'select * from (select * from fs_nims_pnwrs.nims_pop_expall_vw order by plt_cn) where statecd = 53'
					 ))

	getFIA(chn1
					 ,dbqrys=c(
					 	tr1 = 'select * from (select * from fs_nims_pnwrs.nims_tree_vw order by plt_cn) where statecd = 53 and invyr > 2013 and invyr < 2050 '
					 )
	)

	getFIA(chn1
					 ,dbqrys=c(
					 	wts = 'select * from fs_nims_pnwrs.nims_pop_expall_vw where rownum < 1000'
					 )
	)

	getFIA(chn1
					 ,dbqrys=c(
					 	tr = 'fs_nims_pnwrs.nims_tree_vw where rownum < 50'
					 	,pl_snp = 'fs_nims_pnwrs.nims_plotsnap_vw where rownum < 50'
					 	,cond = 'fs_nims_pnwrs.nims_cond_vw where rownum < 50'
					 	,coords = 'ANL_PNW_FIA_PGM.PGM_BEST_COORDS_VW where rownum < 50'
					 	,wts = 'fs_nims_pnwrs.nims_pop_expall_vw where rownum < 50'
					 ))

}


