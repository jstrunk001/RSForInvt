#'@name compileTrees
#'@title
#'  Compile individual tree attributes
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
#'@param tr data frame of tree records
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
#'
#'     #create a fake dataset
#'     set.seed=111
#'     nfake=50
#'     df_fake = data.frame(trid=1:50
#'                        ,db=10*abs(rnorm(nfake))
#'                        ,ht=100*abs(rnorm(nfake))
#'                        ,spp = sample(c("a","b","c","d") , nfake , T)
#'     )
#'
#'    #compile fake datase
#'     test =
#'     compileTrees(
#'       df_fake
#'       ,trID = "trid"
#'       ,sppNm = "spp"
#'       ,dbNm = "db"
#'       ,htNm = "ht"
#'       ,dbclNm = "dbcl"
#'       ,dbcl = c(seq(0,32,4),50,1000)
#'       ,dbclY = c("ba_ft")
#'       ,sppY = c("ba_ft")
#'       ,sppDbclY = c("ba_ft")
#'
#'       ,fnCompute =
#'         list(
#'           ba_ft
#'           ,dbcl
#'           ,dbclY
#'           ,sppY
#'           ,dbclSppY
#'         )
#'     )
#'
#@import some_package,some_package2
#'
#'@export
#'@rdname compileTrees
#@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#updates to do:
#	enable sql queries agains each input table
compileTrees=function(

	tr
	# ,trID = "CN"
	# ,sppNm = "SPGRPCD"
	# ,dbNm = "DIA"
	# ,htNm = "HT"
	# ,dbclNm = "dbcl"
	# ,dbcl = c(seq(0,32,4),50,1000)
	# ,dbclY = c("ba_ft")
	# ,sppY = c("ba_ft")
	# ,sppDbclY = c("ba_ft")


	,fnCompute = list(

	  ba_ft

	 )

 ,...

){



	tr_in = tr

	#iterate through computers and assign names or use internal DF names
	for(i in 1:length(fnCompute)){

		fni = fnCompute[[i]]
		tr_in = fni(tr_in,...)
		#if(class(resi) == "data.frame") tr_in = data.frame(tr_in, resi)
		#else tr_in[, names(fnCompute)[i]] = resi
	}

	return(tr_in)


}

#'@export
#'@rdname compileTrees
ba_ft = function(x,dbNm,...) data.frame(x, ba_ft = 0.005454 * (x[,dbNm]^2))

#'@export
#'@rdname compileTrees
dbcl = function(x , dbNm="dbh" , dbcl=c(seq(0,32,4),50,1000) , dbclNm = "dbcl", ...){
  labelsDBCL = (dbcl[-1] + dbcl[-length(dbcl)]) / 2
  res_dbcl = data.frame(labelsDBCL[cut(x[,dbNm],dbcl,labels=FALSE)])
  names(res_dbcl) = dbclNm
  res_df = data.frame(x,res_dbcl)
  return(res_df)
}

#'@export
#'@rdname compileTrees
dbclY = function(x,trID,dbclNm,dbclY,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(dbclY)){

    #cross dbcl with response attributes
    mi = melt(x_in[,c(trID,dbclNm,dbclY[i])],id.vars=c(trID,dbclNm) )
    fi = as.formula(paste("variable +",trID,"~",dbclNm))
    dfi = dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(dbclY[i], paste(dbclNm,names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi, by = trID)
  }
  return(x_in)
}

#'@export
#'@rdname compileTrees
sppY = function(x,trID,sppY,sppNm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(sppY)){

    #cross dbcl with response attributes
    mi = melt(x_in[,c(trID,sppNm,sppY[i])],id.vars=c(trID,sppNm) )
    fi = as.formula(paste("variable +",trID,"~",sppNm))
    dfi = dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(sppY[i], paste(sppNm,names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = trID)
  }
  return(x_in)
}

#'@export
#'@rdname compileTrees
dbclSppY = function(x,trID,sppY,dbclNm,sppNm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(sppY)){

    #cross dbcl with response attributes

    mi = melt(x_in[,c(trID,sppNm,dbclNm,sppY[i])],id.vars=c(trID,sppNm,dbclNm) )


    fi = as.formula(paste("variable +",trID,"~",sppNm,"+",dbclNm))
    dfi = dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(sppNm,dbclNm,names(dfi)[-1],sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = trID)
  }
  return(x_in)
}


if(F){

  set.seed=111
  nfake=50
  df_fake = data.frame(trid=1:50
                       ,db=10*abs(rnorm(nfake))
                       ,ht=100*abs(rnorm(nfake))
                       ,spp = sample(c("a","b","c","d") , nfake , T)
                      )

  test =
  compileTrees(
                df_fake
               ,trID = "trid"
               ,sppNm = "spp"
               ,dbNm = "db"
               ,htNm = "ht"
               ,dbclNm = "dbcl"
               ,dbcl = c(seq(0,32,4),50,1000)
               ,dbclY = c("ba_ft")
               ,sppY = c("ba_ft")
               ,sppDbclY = c("ba_ft")

               ,fnCompute =
                 list(
                   ba_ft
                   ,dbcl
                   ,dbclY
                   ,sppY
                   ,dbclSppY
                   )
               )


}



#
#
#
# defComputeTL = list(
#
# 	ba_ft = function(x,dbNm) data.frame(x, ba_ft = 0.005454 * (x[,args[["dbNm"]]]^2))
#
# 	,dbcl = function(x,args,...){
# 		labelsDBCL = (args[["dbcl"]][-1] + args[["dbcl"]][-length(args[["dbcl"]])]) / 2
# 		res_dbcl = data.frame(labelsDBCL[cut(x[,args[["dbNm"]]],args[["dbcl"]],labels=FALSE)])
# 		names(res_dbcl) = args[["dbclNm"]]
# 		res_df = data.frame(x,res_dbcl)
# 		return(res_df)
# 	}
#
# 	,dbclY = function(x,args,...){
#
# 		require("reshape2")
# 		x_in = x
#
# 		for(i in 1:length(args[["dbclY"]])){
#
# 			#cross dbcl with response attributes
# 			mi = melt(x_in[,c(args[["trID"]],args[["dbclNm"]],args[["dbclY"]][i])],id.vars=c(args[["trID"]],args[["dbclNm"]]) )
# 			fi = as.formula(paste("variable +",args[["trID"]],"~",args[["dbclNm"]]))
# 			dfi = dcast(mi, formula =  fi)[,-1]
# 			names(dfi)[-1] = paste(args[["dbclY"]][i], paste(args[["dbclNm"]],names(dfi)[-1],sep=""),sep="_")
#
# 			#merge back in
# 			x_in = merge(x_in, dfi, by = args[["trID"]])
# 		}
#
# 		return(x_in)
#
# 	}
#
# 	,sppY = function(x,args,...){
#
# 		require("reshape2")
# 		x_in = x
#
# 		for(i in 1:length(args[["sppY"]])){
#
# 			#cross dbcl with response attributes
# 			mi = melt(x_in[,c(args[["trID"]],args[["sppNm"]],args[["sppY"]][i])],id.vars=c(args[["trID"]],args[["sppNm"]]) )
# 			fi = as.formula(paste("variable +",args[["trID"]],"~",args[["sppNm"]]))
# 			dfi = dcast(mi, formula =  fi)[,-1]
# 			names(dfi)[-1] = paste(args[["sppY"]][i], paste(args[["sppNm"]],names(dfi)[-1],sep=""),sep="_")
#
# 			#merge back in
# 			x_in = merge(x_in, dfi,  by = args[["trID"]])
# 		}
# 		return(x_in)
# 	}
#
# 	,dbclSppY = function(x,args,...){
#
# 		require("reshape2")
# 		x_in = x
#
# 		for(i in 1:length(args[["sppY"]])){
#
# 			#cross dbcl with response attributes
#
# 			mi = melt(x_in[,c(args[["trID"]],args[["sppNm"]],args[["dbclNm"]],args[["sppY"]][i])],id.vars=c(args[["trID"]],args[["sppNm"]],args[["dbclNm"]]) )
#
#
# 			fi = as.formula(paste("variable +",args[["trID"]],"~",args[["sppNm"]],"+",args[["dbclNm"]]))
# 			dfi = dcast(mi, formula =  fi)[,-1]
# 			names(dfi)[-1] = paste(args[["sppNm"]],args[["dbclNm"]],names(dfi)[-1],sep="_")
#
# 			#merge back in
# 			x_in = merge(x_in, dfi,  by = args[["trID"]])
# 		}
# 		return(x_in)
# 	}
# )
#
# if(F){
#
# 	if(!"tr1" %in% ls()) tr1 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\tr.rds")
# 	if(!"tr2" %in% ls()) tr2 = compileTrees(tr1)
#
# }
