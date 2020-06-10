#'@name compileTrees
#'@title
#'  Facilitate computation of tree level attributes (ba, volume, etc.)
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
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
#'1.0 \tab 6/10/2020 Created  \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param tlDF data frame of tree records
#'@param fnCompute sequential list of functions to
#'apply to tree data, earlier results (e.g. ba) are available to later
#'functions. Every function should accept an elipsis
#'@param ... arguments to functions in fnCompute

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
#'     df_fake = data.frame(trID=1:50
#'                        ,db=10*abs(rnorm(nfake))
#'                        ,ht=100*abs(rnorm(nfake))
#'                        ,spp = sample(c("a","b","c","d") , nfake , T)
#'     )
#'
#'    #compile fake datase
#'     test =
#'     compileTrees(
#'       df_fake
#'       ,trID = "trID"
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
#@import reshape2
#'

#@seealso \code{\link{dcast}}\cr \code{\link{melt}}\cr

#updates to do:
#

#'@export
#'@rdname compileTrees
compileTrees=function(

  tlDF
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
      tpa
      ,ba_ft
      ,dbcl
      ,dbclY
      ,sppY
      ,dbclSppY
  )

  ,...

){

  tlDF_in = tlDF

  #iterate through computers and assign names or use internal DF names
  for(i in 1:length(fnCompute)){

    fni = fnCompute[[i]]
    tlDF_in = fni(tlDF_in,...)
    if(class(tlDF_in) != "data.frame") stop("All functions provided in 'fnsCompute = list()' argument must return a dataframe composed of tlDF and any new columns created.")

  }

  return(tlDF_in)


}

#'@export
#'@rdname compileTrees
ba_ft = function(x,dbNm,...) data.frame(x, ba_ft = 0.005454 * (x[,dbNm]^2))

tpa = function(x,acresNm,nTreesNm,...){
  if(is.na(nTreesNm))  data.frame(x, TPA = 1 / x[,acresNm])
  if(!is.na(nTreesNm))  data.frame(x, TPA = x[,nTreesNm]  / x[,acresNm] )
}

tph = function(x,haNm,nTreesNm,...){
  if(is.na(nTreesNm))  data.frame(x, TPH = 1 / x[,haNm])
  if(!is.na(nTreesNm))  data.frame(x, TPH = x[,nTreesNm]  /x[,acresNm] )
}

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
    mi = reshape2::melt(x_in[,c(trID,dbclNm,dbclY[i])],id.vars=c(trID,dbclNm) )
    fi = as.formula(paste("variable +",trID,"~",dbclNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
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
    mi = reshape2::melt(x_in[,c(trID,sppNm,sppY[i])],id.vars=c(trID,sppNm) )
    fi = as.formula(paste("variable +",trID,"~",sppNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(sppY[i], paste(sppNm,names(dfi)[-1],sep="_"),sep="_")

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
    mi = reshape2::melt(x_in[,c(trID,sppNm,dbclNm,sppY[i])],id.vars=c(trID,sppNm,dbclNm) )

    #append spp and dbcl to improve readability of final columns
    mi[,sppNm] = paste(sppNm,mi[,sppNm],sep="_")
    mi[,dbclNm] = paste(dbclNm,mi[,dbclNm],sep="_")

    #merge data
    fi = as.formula(paste("variable +",trID,"~",sppNm,"+",dbclNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]

    #merge back in
    x_in = merge(x_in, dfi,  by = trID)
  }
  return(x_in)
}

#test this code
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
          tpa
          ,ba_ft
          ,dbcl
          ,dbclY
          ,sppY
          ,dbclSppY
        )
    )

    test

}

