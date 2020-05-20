#'@title
#'  Predict biomass using National Volume Estimator Library (NVEL)
#'
#'@description
#'  Predict biomass using National Volume Estimator Library (NVEL)
#'
#'@details
#'  Predict biomass using National Volume Estimator Library (NVEL)
#'
#'  see:
#'  https://www.fs.fed.us/forestmanagement/products/measurement/volume/nvel/index.php
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/19/2020 Implemented \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param dfTL tree list with a minimum of diamter and height columns
#'@param bioeq (optional) use single equation for all trees
#@param region (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#@param forest (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#@param district (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param bioeqNm  (optional) column name in DFTL: provide region, forest, district for every tree in dfTL
#@param regionNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#@param forestNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#@param districtNm (optional) column name in DFTL: provide region, forest, district for every tree in dfTL
#'@param spcdNm (required) column name in DFTL: USFS species code
#'@param dbhNm  (required) column name in DFTL: dbh in inches
#'@param htNm (required) column name in DFTL: height in feet
#@param pulpDbNm (optional) column name in DFTL: upper db miminum for pulp (in)
#'@param merchDbNm  (optional) column name in DFTL: upper db miminum for saw (in)
#'@param htPrd1Nm (optional) column name in DFTL: Height to the minimum top diameter inside bark for primary product (ft)
#'@param htPrd2Nm (optional) column name in DFTL: Height to the minimum top diameter inside bark for secondary product (ft)
#@param upHt1Nm  (optional) column name in DFTL: Upper stem height in feet where upper stem diameter was measured or where AVG1 is to be applied. OR For Region 8 the upper stem reference height defined by their profile model (ht0, ht4, ht7, ht9). OR For region 9 the UPSHT1 is the height to 7.6/9.6 top diameter when HT1PRD is not the height to 7.6/9.6 top diameter.
#@param upDb1Nm  (optional) column name in DFTL: Upper stem diameter measured at
#@param stumpHtNm  (optional) column name in DFTL: stump height (ft)
#@param fclassNm (optional) column name in DFTL: Girard’s form class.  Diameter at the top of the first log given as a percent of DBH.
#@param dbtbhNm  (optional) column name in DFTL: Double bark thickness at breast height in inches
#@param btrNm  (optional) column name in DFTL: Bark thickness ratio given as the percent of diameter inside bark to diameter outside bark.  (dib/dob *100).
#'@param dll_64  path to 64bit dll
#'@param dll_32  path to 64bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_vol name of volume prediction function call in NVEL .dll
#'@param dll_func_bioeq name of volume equation chooser function call in NVEL .dll
#'@param nclus number of cores to use
#'
#'@return
#'  a data.frame reformatted to include all of the original columns, any missing columns for the NVEL .dll, 15 columns of predicted volumes, and an error column
#'
#'
#'@examples
#'
#'         #grab list of species
#'         if(!"dfSpp" %in% ls()){
#'           library(RSQLite)
#'           db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RSForInvt"))
#'           dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
#'           dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
#'           dbDisconnect(db0)
#'         }
#'
#'         #build a fake tree list
#'         if(!"dfSpp" %in% ls()){
#'           set.seed=111
#'           nfake=length(unique(dfCoeff$species_code))
#'
#'           df_fake = data.frame(
#'             trid=1:(nfake)
#'             ,region = 6
#'             ,forest = "01"
#'             ,district = "01"
#'             ,dbh=10*abs(rnorm(nfake))
#'             ,ht=100*abs(rnorm(nfake))
#'             ,spcd = unique(dfCoeff$species_code)#'     sample(c("a","b","c","d") , nfake , T)
#'           )
#'
#'         }
#'
#'         #get volumes
#'         NVEL_volume( dfTL = df_fake )
#'
#'
#'
#'@import plyr
#'
#'@export
#
<<<<<<< HEAD
#'@seealso \code{\link{NVEL_voleq}}\cr  \code{\link{NVEL_volume}}\cr
=======
#'@seealso \code{\link{NVEL_bioeq}}\cr
>>>>>>> 505cc0d6b77f2efa63cd656e05e7f91f860843d1

#Desired upgrades to this function:
#
#

NVEL_biomass=function(

  dfTL = data.frame(dbh=15,ht=65,cl=30)

  #force a specific biomass equation / geosubregion for every tree
  ,bioeq = NA
  ,geosub = NA

  #optional, provide biomass equation for every tree
  ,bioeqNm = c("bioeq")
  ,geosubNm = "geosub"
  ,spcdNm = "spcd"
  ,dbhNm = "dbh"
  ,htNm = "ht"
  ,stemsNm = "stems"
  ,clNm = "cl"
  ,merchDbNm = c("merchDb")
  ,htPrd1Nm = c("htPrd1")
  ,htPrd2Nm = c("htPrd2")
  ,cv4Nm = "cv4"

  ,dll_64 = 'lib/vollib-64bits/vollib.dll'
  ,dll_32 = 'lib/vollib-32bits/vollib.dll'
  ,load_dll = T

  ,dll_func_bio = "biomasslib_r"

  #,ncore = 4

){
  options(stringsAsFactors = F)
  #load dll if needed
  if(load_dll) .load_dll(dll_64,dll_32,dll_func )

  #test for existence of bioeq
  # get_bioeq = T
  # if(!is.na(bioeq)) get_bioeq = F
  # if(!is.na(bioeqNm)[1]) if((bioeqNm)[1] %in% names(dfTL)) if( sum(sapply(dfTL[,bioeqNm],nchar)==0 ) == 0 ) get_bioeq = F

  #deal with column names, add all columns / rename
  dfTL0_in = .formatTL2NVEL_bio(
    dfTL0 = dfTL
    ,bioeq  = bioeq[1]
    ,geosub = geosub[1]
    ,bioeqNm = bioeqNm[1]
    ,geosubNm = geosubNm[1]
    ,spcdNm = spcdNm[1]
    ,dbhNm  = dbhNm[1]
    ,htNm  = htNm[1]
    ,stemsNm =  stemsNm
    ,clNm = clNm[1]
    ,merchDbNm = merchDbNm
    ,htPrd1Nm = htPrd1Nm
    ,htPrd2Nm = htPrd2Nm
    ,cv4Nm = cv4Nm
  )

  #compute biomass
  #turn of warnings temporarily, this generates scads of them
  defaultW <- getOption("warn")
  options(warn = -1)
  bio_pd0  = mapply(.fn_fortran_bio

                   ,bioeq = dfTL0_in[,bioeqNm[1]]
                   ,geosub = dfTL0_in[,geosubNm[1]]

                   ,spcd = dfTL0_in[,spcdNm[1]]
                   ,dbh  = dfTL0_in[,dbhNm[1]]
                   ,ht  = dfTL0_in[,htNm[1]]
                   ,stems  = dfTL0_in[,stemsNm[1]]
                   ,cl =  dfTL0_in[,clNm[1]]
                   ,merchDb = dfTL0_in[,merchDbNm[1]]
                   ,htPrd1 = dfTL0_in[,htPrd1Nm[1]]
                   ,htPrd2 = dfTL0_in[,htPrd2Nm[1]]
                   ,cv4 =   dfTL0_in[,cv4Nm[1]]

                   ,MoreArgs=list(dll_func_bio=dll_func_bio)
                   ,SIMPLIFY = F)
  #turn warnings back on
  options(warn = defaultW)

  #merge predictions together and add their names
  bio_pd0_df = plyr::rbind.fill(bio_pd0 )
  #names(bio_pd0_df )[1:2] = c("GRNBIOGRN","DRYBIO")

  #return formatted tree list with predicted volumes
  data.frame(dfTL0_in,bio_pd0_df )

}

#load dll if needed
.load_dll = function(dll_64,dll_32,dll_func ){

  arch_in = R.Version()$arch
  loaded_dlls_in = names(getLoadedDLLs())
  dll_loaded = "vollib" %in% loaded_dlls_in
  if(arch_in == "x86_64" & !dll_loaded) dyn.load(system.file(dll_64, package="RSForInvt"))
  if(arch_in == "x86_32" & !dll_loaded) dyn.load(system.file(dll_32, package="RSForInvt"))

}

#call fortran
.fn_fortran_bio = function(
  bioeq
  ,geosub
  ,spcd
  ,dbh
  ,ht
  ,stems
  ,cl
  ,merchDb
  ,htPrd1
  ,htPrd2
  ,cv4

  ,dll_func_bio
){


  res_bio0 =.Fortran(
    dll_func_bio
    ,as.character(bioeq)
    ,as.double(dbh)
    ,as.double(ht)
    ,as.double(cl)
    ,as.double(htPrd1)
    ,as.double(htPrd2)
    ,as.double(cv4)
    ,as.double(merchDb)
    ,as.integer(stems)
    ,as.double(0)
    ,as.double(0)
    ,as.integer(0)
    ,as.integer(spcd)
    ,as.character(geosub)

    ,PACKAGE = "vollib"
  )
  #return
  data.frame(biogrn = res_bio0[[10]] , biodry = res_bio0[[11]]  , err=res_bio0[[12]])


}



#data tree list and format it for these functions
#typically not needed by users
.formatTL2NVEL_bio = function(
  dfTL0

  ,bioeq
  ,geosub

  ,bioeqNm
  ,geosubNm

  ,spcdNm
  ,dbhNm
  ,htNm
  ,stemsNm
  ,clNm
  ,merchDbNm
  ,htPrd1Nm
  ,htPrd2Nm
  ,cv4Nm

){

  dfTL1 = dfTL0

  nms_in = c(
    bioeq= bioeqNm[1]
    ,geosub = geosubNm[1]
    ,spcd = spcdNm[1]
    ,dbh  = dbhNm[1]
    ,ht  = htNm[1]
    ,stems = stemsNm[1]
    ,cl = clNm[1]
    ,merchDb = merchDbNm[1]
    ,htPrd1 = htPrd1Nm[1]
    ,htPrd2 = htPrd2Nm[1]
    ,cv4 = cv4Nm[1]
  )


  #add columns with na in nms
  na_nms = is.na(nms_in)
  nms_fill_NA = names(nms_in)[na_nms]
  dfTL1[,nms_fill_NA] = 0

  #add missing columns
  na_nms1 =! nms_in %in% names(dfTL1)
  nms_fill_NA1 = names(nms_in)[na_nms1]
  dfTL1[,nms_fill_NA1] = 0

  #update existing column names with fixed column names
  nms_in[na_nms] = names(nms_in)[na_nms]
  nms_in[na_nms1] = names(nms_in)[na_nms1]
  names(dfTL1[,nms_in]) = names(nms_in)

  #overwrite names
  if(!is.na(bioeq)) dfTL1[,"bioeq"] = bioeq
  if(!is.na(geosub)) dfTL1[,"geosub"] = geosub

  #return correctly named data
  dfTL1[,names(nms_in)]

}



if(F){

  library(RSForInvt)

  if(!"dfSpp" %in% ls()){

    library(RSQLite)
    db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RSForInvt"))
    dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
    dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
    dbDisconnect(db0)

    set.seed=111
    nfake=length(unique(dfCoeff$species_code))

    df_fake = data.frame(
      trid=1:(nfake)
      ,region = 6
      ,forest = "01"
      ,district = "01"
      ,dbh=10*abs(rnorm(nfake))
      ,ht=100*abs(rnorm(nfake))
      ,spcd = unique(dfCoeff$species_code)# sample(c("a","b","c","d") , nfake , T)
    )

  }

  NVEL_biomass( dfTL = df_fake , bioeq = "AFF019AST01D" )

}
